library(jiebaR)
setwd('d:/ihong/work/ptt/tm/')
## 接資料庫進來
library(RODBC)
# connect to MS-SQL db, 
conndb <- odbcDriverConnect("cxcxxxcs")

##最威現金卡台新gogo
PTT <- sqlQuery(conndb,"select [內文] from External.dbo.PTT
                where [看版] like 'creditcard'
                  and
                    [標題] like '%中信%'
                  and
	                  [發文日期] between
		                  getdate()-60
                    and
		                  getdate()
                order by [發文日期] DESC "
                )

## 引進tmcn/tm/ ### 作斷詞處理/清理
library(tm)
library(tmcn)
mixseg = worker(stop_word = '../data/stop_words.txt')
xtext = PTT$內文
xtext = as.character(xtext)


cnreader<-function(elem,language,id)
{
  #進行分詞
  words <- mixseg[elem$content]
  #合併新詞為文檔該文檔可被tm識別
  ncon <- paste(words,collapse=" ")
  PlainTextDocument(ncon, id=id,language=language)
}

##### 正確調用reader(並斷詞完畢)####
corp0 = VCorpus(VectorSource(xtext),
                readerControl = list(reader=cnreader))

as.character(corp0[[1]])

# corp0 = Corpus(VectorSource(xtext))
corp2 = tm_map(corp0, stripWhitespace)
corp2 = tm_map(corp2, removePunctuation)
corp2 = tm_map(corp2, removeNumbers)
removeURL = function(x) gsub("http[[:alnum:]]*", "", x)
corp2 = tm_map(corp2, removeURL)
corp2 = tm_map(corp2,removeNext)
corp2 = tm_map(corp2,removespace)
corp2 = tm_map(corp2, PlainTextDocument)
corp2 = tm_map(corp2, removeWords,stopwords("english"))
stopwords.jieba = readLines('../data/stop_words.txt')
sw = c(iconv(stopwords.jieba,'utf8','utf8'),'imgur','http','額度','申請','申辦','卡片',
       '證明','年資','客服','職業','證明','卡','卡別','辦卡','加辦',
       '台新','國泰','世華','永豐','中信','玉山','花旗','富邦','元大',
       '工作','類別','之財力','查詢','財力','回饋','活動','消費','金額',
       '優惠','元','金額','刷卡','送','月','期間','華華','華南','心得',
       '電話','之')

corp2 = tm_map(corp2,removeWords,sw)
corp2 = tm_map(corp2, function(word){gsub("[0-9]","",word)})

corp2 = tm_map(corp2, PlainTextDocument)
corp3 = Corpus(VectorSource(corp2))

# mixseg = worker()
###  tdm / tfidf

tdm = TermDocumentMatrix(corp3,control = list(wordLengths=c(2,Inf)))
dtm = t(tdm)

######################################################################
# 文字雲 wordcloud
######################################################################

#畫出在所有文件中出現次數超過20次的詞
# library(wordcloud)
# 
# m1 = as.matrix(tdm)
# v  = sort(rowSums(m1), decreasing = TRUE)
# d  = data.frame(word = names(v), freq = v)
# 
# wordcloud(d$word, d$freq, min.freq = 20, 
#           random.order = F, ordered.colors = F, 
#           colors = rainbow(length(row.names(m1)))
#           )
# 
# 

#######計算tfidf平均值 ################################3
# inspect(dtm[1:10,20:30])

tt = tdm$dimnames$Terms
# 
tt2 = gsub(" ","",tt)
tt3 = gsub("\n","",tt2)
tdm$dimnames$Terms = tt3
tfidf = apply(tdm,1,mean)  # row = docs, col = terms
tfidfsort = sort(tfidf,decreasing=T)
tfidfsort[1:10]
tfidf.ten = tfidfsort[1:10]
barplot(tfidf.ten)
tfidf.ten


###########################################################
# tdm$dimnames$Docs = sapply(PTT$標題,substr,1,4)
inspect(tdm[1:3,1:3])

##############k-means####################################
tdm2 = removeSparseTerms(tdm, sparse=0.95)

m2 = as.matrix(tdm2)
m3 = t(m2) ## dtm
set.seed(101)
k = 5
result = kmeans(m3, k)
# cluster centers
# round(result$centers, digits=2)

# K-Means :查看每一群頻率最高的三 篇文章
for (i in 1:k) {
  cat(paste("cluster ", i, ": ", sep=""))
  s <- sort(result$centers[i,], decreasing=T)
  cat(names(s)[1:5], "\n")
}


##############################################################
## Correlation 
##############################################################
findAssocs(t(tdm2),"玉山",corlimit = 0.3)
library(Rgraphviz)
dtm2 <- t(tdm2)
findFreqTerms(dtm2,lowfreq = 50)[1:5]
dtm2
# plot(dtm2,   ### 中文無法正確顯示
#      terms=findFreqTerms(dtm2, lowfreq=50)[1:5],
#      corThreshold=0.7)
# result$cluster


###############################################################
# hclust
###############################################################
tdm2 = removeSparseTerms(tdm, sparse=0.85)
dtm2 = t(tdm2)
m2 = as.matrix(tdm2)
m22 = as.matrix(dtm2)
# cluster term
distMatrix = dist(scale(m2))
fit = hclust(distMatrix,method = "ward.D2")
plot(fit,hang=-1)
rect.hclust(fit,k=5)

row.names(m2)

###############################
## LDA topic model
###############################
library(topicmodels)

#' Convert the output of a topicmodels Latent Dirichlet Allocation to JSON
#' for use with LDAvis
#'
#' @param fitted Output from a topicmodels \code{LDA} model.
#' @param corpus Corpus object used to create the document term
#' matrix for the \code{LDA} model. This should have been create with
#' the tm package's \code{Corpus} function.
#' @param doc_term The document term matrix used in the \code{LDA}
#' model. This should have been created with the tm package's 
#' \code{DocumentTermMatrix} function.
#'
#' @seealso \link{LDAvis}.
#' @export

myDtm <- DocumentTermMatrix(corp3, 
                            control = list(wordLengths=c(2,Inf), weighting=weightTf))

myDtm2 <- removeSparseTerms(myDtm, sparse=0.85)

rowTotals <- apply(myDtm2, 1, sum)
dtm2 <- myDtm2[rowTotals>0,]

dtm_LDA <- LDA(dtm2, 5)
str(dtm_LDA)

terms(dtm_LDA)
topics(dtm_LDA)

topicmodels_json_ldavis <- function(fitted, corpus, doc_term){
  # Required packages
  library(topicmodels)
  library(dplyr)
  library(stringi)
  library(tm)
  library(LDAvis)
  
  # Find required quantities
  phi <- posterior(fitted)$terms %>% as.matrix
  theta <- posterior(fitted)$topics %>% as.matrix
  vocab <- colnames(phi)
  doc_length <- vector()
  for (i in 1:length(corpus)) {
    temp <- paste(corpus[[i]]$content, collapse = ' ')
    doc_length <- c(doc_length, stri_count(temp, regex = '\\S+'))
  }
  temp_frequency <- inspect(doc_term)
  freq_matrix <- data.frame(ST = colnames(temp_frequency),
                            Freq = colSums(temp_frequency))
  rm(temp_frequency)
  
  # Convert to json
  json_lda <- LDAvis::createJSON(phi = phi, theta = theta,
                                 vocab = vocab,
                                 doc.length = doc_length,
                                 term.frequency = freq_matrix$Freq)
  
  return(json_lda)
}
topicmodels_json_ldavis(dtm_LDA,dtm2)
###########################
# Network of Terms
###########################
# tdm = TermDocumentMatrix(corp3,
#                          control=list(wordLengths=c(1,Inf)))
tdm2 = removeSparseTerms(tdm,0.8)
c3 = as.matrix(tdm2)
c4 = c3%*% t(c3)
library(igraph)

# remove loops
g = graph.adjacency(c4, weighted=T,mode="undirected")

# remove loops
g = simplify(g)
# set labels and degrees of vertices
V(g)$label = V(g)$name
V(g)$degree = degree(g)
# set seed to make the layout reproducible
set.seed(3952)
layout1 = layout.fruchterman.reingold(g)
# plot(g, layout=layout1)
# 
V(g)$label.cex <- 0.8 * V(g)$degree / max(V(g)$degree)+ .2
V(g)$label.color <- rgb(0, 0, .2, .8)
V(g)$frame.color <- NA
egam <- (log(E(g)$weight)+.4) / max(log(E(g)$weight)+.4)
E(g)$color <- rgb(.5, .5, 0, egam)
E(g)$width <- egam
# plot the graph in layout1
plot(g, layout=layout1)






###################################
### Two mode network
####################################

# create a graph
g <- graph.incidence(tdm2, mode=c("all"))
## 移除超大關鍵字
# idx <- which(dimnames(tdm2)$Terms %in% c("優惠", "回饋", "gogo","richart"))
idx <- which(dimnames(tdm2)$Terms %in% c(""))
M <- tdm2[-idx,]
# get index for term vertices and tweet vertices
nTerms <- nrow(M)
nDocs <- ncol(M)
idx.terms <- 1:nTerms
idx.docs <- (nTerms+1):(nTerms+nDocs)
# set colors and sizes for vertices
V(g)$degree <- degree(g)
V(g)$color[idx.terms] <- rgb(0, 1, 0, .5)
V(g)$size[idx.terms] <- 0.5
V(g)$color[idx.docs] <- rgb(1, 0, 0, .4)
V(g)$size[idx.docs] <- 0.5
V(g)$frame.color <- NA
# set vertex labels and their colors and sizes
V(g)$label <- V(g)$name
V(g)$label.color <- rgb(0, 0, 0, 0.5)
V(g)$label.cex <- 0.5*V(g)$degree/max(V(g)$degree) + 1
# set edge width and color
E(g)$width <- .5
E(g)$color <- rgb(.5, .5, 0, .3)

set.seed(101)
plot(g, layout=layout.fruchterman.reingold)



##########################################################################
##  斷詞練習區
############################################################################
# inspect(tdm)
## 按照不同行切詞,
cutter <- worker(bylines = TRUE)
cut_result2<-cutter[c("這是一種很棒的選擇","對你我來說都是好事")]
cut_result2[[2]]
cut_result2

## key workers

keyworker = worker("keywords")
cutter = worker()
vector_keywords(cutter["这是一个比较长的测试文本。"],keyworker)
vector_keywords(c("今天","天气","真的","十分","不错","的","感觉"),keyworker)

## tag
words = "我愛臺灣"
tagger = worker("tag",dict = '../dict/dict.txt.big')
# cc[words]
tagger[words]


## keywords 關鍵字提取
keys = worker("keywords", topn = 2)
keys <= "我愛台北西門町"

keyworker = worker("keywords")
cutter = worker()
vector_keywords(cutter["这是一个比较长的测试文本。"],keyworker)
