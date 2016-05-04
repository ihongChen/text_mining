## install package

library(jiebaR)
library(dplyr)
library(RMySQL)

# connect to MySQL db, 
m <- dbDriver("MySQL")
con<-dbConnect(m,user='mcm',password='welcome323',host='omega',dbname='yeast');
mydb <- dbConnect(m,user='ehome4829',
                  password='a126234829',
                  host='ihongchen.ctbx4pq8or72.us-west-2.rds.amazonaws.com',
                  port=3306,
                  dbname='test');

## must set to be UTF8 setting, else ???? occur
dbSendQuery(mydb,"SET NAMES utf8")

dbGetQuery(mydb,"show variables like 'character_set_%'")
res <- dbGetQuery(mydb,"select `內文` from PTT limit 1;")

## segmentation
setwd("~/Dropbox/work/text_mining/")
cc <- worker(stop_word = '../stop_words.txt',user = '../userdict.txt',dict = '../dict.txt.big')
text.seg <- cc[as.character(res$內文)]
doc.list <-as.list(text.seg)
## pre-processing, stop words,
pat <- "[^0-9.]"
text.seg<-grep(pat,text.seg,value=TRUE)
term.tables<-table(text.seg)
term.tables <- sort(term.tables, decreasing = TRUE)

del <- term.tables < 2| nchar(names(term.tables))<2
term.tables_final <- term.tables[!del]

vocab <- names(term.tables_final)

## word

get.terms <- function(x) {
  index <- match(x, vocab)  # 获取词的ID
  index <- index[!is.na(index)]  #去掉没有查到的，也就是去掉了的词
  rbind(as.integer(index - 1), as.integer(rep(1, length(index))))   #生成上图结构
}
documents <- lapply(doc.list, get.terms)

## LDA model
K <- 5   #主题数
G <- 5000    #迭代次数
alpha <- 0.10   
eta <- 0.02

### 其中α，大家可以调大调小了试试看，调大了的结果是每个文档接近同一个topic，
### 即让p(wi|topici)发挥的作用小，这样p(di|topici)发挥的作用就大。
## 其中的β，调大的结果是让p(di|topici)发挥的作用变下，
## 而让p(wi|topici)发挥的作用变大，体现在每个topic更集中在几个词汇上面，
## 或者而每个词汇都尽可能的百分百概率转移到一个topic上。

library(lda) 
set.seed(357) 
fit <- lda.collapsed.gibbs.sampler(documents = documents, K = K, vocab = vocab, num.iterations = G, alpha = alpha, eta = eta, initial = NULL, 
                                   burnin = 0, compute.log.likelihood = TRUE)


## visualizaion

heta <- t(apply(fit$document_sums + alpha, 2, function(x) x/sum(x)))  #文档—主题分布矩阵
phi <- t(apply(t(fit$topics) + eta, 2, function(x) x/sum(x)))  #主题-词语分布矩阵
term.frequency <- as.integer(term.tables_final)   #词频
doc.length <- sapply(documents, function(x) sum(x[2, ])) #每篇文章的长度，即有多少个词

## 
library(LDAvis)
json <- createJSON(phi = phi, theta = theta, 
                   doc.length = doc.length, vocab = vocab,
                   term.frequency = term.frequency)
#json为作图需要数据，下面用servis生产html文件，通过out.dir设置保存位置
serVis(json, out.dir = './vis', open.browser = FALSE)