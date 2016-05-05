## install package

library(jiebaR)
library(dplyr)
library(RODBC)

# connect to MS-SQL db, 
conndb <- odbcDriverConnect("Driver=SQL Server;Server=xxxxx;Database=xxxxx;Uid=xx;Pwd=xxxxx;")
## must set to be UTF8 setting, else ???? occur
PTT <- sqlQuery(conndb,"select top 1000 * from External.dbo.PTT where [看版] like 'creditcard' ")

PTT.list <- as.list(PTT$'內文')
## segmentation

setwd("D:/ihong/work/ptt/topic-modeling/")
cc <- worker(stop_word = '../data/stop_words.txt',user = '../data/userdict.txt',dict = '../data/dict.txt.big.txt')
text.seg <- cc[as.character(unlist(PTT.list))] ## ERROR ->R update 3.2.5
doc.list <-as.list(text.seg)
## pre-processing, stop words,
pat <- "[^0-9.]"
text.seg<-grep(pat,text.seg,value=TRUE)
term.tables<-table(text.seg)
term.tables <- sort(term.tables, decreasing = TRUE)

del <- term.tables < 2| nchar(names(term.tables))<2
term.tables <- term.tables[!del]

vocab <- names(term.tables)

## word

get.terms <- function(x) {
  index <- match(x, vocab)  # grab vocab id
  index <- index[!is.na(index)]  #ignore stop/na word
  rbind(as.integer(index - 1), as.integer(rep(1, length(index))))   #feed lda model structure
}
documents <- lapply(doc.list, get.terms)

## LDA model
K <- 10   # numbers of topics
G <- 5000    #iteration times
alpha <- 0.10   
eta <- 0.02


library(lda) 
set.seed(357) 
fit <- lda.collapsed.gibbs.sampler(documents = documents, K = K, vocab = vocab, num.iterations = G, alpha = alpha, eta = eta, initial = NULL, 
                                   burnin = 0, compute.log.likelihood = TRUE)


## visualizaion

theta <- t(apply(fit$document_sums + alpha, 2, function(x) x/sum(x)))  #topic-model matrix
phi <- t(apply(t(fit$topics) + eta, 2, function(x) x/sum(x)))  #topic-models ditrb matrix
term.frequency <- as.integer(term.tables)   #term frequency
doc.length <- sapply(documents, function(x) sum(x[2, ])) # How many words in each article

## 
library(LDAvis)
json <- createJSON(phi = phi, theta = theta, 
                   doc.length = doc.length, vocab = vocab,
                   term.frequency = term.frequency)
#json to keep data
serVis(json, out.dir = './vis', open.browser = FALSE)

## 
writeLines(iconv(readLines("./vis/lda.json"), from = "big5", to = "UTF8"), 
           file("./vis/lda.json", encoding="UTF-8"))
