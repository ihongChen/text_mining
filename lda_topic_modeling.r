## install package

library(jiebaR)
library(dplyr)
library(RMySQL)

# connect to MySQL db, 
m <- dbDriver("MySQL")
con<-dbConnect(m,user='mcm',password='welcome323',host='omega',dbname='yeast');
mydb <- dbConnect(m,user='xxxxxx',
                  password='xxxxxx',
                  host='ihongchen.xxxxxxxx',
                  port=xxxx,
                  dbname='xxxx');

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
  index <- match(x, vocab)  # grab vocab id
  index <- index[!is.na(index)]  #ignore stop/na word
  rbind(as.integer(index - 1), as.integer(rep(1, length(index))))   #feed lda model structure
}
documents <- lapply(doc.list, get.terms)

## LDA model
K <- 5   # numbers of topics
G <- 5000    #iteration times
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

theta <- t(apply(fit$document_sums + alpha, 2, function(x) x/sum(x)))  #topic-model matrix
phi <- t(apply(t(fit$topics) + eta, 2, function(x) x/sum(x)))  #topic-models ditrb matrix
term.frequency <- as.integer(term.tables_final)   #term frequency
doc.length <- sapply(documents, function(x) sum(x[2, ])) # How many words in each article

## 
library(LDAvis)
json <- createJSON(phi = phi, theta = theta, 
                   doc.length = doc.length, vocab = vocab,
                   term.frequency = term.frequency)
#json to keep data
serVis(json, out.dir = './vis', open.browser = TRUE)
