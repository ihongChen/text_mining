# this is the test file for handling the large Corpus in ptt creditcard board

setwd("D:/ihong/work/ptt/ex/zhao_R_and_dm/ptt_ex/")

library(RODBC)
library(jiebaR)
library(dplyr)


# connect to MS-SQL db, 
conndb <- odbcDriverConnect("Driver=SQL Server;Server=xxxxxx;Database=xxxx;Uid=xxx;Pwd=xxxxxxx;")
## 
PTT <- sqlQuery(conndb,"select top 1000 * from External.dbo.PTT where [看版] like 'creditcard' order by [發文日期] DESC")
# where [看版] like 'creditcard'
PTT$標題[1:3]
library(tm)
# build a corpus, and specify the source to be character vectors
myCorpus <-Corpus(VectorSource(PTT$標題))
myCorpus <- tm_map(myCorpus,content_transformer(tolower))
lapply(myCorpus[1:100],as.character)

##
## test jiebaR
xx<-c(
  "进入", "一个", "平衡", "时代", "现在", "是", "住宅", "价格上涨",
  "太快", "政府", "采用", "政策", "方式", "调控", "这些", "资金",
  "就", "有", "往", "商业地产", "走", "的", "趋势", "因为", "商业地产",
  "把", "自己", "划分", "到", "这", "一类", "去", "从", "职业",
  "来说", "我", "可能", "是", "设计师", "医生", "老师", "记者",
  "那", "我", "就", "做", "一个", "好", "的", "记者", "好", "的",
  "医生", "这是", "社会", "上", "需要", "的", "现在", "这个", "时代",
  "确实", "是", "一个", "特别", "好", "的", "时代", "也", "是")

corpus <- Corpus(VectorSource(xx))
dtm_psy = TermDocumentMatrix(corpus)
tdm = DocumentTermMatrix(corpus,control = list(wordLengths = c(1, Inf)))
inspect(tdm)
class(tdm)
View(as.data.frame(as.matrix(tdm)))

head(inspect(tdm))

## 
cc.seg<-worker(dict='../text_mining/data/dict.txt.big.txt',
       user='../text_mining/data/userdict.txt',stop_word = '../text_mining/data/stop_words.txt')
title.seg <- cc.seg[as.character(PTT$標題)]
corpus <- Corpus(VectorSource(title.seg))
tDm <- TermDocumentMatrix(corpus, control = list(minWordLength = 1))

inspect(tDm[50:60,400:410])


# Frequent Terms and Associations
findFreqTerms(tDm, lowfreq=10)

# which words are associated with "r"?
findAssocs(tDm, '聯名卡\n', 0.2) 

termFrequency <- rowSums(as.matrix(tDm))
termFrequency<-subset(termFrequency, termFrequency>=10)

library(ggplot2)
df <- data.frame(term=names(termFrequency), freq=termFrequency)
ggplot(df, aes(x=term, y=freq)) + geom_bar(stat="identity") +
  xlab("單詞") + ylab("次數") + coord_flip()

