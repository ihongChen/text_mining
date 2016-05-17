library(jiebaR)
library(ropencc)
setwd('Dropbox/work/jieba_cut/')
ccst = converter(T2S)
ccst["開放中文轉換"]
cc = worker(stop_word = '../dict/stop_words.txt')
cc
cc["這是一個測試"] # or segment("这是一个测试", cc)
cc["./金庸_射雕英雄傳_1.txt"]

## 過濾詞使用
filter_words = c("我","你","她","大家")
text <- '我是測試文本，用於測試分詞效果'
result <- cc['我是測試文本，用於測試分詞效果']
filter_segment(result,filter_words = filter_words)
result2 = segment(text,cc)
filter_segment(result2,filter_words= filter_words)

## 接資料庫進來
library(RMySQL)
library(dplyr)

# connect to MySQL db, 
m <- dbDriver("MySQL")
mydb <- dbConnect(m,user='ehome4829',
                  password='a126234829',
                  host='ihongchen.ctbx4pq8or72.us-west-2.rds.amazonaws.com',
                  port=3306,
                  dbname='test');

## must set to be UTF8 setting, else ???? occur
dbSendQuery(mydb,"SET NAMES utf8")

# dbGetQuery(mydb,"show variables like 'character_set_%'")
res <- dbGetQuery(mydb,"select * from PTT where `標題` like '%GOGO%' limit 100;")

text1 <- as.list(res$內文[1])
text1
cut_result <-cc[text1[[1]]]

## 引進tmcn/tm/
library(tm)
library(tmcn)
class(res)
xtext = res$內文


cnreader<-function(elem,language,id)
{
  #进行分词
  words <- mixseg[elem$content]
  #合并分词的结果为新的文档，该文档可为tm正确的识别
  ncon <- paste(words,collapse=" ")
  PlainTextDocument(ncon, id=id,language=language)
}

##### 正確調用reader(並斷詞完畢)####
corp0 = VCorpus(VectorSource(xtext),
               readerControl = list(reader=cnreader))

as.character(mcop[[1]])

# corp0 = Corpus(VectorSource(xtext))
corp2 = tm_map(corp0, stripWhitespace)
corp2 = tm_map(corp2, removePunctuation)
corp2 = tm_map(corp2, removeNumbers) 
corp2 = tm_map(corp2, PlainTextDocument)
corp2 = tm_map(corp2, removeWords,stopwords("english"))
stopwords.jieba = readLines('../dict/stop_words.txt')
corp2 = tm_map(corp2,removeWords,stopwords.jieba)
corp2 = tm_map(corp2, function(word){gsub("[0-9]","",word)})

corp2 = tm_map(corp2, PlainTextDocument)
corp3 = Corpus(VectorSource(corp2))

# mixseg = worker()
###  tdm / tfidf

tdm = TermDocumentMatrix(corp3,control = list(wordLengths=c(2,Inf)))
dtm = t(tdm)
tfidf = apply(tdm,1,mean)  # row = docs, col = terms
tfidfsort = sort(tfidf,decreasing=T)
tfidfsort[1:10]
inspect(dtm[1:10,20:30])



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

