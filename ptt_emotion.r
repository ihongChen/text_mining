library(stringr)
library(RODBC)
library(jiebaR)
library(tm)
library(dplyr)
setwd('d:/ihong/work/ptt/tm/')
# connect to MS-SQL db, 
conndb <- odbcDriverConnect("xxxxxxxx;")

#### get corpus #### 
corpusGet<-function (query){
  # 輸入銀行名,輸出TfIdf關鍵字

  PTT <- sqlQuery(conndb,query)
  
  ## 引進tmcn/tm/ ### 作斷詞處理/清理
  
  mixseg = worker(stop_word = 'D:/ihong/work/ptt/data/stop_words.txt',
                  user='D:/ihong/work/ptt/data/userdict.txt')
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
  
  ##### 正確調用reader(並斷詞完畢)
  corp0 = VCorpus(VectorSource(xtext),
                  readerControl = list(reader=cnreader))
  # as.character(corp0[[1]])
  
  # corp0 = Corpus(VectorSource(xtext))
  corp2 = tm_map(corp0, removeNumbers)
  removeURL = function(x) gsub("http[[:alnum:]]*", "", x)
  corp2 = tm_map(corp2, removeURL)
  corp2 = tm_map(corp2, PlainTextDocument)
  corp2 = tm_map(corp2, removeWords,stopwords("english"))
  corp2 = tm_map(corp2, content_transformer(tolower) )
  stopwords.jieba = readLines('../data/stop_words.txt')
  sw = c(iconv(stopwords.jieba,'utf8','utf8'),'imgur','http','額度','申請','申辦','卡片','re','問題','情報',
         '證明','年資','客服','職業','證明','卡','卡別','辦卡','加辦',
         '工作','類別','之財力','查詢','財力','回饋','活動','消費','金額',
         '優惠','元','金額','刷卡','送','月','期間','心得',
         '億元','年 ','晚上','萬',"調整",'其實','調額','之後','比較',
         '線上','簡訊','通知','就是','真的','ts',
         '電話','之')
  
  corp2 = tm_map(corp2,removeWords,sw)
  corp2 = tm_map(corp2, function(word){gsub("[0-9]","",word)})
  # corp2 = tm_map(corp2, stripWhitespace)
  corp2 = tm_map(corp2, removePunctuation)
  
  corp2 = tm_map(corp2, PlainTextDocument)
  corp3 = Corpus(VectorSource(corp2))
  
  return(corp3)
}

##query ### 
bank <- '台新'
day <- 30

query <- sprintf("select [標題],[內文] from External.dbo.PTT
                   where [看版] like 'creditcard'
                   and
                   [標題] like \'%%%s%%\'
                   and
                   [發文日期] between
                   getdate()-%d
                   and
                   getdate()
                   order by [發文日期] DESC ",bank,day)

corp3 = corpusGet(query)

dtm = DocumentTermMatrix(
  corp3,control = list(wordLengths=c(2,10),
                       weighting = function(x) weightTfIdf(x,normalize=FALSE)))


###################################################################
# 開始讀入並處理 NTU 正、負評詞庫
###################################################################
negtxt2 = readLines("d:/ihong/work/ptt/data/ntu_negative2.txt",encoding="UTF-8")
negtxt2 = negtxt2[-which(negtxt2=='')]
postxt2 = readLines("d:/ihong/work/ptt/data/ntu_positive2.txt",encoding="UTF-8")
postxt2 = postxt2[-which(postxt2=='')]
# 假設 corp3 是最後的語料庫變數名稱   
m = length(corp3)

# 建立正評變數矩陣:橫列是文件，直行是正評詞彙
Mpos = matrix(0,m,length(postxt2))
for (i in 1:m)
{
  vtmp = str_split(corp3[[i]]$content," ")[[1]]
  m0 = table(match(vtmp,postxt2)) #多少正面情緒字眼ex. 5: 五個, 10:十個
  m0idx = as.integer(names(m0))
  Mpos[i,m0idx] = m0
}

colnames(Mpos) = postxt2

# 建立負評變數矩陣:橫列是文件，直行是負評詞彙
Mneg = matrix(0,m,length(negtxt2))
for (i in 1:m)
{
  vtmp = str_split(corp3[[i]]$content," ")[[1]]
  m0 = table(match(vtmp,negtxt2))
  m0 = m0*(-1) #多少負面情緒字眼ex. -5: 五個, -10:十個
  m0idx = as.integer(names(m0))
  Mneg[i,m0idx] = m0
}

colnames(Mneg) = negtxt2

# 結合正、負評變數矩陣與評分

Mpn = data.frame(Mpos,Mneg)
View(Mpn)
dim(Mpos)
dim(Mneg)
dim(Mpn)

score = rowSums(Mpn)
Mpn$score = score  

## 選出每篇文章的關鍵字詞

PTT <- sqlQuery(conndb,query)
PTT$score = score
cut = worker("keywords",topn=5)
# cut2 = worker("tag")
temp = NULL
for (e in 1:length(score)){
  kw = paste0(cut[corp3[[e]]$content],collapse = ",")
  temp = c(temp,kw)
}
PTT$keyword = temp ## PTT
View(PTT)

select(arrange(PTT,score,desc(score)),標題,score)
temp = select(PTT,標題,score)
View(arrange(temp,desc(score)))
