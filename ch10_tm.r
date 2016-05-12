setwd("Dropbox/work/text_mining/zhao_data_mining/")
load("RDataMining-Tweets-20160203.rdata")
library(twitteR)

df <- twListToDF(tweets)
head(df)
colnames(df)
dim(df)
head(df$text)
library(tm)
# build a corpus, and specify the source to be character vectors
myCorpus <-Corpus(VectorSource(df$text))
# convert to lowercase
# myCorpus <- tm_map(myCorpus, content_transformer(tolower))
myCorpus <- tm_map(myCorpus,content_transformer(tolower))
# remove URLs
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(removeURL))

# remove anything other than English letters or space
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(removeNumPunct))

# remove punctuation
# removePunctuation
# myCorpus <- tm_map(myCorpus, removePunctuation)
# myCorpus <- tm_map(myCorpus, removeNumbers)

# add two extra stop words: "available" and "via"
myStopwords <- c(stopwords(kind="en"), "available", "via")
myStopwords <- setdiff(myStopwords, c("r", "big"))
myStopwords

myCorpus <- tm_map(myCorpus, content_transformer(stripWhitespace))
# getTransformations()
myCorpus <- tm_map(myCorpus,removeWords,myStopwords,lazy = TRUE) # 

dictCorpus <- myCorpus
# stem words in a text document with the snowball stemmers,
# which requires packages Snowball, RWeka, rJava, RWekajars
# myCorpus <- tm_map(myCorpus, stemDocument)

# inspect the first three ``documents"
inspect(myCorpus[1:5])

# stem completion
# myCorpus <- tm_map(myCorpus, stemCompletion, dictionary=dictCorpus,lazy=TRUE) # fail to use/ 一用就掛
# inspect(myCorpus)
myDtm <- TermDocumentMatrix(myCorpus, control = list(minWordLength = 1))
inspect(myDtm[500:510,31:40])


# Frequent Terms and Associations
findFreqTerms(myDtm, lowfreq=10)

# which words are associated with "r"?
findAssocs(myDtm, 'r', 0.3)

# which words are associated with "mining"?
# Here "miners" is used instead of "mining",
# because the latter is stemmed and then completed to "miners". :-(
findAssocs(myDtm, 'miners', 0.30)

termFrequency <- rowSums(as.matrix(myDtm))
termFrequency<-subset(termFrequency, termFrequency>=10)
library(ggplot2)
df <- data.frame(term=names(termFrequency), freq=termFrequency)
ggplot(df, aes(x=term, y=freq)) + geom_bar(stat="identity") +
  xlab("Terms") + ylab("Count") + coord_flip()
# wordcloud

library(wordcloud)
m <- as.matrix(myDtm)
# calculate the frequency of words
v <- sort(rowSums(m), decreasing=TRUE)

myNames <- names(v)
k <- which(names(v)=="miners")
myNames[k] <- "mining"
d <- data.frame(word=myNames, freq=v)
wordcloud(d$word, d$freq, min.freq=10)
