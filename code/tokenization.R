library(tokenizers)
library(textstem)
library(stopwords)

library(tm)
library(SnowballC)

reviewdata = read.csv("finaldata.csv", header=T)

# make stopwords
stopwords.en = read.table("stopwords_en.txt", header=FALSE)

mystopwords = unique(c(stopwords.en$V1, stopwords("en")))

words = tokenize_words(reviewdata$text, stopwords = mystopwords, strip_numeric=TRUE,)

for(i in 1:length(words)){
  words[[i]] = stemDocument(words[[i]]) #stemming
  words[[i]] = lemmatize_words(words[[i]]) #lemmatization
}

save(words, file = "words.Rdata")
