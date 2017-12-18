require(tidyverse)
require(dplyr)
require(plyr)
library(readr)
library(SentimentAnalysis)
library(SnowballC)
library(tm)
library(spikeslab)
source("r scripts/cleanTweetText.R")



twitter_data<-read_csv("data/twitter_data.csv", 
                        locale = locale())

# Aufbereiten Twitter Datensatz
twitter_data<-twitter_data%>%filter(lang=="de" | lang == "da")

#Entfernt alle Links (http(s) und alle Zeichen bis zum nächsten Leerzeichen)
twitter_data$text<-gsub("http[s]?://t\\.co/[^ ]{10}","",twitter_data$text)
twitter_data$text<-gsub("http.*[^\\s]+","",twitter_data$text)


#Entfernt alle abgeschnittenen Links
twitter_data$text<-gsub("htt[p]?\U2026","",twitter_data$text)

#Entfernen von abgeschnittenen Tweets
twitter_data<-twitter_data[!grepl("\U2026", twitter_data$text),]

#CleanTweetText auf den gesamten Twitter Datensatz
twitter_data<-twitter_data%>%mutate(text=cleanTweetText(text))

# Aufbereiten für Random Sample
twitter_data<-twitter_data%>%mutate(sentimentScore=NA)

# NUR FÜR EXCEL!
twitter_data$text<-gsub(","," ",twitter_data$text)
twitter_data$text<-gsub(";"," ",twitter_data$text)

# Random Sample
twitter_data_frac<-twitter_data%>%sample_n(1000, replace=FALSE)
twitter_data_frac<-twitter_data_frac%>%select(ID,sentimentScore,text)

write.table(twitter_data_frac, "data/Twitter_Sentiment_TrainData.csv", sep = ",")


## Auswertung

# Einlesen
twitter_data_sentiment_david<-read_delim("data/SentimentAnalyseDavid.csv", 
                                   ";", escape_double = FALSE, trim_ws = TRUE)

twitter_data_sentiment_jakob<-read_delim("data/SentimentAnalyseJakobMatchedFromDavid.csv", 
                                         ";", escape_double = FALSE, trim_ws = TRUE)
colnames(twitter_data_sentiment_jakob)<-c("Nummer","ID","isMatch","sentimentScore","text")
twitter_data_sentiment_jakob<-twitter_data_sentiment_jakob[,c("Nummer","ID","sentimentScore","isMatch","text")]

twitter_data_sentiment<-rbind(twitter_data_sentiment_david,twitter_data_sentiment_jakob)

# Spalten Säubern (ID leider verfälscht)
twitter_data_sentiment<-twitter_data_sentiment%>%select(-c(Nummer,ID))

# Säubern
twitter_data_sentiment$text<-gsub("http[s]?://t\\.co/[^ ]{10}","",twitter_data_sentiment$text)
twitter_data_sentiment$text<-gsub("http.*[^\\s]+","",twitter_data_sentiment$text)
twitter_data_sentiment$text<-gsub("htt[p]?\U2026","",twitter_data_sentiment$text)
twitter_data_sentiment<-twitter_data_sentiment[!grepl("\U2026", twitter_data_sentiment$text),]

# Fehlerhaft bei ä,ü,ö
twitter_data_sentiment<-twitter_data_sentiment%>%mutate(text=cleanTweetText(text))

# Nur Matches der Kreuzvalid.
twitter_data_sentiment_match<-twitter_data_sentiment%>%filter(isMatch =="1")

# Umwandeln der Scores in numeric mit "."
twitter_data_sentiment_match<-twitter_data_sentiment_match%>%mutate(sentimentScore = sub(",", ".", sentimentScore, fixed = TRUE))
twitter_data_sentiment_match[,"sentimentScore"]<-sapply(twitter_data_sentiment_match[,"sentimentScore"],as.numeric)
twitter_data_sentiment_match<-twitter_data_sentiment_match%>%filter(sentimentScore != "NA")
# Testweise join für orginalen Text
# twitter_data_merge<-merge(x = twitter_data, y = twitter_data_sentiment, by = "text", all.x = TRUE)


x<-transformIntoCorpus(twitter_data_sentiment_match$text)
response<-as.numeric(as.character(twitter_data_sentiment_match$sentimentScore))

dict<-generateDictionary(twitter_data_sentiment_match$text,response)

dict<-generateDictionary(x,response,modelType = "lasso", filterTerms = NULL, control = list(),
                         minWordLength = 3, sparsity = 0.99999999, weighting= function(x)
                           tm::weightTfIdf(x, normalize = TRUE))
summary(dict)
dict

