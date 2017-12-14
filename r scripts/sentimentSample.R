require(tidyverse)
require(dplyr)
library(readr)
library(SentimentAnalysis)
source("r scripts/cleanTweetText.R")

twitter_data<-read_csv("data/twitter_data.csv", 
                        locale = locale())

twitter_data_sentiment<-read_delim("data/Twitter_Sentiment_TrainData.csv", 
                                                                   ";", escape_double = FALSE, trim_ws = TRUE)
twitter_data_sentiment<-twitter_data_sentiment%>%select(-c(Nummer))

# Aufbereiten Twitter Datensatz
twitter_data<-twitter_data%>%filter(lang=="de" | lang == "da")

#Entfernt alle Links (http(s) und alle Zeichen bis zum nächsten Leerzeichen)
twitter_data$text<-gsub("http[s]?://t\\.co/[^ ]{10}","",twitter_data$text)
twitter_data$text<-gsub("http.*[^\\s]+","",twitter_data$text)


#Entfernt alle abgeschnittenen Links
twitter_data$text<-gsub("htt[p]?\U2026","",twitter_data$text)

#Entfernen von abgeschnittenen Tweets
twitter_data<-twitter_data[!grepl("\U2026", twitter_data$text),]

# Aufbereiten für Random Sample
twitter_data<-twitter_data%>%mutate(sentimentScore=NA)

# NUR FÜR EXCEL!
twitter_data$text<-gsub(","," ",twitter_data$text)
twitter_data$text<-gsub(";"," ",twitter_data$text)

# Random Sample
twitter_data_frac<-twitter_data%>%sample_n(1000, replace=FALSE)
twitter_data_frac<-twitter_data_frac%>%select(ID,sentimentScore,text)

write.table(twitter_data_frac, "data/Twitter_Sentiment_TrainData.csv", sep = ",")


# Auswertung

twitter_data_sentiment<-twitter_data_sentiment%>%mutate(text=cleanTweetText(text))
text<-transformIntoCorpus(twitter_data_sentiment$text)
response<-as.numeric(as.character(twitter_data_sentiment$sentimentScore))
dict<-generateDictionary(twitter_data_sentiment$text,response, language="german", modelType = "lasso", filterTerms = NULL, control = list(), sparsity = 0.9, minWordLength = 3)
