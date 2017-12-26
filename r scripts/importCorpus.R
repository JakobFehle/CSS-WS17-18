source("r scripts/cleanTweetText.R")
require(tidyverse)
require(dplyr)
require(plyr)
library(readr)

twitter_data<-read_csv("twitter data/twitter_data.csv", 
                       locale = locale())

# Aufbereiten Twitter Datensatz
twitter_data<-twitter_data_cleaned<-twitter_data%>%filter(lang=="de" | lang == "da")

#Entfernt alle Links (http(s) und alle Zeichen bis zum nächsten Leerzeichen)
twitter_data$text<-twitter_data_cleaned$text<-gsub("http[s]?://t\\.co/[^ ]{10}","",twitter_data_cleaned$text)
twitter_data$text<-twitter_data_cleaned$text<-gsub("http.*[^\\s]+","",twitter_data_cleaned$text)
twitter_data$text<-twitter_data_cleaned$text<-gsub("https","",twitter_data_cleaned$text)
twitter_data$text<-twitter_data_cleaned$text<-gsub("http","",twitter_data_cleaned$text)


#Entfernt alle abgeschnittenen Links
twitter_data$text<-twitter_data_cleaned$text<-gsub("htt[p]?\U2026","",twitter_data$text)

#Entfernen von abgeschnittenen Tweets
twitter_data<-twitter_data_cleaned<-twitter_data[!grepl("\U2026", twitter_data$text),]

#CleanTweetText auf den gesamten Twitter Datensatz
twitter_data<-twitter_data%>%mutate(text=cleanCorpus(text))
twitter_data$text<-gsub("<[^\\s]+>","",twitter_data$text)
twitter_data_cleaned<-twitter_data_cleaned%>%mutate(text=cleanTweetText(text))
twitter_data_cleaned$text<-iconv(twitter_data_cleaned$text, 'UTF-8','ASCII')



# Stemming
twitter_data_cleaned$text<-apply(twitter_data_cleaned[,"text"],1,function(x) stemTweetText(x))


