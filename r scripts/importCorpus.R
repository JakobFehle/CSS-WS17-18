source("r scripts/cleanTweetText.R")
require(tidyverse)
require(dplyr)
require(plyr)
library(readr)

twitter_data<-read_csv("twitter data/twitter_data.csv", 
                       locale = locale())

# Aufbereiten Twitter Datensatz
twitter_data<-twitter_data%>%filter(lang=="de" | lang == "da")

#Entfernt alle Links (http(s) und alle Zeichen bis zum nächsten Leerzeichen)
twitter_data$text<-gsub("http[s]?://t\\.co/[^ ]{10}","",twitter_data$text)
twitter_data$text<-gsub("http.*[^\\s]+","",twitter_data$text)
twitter_data$text<-gsub("https","",twitter_data$text)
twitter_data$text<-gsub("http","",twitter_data$text)


#Entfernt alle abgeschnittenen Links
twitter_data$text<-gsub("htt[p]?\U2026","",twitter_data$text)

#Entfernen von abgeschnittenen Tweets
twitter_data<-twitter_data[!grepl("\U2026", twitter_data$text),]

#CleanTweetText auf den gesamten Twitter Datensatz
twitter_data<-twitter_data%>%mutate(text=cleanTweetText(text))

# Stemming
twitter_data$text<-apply(twitter_data[,"text"],1,function(x) stemTweetText(x))
