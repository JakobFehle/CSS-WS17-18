require(tidyverse)
require(dplyr)
library(readr)
source("r scripts/cleanTweetText.R")

twitter_data<-read_csv("D:/GitHub/CSS-WS17-18/data/twitter_data.csv", 
                        locale = locale())

twitter_data_sentiment<-read_delim("D:/GitHub/CSS-WS17-18/data/Twitter_Sentiment_TrainData2.csv", 
                                                                   ";", escape_double = FALSE, trim_ws = TRUE)
twitter_data_sentiment<-twitter_data_sentiment%>%select(-c(Nummer))

twitter_data<-twitter_data%>%mutate(sentimentScore=NA)
twitter_data$text<-gsub(","," ",twitter_data$text)
twitter_data$text<-gsub(";"," ",twitter_data$text)
twitter_data_frac<-twitter_data%>%sample_n(1000, replace=FALSE)
twitter_data_frac<-twitter_data_frac%>%select(ID,sentimentScore,text)

write.table(twitter_data_frac, "data/Twitter_Sentiment_TrainData.csv", sep = ",")
