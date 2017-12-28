source("r scripts/cleanTweetText.R")
require(tidyverse)
require(dplyr)
require(plyr)
library(readr)

twitter_data_sentiment<-read_delim("twitter data/SentimentAnalyse#1.csv",";"
                                   ,escape_double = FALSE, trim_ws =TRUE, 
                                   locale = locale())
twitter_data_sentiment_jakob_snd<-read_delim("twitter data/SentimentAnalyse#2.csv", 
                                             ";", escape_double = FALSE, trim_ws = TRUE, 
                                             locale = locale())


twitter_data_sentiment_jakob_snd<-twitter_data_sentiment_jakob_snd%>%select(-c(X1,ID,positveSentimentScore,negativeSentimentScore))
twitter_data_sentiment_jakob_snd<-twitter_data_sentiment_jakob_snd%>%filter(sentimentScore != "NA")
twitter_data_sentiment_jakob_snd$isMatch<-"1"
twitter_data_sentiment<-twitter_data_sentiment%>%select(-c(ID))
twitter_data_sentiment<-rbind(twitter_data_sentiment,twitter_data_sentiment_jakob_snd)
twitter_data_sentiment<-twitter_data_sentiment%>%filter(isMatch=="1")
twitter_data_sentiment<-twitter_data_sentiment%>%mutate(text=cleanTweetText(text))
twitter_data_sentiment<-twitter_data_sentiment%>%filter(text!="")

twitter_data2<-read_delim("twitter data/twitter_data.csv", 
                                             ",", escape_double = FALSE, trim_ws = TRUE, 
                                             locale = locale())

twitter_data<-twitter_data%>%mutate(text=cleanTextForMerge(text))
twitter_data<-twitter_data%>%filter(text != "")

twitter_data_sentiment_anti<-anti_join(x=twitter_data_sentiment,y=twitter_data)
twitter_data_sentiment22<-join(x=twitter_data_sentiment,y=twitter_data, type = "inner")
twitter_data_sentiment22<-twitter_data_sentiment22[!duplicated((twitter_data_sentiment22$text)),]


twitter_data2<-twitter_data2%>%select(-c(isRetweet,isQuote,inReplyToUser,lang,retweetCount,favouriteCount,hashtagCount,mentionCount, inReplyToStatus,isSourceTweet,userID,createdAT,insertedAT,charCount,tokenCount,urlCount))
twitter_data_sentiment22<-twitter_data_sentiment22%>%select(c(ID,sentimentScore))

twitter_data_WS_join<-join(x=twitter_data_sentiment22,y=twitter_data2, type = "inner", by="ID")

twitter_data_WS_join$text<-unlist(twitter_data_WS_join$text)
write.table(twitter_data_WS_join, "twitter data/twitterDataMatches.csv", sep=",",col.names = TRUE, row.names = FALSE)

#######
twitterDataWithSentiment$ID<-twitter_data$ID
twitterDataWithSentiment<-twitterDataWithSentiment%>%select(ID,everything())

twitter_data_WS_join<-twitter_data_WS_join%>%select(ID,sentimentScore)

twitterDataWithSentiment2<-join(x=twitterDataWithSentiment,twitter_data_WS_join,type="left",by="ID")

twitterDataWS<-twitterDataWithSentiment2%>%filter(sentimentScore!="NA")
twitterDataWS<-twitterDataWS[!duplicated((twitterDataWS$text)),]

write.table(twitterDataWS, "twitter data/twitterDataBaselineWithAllDicts.csv", sep=",",col.names = TRUE, row.names = FALSE)
