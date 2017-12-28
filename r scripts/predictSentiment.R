require(tidyverse)
require(dplyr)
library(readr)
library(SentimentAnalysis)

lasso_5lvl<-read("dictionarys/lasso-5level.dict")
lasso_3lvl<-read("dictionarys/lasso-3level.dict")
lasso_2lvl<-read("dictionarys/lasso-2level.dict")

ridge_5lvl<-read("dictionarys/ridge-5level.dict")
ridge_3lvl<-read("dictionarys/ridge-3level.dict")
ridge_2lvl<-read("dictionarys/ridge-2level.dict")

enet_5lvl<-read("dictionarys/enet-5level.dict")
enet_3lvl<-read("dictionarys/enet-3level.dict")
enet_2lvl<-read("dictionarys/enet-2level.dict")


# Dataset is too big -> split
twitter_data1<-twitter_data[1:120000,]
twitter_data2<-twitter_data[120001:255307,]
twitter_data_cleaned1<-twitter_data_cleaned[1:120000,]
twitter_data_cleaned2<-twitter_data_cleaned[120001:255307,]

twitter_data1$ridge5<-as.numeric(unlist(predict(ridge_5lvl,twitter_data_cleaned1$text)))
twitter_data2$ridge5<-as.numeric(unlist(predict(ridge_5lvl,twitter_data_cleaned2$text)))

twitter_data1$ridge3<-as.numeric(unlist(predict(ridge_3lvl,twitter_data_cleaned1$text)))
twitter_data2$ridge3<-as.numeric(unlist(predict(ridge_3lvl,twitter_data_cleaned2$text)))

twitter_data1$ridge2<-as.numeric(unlist(predict(ridge_2lvl,twitter_data_cleaned1$text)))
twitter_data2$ridge2<-as.numeric(unlist(predict(ridge_2lvl,twitter_data_cleaned2$text)))

twitter_data1$enet5<-as.numeric(unlist(predict(enet_5lvl,twitter_data_cleaned1$text)))
twitter_data2$enet5<-as.numeric(unlist(predict(enet_5lvl,twitter_data_cleaned2$text)))

twitter_data1$enet3<-as.numeric(unlist(predict(enet_3lvl,twitter_data_cleaned1$text)))
twitter_data2$enet3<-as.numeric(unlist(predict(enet_3lvl,twitter_data_cleaned2$text)))

twitter_data1$enet2<-as.numeric(unlist(predict(enet_2lvl,twitter_data_cleaned1$text)))
twitter_data2$enet2<-as.numeric(unlist(predict(enet_2lvl,twitter_data_cleaned2$text)))

twitter_data1$lasso5<-as.numeric(unlist(predict(lasso_5lvl,twitter_data_cleaned1$text)))
twitter_data2$lasso5<-as.numeric(unlist(predict(lasso_5lvl,twitter_data_cleaned2$text)))

twitter_data1$lasso3<-as.numeric(unlist(predict(lasso_3lvl,twitter_data_cleaned1$text)))
twitter_data2$lasso3<-as.numeric(unlist(predict(lasso_3lvl,twitter_data_cleaned2$text)))

twitter_data1$lasso2<-as.numeric(unlist(predict(lasso_2lvl,twitter_data_cleaned1$text)))
twitter_data2$lasso2<-as.numeric(unlist(predict(lasso_2lvl,twitter_data_cleaned2$text)))

twitter_data_WS<-rbind(twitter_data1,twitter_data2)
twitter_data_WS<-twitter_data_WS%>%select(-c(inReplyToUser,lang,retweetCount,ID,favouriteCount,hashtagCount,mentionCount, inReplyToStatus,isSourceTweet,userID,createdAT,insertedAT,charCount,tokenCount,urlCount))
twitter_data_WS<-twitter_data_WS%>%mutate(lasso5=round(lasso5, 3))
twitter_data_WS<-twitter_data_WS%>%mutate(lasso3=round(lasso3, 3))
twitter_data_WS<-twitter_data_WS%>%mutate(lasso2=round(lasso2, 3))
twitter_data_WS<-twitter_data_WS%>%mutate(ridge5=round(ridge5, 3))
twitter_data_WS<-twitter_data_WS%>%mutate(ridge3=round(ridge3, 3))
twitter_data_WS<-twitter_data_WS%>%mutate(ridge2=round(ridge2, 3))
twitter_data_WS<-twitter_data_WS%>%mutate(enet5=round(enet5, 3))
twitter_data_WS<-twitter_data_WS%>%mutate(enet3=round(enet3, 3))
twitter_data_WS<-twitter_data_WS%>%mutate(enet2=round(enet2, 3))

write.table(twitter_data_WS, "twitter data/twitterDataWithSentiment.csv", sep=",",col.names = TRUE, row.names = FALSE)
