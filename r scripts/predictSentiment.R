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

twitter_data$lasso5<-""

predict<-predict(ridge_5lvl,twitter_data_cleaned$text)
twitter_data$lasso5<-as.numeric(unlist(predict))
twitter_data<-twitter_data%>%mutate(lasso5=round(lasso5, 5))

write.csv(twitter_data, "twitter data/twitterDataWithSentiment.csv", row.names = FALSE, col.names = TRUE)

as.numeric(unlist(predict))

twitter_data1<-twitter_data[1:120000,]
twitter_data_cleaned1<-twitter_data_cleaned[1:120000,]
predict1<-predict(ridge_5lvl,twitter_data_cleaned1$text)
