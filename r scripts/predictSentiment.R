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

sentiWS<-read("dictionarys/sentiWSDict.dict")

# Dataset is too big -> split
twitter_data1<-twitter_data[1:120000,]
twitter_data2<-twitter_data[120001:253977,]
twitter_data_cleaned1<-twitter_data_cleaned[1:120000,]
twitter_data_cleaned2<-twitter_data_cleaned[120001:253977,]

pred_ridge51<-predict(ridge_5lvl,twitter_data_cleaned1$text)
twitter_data1$ridge5<-as.numeric(unlist(pred_ridge51))
pred_ridge52<-predict(ridge_5lvl,twitter_data_cleaned2$text)
twitter_data2$ridge5<-as.numeric(unlist(pred_ridge52))

pred_ridge31<-predict(ridge_3lvl,twitter_data_cleaned1$text)
twitter_data1$ridge3<-as.numeric(unlist(pred_ridge31))
pred_ridge32<-predict(ridge_3lvl,twitter_data_cleaned2$text)
twitter_data2$ridge3<-as.numeric(unlist(pred_ridge32))

twitter_data1$ridge2<-as.numeric(unlist(predict(ridge_2lvl,twitter_data_cleaned1$text)))
twitter_data2$ridge2<-as.numeric(unlist(predict(ridge_2lvl,twitter_data_cleaned2$text)))

twitter_data1$enet5<-as.numeric(unlist(predict(enet_5lvl,twitter_data_cleaned1$text)))
twitter_data2$enet5<-as.numeric(unlist(predict(enet_5lvl,twitter_data_cleaned2$text)))

pred_enet31<-predict(enet_3lvl,twitter_data_cleaned1$text)
twitter_data1$enet3<-as.numeric(unlist(pred_enet31))
pred_enet32<-predict(enet_3lvl,twitter_data_cleaned2$text)
twitter_data2$enet3<-as.numeric(unlist(pred_enet32))

twitter_data1$enet2<-as.numeric(unlist(predict(enet_2lvl,twitter_data_cleaned1$text)))
twitter_data2$enet2<-as.numeric(unlist(predict(enet_2lvl,twitter_data_cleaned2$text)))

twitter_data1$lasso5<-as.numeric(unlist(predict(lasso_5lvl,twitter_data_cleaned1$text)))
twitter_data2$lasso5<-as.numeric(unlist(predict(lasso_5lvl,twitter_data_cleaned2$text)))

pred_lasso31<-predict(lasso_3lvl,twitter_data_cleaned1$text)
twitter_data1$lasso3<-as.numeric(unlist(pred_lasso31))
pred_lasso32<-predict(lasso_3lvl,twitter_data_cleaned2$text)
twitter_data2$lasso3<-as.numeric(unlist(pred_lasso32))

twitter_data1$lasso2<-as.numeric(unlist(predict(lasso_2lvl,twitter_data_cleaned1$text)))
twitter_data2$lasso2<-as.numeric(unlist(predict(lasso_2lvl,twitter_data_cleaned2$text)))

pred_sentiWS1<-predict(sentiWS,twitter_data_cleaned1$text)
twitter_data1$sentiWS<-as.numeric(unlist(pred_sentiWS1))
pred_sentiWS2<-predict(sentiWS,twitter_data_cleaned2$text)
twitter_data2$sentiWS<-as.numeric(unlist(pred_sentiWS2))


twitter_data_WS<-rbind(twitter_data1,twitter_data2)
twitter_data_WS<-twitter_data_WS%>%select(-c(ridge5,inReplyToUser,lang,retweetCount,ID,favouriteCount,hashtagCount,mentionCount, inReplyToStatus,isSourceTweet,userID,createdAT,insertedAT,charCount,tokenCount,urlCount))
#twitter_data_WS<-twitter_data_WS%>%mutate(lasso5=round(lasso5, 3))
twitter_data_WS<-twitter_data_WS%>%mutate(lasso3=round(lasso3, 3))
#twitter_data_WS<-twitter_data_WS%>%mutate(lasso2=round(lasso2, 3))
#twitter_data_WS<-twitter_data_WS%>%mutate(ridge5=round(ridge5, 3))
twitter_data_WS<-twitter_data_WS%>%mutate(ridge3=round(ridge3, 3))
#twitter_data_WS<-twitter_data_WS%>%mutate(ridge2=round(ridge2, 3))
#twitter_data_WS<-twitter_data_WS%>%mutate(enet5=round(enet5, 3))
twitter_data_WS<-twitter_data_WS%>%mutate(enet3=round(enet3, 3))
#twitter_data_WS<-twitter_data_WS%>%mutate(enet2=round(enet2, 3))
twitter_data_WS<-twitter_data_WS%>%mutate(sentiWS=round(sentiWS, 3))

twitter_data_WS<-twitter_data_WS%>%mutate(sentiStrenth=as.numeric(positveSentimentScore)+as.numeric(negativeSentimentScore))
twitter_data_WS<-twitter_data_WS%>%select(-c(positveSentimentScore,negativeSentimentScore))

write.table(twitter_data_WS, "twitter data/twitterDataWithSentiment3Level.csv", sep=",",col.names = TRUE, row.names = FALSE)

nrow(twitter_data_WS%>%filter(lasso3==0))/nrow(twitter_data_WS)
nrow(twitter_data_WS%>%filter(ridge3==0))/nrow(twitter_data_WS)
nrow(twitter_data_WS%>%filter(enet3==0))/nrow(twitter_data_WS)
nrow(twitter_data_WS%>%filter(sentiWS==0))/nrow(twitter_data_WS)
nrow(twitter_data_WS%>%filter(sentiStrenth==0))/nrow(twitter_data_WS)

pred_lasso3<-rbind(pred_lasso31,pred_lasso32)
pred_ridge3<-rbind(pred_ridge31,pred_ridge32)
pred_enet3<-rbind(pred_enet31,pred_enet32)
pred_sentiWS<-rbind(pred_sentiWS1,pred_sentiWS2)

DictCorrelation<-unlist(compareToResponse(pred_lasso3,twitter_data_WS$ridge3))
DictCorrelation<-as.data.frame(as.table(DictCorrelation))
DictCorrelation<-DictCorrelation%>%select(-c(Var2))
colnames(DictCorrelation)<-c("Vars","LassoRidge")
DictCorrelation$LassoEnet<-unlist(compareToResponse(pred_lasso3,twitter_data_WS$enet3))
DictCorrelation$LassoSentiWS<-unlist(compareToResponse(pred_lasso3,twitter_data_WS$sentiWS))
DictCorrelation$LassoSentiStrenth<-unlist(compareToResponse(pred_lasso3,twitter_data_WS$sentiStrenth))

DictCorrelation$RidgeEnet<-unlist(compareToResponse(pred_ridge3,twitter_data_WS$enet3))
DictCorrelation$RidgeSentiWS<-unlist(compareToResponse(pred_ridge3,twitter_data_WS$sentiWS))
DictCorrelation$RidgeSentiStrenth<-unlist(compareToResponse(pred_ridge3,twitter_data_WS$sentiStrenth))

DictCorrelation$EnetSentiWS<-unlist(compareToResponse(pred_enet3,twitter_data_WS$sentiWS))
DictCorrelation$EnetSentiStrenth<-unlist(compareToResponse(pred_enet3,twitter_data_WS$sentiStrenth))

DictCorrelation$SentiWSSentiStrenth<-unlist(compareToResponse(pred_sentiWS,twitter_data_WS$sentiStrenth))

write.table(DictCorrelation, "twitter data/dictCorrelation.csv",sep=",",col.names = TRUE, row.names = FALSE)
