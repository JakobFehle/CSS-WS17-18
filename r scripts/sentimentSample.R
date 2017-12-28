require(tidyverse)
require(dplyr)
require(plyr)
library(readr)
library(SentimentAnalysis)
library(SnowballC)
library(tm)
library(spikeslab)
source("r scripts/cleanTweetText.R")

## Auswertung

# Einlesen

twitter_data_sentiment<-read_delim("twitter data/SentimentAnalyse#1.csv",";"
                                   ,escape_double = FALSE, trim_ws =TRUE, 
                                   locale = locale())

twitter_data_sentiment_david_snd<-read_delim("data/SentimentAnalyseDavidzweiterDurchlauf.csv",";"
                                           ,escape_double = FALSE, trim_ws =TRUE, 
                                           locale = locale())

twitter_data_sentiment_jakob_snd<-read_delim("twitter data/SentimentAnalyse#2.csv", 
                                         ";", escape_double = FALSE, trim_ws = TRUE, 
                                         locale = locale())


colnames(twitter_data_sentiment_david_snd)<-c("sentimentScore","text")
twitter_data_sentiment<-twitter_data_sentiment%>%select(-c(ID))
twitter_data_sentiment_jakob_snd<-twitter_data_sentiment_jakob_snd%>%select(-c(X1,ID,positveSentimentScore,negativeSentimentScore))
twitter_data_sentiment_jakob_snd<-twitter_data_sentiment_jakob_snd%>%filter(sentimentScore != "NA")
twitter_data_sentiment_jakob_snd$isMatch<-"1"
twitter_data_sentiment_david_snd$isMatch<-"1"

#temp Plot für Paper
values <-c(nrow(twitter_data_sentiment%>%filter(isMatch=="1")), nrow(twitter_data_sentiment%>%filter(isMatch=="0.5")), nrow(twitter_data_sentiment
                                                                                                                           %>%filter(isMatch=="0")))
values
go <-c("matched","polarity matched","not matched")
basic_plot_data<-data.frame(go,values)
basic_plot <- ggplot(basic_plot_data, aes(y = values,x =go))
basic_plot +geom_bar(stat = "identity", fill='#890E1C') + labs(x = "", y="count of tweets")



twitter_data_sentiment<-rbind(twitter_data_sentiment,twitter_data_sentiment_jakob_snd,twitter_data_sentiment_david_snd)

twitter_data_sentiment<-twitter_data_sentiment%>%mutate(text=cleanTweetText(text))

# Umwandeln der Scores in numeric mit "."
twitter_data_sentiment<-twitter_data_sentiment%>%filter(sentimentScore != "NA")
twitter_data_sentiment<-twitter_data_sentiment%>%filter(text != "")
twitter_data_sentiment<-twitter_data_sentiment%>%mutate(sentimentScore = sub(",", ".", sentimentScore, fixed = TRUE))
twitter_data_sentiment$sentimentScore<-as.numeric(as.character(twitter_data_sentiment$sentimentScore))

# Stemming
twitter_data_sentiment$text<-apply(twitter_data_sentiment[,"text"],1,function(x) stemTweetText(x))

# Nur Matches der Kreuzvalid.
twitter_data_sentiment_match<-twitter_data_sentiment%>%filter(isMatch =="1")



##########
## 5 Level
##########

twitter_data_sentiment_5lvl<-twitter_data_sentiment_match
twitter_data_sentiment_5lvl<-twitter_data_sentiment_5lvl%>%select("sentimentScore", "text")

write.csv(twitter_data_sentiment_5lvl, "data/SentimentAnalyse_5LevelDict.csv")

response5<-twitter_data_sentiment_5lvl$sentimentScore
x5<-transformIntoCorpus(twitter_data_sentiment_5lvl$text)
xDTM5<-toDocumentTermMatrix(x5, language = "german", minWordLength = 3,
                          sparsity = NULL, removeStopwords = TRUE, stemming = FALSE)

#1024
dict_lasso5<-generateDictionary(xDTM5,response5,modelType = "lasso", sparsity = 0.99999999, language = "german")


dict_lm5<-generateDictionary(xDTM5,response5,modelType = "lm", filterTerms = NULL, control = list(), sparsity = 0.99999999, language = "german")

#1317
dict_enet5<-generateDictionary(xDTM5,response5,modelType = "enet", filterTerms = NULL, control = list(), sparsity = 0.99999999, language = "german")

#6235
dict_ridge5<-generateDictionary(xDTM5,response5,modelType = "ridge", filterTerms = NULL, control = list(), sparsity = 0.99999999, language = "german")




summary(dict_lasso5)
dict_lasso5
plot(dict_lasso5)

summary(dict_lm5)
#dict_lm5
plot(dict_lm5)

summary(dict_enet5)
#dict_enet5
plot(dict_enet5)

summary(dict_ridge5)
#dict_ridge5
plot(dict_ridge5)

write(dict_lasso5, "dictionarys/lasso-5level.dict")
write(dict_lm5, "dictionarys/lm-5level.dict")
write(dict_enet5, "dictionarys/enet-5level.dict")
write(dict_ridge5, "dictionarys/ridge-5level.dict")


###########
## 3 Level
###########

twitter_data_sentiment_3lvl<-twitter_data_sentiment_match%>%mutate(sentimentScore = sub("0.5","1.0", sentimentScore, fixed = TRUE))
twitter_data_sentiment_3lvl<-twitter_data_sentiment_3lvl%>%select("sentimentScore", "text")
twitter_data_sentiment_3lvl$sentimentScore<-as.numeric(as.character(twitter_data_sentiment_3lvl$sentimentScore))

write.csv(twitter_data_sentiment_3lvl, "data/SentimentAnalyse_3LevelDict")

x3<-transformIntoCorpus(twitter_data_sentiment_3lvl$text)
xDTM3<-toDocumentTermMatrix(x3, language = "german", minWordLength = 3,
                            sparsity = NULL, removeStopwords = TRUE, stemming = FALSE)
response3<-twitter_data_sentiment_3lvl$sentimentScore

#1241
dict_lasso3<-generateDictionary(xDTM3,response3,modelType = "lasso", filterTerms = NULL, control = list(), sparsity = 0.99999999, language = "german")
summary(dict_lasso3)
dict_lm3<-generateDictionary(xDTM3,response3,modelType = "lm", filterTerms = NULL, control = list(), sparsity = 0.99999999, language = "german")

#1514
dict_enet3<-generateDictionary(xDTM3,response3,modelType = "enet", filterTerms = NULL, control = list(), sparsity = 0.99999999, language = "german")

#6235
dict_ridge3<-generateDictionary(xDTM3,response3,modelType = "ridge", filterTerms = NULL, control = list(), sparsity = 0.99999999, language = "german")


summary(dict_lasso3)
dict_lasso3
plot(dict_lasso3)

summary(dict_lm3)
dict_lm3
plot(dict_lm3)

summary(dict_enet3)
dict_enet3
plot(dict_enet3)

summary(dict_ridge3)
dict_ridge3
plot(dict_ridge3)

write(dict_lasso3, "dictionarys/lasso-3level.dict")
write(dict_lm3, "dictionarys/lm-3level.dict")
write(dict_enet3, "dictionarys/enet-3level.dict")
write(dict_ridge3, "dictionarys/ridge-3level.dict")




###########
## 2 Level
###########

twitter_data_sentiment_2lvl<-twitter_data_sentiment_match%>%mutate(sentimentScore = sub("0.5","1", sentimentScore, fixed = TRUE))
twitter_data_sentiment_2lvl<-twitter_data_sentiment_2lvl%>%filter(sentimentScore != 0)
twitter_data_sentiment_2lvl<-twitter_data_sentiment_2lvl%>%select("sentimentScore", "text")
twitter_data_sentiment_2lvl$sentimentScore<-as.numeric(as.character(twitter_data_sentiment_2lvl$sentimentScore))

write.csv(twitter_data_sentiment_2lvl, "data/SentimentAnalyse_2LevelDict")

x2<-transformIntoCorpus(twitter_data_sentiment_2lvl$text)
xDTM2<-toDocumentTermMatrix(x2, language = "german", minWordLength = 3,
                            sparsity = NULL, removeStopwords = TRUE, stemming = FALSE)
response2<-twitter_data_sentiment_2lvl$sentimentScore

#735
dict_lasso2<-generateDictionary(xDTM2,response2,modelType = "lasso", filterTerms = NULL, control = list(), sparsity = 0.99999999, language = "german")

dict_lm2<-generateDictionary(xDTM2,response2,modelType = "lm", filterTerms = NULL, control = list(), sparsity = 0.99999999, language = "german")

#1403
dict_enet2<-generateDictionary(xDTM2,response2,modelType = "enet", filterTerms = NULL, control = list(), sparsity = 0.99999999, language = "german")


dict_ridge2<-generateDictionary(xDTM2,response2,modelType = "ridge", filterTerms = NULL, control = list(), sparsity = 0.99999999, language = "german")



summary(dict_lasso2)
dict_lasso2
plot(dict_lasso2)

summary(dict_lm2)
dict_lm2
plot(dict_lm2)

summary(dict_enet2)
dict_enet2
plot(dict_enet2)

summary(dict_ridge2)
dict_ridge2
plot(dict_ridge2)

write(dict_lasso2, "dictionarys/lasso-2level.dict")
write(dict_lm2, "dictionarys/lm-2level.dict")
write(dict_enet2, "dictionarys/enet-2level.dict")
write(dict_ridge2, "dictionarys/ridge-2level.dict")



test_documents<-twitter_data_sentiment_5lvl$text
test_response<-twitter_data_sentiment_5lvl$sentimentScore
pred<-predict(dict_lasso5,test_documents)
compareToResponse(pred,test_response)






### Experimente
#only selecting the text column for sentiment analyses
twitter_data_for_sentiment<-twitter_data%>%select("text")

twitter_data_for_sentiment<-twitter_data_for_sentiment[1:30000,]

sentiment <-predict(dict_level3,twitter_data_for_sentiment$text)

twitter_data_for_sentiment<-twitter_data_for_sentiment%>%mutate(sentimentScore=NA)

twitter_data_for_sentiment$sentimentScore<-sentiment



plotSentiment(sentiment)
hist(as.numeric(unlist(sentiment)))
dict_level3
#Erstellen des Dict auf Level 5 Baseline
x_level5<-transformIntoCorpus(baseline_dict_level5$text)
response_level5<-as.numeric(as.character(baseline_dict_level5$sentimentScore))

dict_level5<-generateDictionary(x_level5,response_level5,modelType = "lasso", filterTerms = NULL, control = list(),
                                minWordLength = 3, sparsity = 0.99999999, weighting= function(x)
                                  tm::weightTfIdf(x, normalize = TRUE))



#Comparing dicts for chap 5

#Ridged
dict_ridge_2level <-read("dictionarys/ridge-2level.dict")
dict_ridge_3level <-read("dictionarys/ridge-3level.dict")
dict_ridge_5level <-read("dictionarys/ridge-5level.dict")
dict_lasso_2level <-read("dictionarys/lasso-2level.dict")
dict_lasso_3level <-read("dictionarys/lasso-3level.dict")
dict_lasso_5level <-read("dictionarys/lasso-5level.dict")
dict_enet_2level <-read("dictionarys/enet-2level.dict")
dict_enet_3level <-read("dictionarys/enet-3level.dict")
dict_enet_5level <-read("dictionarys/enet-5level.dict")

summary(dict_enet_5level)

                  

compareDictionaries(dict_enet_2level,dict_enet_5level)
compareDictionaries(dict_enet_3level,dict_enet_2level)  
compareDictionaries(dict_enet_3level,dict_enet_5level)

