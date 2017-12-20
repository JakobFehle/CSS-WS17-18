require(tidyverse)
require(dplyr)
require(plyr)
library(readr)
library(SentimentAnalysis)
library(SnowballC)
library(tm)
library(spikeslab)
source("r scripts/cleanTweetText.R")



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

# Aufbereiten für Random Sample
twitter_data<-twitter_data%>%mutate(sentimentScore=NA)

# NUR FÜR EXCEL!
twitter_data$text<-gsub(","," ",twitter_data$text)
twitter_data$text<-gsub(";"," ",twitter_data$text)

# Random Sample
twitter_data_frac<-twitter_data%>%sample_n(1000, replace=FALSE)
twitter_data_frac<-twitter_data_frac%>%select(ID,sentimentScore,text)

write.table(twitter_data_frac, "twitter data/Twitter_Sentiment_TrainData.csv", sep = ",")

#Schreiben der Tabelle für zweiten Bewertungsdurchlauf
write.table(twitter_data_frac, "data/Twitter_Sentiment_TrainData_zweiterDurchlauf.csv", sep = ",")


## Auswertung

# Einlesen
twitter_data_sentiment_david<-read_delim("twitter data/SentimentAnalyseDavid.csv", 
                                   ";", escape_double = FALSE, trim_ws = TRUE)
twitter_data_sentiment_david_snd<-read_delim("data/SentimentAnalyseDavidzweiterDurchlauf.csv",";"
                                           ,escape_double = FALSE, trim_ws =TRUE)

twitter_data_sentiment_jakob<-read_delim("twitter data/SentimentAnalyseJakobMatchedFromDavid.csv", 
                                         ";", escape_double = FALSE, trim_ws = TRUE)

colnames(twitter_data_sentiment_jakob)<-c("Nummer","ID","isMatch","sentimentScore","text")
twitter_data_sentiment_jakob<-twitter_data_sentiment_jakob[,c("Nummer","ID","sentimentScore","isMatch","text")]

twitter_data_sentiment<-rbind(twitter_data_sentiment_david,twitter_data_sentiment_jakob)



# Spalten Säubern (ID leider verfälscht)
twitter_data_sentiment<-twitter_data_sentiment%>%select(-c(Nummer,ID))

# Säubern
twitter_data_sentiment$text<-gsub("http[s]?\\://t\\.co/[^ ]{10}","",twitter_data_sentiment$text)
twitter_data_sentiment$text<-gsub("http.*[^\\s]+","",twitter_data_sentiment$text)
twitter_data_sentiment$text<-gsub("htt[p]?\U2026","",twitter_data_sentiment$text)
twitter_data_sentiment$text<-gsub("https","",twitter_data_sentiment$text)
twitter_data_sentiment$text<-gsub("http","",twitter_data_sentiment$text)
twitter_data_sentiment<-twitter_data_sentiment[!grepl("\U2026", twitter_data_sentiment$text),]

# Fehlerhaft bei ä,ü,ö
twitter_data_sentiment<-twitter_data_sentiment%>%mutate(text=cleanTweetText(text))

# Nur Matches der Kreuzvalid.
twitter_data_sentiment_match<-twitter_data_sentiment%>%filter(isMatch =="1")

# Umwandeln der Scores in numeric mit "."
twitter_data_sentiment_match<-twitter_data_sentiment_match%>%mutate(sentimentScore = sub(",", ".", sentimentScore, fixed = TRUE))
twitter_data_sentiment_match[,"sentimentScore"]<-sapply(twitter_data_sentiment_match[,"sentimentScore"],as.numeric)
twitter_data_sentiment_match<-twitter_data_sentiment_match%>%filter(sentimentScore != "NA")
twitter_data_sentiment_match<-twitter_data_sentiment_match%>%filter(sentimentScore != 0)
twitter_data_sentiment_match<-twitter_data_sentiment_match%>%mutate(sentimentScore = sub("0.5","1", sentimentScore, fixed = TRUE))
# Testweise join für orginalen Text
# twitter_data_merge<-merge(x = twitter_data, y = twitter_data_sentiment, by = "text", all.x = TRUE)

#Entfernen der isMatch Spalte
twitter_data_sentiment_match<-twitter_data_sentiment_match%>%select("sentimentScore", "text")

#Anfügen des zweiten Sentimentbewertungsdurchlauf

twitter_data_sentiment_match<-rbind(twitter_data_sentiment_match, twitter_data_sentiment_david_snd)


#Erstellen des CSV Datensatzs für die Dictornary-Erstellung
write.table(twitter_data_sentiment_match, "data/SentimentForDict5LevelSkala.csv", sep = ",")

#Einlesen der Baseline

baseline_dict_level3<-read_delim("data/SentimentForDict3LevelSkala.csv", 
                                         ";", escape_double = FALSE, trim_ws = TRUE)


baseline_dict_level5<-read_delim("data/SentimentforDict5LevelSkala.csv", 
                                 ";", escape_double = FALSE, trim_ws = FALSE)



#Erstellen des Dictornary auf Level 3 Baseline
x_level3<-transformIntoCorpus(baseline_dict_level3$text)
response_level3<-as.numeric(as.character(baseline_dict_level3$sentimenScore))

dict_level3<-generateDictionary(x_level3,response_level3,modelType = "enet", filterTerms = NULL, control = list(),
                         minWordLength = 3, sparsity = 0.999999999999, weighting= function(x)
                           tm::weightTfIdf(x, normalize = TRUE))

summary(dict_level3)
dict_level3

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


compareDictionaries(dict_level3,dict_level5)                    

dict_lasso_pol<-generateDictionary(xDM,response,modelType = "lasso", filterTerms = NULL, control = list(), sparsity = 0.99999999, weighting= function(x)
                           tm::weightTfIdf(x, normalize = TRUE), language = "german")

dict_lm_pol<-generateDictionary(x,response,modelType = "lm", filterTerms = NULL, control = list(), sparsity = 0.99999999, weighting= function(x)
                              tm::weightTfIdf(x, normalize = TRUE), language = "german")

dict_enet_pol<-generateDictionary(x,response,modelType = "enet", filterTerms = NULL, control = list(), sparsity = 0.99999999, weighting= function(x)
  tm::weightTfIdf(x, normalize = TRUE), language = "german")

dict_ridge_pol<-generateDictionary(x,response,modelType = "ridge", filterTerms = NULL, control = list(), sparsity = 0.99999999, weighting= function(x)
  tm::weightTfIdf(x, normalize = TRUE), language = "german")

summary(dict_lasso_pol)
plot(dict_lasso_pol)
plot(dict_lm_pol)
plot(dict_enet_pol)
plot(dict_ridge_pol)

# Shit
#twitter_data_exp<-twitter_data%>%select(c("ID","positveSentimentScore","negativeSentimentScore","text"))
#twitter_data_exp$sentimentScore<-""
#twitter_data_exp<-twitter_data_exp[c("ID","positveSentimentScore","negativeSentimentScore","sentimentScore","text")]

#twitter_data_exp<-twitter_data_exp%>%arrange(desc(positveSentimentScore),desc(negativeSentimentScore))
#twitter_data_exp_pos<-head(twitter_data_exp,200)
#twitter_data_exp_pos<-twitter_data_exp[1:400,]
#write.csv(twitter_data_exp_pos, "twitter data/ExampleTweetsSentiStrengthPos.csv")

#twitter_data_exp<-twitter_data_exp%>%arrange(negativeSentimentScore,positveSentimentScore)
#twitter_data_exp_neg<-head(twitter_data_exp,200)
#write.csv(twitter_data_exp_neg, "twitter data/ExampleTweetsSentiStrengthNeg.csv")
#twitter_data_exp_neg<-twitter_data_exp[201:400,]
#write.csv(twitter_data_exp_neg, "twitter data/ExampleTweetsSentiStrengthNeg2.csv")


# Umgehen von Stemming
xDM<-toDocumentTermMatrix(x, language = "german", minWordLength = 3,
                     sparsity = NULL, removeStopwords = TRUE, stemming = FALSE,
                     weighting = function(x) tm::weightTfIdf(x, normalize = FALSE))

# Korrekte dekodierung von <FC> etc in ü, ä, ö!
