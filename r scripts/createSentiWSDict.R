require(tidyverse)
require(dplyr)
library(readr)
library(SentimentAnalysis)

sentiWSPositTerms<-read_delim("dictionarys/SentiWS/SentiWS_v1.8c_Positive.txt",
                            delim = "\t",
                            col_names = c("word","score","addWords"),col_types ="cdc") 
sentiWSNegTerms<-read_delim("dictionarys/SentiWS/SentiWS_v1.8c_Negative.txt",
                              delim = "\t",
                              col_names = c("word","score","addWords"),col_types ="cdc") 
sentiWSWordlist<-rbind(sentiWSPositTerms,sentiWSNegTerms)  

sentiWSWordlist$word<-gsub("\\|NN","",sentiWSWordlist$word)


sentiWSWordlist<-sentiWSWordlist%>%mutate(addWords = strsplit(as.character(addWords),","))%>%unnest(addWords)
sentiWSWordlist2<-sentiWSWordlist%>%select(c(addWords,score))
sentiWSWordlist2<-sentiWSWordlist2%>%filter(addWords!="")
sentiWSWordlist<-sentiWSWordlist%>%select(-c(addWords))
colnames(sentiWSWordlist2)<-c("word","score")
sentiWSWordlist<-rbind(sentiWSWordlist,sentiWSWordlist2)
sentiWSWordlist$word<-gsub("ä","ae",sentiWSWordlist$word)
sentiWSWordlist$word<-gsub("ö","oe",sentiWSWordlist$word)
sentiWSWordlist$word<-gsub("ü","ue",sentiWSWordlist$word)
sentiWSWordlist<-sentiWSWordlist%>%mutate(word=tolower(word))
sentiWSWordlist<-sentiWSWordlist[!duplicated(sentiWSWordlist$word),]

sentiWSDict<-SentimentDictionaryWeighted(sentiWSWordlist$word,sentiWSWordlist$score,idf = rep(1,length(sentiWSWordlist$word)))

twitterDataBaselineWithAllDicts<-read_delim("twitter data/twitterDataBaselineWithAllDicts.csv",",")

twitterDataBaselineWithAllDicts<-twitterDataBaselineWithAllDicts%>%mutate(text=cleanTweetText(text))

twitterDataBaselineWithAllDicts$SentiWS<-as.numeric(unlist(predict(sentiWSDict, twitterDataBaselineWithAllDicts$text)))
twitterDataBaselineWithAllDicts<-twitterDataBaselineWithAllDicts%>%mutate(sentiWS=round(sentiWS, 3))

write(sentiWSDict,"dictionarys/sentiWSDict.dict")
