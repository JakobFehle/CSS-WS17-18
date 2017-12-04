require(tidyverse)
require(ggraph2)
library(gridExtra)
library(grid)
source('scripts/cleanHashtags.R')
source('scripts/cleanTweetText.R')

twitter_data_sample<-read_csv("data/twitter_data_sample.csv", 
                                locale = locale())

# Anzahl Tweets als Retweets
twitter_data_retweets<-twitter_data_sample%>%filter(isRetweet==1)
nrow(twitter_data_retweets)

# Anzahl Tweets als Quotes
twitter_data_quotes<-twitter_data_sample%>%filter(isQuote==1)
nrow(twitter_data_quotes)

# Anzahl Tweets als Antworten auf andere Tweets
twitter_data_replies<-twitter_data_sample%>%filter(inReplyToUser!='null')
nrow(twitter_data_replies)

# Timeline Tweets weeksTillElection
twitter_data_timeline<-twitter_data_sample%>%select(ID, party, retweetCount, weeksTillElection)
twitter_data_timeline<-twitter_data_timeline[order(twitter_data_timeline$weeksTillElection, decreasing=TRUE),]
twitter_data_timeline<-twitter_data_timeline%>%
  group_by(party, weeksTillElection)%>%
    summarise(count=n(),retweetCount=sum(retweetCount))

ggplot(twitter_data_timeline,aes(x=weeksTillElection,y=count))+
  geom_bar(stat="identity")+
    ylab("Anzahl Tweets")+
      xlab("Wochen bis zur Wahl")

ggplot(twitter_data_timeline,aes(x=weeksTillElection,y=retweetCount))+
  geom_bar(stat="identity")+
    ylab("Anzahl Retweets")+
      xlab("Wochen bis zur Wahl")

# Timeline AfD

twitter_data_timeline_afd<-twitter_data_timeline%>%filter(party=='AfD')

plot_timeline_afd<-ggplot(twitter_data_timeline_afd,aes(x=weeksTillElection,y=count))+
  geom_bar(stat="identity")+
    ylab("Anzahl Tweets")+
      xlab("Wochen bis zur Wahl")+
        labs(title="AfD")
      

# Timeline SPD

twitter_data_timeline_spd<-twitter_data_timeline%>%filter(party=='SPD')

plot_timeline_spd<-ggplot(twitter_data_timeline_spd,aes(x=weeksTillElection,y=count))+
  geom_bar(stat="identity")+
    ylab("Anzahl Tweets")+
      xlab("Wochen bis zur Wahl")+
        labs(title="SPD")

# Timeline CDU

twitter_data_timeline_cdu<-twitter_data_timeline%>%filter(party=='CDU')

plot_timeline_cdu<-ggplot(twitter_data_timeline_cdu,aes(x=weeksTillElection,y=count))+
  geom_bar(stat="identity")+
    ylab("Anzahl Tweets")+
      xlab("Wochen bis zur Wahl")+
        labs(title="CDU")

# Timeline CSU

twitter_data_timeline_csu<-twitter_data_timeline%>%filter(party=='CSU')

plot_timeline_csu<-ggplot(twitter_data_timeline_csu,aes(x=weeksTillElection,y=count))+
  geom_bar(stat="identity")+
    ylab("Anzahl Tweets")+
      xlab("Wochen bis zur Wahl")+
        labs(title="CSU")

# Timeline Linke

twitter_data_timeline_linke<-twitter_data_timeline%>%filter(party=='DIE LINKE')

plot_timeline_linke<-ggplot(twitter_data_timeline_linke,aes(x=weeksTillElection,y=count))+
  geom_bar(stat="identity")+
    ylab("Anzahl Tweets")+
      xlab("Wochen bis zur Wahl")+
        labs(title="Die Linke")

# Timeline Grüne

twitter_data_timeline_gruene<-twitter_data_timeline%>%filter(party=='GRUENE')

plot_timeline_gruene<-ggplot(twitter_data_timeline_gruene,aes(x=weeksTillElection,y=count))+
  geom_bar(stat="identity")+
    ylab("Anzahl Tweets")+
      xlab("Wochen bis zur Wahl")+
        labs(title="Grüne")

# Timeline FDP

twitter_data_timeline_fdp<-twitter_data_timeline%>%filter(party=='FDP')

plot_timeline_fdp<-ggplot(twitter_data_timeline_fdp,aes(x=weeksTillElection,y=count))+
  geom_bar(stat="identity")+
    ylab("Anzahl Tweets")+
      xlab("Wochen bis zur Wahl")+
        labs(title="FDP")

grid.arrange(plot_timeline_afd,
             plot_timeline_gruene,
             plot_timeline_spd,
             plot_timeline_cdu,
             plot_timeline_csu,
             plot_timeline_linke,
             plot_timeline_fdp)
