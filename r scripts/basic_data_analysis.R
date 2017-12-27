require(tidyverse)
require(ggraph2)
library(gridExtra)
library(grid)
source('r scripts/cleanHashtags.R')
source('r scripts/cleanTweetText.R')

twitter_data_sample<-read_csv("twitter data/twitter_data.csv", 
                                locale = locale())

# count of tweets als Retweets
twitter_data_retweets<-twitter_data_sample%>%filter(isRetweet==1)

nrow(twitter_data_sample)
nrow(twitter_data_retweets)

# count of tweets als Quotes
twitter_data_quotes<-twitter_data_sample%>%filter(isQuote==1)
nrow(twitter_data_quotes)

# count of tweets als Antworten auf andere Tweets
twitter_data_replies<-twitter_data_sample%>%filter(inReplyToUser!='null')
nrow(twitter_data_replies)

#Plot for Basic Analysis
basic <-c("Retweet","Quotes","Answer")
values <-c(nrow(twitter_data_retweets),nrow(twitter_data_quotes),nrow(twitter_data_replies))
basic_plot_data<-data.frame(basic,values)
basic_plot <- ggplot(basic_plot_data, aes(y = values,x =basic))
basic_plot +geom_bar(stat = "identity", fill='#890E1C') + labs(x = "", y="count of tweets")

# Timeline Tweets weeksTillElection
twitter_data_timeline<-twitter_data_sample%>%select(ID, party, retweetCount, weeksTillElection)
twitter_data_timeline<-twitter_data_timeline[order(twitter_data_timeline$weeksTillElection, decreasing=TRUE),]
twitter_data_timeline<-twitter_data_timeline%>%
  group_by(party, weeksTillElection)%>%
    summarise(count=n(),retweetCount=sum(retweetCount))

ggplot(twitter_data_timeline,aes(x=weeksTillElection,y=count))+
  geom_bar(stat="identity" ,fill='#890E1C')+
    ylab("count of tweets")+
      xlab("weeks until election")

ggplot(twitter_data_timeline,aes(x=weeksTillElection,y=retweetCount))+
  geom_bar(stat="identity", fill='#890E1C')+
    ylab("Anzahl Retweets")+
      xlab("weeks until election")

# Timeline AfD

twitter_data_timeline_afd<-twitter_data_timeline%>%filter(party=='AfD')

plot_timeline_afd<-ggplot(twitter_data_timeline_afd,aes(x=weeksTillElection,y=count))+
  geom_bar(stat="identity", fill="blue")+
    ylab("count of tweets")+
      xlab("weeks until election")+
        labs(title="AfD")
      

# Timeline SPD

twitter_data_timeline_spd<-twitter_data_timeline%>%filter(party=='SPD')

plot_timeline_spd<-ggplot(twitter_data_timeline_spd,aes(x=weeksTillElection,y=count))+
  geom_bar(stat="identity", fill="red")+
    ylab("count of tweets")+
      xlab("weeks until election")+
        labs(title="SPD")

# Timeline CDU

twitter_data_timeline_cdu<-twitter_data_timeline%>%filter(party=='CDU')

plot_timeline_cdu<-ggplot(twitter_data_timeline_cdu,aes(x=weeksTillElection,y=count))+
  geom_bar(stat="identity", fill="black")+
    ylab("count of tweets")+
      xlab("weeks until election")+
        labs(title="CDU")

# Timeline CSU

twitter_data_timeline_csu<-twitter_data_timeline%>%filter(party=='CSU')

plot_timeline_csu<-ggplot(twitter_data_timeline_csu,aes(x=weeksTillElection,y=count))+
  geom_bar(stat="identity", fill="black")+
    ylab("count of tweets")+
      xlab("weeks until election")+
        labs(title="CSU")

# Timeline Linke

twitter_data_timeline_linke<-twitter_data_timeline%>%filter(party=='DIE LINKE')

plot_timeline_linke<-ggplot(twitter_data_timeline_linke,aes(x=weeksTillElection,y=count))+
  geom_bar(stat="identity", fill='#FF3399')+
    ylab("count of tweets")+
      xlab("weeks until election")+
        labs(title="Die Linke")

# Timeline Grüne

twitter_data_timeline_gruene<-twitter_data_timeline%>%filter(party=='GRUENE')

plot_timeline_gruene<-ggplot(twitter_data_timeline_gruene,aes(x=weeksTillElection,y=count))+
  geom_bar(stat="identity", fill="green")+
    ylab("count of tweets")+
      xlab("weeks until election")+
        labs(title="Grüne")

# Timeline FDP

twitter_data_timeline_fdp<-twitter_data_timeline%>%filter(party=='FDP')

plot_timeline_fdp<-ggplot(twitter_data_timeline_fdp,aes(x=weeksTillElection,y=count))+
  geom_bar(stat="identity", fill="yellow")+
    ylab("count of tweets")+
      xlab("weeks until election")+
        labs(title="FDP")


#CSU aus Grid entfern für bessere 3x2 Darstellung, sehr geringes Tweetvolume
grid.arrange(plot_timeline_afd,
             plot_timeline_gruene,
             plot_timeline_spd,
             plot_timeline_cdu,
             plot_timeline_linke,
             plot_timeline_fdp)
