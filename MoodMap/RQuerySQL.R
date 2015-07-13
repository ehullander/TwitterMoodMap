library(RMySQL)
library(ggplot2)
require("googleVis")
library(dplyr)

###load workspace that includes all connection info.  Keep environment out of 
load('C:/Users/Eric/Desktop/Learning/Python/Twitterstream/myEnvironment.RData')
con <- dbConnect(MySQL(),
                 user = readline(prompt="Enter username: "),
                 password = readline(prompt="Enter password: "),
                 host = 'mydbehullander.c2hqmnrywoe9.us-west-2.rds.amazonaws.com',
                 dbname='TwitterStream')


####make class for subsetting
###need sentiment scored tweets for this
DMA<-function(todayscore)
{
statedata <- read.csv("statepopDMA.csv", colClasses = "character")
temp<-right_join(statedata,todayscore,by=c("CityState"="place"))
temp<-select(temp,DMA=DMA.Region.Code,Region=DMA.Region,scores,StateAbb,CityState,tweet)
tempgroup<-group_by(temp,DMA,Region)
summarize(tempgroup,mood=mean(scores),sd(scores),n())
}

####make class of SQL queries, key word, getme, getdate range, get region
getme<-function(term)
{
  SQL<-paste("SELECT * FROM TwitterStream.tweets WHERE tweets.tweet LIKE '%",term,"%'",sep="")
  dbGetQuery(con,SQL)
}
getday<-function(day)
  {
    SQL<-paste("SELECT * FROM TwitterStream.tweets WHERE tweets.date='",day,"'",sep="")
    dbGetQuery(con,SQL)
  }



###make classes.  Include options for google etc.
plotme<-function(df,alpha=.5,size=1)
{
  usa<-map_data("county")
  #df<-data.frame(select(df,longitude,latitude))
  #print(head(df))
  ggplot(data=df, aes(x=longitude,y=latitude)) +
    geom_polygon(data=usa, aes(long, lat, group=group), 
                 colour="black", 
                 alpha=.9) +
    geom_point(colour="green", alpha=alpha, size=size) +
  xlim(-130, -60) + ylim(20, 52)
}

plotmegoog<-function(df)
{
Intensity1 <- gvisGeoChart(df, "DMA", "mood", hovervar = "Region",
                           options=list(region="US", displayMode="regions", 
                                        resolution="metros", colors="['#0033CC','#999999','#FFFF00']"))

plot(Intensity1)
}
##Add passion score?
sent<-function(tweets)
{
  AFINN <- read.table("AFINN-111.txt", header=FALSE, sep="\t",  quote='', comment='',colClasses = c("character", "numeric"))
  AFINN[,2]<-as.integer(AFINN[,2])
  sentiment<-function(text)
  {
    s<-data.frame(strsplit(as.character(text)," "))
    x<-merge(s, AFINN, by.x=names(s),by.y='V1', all.x=FALSE)
    sum(x$V2, na.rm=TRUE)
  }
scores<-vapply(tweets[,'tweet'],sentiment, FUN.VALUE=integer(1))
cbind(tweets,scores)
}


dbDisconnect(con)


###Example
load('C:/Users/Eric/Desktop/Learning/Python/Twitterstream/myEnvironment.RData')
con <- dbConnect(MySQL(),
                 user = readline(prompt="Enter username: "),
                 password = readline(prompt="Enter password: "),
                 host = 'mydbehullander.c2hqmnrywoe9.us-west-2.rds.amazonaws.com',
                 dbname='TwitterStream')
df<-getme("obama")
df<-sent(df)
today<-DMA(df)
plotmegoog(today)
plotme(df)
dbDisconnect(con)

