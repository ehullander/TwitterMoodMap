library(ROAuth)
library(streamR)
library(RJSONIO)
library(RMySQL)
library(dplyr)

t=60
##Load Authorization keys
if(!file.exists('C:/Users/Eric/Desktop/Learning/Python/Twitterstream/myEnvironment.RData'))
{
    requestURL <- "https://api.twitter.com/oauth/request_token"
    accessURL <- "https://api.twitter.com/oauth/access_token"
    authURL <- "http://api.twitter.com/oauth/authorize"
    consumerKey <- ""
    consumerSecret <- ""
    my_oauth <- OAuthFactory$new(consumerKey=consumerKey,
                                 consumerSecret=consumerSecret, requestURL=requestURL,
                                 accessURL=accessURL, authURL=authURL)
    
    my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
    save.image("C:/Users/Eric/Desktop/Learning/Python/Twitterstream/myEnvironment.RData")
} else {load('C:/Users/Eric/Desktop/Learning/Python/Twitterstream/myEnvironment.RData')}
  
tweets<-data.frame()
  #streamtweets
  temp<-filterStream(file="",
                locations=c(-125.0011, 24.9493, -66.9326, 49.5904), 
               timeout=t, 
               oauth=my_oauth)
  #parse tweets
  j=0
  L=length(temp)
  for (i in 1:L)
  {
    if (isValidJSON(I(temp[i]))==TRUE)
    {
      j=j+1
      
      JSON<-fromJSON(temp[i])
      if(!is.null(JSON$text)&!is.null(JSON$place$full_name))
      {
        tweets[j,1]<-JSON$id_str
        tweets[j,2]<-JSON$user$screen_name
        tweets[j,3]<-as.character(strptime(paste(substr(JSON$created_at, 5,10), 
                                                 substr(JSON$created_at, 27,30), 
                                                 substr(JSON$created_at, 12,19)), 
                                           "%b %d %Y %H:%M:%S"))
        tweets[j,4]<-JSON$place$bounding_box$coordinates[[1]][[1]][2]
        tweets[j,5]<-JSON$place$bounding_box$coordinates[[1]][[1]][1]
        tweets[j,6]<-JSON$place$place_type
        tweets[j,7]<-JSON$place$full_name
        tweets[j,8]<-JSON$user$lang
        tweets[j,9]<-JSON$text  
      }
    }
  }
  #return tweets df
  colnames<-c("V1","user","date","latitude","longitude","placetype","place","lang","tweet")
  colnames(tweets)<-colnames
  #tweets
###Connect to 
con <- dbConnect(MySQL(),
                 user = readline(prompt="Enter username: "),
                 password = readline(prompt="Enter password: "),
                 host = 'mydbehullander.c2hqmnrywoe9.us-west-2.rds.amazonaws.com',
                 dbname='TwitterStream')

dbWriteTable(conn = con, name = 'tweets', value = tweets, append=TRUE)
dbDisconnect(con)
