
require('RJSONIO')
require("googleVis")

#Reads JSON data and Dictionary files
filename<-"output10.txt"
f=file(filename)
date<-file.mtime(filename)
statedata <- read.csv("statepopDMA.csv", colClasses = "character")
AFINN <- read.table("AFINN-111.txt", header=FALSE, sep="\t",  quote='', comment='',colClasses = c("character", "numeric"))
tweets<-data.frame()
linn<-readLines(f)
JSON<-0
j=0
for (i in 1:length(linn))
{
  if (isValidJSON(I(linn[i]))==TRUE)
  {
    j=j+1
    
    JSON<-fromJSON(linn[i])
    if(!is.null(JSON$text))
    {
    tweets[j,1]<-JSON$text
    tweets[j,2]<-JSON$place$full_name
    }
  }
  
}
close(f)

#joins state info with tweet info
test<-merge(statedata,tweets,by.x='CityState', by.y='V2')

#Tokenizes a string of text
#merges with the sentiment dictionary AFINN
#Sums the sentiment of the text, and returns
sentiment<-function(text)
{
  s<-data.frame(strsplit(text," "))
  names(s)='words'
  x<-merge(s, AFINN, by.x='words',by.y='V1', all.x=TRUE)
  sum(x$V2, na.rm=TRUE)
}

#build data frame with scores of each tweet.  Could use tapply here
mysent<-0
mysent<-data.frame()
for(i in 1:length(tweets[,1]))
{

  sentstate<-data.frame(text=tweets[i,1],score=sentiment(tweets[i,1]),DMA=test$DMA.Region.Code[i])
  mysent<-rbind(mysent, sentstate)

}
mysent$ones<-1
df<-with(mysent, tapply(score, DMA, sum, na.rm=TRUE, row.names=NULL, simplify=FALSE))
df<-data.frame(DMA=as.numeric(row.names(df)),scores=as.numeric(df))
DMAs<-unique(data.frame(Region=statedata$DMA.Region,DMA=as.numeric(statedata$DMA.Region.Code)))
df<-merge(df,DMAs,by.x='DMA',by.y='DMA')
df<-rbind(c(1000,100),df)
df<-rbind(c(1000,-100),df)
df<-cbind(df,date)

dt<-with(mysent, tapply(ones, DMA, sum, na.rm=TRUE, row.names=NULL, simplify=FALSE))
dt<-data.frame(DMA=as.numeric(row.names(dt)),count=as.numeric(dt))
DMAs<-unique(data.frame(Region=statedata$DMA.Region,DMA=as.numeric(statedata$DMA.Region.Code)))
dt<-merge(dt,DMAs,by.x='DMA',by.y='DMA')
dt<-cbind(dt,date)

write.table(df,"tweetscores.csv", append=TRUE, sep=",", col.names=NA)
#Google Intensity plot
Intensity1 <- gvisGeoChart(df, "DMA", "scores", hovervar = "Region",
                           options=list(region="US", displayMode="regions", 
                                        resolution="metros", colors="['#0033CC','#999999','#FFFF00']"))

plot(Intensity1)

Intensity2 <- gvisGeoChart(dt, "DMA", "count", hovervar = "Region",
                           options=list(region="US", displayMode="regions", 
                                        resolution="metros", colors="['#FFFFFF','#000000']"))

plot(Intensity2)
