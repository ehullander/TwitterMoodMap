
require('RJSONIO')
require("googleVis")


#Reads JSON data and Dictionary files

f<-system('python twitter.py', intern=TRUE)
#f=file(filename)
date<-date()
statedata <- read.csv("statepopDMA.csv", colClasses = "character")
AFINN <- read.table("AFINN-111.txt", header=FALSE, sep="\t",  quote='', comment='',colClasses = c("character", "numeric"))
AFINN[,2]<-as.integer(AFINN[,2])
tweets<-data.frame()
#linn<-readLines(f)
JSON<-0
j=0
L=length(f)
for (i in 1:L)
{
  if (isValidJSON(I(f[i]))==TRUE)
  {
    j=j+1
    
    JSON<-fromJSON(f[i])
    if(!is.null(JSON$text)&!is.null(JSON$place$full_name))
    {
    tweets[j,1]<-JSON$text
    tweets[j,2]<-JSON$place$full_name
    }
  }
  
}
#close(f)

#Tokenizes a string of text
#merges with the sentiment dictionary AFINN
#Sums the sentiment of the text, and returns
sentiment<-function(text)
{
  s<-data.frame(strsplit(text," "))
  x<-merge(s, AFINN, by.x=names(s),by.y='V1', all.x=FALSE)
  sum(x$V2, na.rm=TRUE)
}

tweets$scores<-vapply(tweets[,'V1'],sentiment, FUN.VALUE=integer(1))
tweets<-merge(statedata,tweets,by.x='CityState', by.y='V2')
DMAs<-unique(data.frame(Region=statedata$DMA.Region,DMA=as.numeric(statedata$DMA.Region.Code)))

tweets$ones<-1
df<-with(tweets, tapply(scores, DMA.Region.Code, mean, na.rm=TRUE, row.names=NULL, simplify=FALSE))
dt<-with(tweets, tapply(scores, DMA.Region.Code, sd, na.rm=TRUE,  simplify=FALSE))
dx<-with(tweets, tapply(ones, DMA.Region.Code, sum, na.rm=TRUE,  simplify=FALSE))
df<-data.frame(DMA=as.numeric(row.names(df)),scores=as.numeric(df), sd=as.numeric(dt), count=as.numeric(dx))
df<-merge(df,DMAs,by.x='DMA',by.y='DMA')
maxscore<-max(df$scores)
df$scores<-round(df$scores,2)
df$sd<-round(df$sd,2)


#df<-rbind(c(1000,-3),df)
#df<-rbind(c(1000,3),df)
df<-cbind(df,date)


write.table(df,"tempscores.csv", append=TRUE, sep=",", col.names=NA)
#Google Intensity plot

Intensity1 <- gvisGeoChart(df, "DMA", "scores", hovervar = "Region",
                           options=list(region="US", displayMode="regions", 
                                        resolution="metros", colors="['#0033CC','#999999','#FFFF00']"))

plot(Intensity1)

Intensity2 <- gvisGeoChart(df, "DMA", "count", hovervar = "Region",
                           options=list(region="US", displayMode="regions", 
                                        resolution="metros", colors="['#FFFFFF','#000000']"))

plot(Intensity2)
