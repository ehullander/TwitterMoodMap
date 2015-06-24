require('rJava')
require('NLP')
require('openNLP')
require('RJSONIO')
require("googleVis")

#Reads JSON data and Dictionary files
f=file("output10.txt","r")
statedata <- read.csv("statepopDMA.csv", colClasses = "character")
AFINN=mydata <- read.table("AFINN-111.txt", header=FALSE, sep="\t",  quote='', comment='',colClasses = c("character", "numeric"))
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

##tokenizing function.  Call this on tweets[j,1]
#returns a dataframe with bag of words and POS


#Takes dataframe returned by note
#assigns sentiment score to each word
sentiment<-function(s)
{
  names(s)='words'
  merge(s, AFINN, by.x='words',by.y='V1', all.x=TRUE)
}

mysent<-0
mysent<-data.frame()
for(i in 1:length(tweets[,1]))
{
  #tokenize the tweet
  #mynote<-note(tweets[i,1])
  s<-data.frame(strsplit(tweets[i,1]," "))
  #sentiment score the tweet
  DMA<-statedata[statedata$CityState%in%tweets$V2[i],11]
  Region<-statedata[statedata$CityState%in%tweets$V2[i],10]
  sentstate<-data.frame(sentiment(s),tweets[i,2], DMA[1], Region[1])
  mysent<-rbind(mysent, sentstate)
  #print(data.frame(mysent,tweets[i,2]))
  
}

sentscores<-data.frame(with(mysent, tapply(V2, DMA.1., sum, na.rm=TRUE, row.names=NULL, simplify=FALSE)))
df<-data.frame(DMA=as.numeric(row.names(sentscores)),scores=as.numeric(sentscores[,1]))
Intensity1 <- gvisGeoChart(df, "DMA", "scores",
                           options=list(region="US", displayMode="regions", 
                                        resolution="metros", colors="['#0033CC','#999999','#FFFF00']"))
plot(Intensity1)