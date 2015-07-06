
require('RJSONIO')
require("ggplot2")
require("dplyr")
require("maps")


#Reads JSON data and Dictionary files
filename<-"output16.txt"
f=file(filename)
date<-file.mtime(filename)
statedata <- read.csv("statepopDMA.csv", colClasses = "character")
AFINN <- read.table("AFINN-111.txt", header=FALSE, sep="\t",  quote='', comment='',colClasses = c("character", "numeric"))
AFINN[,2]<-as.integer(AFINN[,2])
tweets<-data.frame()
linn<-readLines(f)
JSON<-0
j=0
L=length(linn)
for (i in 1:L)
{
  if (isValidJSON(I(linn[i]))==TRUE)
  {
    j=j+1
    
    JSON<-fromJSON(linn[i])
    if(!is.null(JSON$text)&!is.null(JSON$place$full_name))
    {
    tweets[j,1]<-JSON$text
    tweets[j,2]<-JSON$place$full_name
    tweets[j,3]<-JSON$place$bounding_box$coordinates[[1]][[1]][2]
    tweets[j,4]<-JSON$place$bounding_box$coordinates[[1]][[1]][1]
    }
  }
}
close(f)



#Tokenizes a string of text
#merges with the sentiment dictionary AFINN
#Sums the sentiment of the text, and returns
sentiment<-function(text)
{
  s<-data.frame(strsplit(as.character(text)," "))
  x<-merge(s, AFINN, by.x=names(s),by.y='V1', all.x=FALSE)
  sum(x$V2, na.rm=TRUE)
}
tweets$scores<-vapply(tweets[,'V1'],sentiment, FUN.VALUE=integer(1))



alpha=.1
test<-select(inner_join(statedata,tweets,by=c("CityState"="V2")),CityState,pop, scores, V3, V4)
test<-mutate(test,scorewt=scores*alpha)
#x<-model.matrix(~.+0, data=select(test,V4,V3,scorewt))
x<-select(test,V4,V3,scorewt)
y<-kmeans(x,30, nstart=25)
xclust<-data.frame(y$cluster,test)
xclust<-group_by(xclust,y.cluster)
xclustsum<-summarize(xclust,sum(scorewt),var(scorewt),n())
xclustsumcenter<-cbind(y$centers,xclustsum)
head(arrange(xclustsumcenter,V4,V3))


usa<-map_data("county")
#canmex<-filter(map_data("world"),region=="Canada"|region=="Mexico")

dfx<-data.frame(x)
ggplot(dfx, aes(dfx$V4,dfx$V3)) +
  geom_polygon(data=usa, aes(long, lat, group=group), 
               colour="black", 
               alpha=.9) +
  #geom_polygon(data=canmex, aes(long, lat, group=group), 
  #             colour="black", 
  #             alpha=.9) +
  #scale_colour_gradientn(colours=rainbow(60)) +
  geom_point(colour="green", alpha=.05, size=.5) +
  #geom_point(data=xclustsumcenter, aes(V4,V3), colour="white",size=10, alpha=.5) +
  #geom_point(colour="green", alpha=.02, size=.5) +
  xlim(-130, -60) + ylim(20, 52)

usa<-map_data("county")
#canmex<-filter(map_data("world"),region=="Canada"|region=="Mexico")

dfx<-data.frame(xclustsumcenter)
ggplot(dfx, aes(V4,V3)) +
  geom_polygon(data=usa, aes(long, lat, group=group), 
               colour="black", 
               alpha=.9) +
  #geom_polygon(data=canmex, aes(long, lat, group=group), 
  #             colour="black", 
  #             alpha=.9) +
  #scale_colour_gradientn(colours=rainbow(60)) +
  #geom_point(colour=y$cluster, alpha=.05, size=.5) +
  #geom_point(colour=-(dfx$scorewt*50)+80,size=(dfx$n..)/50, alpha=.5) +
  geom_point(colour=-(dfx$scorewt*50)+80,size=10, alpha=(dfx$n..)/max(dfx$n..)) +
  #geom_point(colour="green", alpha=.02, size=.5) +
  xlim(-130, -60) + ylim(20, 52)

y$tot.withinss/y$totss

locscore<-cbind(y$cluster,x)
####other functions
#test for alpha, a scaling factor for z-axis variable
Opt_alpha<-function(tweets)
{
  Wlamda<-data.frame(alpha=NA,lamda=NA)
  for(j in 1:10)
  {
    alpha=(j-1)/100
    test<-mutate(tweets,scorewt=scores*alpha)
    x<-model.matrix(~.+0, data=select(test,V4,V3,scorewt))
    for(i in 1:10)
    {
      y<-kmeans(x,30, nstart=25)
      Wlamda<-rbind(Wlamda, c(alpha=alpha,lamda=y$tot.withinss/y$totss))
    }
  }
}
#test for number of clusters
Opt_clust<-function(tweets)
{
  Wlamda<-data.frame(alpha=NA,lamda=NA)
  for(j in 1:100)
  {
    alpha=.01
    test<-mutate(tweets,scorewt=scores*alpha)
    x<-model.matrix(~.+0, data=select(test,V4,V3,scorewt))
    for(i in 1:1)
    {
      y<-kmeans(x,j, nstart=25)
      Wlamda<-rbind(Wlamda, c(alpha=j,lamda=y$tot.withinss/y$totss))
    }
  }
  
  ggplot(Wlamda, aes(alpha, lamda)) +
    geom_point()
}
