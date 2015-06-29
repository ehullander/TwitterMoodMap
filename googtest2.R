

require("googleVis")


#Reads JSON data and Dictionary files

df <- read.csv("tweetscores.csv", colClasses = "character")

Intensity1 <- gvisGeoChart(df, "DMA", "scores", hovervar = "Region",
                           options=list(region="US", displayMode="regions", 
                                        resolution="metros", colors="['#0033CC','#999999','#FFFF00']"))

plot(Intensity1)

Intensity2 <- gvisGeoChart(df, "DMA", "count", hovervar = "Region",
                           options=list(region="US", displayMode="regions", 
                                        resolution="metros", colors="['#FFFFFF','#000000']"))

plot(Intensity2)
