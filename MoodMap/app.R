
library(shiny)
require("googleVis")

DF <- read.csv("tweetscores.csv", colClasses = "character")
DF<-DF[DF$X!="",]
DF$date<-as.Date(DF$date, "%m/%d/%Y %H:%M")
DF$count<-as.double(DF$count)
DF$scores<-as.double(DF$scores)
DF$sd<-as.double(DF$sd)
dates<-unique(as.character(DF$date))
uniqueregions=as.character(unique(DF$Region))
uniqueDMA=as.character(unique(DF$DMA))
names(uniqueDMA)<-uniqueregions
uniqueDMA<-c("",uniqueDMA)

#names(regionlist)<-"DMA"
#regionlist$DMA<-as.character(regionlist$DMA)

#week<-weekdays(as.Date(dates))

ui <- fluidPage(
  mainPanel(
  HTML('Mood of the Nation based on twitter tweets </br>'),
  htmlOutput("gvis"),
  
  htmlOutput("timeline"),
  sliderInput(inputId = "when", 
              label= "Date:",
              value=1,
              min=1, 
              max=length(dates), 
              animate=TRUE,
              step=1),
  HTML('Pick a mininum tweet count'),
  numericInput(inputId = "qty", 
              label="MinTweets", 
              value=2),
  HTML('Pick a Region (default is SF, CA)'),
  selectInput(inputId="DMA",
              label = "DMA Region",
              choices=uniqueDMA),
  
  #textInput(inputId = "DMA", 
  #             label="DMA Region", 
  #             value=""),

  
    dataTableOutput('table')
  )
  
)


server <- function(input, output) {

  dt<- reactive({
    if(input$DMA!="")
    {
      dt<-subset(DF,DMA==input$DMA)
    }
    else
    {
      dt<- subset(DF,as.numeric(count)>input$qty)
    }
    
  })
  

  df<- reactive({
    if(input$DMA!="")
    {
      df<-subset(DF,date==dates[input$when] & DMA==input$DMA)
    }
    else
    {
      df<- subset(DF,date==dates[input$when] & as.numeric(count)>input$qty)
    }
    
  })
  
  st<- reactive({
    if(input$DMA!="")
    {
      #st<-paste("US-",input$DMA, sep="")
      st<- "US"
    }
    else
    {
      st<- "US"
    }
    
  })
  
  output$gvis <- renderGvis({
                  gvisGeoChart(df(), "DMA", "scores", hovervar = "Region",
                              options=list(region=st(), displayMode="regions", 
                                          resolution="metros", 
                                          colorAxis="{minValue:'-1.5', maxValue:'1.5'}",
                                          colors="['#0033CC','#999999','#FFFF00']"))
                            })
  output$timeline<-renderGvis({
                  gvisAnnotatedTimeLine(dt(),
                                        datevar = "date", 
                                        numvar="scores", 
                                        idvar = "",
                                        titlevar="", annotationvar="",
                                        date.format = "%m/%d/%Y %H:%M",
                                        options = list())
                             })

  
  output$table <- renderDataTable(df())
  
}
shinyApp(ui = ui, server = server)
#shinyapps::deployApp()