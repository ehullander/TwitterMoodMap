
library(shiny)
require("googleVis")

DF <- read.csv("tweetscores.csv", colClasses = "character")
DF<-DF[DF$X!="",]
DF$date<-factor(DF$date)
DF$count<-as.double(DF$count)
DF$scores<-as.double(DF$scores)
DF$sd<-as.double(DF$sd)
dates<-unique(as.character(DF$date))
#week<-weekdays(as.Date(dates))

ui <- fluidPage(
  mainPanel(
    htmlOutput("gvis"),
  sliderInput(inputId = "when", 
              label= "Date:",
              value=1,
              min=1, 
              max=length(dates), 
              animate=TRUE,
              step=1),
  numericInput(inputId = "qty", 
              label="MinTweets", 
              value=300),
  textInput(inputId = "DMA", 
               label="DMA Region", 
               value=""),

  
    dataTableOutput('table')
  )
  
)


server <- function(input, output) {

  

  df<- reactive({
    if(input$DMA!="")
    {
      df<-subset(DF,date==dates[input$when] & as.numeric(count)>input$qty & DMA==input$DMA)
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
  
  output$table <- renderDataTable(df())
  
}
shinyApp(ui = ui, server = server)
#shinyapps::deployApp()