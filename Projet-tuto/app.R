library(shiny)
library(shinyWidgets)
library(dslabs)
library(tidyverse)
library(plotly)


library(dplyr)
library(tidyverse)
library(ggplot2)
library(base)
library(stringr)
library(shiny)
library(shinyWidgets)
library(dslabs)
library(plotly)
library(lubridate)



cov<-read.csv2("https://raw.githubusercontent.com/ec-jrc/COVID-19/master/data-by-region/jrc-covid-19-all-days-by-regions.csv", header = T, sep = ",")
str(cov)
cov$Date <- ymd(as.Date(cov$Date , format="%Y-%m-%d"))


cov <- mutate(cov) %>% 
  pivot_longer(cols = c(CumulativeDeceased), 
               names_to = "data", values_to = "value")

ui <- fluidPage(
  
  titlePanel("Nombre des cas par pays (EUROPE)"),
  sidebarLayout(
    sidebarPanel(
      # inputs
      checkboxGroupInput("covInput", "Pays",
                         choices = c(
                           "Germany",
                           "Switzerland", 
                           "Luxembourg"
                           ,"Belgium", "Spain", "Netherlands","Greece","Portugal","Sweden","Finland", "Hungary","Slovakia"))
      
      
    ),  
    
    mainPanel(
      plotOutput("covplot"),
      br(), br(),
      
      br(), br(),
      plotlyOutput("distplot")
    ) 
  )   
)   

server <- function(input, output) {
  
  d <- reactive({
    cov %>%
      filter(CountryName %in% input$covInput)
  }) 
  
  
  output$covplot <- renderPlot({
    
    ggplot(d(), aes(x=Date, y = CumulativePositive, color=CountryName)) +
      geom_line() + 
      theme_bw() +
      xlab("Temps") +
      ylab(input$dataInput) +
      ggtitle("Nombre des cas en fonction du temps")
  })
  
  output$stats <- renderPrint({
    
    aggregate(value ~ cov, data = d(), sum)
    
  })
  
  output$distplot <- renderPlotly({
    
    box <- plot_ly(d(), y = ~value,
                   color = ~CountryName, type = "box")  %>%
      layout(title = "Distribution des nouveaux cas par pays",
             yaxis = list(title=input$dataInput))
    
  })
  
}

shinyApp(ui=ui, server=server)
