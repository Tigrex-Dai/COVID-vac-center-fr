library(ggplot2)
library(dplyr)
library(plotly)
library(base)
library(formattable)
library(highcharter)
library(broom)
library(shiny)

tabcov<-read.csv("https://www.data.gouv.fr/fr/datasets/r/f335f9ea-86e3-4ffa-9684-93c009d5e617", header = T, sep = ",")

colnames(tabcov)

tabcovid<-tabcov %>% select (c("date","incid_hosp", "incid_rad", "incid_dchosp"))
colnames(tabcovid)<- c("date","hospitalises", "rétablis", "decedes") 
tabcovid19<-tabcovid %>% filter(!is.na(tabcovid$hosp))
tabcovid19$date<-as.Date(tabcovid19$date)



sign_formatter <- formatter("span", 
                            style = x ~ style(color = ifelse(x > 0, "green", 
                                                             ifelse(x < 0, "red", "black"))))
tabcovid19 %>%
  formattable(list(
    hospitalises = color_tile("transparent","darkorange"),
    rétablis = color_tile("transparent", "darkgreen"),
    decedes = color_tile("transparent", "darkred")))

tabcov2<-read.csv("https://www.data.gouv.fr/fr/datasets/r/fa4ad329-14ec-4394-85a4-c5df33769dff", header = T, sep = ";")
tabfinal<-tabcov%>%inner_join(tabcov2,by =c("date" = "jour"))
colnames(tabfinal)
tabcovfinal<-tabfinal %>% select (c("date","n_complet", "incid_dchosp"))

colnames(tabcovfinal)<- c("date","vaccines2", "decedes")

#tabcovfinal<-tabcovfinal[c(4,35,64,94,125,155,186,216,247,278,308,339,353),]
tabcovfinal$vaccines2<-log(tabcovfinal$vaccines2)
tabcovfinal$decedes<-log(tabcovfinal$decedes)
tabcovfinal
max(tabcovfinal$vaccines2)
#Nuage de points + droite de régression
ui <- fluidPage(
  fluidRow(
    column(width = 6,
           plotOutput("plot1", height = 350,
                      click = "plot1_click",
                      brush = brushOpts(
                        id = "plot1_brush"
                      )
           ),
           actionButton("exclude_toggle", "Toggle points"),
           actionButton("exclude_reset", "Reset")
    )
  )
)

server <- function(input, output) {
  # For storing which rows have been excluded
  vals <- reactiveValues(
    keeprows = rep(TRUE, nrow(tabcovfinal))
  )
  
  output$plot1 <- renderPlot({
    # Plot the kept and excluded points as two separate data sets
    keep    <- tabcovfinal[ vals$keeprows, , drop = FALSE]
    exclude <- tabcovfinal[!vals$keeprows, , drop = FALSE]
    
    ggplot(keep, aes(decedes, vaccines2)) + geom_point() +
      geom_smooth(method = lm, fullrange = TRUE, color = "black") +
      geom_point(data = exclude, fill = NA, color = "black") +
      coord_cartesian(xlim = c(1, 7), ylim = c(1,14))
  })
  
  # Toggle points that are clicked
  observeEvent(input$plot1_click, {
    res <- nearPoints(tabcovfinal, input$plot1_click, allRows = TRUE)
    
    vals$keeprows <- xor(vals$keeprows, res$selected_)
  })
  
  # Toggle points that are brushed, when button is clicked
  observeEvent(input$exclude_toggle, {
    res <- brushedPoints(tabcovfinal, input$plot1_brush, allRows = TRUE)
    
    vals$keeprows <- xor(vals$keeprows, res$selected_)
  })
  
  # Reset all points
  observeEvent(input$exclude_reset, {
    vals$keeprows <- rep(TRUE, nrow(tabcovfinal))
  })
  
}

shinyApp(ui, server)
