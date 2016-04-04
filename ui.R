library(shiny)
library(ggplot2)
## User interface
ui <- fluidPage(
    headerPanel("UNE Weather Station, Puerto Rico"),
    
    sidebarPanel(
        selectInput("measure", "Select a weather measure:", 
                    choices = c("Outside Temperature", "Outside Humidity", "Dew Point",
                    						"Wind Speed", "Heat Index", "Solar Radiation",
                    						"UV Index", "UV Dose")),

        br(),
        br(),
        h4(span("Developed by", style = "color:grey")),
        img(src = "Outcome_Project_Logo.png", height = 72, width = 200),
        width = 4,
        br(),
        br()
    ),
    
    
    mainPanel(
        plotOutput("trends"),
        h6("Source: Universidad del Este")))