library(shiny)
library(ggplot2)
## User interface
ui <- navbarPage(
	title = "Universidad del Este Weather Station, Puerto Rico",
	tabPanel(title = "Time series",
	fluidPage(
    sidebarPanel(
        selectInput("measure", "Select a weather measure:", 
                    choices = c("Outside Temperature", "Outside Humidity", "Dew Point",
                    						"Wind Speed", "Heat Index", "Solar Radiation",
                    						"UV Index", "UV Dose")),

        br(),
        br(),
        h4(span("Developed by", style = "color:grey")),
        img(src = "Outcome_Project_Logo.png", height = 72, width = 200),
        width = 2,
        br(),
        br()
    ),
    
    mainPanel(
        plotOutput("hourly"),
        plotOutput("daily"),
        plotOutput("weekly"),
        h6("Data Source: Universidad del Este"))
    )), # Close first fluid page
 
# Second Panel
  tabPanel(title = "Correlation",
  fluidPage(
  	sidebarPanel(
  		selectInput("time", "Select time scale",
  								choices = c("Hourly", "Daily", "Weekly")),
  		br(),
  		br(),
  		h4(span("Developed by", style = "color:grey")),
  		img(src = "Outcome_Project_Logo.png", height = 72, width = 200),
  		width = 2,
  		br(),
  		br()
  	),
    	mainPanel(
    			 plotOutput("cor_plot", width = "100%"),
    			 h6("Data Source: Universidad del Este"))
  	)) # Close second fluid page
) # navbarPage close