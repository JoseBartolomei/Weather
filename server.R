library(shiny)
library(ggplot2)

# Load data
load("./data/weather.Rdata")

## Server
shinyServer(function(input, output) {


    output$hourly <- renderPlot({
    	
ggplot(data = weather, aes(x = Date2, y = switch (input$measure,
																					"Outside Temperature" = weather$Temp_Out,
																					"Outside Humidity" = weather$Hum_Out,
																					"Dew Point" = weather$Dew_Pt,
																					"Wind Speed" = weather$Wind_Speed,
																					"Heat Index" = weather$Heat_Index,
																					"Solar Radiation" = weather$Solar_Rad,
																					"UV Index" = weather$UV_Index,
																					"UV Dose" = weather$UV_Dose))) +
    		geom_line(alpha = 0.5) +
    		
    		geom_smooth(na.rm = TRUE, colour = "green", fill = "pink") +
    		
    		ylab(switch(input$measure,
    								 "Outside Temperature" = "Fahrenheit",
    								 "Outside Humidity" = "Percent",
    								 "Dew Point" = "Fahrenheit",
    								 "Wind Speed" = "Miles per hour",
    								 "Heat Index" = "Fahrenheit",
    								"Solar Radiation" = "Watts per square meter (Wh/m^2)", 
    								"UV Index" = "Minimum Erythemal Dose (MEDs)",
    								"UV Dose" = "Minimum Erythemal Dose (MEDs)")) +
    			
    		xlab("Date - Time") +
    		
    		ggtitle(paste("Hourly measurement of", switch(input$measure,
    									 "Outside Temperature" = "outside temperature",
    									 "Outside Humidity" = "humidity",
    									 "Dew Point" = "dew point",
    									 "Wind Speed" = "wind speed",
    									 "Heat Index" = "heat index",
    									 "Solar Radiation" = "solar radiation", 
    									 "UV Index" = "UV index",
    									 "UV Dose" = "UV dose"), 
    									"at Universidad del Este, Carolina, Puerto Rico from", min(weather$Date2, na.rm = TRUE),
    									"to",
    									max(weather$Date2, na.rm = TRUE), sep = " ")
    		)
        
    })
    
    output$daily <- renderPlot({
    	ggplot(data = weather, aes(x = dmy, y = switch (input$measure,
    																									"Outside Temperature" = weather$Temp_Out,
    																									"Outside Humidity" = weather$Hum_Out,
    																									"Dew Point" = weather$Dew_Pt,
    																									"Wind Speed" = weather$Wind_Speed,
    																									"Heat Index" = weather$Heat_Index,
    																									"Solar Radiation" = weather$Solar_Rad,
    																									"UV Index" = weather$UV_Index,
    																									"UV Dose" = weather$UV_Dose))) +
    		stat_summary(fun.data = "mean_cl_boot", geom = "line", na.rm = TRUE) +
    		geom_smooth(na.rm = TRUE, colour = "green", fill = "pink") +
    		
    		ylab(switch(input$measure,
    								"Outside Temperature" = "Fahrenheit",
    								"Outside Humidity" = "Percent",
    								"Dew Point" = "Fahrenheit",
    								"Wind Speed" = "Miles per hour",
    								"Heat Index" = "Fahrenheit",
    								"Solar Radiation" = "Watts per square meter (Wh/m^2)", 
    								"UV Index" = "Minimum Erythemal Dose (MEDs)",
    								"UV Dose" = "Minimum Erythemal Dose (MEDs)")) +
    		
    		xlab("Day") +
    		
    		ggtitle(paste(switch(input$measure,
    												"Outside Temperature" = "Outside temperature",
    												"Outside Humidity" = "Humidity",
    												"Dew Point" = "Dew point",
    												"Wind Speed" = "Wind speed",
    												"Heat Index" = "Heat index",
    												"Solar Radiation" = "Solar radiation", 
    												"UV Index" = "UV index",
    												"UV Dose" = "UV dose"),
    									"daily mean",
    									"at Universidad del Este, Carolina, Puerto Rico from",
    									min(weather$dmy, na.rm = TRUE),
    									"to",
    									max(weather$dmy, na.rm = TRUE), sep = " "))
    })
    
})