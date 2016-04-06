library(shiny)
library(ggplot2)

# Load data
load("./data/weather.RData")

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
    	ggplot(data = day_df, aes(x = dmy, y = switch (input$measure,
    																									"Outside Temperature" = day_df$Temp_Out,
    																									"Outside Humidity" = day_df$Hum_Out,
    																									"Dew Point" = day_df$Dew_Pt,
    																									"Wind Speed" = day_df$Wind_Speed,
    																									"Heat Index" = day_df$Heat_Index,
    																									"Solar Radiation" = day_df$Solar_Rad,
    																									"UV Index" = day_df$UV_Index,
    																									"UV Dose" = day_df$UV_Dose))) +
    		geom_line(stat = "identity") +
    		geom_smooth() +
    		
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
    
    # week
    output$weekly <- renderPlot({
    	ggplot(data = week_df, aes(x = week, y = switch (input$measure,
    																									"Outside Temperature" = week_df$Temp_Out,
    																									"Outside Humidity" = week_df$Hum_Out,
    																									"Dew Point" = week_df$Dew_Pt,
    																									"Wind Speed" = week_df$Wind_Speed,
    																									"Heat Index" = week_df$Heat_Index,
    																									"Solar Radiation" = weather$Solar_Rad,
    																									"UV Index" = week_df$UV_Index,
    																									"UV Dose" = week_df$UV_Dose))) +
    		geom_line(stat = "identity") +

    		
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
    									"weekly mean",
    									"at Universidad del Este, Carolina, Puerto Rico from",
    									min(weather$dmy, na.rm = TRUE),
    									"to",
    									max(weather$dmy, na.rm = TRUE), sep = " ")) +
    		
    		scale_x_continuous(
    			breaks = min(weather$week, na.rm = TRUE):max(weather$week, na.rm = TRUE))
    })
    
    # cor_plot
    output$cor_plot <-renderPlot({
    switch(input$time,
    			 "Hourly" = plot(weather[, c("Temp_Out", "Hum_Out", "Dew_Pt", "Wind_Speed",
    			 														"Heat_Index", "Solar_Rad", "UV_Index", "UV_Dose")]),
    			 	"Daily" = plot(day_df),
    			 "Weekly" = plot(week_df)
    )
    			 		}, height = 800, width = 1200)

    	
    	}) # shinyServer close