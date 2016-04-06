
####################################################################################
################################ Load Data #########################################
####################################################################################
rm(list = ls(all = TRUE))
gc()

load("./data/weather.RData")

####################################################################################
############################ Data Management #######################################
####################################################################################
# Format colums ---------------------------------------------------------------------

weather$Date2 <- with(weather, paste(Date, Time, sep = " "))

weather$Date2 <- as.POSIXct(
	strptime(weather$Date2, format = "%m/%d/%Y %H:%M %p")
)


# Month - Year Date -----------------------------------------------------------------
# Create a month year vector
weather$my <- strftime(weather$Date2, format = "%m/%Y")

# Create a day-month-year vector ----------------------------------------------------

weather$dmy <- strftime(weather$Date2, format = "%d/%m/%Y")

weather$dmy <- as.Date(weather$dmy, format = "%d/%m/%Y")

# Check vector
aggregate(Temp_Out ~ dmy , data = weather,
					FUN = function (x) mean(x, na.rm = TRUE))


# Wind Speed ------------------------------------------------------------------------

weather$Wind_Speed <- as.double(weather$Wind_Speed)

# Create a weekday vector vector ----------------------------------------------------

library(lubridate)

weather$week <- week(weather$Date2)
# Check vector
aggregate(Temp_Out ~ week , data = weather,
					FUN = function (x) mean(x, na.rm = TRUE))


####################################################################################
################################## Corelation #####################################
####################################################################################


# Hourly correlation plot -----------------------------------------------------------

plot(weather[, c("Temp_Out", "Hum_Out", "Dew_Pt", "Wind_Speed",
															"Heat_Index", "Solar_Rad", "UV_Index", "UV_Dose")])


# Daily aggregation -----------------------------------------------------------------

library(dplyr)
day_df <- weather %>% group_by(dmy) %>% 
	summarise(Temp_Out = mean(Temp_Out, na.rm = TRUE),
						Hum_Out = mean(Hum_Out, na.rm = TRUE),
						Dew_Pt = mean(Dew_Pt, na.rm = TRUE),
						Wind_Speed = mean(Wind_Speed, na.rm = TRUE),
						Heat_Index = mean(Heat_Index, na.rm = TRUE),
						Solar_Rad = mean(Solar_Rad, na.rm = TRUE),
						UV_Index = mean(UV_Index, na.rm = TRUE),
						UV_Dose = mean(UV_Dose, na.rm = TRUE)
	)				


# Weekly aggregatopm ------------------------------------------------------------


library(dplyr)
week_df <- weather %>% group_by(week) %>% 
	summarise(Temp_Out = mean(Temp_Out, na.rm = TRUE),
						Hum_Out = mean(Hum_Out, na.rm = TRUE),
						Dew_Pt = mean(Dew_Pt, na.rm = TRUE),
						Wind_Speed = mean(Wind_Speed, na.rm = TRUE),
						Heat_Index = mean(Heat_Index, na.rm = TRUE),
						Solar_Rad = mean(Solar_Rad, na.rm = TRUE),
						UV_Index = mean(UV_Index, na.rm = TRUE),
						UV_Dose = mean(UV_Dose, na.rm = TRUE)
	)				

week_df <- week_df[1:nrow(week_df)-1, ]
####################################################################################
################################## Save Data #######################################
####################################################################################

save.image(file = "./data/weather.RData")

