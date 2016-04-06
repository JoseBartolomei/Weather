
####################################################################################
################################ Load Data #########################################
####################################################################################
rm(list = ls(all = TRUE))
gc()

load("./data/weather.Rdata")

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

# Create a weekday vector vector ----------------------------------------------------

library(lubridate)

weather$week <- week(weather$Date2)
# Check vector
aggregate(Temp_Out ~ week , data = weather,
					FUN = function (x) mean(x, na.rm = TRUE))

####################################################################################
################################## Save Data #######################################
####################################################################################

sa