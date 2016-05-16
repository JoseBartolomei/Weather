
####################################################################################
################################ Load Data #########################################
####################################################################################
rm(list = ls(all = TRUE))
gc()

load("./data/weather.RData")

# library ---------------------------------------------------------------------------
library(plyr)
library(tidyr)
library(lubridate)

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

####################################################################################
################################## Aggregation #####################################
####################################################################################

####################################################################################
# Functions for aggregation -----------------------------------------------------------------
####################################################################################

UL <- function(x, ...){
	mean(x, na.rm = TRUE) + (qt(.975, df = n-1) * (sd(x, na.rm = TRUE)/sqrt(length(x))))
}

LL <- function(x, ...){
	mean(x, na.rm = TRUE) - (qt(.975, df = n-1) * (sd(x, na.rm = TRUE)/sqrt(length(x))))
}

funs <- c(LL, mean, UL)

####################################################################################
# Daily aggregation -----------------------------------------------------------------
####################################################################################
w_l <- # weather measures list by week
	lapply(X = lapply(X = funs,
										FUN = function(f) ldply(weather[, c("Temp_Out", "Hum_Out", "Dew_Pt",
																												"Wind_Speed", "Heat_Index",
																												"Solar_Rad", "UV_Index",
																												"UV_Dose")],
																						.fun = function(x) by(x, weather$dmy,
																																	FUN = function(x) f(x, na.rm = TRUE)))),
				 FUN = function(x) gather(data = x, key = week, value = "Measure",
				 												 2:length(x), convert = TRUE, factor_key = TRUE))


# weather data frame for ggplot2
gwdf <- data.frame(m_l[[1]], Upper_Limit = m_l[[2]][,3], Lower_Limit = m_l[[3]][, 3])

colnames(gwdf) <- c("measure", "week", "lower_Limit", "mean", "upper_limit")

head(gwdf)


####################################################################################
# Weekly aggregation -----------------------------------------------------------------
####################################################################################


w_l <- # weather measures list by week
	lapply(X = lapply(X = funs,
				FUN = function(f) ldply(weather[, c("Temp_Out", "Hum_Out", "Dew_Pt",
																												"Wind_Speed", "Heat_Index",
																												"Solar_Rad", "UV_Index",
																												"UV_Dose")],
																						.fun = function(x) by(x, weather$week,
				 FUN = function(x) f(x, na.rm = TRUE)))),
				 FUN = function(x) gather(data = x, key = week, value = "Measure",
				 												 2:length(x), convert = TRUE, factor_key = TRUE))


# weather data frame for ggplot2
gwdf <- data.frame(m_l[[1]], Upper_Limit = m_l[[2]][,3], Lower_Limit = m_l[[3]][, 3])

colnames(gwdf) <- c("measure", "week", "lower_Limit", "mean", "upper_limit")

head(gwdf)

###
####################################################################################
################################## Save Data #######################################
####################################################################################

save.image(file = "./data/weather.RData")

