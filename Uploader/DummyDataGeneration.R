# Load necessary libraries
library(lubridate)
library(ggplot2)

# Generate hourly timestamps for the last 2 years
end_time <- round(now(), "hour") + weeks(2)
#start_time <- end_time - years(2)
start_time <- round(now(), "hour")
timestamps <- seq(from = start_time, to = end_time, by = "hour")

# Create a daily pattern
daily_pattern <- function(hour) {
  peak_time <- 15 # 3pm
  low_time <- 3   # 3am
  amplitude <- 3  # Max difference from the average
  
  # Sinusoidal function for daily pattern
  avg_value <- 3.5
  daily_value <- avg_value + amplitude * sin((hour - low_time) * pi / 12)
  return(daily_value)
}

# Create a seasonal pattern
seasonal_pattern <- function(time) {
  day_of_year <- yday(time)
  max_value <- 6
  min_value <- 0.9
  
  # Sinusoidal function for seasonal pattern
  seasonal_value <- (max_value + min_value) / 2 + (max_value - min_value) / 2 * sin((day_of_year - 172) * 2 * pi / 365)
  return(seasonal_value)
}


library(RSQLite)
drv <- dbDriver("SQLite")
con <- dbConnect(drv, dbname = "~/R_trace2/R_trace2/R_Trace/appDep/AMI_Data.db")

createData <- function(i) {
# Combine patterns and add random noise
#set.seed(123) # For reproducibility
values <- sapply(timestamps, function(time) {
  hour <- hour(time)
  daily_val <- daily_pattern(hour)
  seasonal_val <- seasonal_pattern(time)
  noise <- runif(1, -0.5, 0.5)
  value <- daily_val * seasonal_val + noise
  return(max(value, 0)) # Ensure values are above zero
})

# Create a data frame
data <- data.frame(
  AcctNo = i,
  Timestamp = timestamps,
  Readings_KW = values
)
#dbWriteTable(con, "ElectricMeterReadings", data, append = TRUE)
}

listofdata <- lapply(1:11760,createData)
#listofdata <- listofdata[-which(sapply(listofdata, is.null))]
LineLoads_All <<- do.call(rbind, listofdata)
# Plot the time series data
ggplot(data, aes(x = time, y = value)) +
  geom_line() +
  labs(title = "Hourly Timeseries Data", x = "Time", y = "Value") +
  theme_minimal()

write.csv(LineLoads_All, "testdatastart.csv", row.names=FALSE)  
