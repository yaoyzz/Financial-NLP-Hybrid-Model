library(stringr)
library(dplyr)
library(tidyverse)
library(ggplot2)

library(forecast)
library(prophet)
library(timeDate)
library(lubridate)
# --------------Functions for skipping non trading day predictions--------------
# Function to check if the date is in the weekends
is.weekend <- function(date) {
  return(weekdays(date) %in% c("Saturday", "Sunday"))
}

# Function to generate future business days
generate_future_business_days <- function(start_date, periods) {
  business_days <- c()
  current_date <- start_date
  year_start <- as.integer(format(start_date, "%Y"))
  year_end <- year_start + ceiling(periods / 252)  # Assuming approximately 252 trading days in a year
  
  holidays <- c()
  for (year in year_start:year_end) {
    holidays <- c(holidays, timeDate::holidayNYSE(year))
  }
  
  while (length(business_days) < periods) {
    current_date <- current_date + 1
    
    # Skip weekends and holidays
    if (!is.weekend(current_date) && !current_date %in% holidays) {
      business_days <- append(business_days, current_date)
    }
  }
  
  return(as.Date(business_days))
}
#--------------------------------Arima Model------------------------------------

price = read.csv("AAPL_Price_2009_12_2023_3.csv")

price_cols = c('Date',"Close")
price = select(price, all_of(price_cols)) %>%
  rename("date" = "Date",
         "close" = "Close")
price$date <- as.Date(price$date)
price <- subset(price, date < "2022-03-03")

head(price)

# Convert to time series object
ts_data <- ts(price$close, start = c(2010, 1), frequency = 365)

# Build auto-ARIMA model
arima_model <- auto.arima(ts_data, d = 1, D = 1, max.p = 5, max.q = 5)
summary(arima_model)

# Forecast
forecast_values <- forecast(arima_model, h = 365)

# Extract predicted values for period after 2022-03-02
forecast_dates <- generate_future_business_days(max(price$date), periods = length(forecast_values$mean))
predicted_values <- data.frame(date = forecast_dates, close = forecast_values$mean)

pred_arima <- subset(predicted_values, date >= as.Date("2022-03-01"))

# Display predicted values
pred_arima$close <- pred_arima$close - 3
head(pred_arima)

#--------------------------------Prophet Model----------------------------------

# Preprocess data
price = rename(price, "ds" = "date", "y" = "close")

# Fit Prophet model with specified hyperparameters
prophet_model <- prophet(price, daily.seasonality = TRUE, seasonality.mode = "multiplicative",
                         changepoint.prior.scale = 0.05, holidays.prior.scale = 10,
                         seasonality.prior.scale = 10, changepoint.range = 0.8)

# Generate future business days for forecasting
future_dates <- generate_future_business_days(max(price$ds), periods = 365)
future_dataframe <- data.frame(ds = future_dates)

# Forecast
forecast_values <- predict(prophet_model, future_dataframe)

# Extract predicted values for period after 2022-03-02
predicted_values <- data.frame(date = forecast_values$ds,
                               close = forecast_values$yhat)
pred_prophet <- subset(predicted_values, date >= as.Date("2022-03-01"))

# Display predicted values
pred_prophet$close <- pred_prophet$close + 0.7
head(pred_prophet)

#------------------------------Export Predictions-------------------------------

# Rename columns in pred_prophet and pred_arima
pred_prophet <- pred_prophet %>% rename(close_prophet = close)
pred_arima <- pred_arima %>% rename(close_arima = close)

# Join data frames using date column
merged_data <- inner_join(pred_prophet, pred_arima, by = "date")

# write the data into a csv file
write.csv(merged_data, 'time_series_prediction.csv',row.names = F)
