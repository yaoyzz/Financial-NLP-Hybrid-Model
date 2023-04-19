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

#------------------------------Data Cleaning------------------------------------

setwd("/Users/xianglinlusam/Desktop/5205Final")
stock_data = read.csv("AAPL_Price_2009_12_2023_3.csv")
library(dplyr)
library(ggplot2);library(ggthemes);library(gridExtra)
library(quantmod);library(xts);library(zoo)
library(forecast)

#Force date into Date format
stock_data[,1] = as.Date(stock_data[,1])

#Check if it is date format
class(stock_data[,1])

ts_prep_close = stock_data %>%
  select(Date,Close)

#Convert it to xts object
stocks_close <- xts(ts_prep_close[,-1], order.by=(ts_prep_close[,1]))
autoplot(stocks_close)

#divide train and test set 
train_close = window(stocks_close,end = "2022-03-01")
test_close = window(stocks_close, start="2022-03-02")
#--------------------------------Arima Model------------------------------------

price = read.csv("AAPL_Price_2009_12_2023_3.csv")

View(price)

price_cols = c('Date',"Close")
price = select(price, all_of(price_cols)) %>%
  rename("date" = "Date",
         "close" = "Close")
price$date <- as.Date(price$date)
# price <- subset(price, date < "2022-03-03")

head(price)

# Convert to time series object
ts_data <- ts(price$close, start = c(2010, 1), frequency = 365)

# Build auto-ARIMA model
arima_model <- auto.arima(ts_data, d = 1, D = 1, max.p = 5, max.q = 5)
summary(arima_model)

# Forecast
forecast_values <- forecast(train_close, h = 365)

# Extract predicted values for period after 2022-03-02
forecast_dates <- generate_future_business_days(max(price$date), periods = length(forecast_values$mean))
predicted_values <- data.frame(date = forecast_dates, close = forecast_values$mean)

pred_arima <- subset(predicted_values, date >= as.Date("2022-03-01"))

# Display predicted values
pred_arima$close <- pred_arima$close - 3
head(pred_arima)


#Build it on train and test data to benchmark the accuracy
arima_model_train = auto.arima(train_close, d = 1, D = 1, max.p = 5, max.q = 5)
arima_model_train_forcast <- forecast(arima_model_train, h =365)
summary(arima_model_train)
forecast::accuracy(arima_model_train_forcast,x = test_close)

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

#Build it on train and test data to benchmark the accuracy

library(caTools)
#This is a dataframe, different from time series data
#To ensure we are on the right data length, we will spefically assign these number into spliting this df
train = price[1:3061, ]
test = price[3062:nrow(price), ]

prophet_model <- prophet(train, daily.seasonality = TRUE, seasonality.mode = "multiplicative",
                         changepoint.prior.scale = 0.05, holidays.prior.scale = 10,
                         seasonality.prior.scale = 10, changepoint.range = 0.8)

test_close_pred <- predict(prophet_model, test)


library(Metrics)
rmse(test$y, test_close_pred$yhat)

#------------------------------Average Model -----------------------------------
average_model = meanf(train_close,h = length(test_close))
average_model

avg_prep = forecast(average_model, x = test_close)

forecast::accuracy(average_model, x = test_close)

#------------------------------Drift Model--------------------------------------
drift_model = rwf(train_close,h=length(test_close),drift = T)
drift_model

forecast::accuracy(drift_model,x = test_close)

drift_prep = forecast(drift_model, x = test_close)

forecast::accuracy(drift_model,x = test_close)

#------------------------------Exponential Smoothing Models --------------------
ses_model = ses(train_close,h = length(test_close))
ses_model

ses_prep = forecast(ses_model, x = test_close)

forecast::accuracy(ses_model,x = test_close)

#------------------------------Holt Models--------------------------------------
holt_model = holt(train_close,h=length(train_close))
holt_model

holt_prep = forecast(holt_model, x = test_close)

forecast::accuracy(holt_model,x = test_close)
#------------------------------Holt_damped_model--------------------------------
holt_damped_model = holt(train_close,h=length(train_close),damped = T)
holt_damped_model

holt_damped_prep = forecast(holt_damped_model, x = test_close)

forecast::accuracy(holt_damped_model,x = test_close)

#------------------------------Performance--------------------------------------

performance = rbind(average_model = forecast::accuracy(average_model,x = test_close)[2,"RMSE"],
                    drift_model = forecast::accuracy(drift_model,x = test_close)[2,"RMSE"],
                    ses_model = forecast::accuracy(ses_model,x = test_close)[2,"RMSE"],
                    holt_model = forecast::accuracy(holt_model,x = test_close)[2,"RMSE"],
                    holt_damped_model = forecast::accuracy(holt_damped_model,x = test_close)[2,"RMSE"],
                    auto_arima = forecast::accuracy(arima_model_train_forcast,x = test_close)[2,"RMSE"],
                    prophet_model = rmse(test$y, test_close_pred$yhat)
)


order_by_rmse = order(performance[, 1])
performance_ordered = performance[order_by_rmse,]

performance_ordered


performance_df = data.frame(date = index(test_close), 
                            close_price = test_close[,1],
                            avg_prep = avg_prep$mean, 
                            Drift_model = drift_prep$mean[1:length(test_close)],
                            Exponential_Smoothing = ses_prep$mean[1:length(test_close)],
                            Holt_model = holt_prep$mean[1:length(test_close)], 
                            holt_damped_model = holt_damped_prep$mean[1:length(test_close)], 
                            auto_arima = arima_model_train_forcast$mean[1:length(test_close)],
                            prophet_model = test_close_pred$yhat)

performance_df


ggplot(performance_df, aes(x = date)) + 
  geom_line(aes(y = close_price, color = "close_price")) +
  geom_line(aes(y = avg_prep, color = "avg_prep" )) +
  geom_line(aes(y = Drift_model, color = "Drift_model")) +
  geom_line(aes(y = Exponential_Smoothing, color = "Exponential_Smoothing")) +
  geom_line(aes(y = Holt_model, color = "Holt_model" )) +
  geom_line(aes(y = holt_damped_model, color = "holt_damped_model")) +
  geom_line(aes(y = auto_arima, color = "auto_arima")) +
  geom_line(aes(y = prophet_model, color = "prophet_model")) +
  labs(title = "Close Price vs. Predicted Price",
       y = "Price",
       color = "Legend") +
  scale_color_manual(name = "",
                     values = c("close_price" = "blue", 
                                "avg_prep" = "green",
                                "Drift_model" = "black",
                                "Exponential_Smoothing" = "red", 
                                "Holt_model" = "purple", 
                                "holt_damped_model" = "gray", 
                                "auto_arima" = "navy", 
                                "prophet_model" = "cyan"))

ggplot(performance_df, aes(x = date)) + 
  geom_line(aes(y = close_price, color = "close_price")) +
  geom_line(aes(y = Drift_model, color = "Drift_model")) +
  geom_line(aes(y = Exponential_Smoothing, color = "Exponential_Smoothing", alpha = 0.05)) +
  geom_line(aes(y = Holt_model, color = "Holt_model" )) +
  geom_line(aes(y = holt_damped_model, color = "holt_damped_model"))
  labs(title = "Close Price vs. Predicted Price",
       y = "Price",
       color = "Legend") +
  scale_color_manual(name = "",
                     values = c("close_price" = "blue", 
                                "Drift_model" = "black",
                                "Exponential_Smoothing" = "red", 
                                "Holt_model" = "purple", 
                                "holt_damped_model" = "green"))

#------------------------------Export Predictions-------------------------------
# write the data into a csv file
write.csv(performance_df, 'time_series_prediction.csv',row.names = F)
