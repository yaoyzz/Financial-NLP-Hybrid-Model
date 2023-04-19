library(stringr)
library(dplyr)
library(tidyverse)
library(ggplot2)

pred = read.csv("30_days_change_prediction.csv")
price = read.csv("AAPL_Price_2009_12_2023_3.csv")

price_cols = c('Date', 'Open',"Close")
price = select(price, all_of(price_cols)) %>%
  rename("date" = "Date",
         "open" = "Open",
         "close" = "Close")

# Left join the data frames on the "id" column
merged_df <- merge(pred, price, by = "date", all.x = TRUE)

#
head(merged_df)


# Create a vector to store "change_30days" values for calculating means
change_vec <- numeric()

# Iterate through the rows of the data frame
for(i in 1:nrow(merged_df)) {
  # Check if the "open" column is NA
  if(is.na(merged_df[i, "open"])) {
    # Add the current and previous row's "change_30days" values to the vector
    change_vec <- c(change_vec, merged_df[i, "change_30days"], 
                    merged_df[i-1, "change_30days"][!is.na(merged_df[i-1, "open"])])
  } else {
    # Replace the "change_30days" value with the mean of non-NA values in the vector
    if(length(change_vec) > 0) {
      merged_df[i-1, "change_30days"] <- mean(change_vec)
      # Clear the vector
      change_vec <- numeric()
    }
  }
}
rownames(merged_df) <- seq(nrow(merged_df))

# Iterate again to replace prediction with mean
for (i in nrow(merged_df):1) {
  if (is.na(merged_df[i, "open"])) {
    merged_df[i-1, "change_30days"] <- merged_df[i, "change_30days"]
  }
}

# delete NA
merged_df <- na.omit(merged_df)
rownames(merged_df) <- seq(nrow(merged_df))

#add pred_30days column
merged_df$comparative_change <- (1 + merged_df$change_30days/100)

first_close = merged_df[31,'close']
merged_df <- merged_df %>%
  mutate(pred_price = ifelse(row_number() <= 30, comparative_change * first_close, NA))


merged_df <- merged_df %>% 
  mutate(pred_price = lag(pred_price, 30))


# # make a new column pred_price = pred_30days from 14 rows before that row
# merged_df$pred_price <- c(rep(NA, 14), merged_df$pred_30days[1:(nrow(merged_df)-14)])

merged_df=merged_df[31:nrow(merged_df), ]
# reset row number
rownames(merged_df) <- seq(nrow(merged_df))


# Iterate over each row of the merged_df
for (i in 1:nrow(merged_df)) {
  
  # Calculate the new value for pred_price
  value <- merged_df[i, "pred_price"] * merged_df[i, "comparative_change"]
  
  # Assign the new value to pred_price 30 rows beneath
  if (i <= (nrow(merged_df) - 30)) {
    merged_df[i+30, "pred_price"] <- value
  }
  
}

#---------------------------Project all predictions-----------------------------

# reset row number
rownames(merged_df) <- seq(nrow(merged_df))
merged_df = select(merged_df, all_of( c('date', 'close',"pred_price") )) %>%
  rename("close_ensemble" = "pred_price")

# change date's data type
merged_df$date <- as.Date(merged_df$date)

# import Time series models' prediction
pred_time_series = read.csv("time_series_prediction.csv")
pred_time_series$date <- as.Date(pred_time_series$date)

# Inner join the tabels
merged_df <- inner_join(merged_df, pred_time_series, by = "date")

# Plot close, close_prophet, close_arima, and close_ensemble
ggplot(merged_df, aes(x = date)) + 
  geom_line(aes(y = close, color = "Close")) +
  geom_line(aes(y = close_prophet, color = "Close Prophet")) +
  geom_line(aes(y = close_arima, color = "Close ARIMA")) +
  geom_line(aes(y = close_ensemble, color = "Close Ensemble")) +
  labs(title = "Close Price vs. Predicted Price",
       y = "Price",
       color = "Legend") +
  scale_color_manual(name = "",
                     values = c("Close" = "blue", 
                                "Close Prophet" = "green",
                                "Close ARIMA" = "orange",
                                "Close Ensemble" = "red"))

