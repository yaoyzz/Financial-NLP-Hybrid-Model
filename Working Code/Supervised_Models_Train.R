library(stringr)
library(rvest)
library(dplyr)
library(tidyverse)
library(tidytext)
library(readr)
library(tm)
library(tidyr)
library(caret)
library(Metrics)
library(ggplot2)
library(catboost)

setwd('C://Users//User//Desktop//5205//Final Project//News Data')
df = read.csv("AAPL_price_news_2010_2023.csv")

# set the random seed for reproducibility
set.seed(2023)

# filter out date
df = select(df, -date)

# split the data into training and testing sets
train_index <- createDataPartition(df$change_1day, p = 0.8, list = FALSE)
train <- df[train_index, ]
test <- df[-train_index, ]

# Check the existance of na
sum(is.na(df))

# Sum up the number of infinite values across all columns
sum(apply(df, 2, function(x) sum(is.infinite(x))))

# use grep() to find column names that start with change_
cols_change <- grep("^change_", names(df), value = TRUE)
cols_change

# Initialize an empty list to store the RMSE values
lm_rmse_list <- list()
cat_rmse_list <- list()

# loop over outcome variables
for (i in 1:length(cols_change)) {
  
  # identify the predictor variables that are not in cols_change
  predictors <- setdiff(names(train), cols_change)
  
  # fit a linear regression model using all predictor variables
  model <- lm(paste(cols_change[i], "~", paste(predictors, collapse = "+")), data = train)
  
  # use the model to predict the outcome variable
  predicted_values <- predict(model, newdata = test)
  
  # calculate the RMSE for linear regression
  rmse_value = rmse(predicted_values, test[[cols_change[i]]])
  
  # Append the RMSE value to the list
  lm_rmse_list <- append(lm_rmse_list, rmse_value)
  
  # Prepare the data for CatBoost
  train_pool <- catboost.load_pool(data = train[predictors], label = train[[cols_change[i]]])
  test_pool <- catboost.load_pool(data = test[predictors], label = test[[cols_change[i]]])
  
  # Fit a CatBoost model using all predictor variables
  cat_model <- catboost.train(train_pool, params = list(iterations = 80, # current best HP is 1000
                                                        learning_rate = 0.1, # 0.025
                                                        depth = 5, # 8
                                                        loss_function = 'RMSE'))
  
  # Use the CatBoost model to predict the outcome variable
  cat_predicted_values <- catboost.predict(cat_model, test_pool)
  
  # Calculate the RMSE for CatBoost predictions
  cat_rmse_value = rmse(cat_predicted_values, test[[cols_change[i]]])
  
  # Append the CatBoost RMSE value to the list
  cat_rmse_list <- append(cat_rmse_list, cat_rmse_value)
}

# Create a data frame containing the RMSE values and their corresponding change_ day labels
lm_rmse_data <- data.frame(
  change_day = cols_change,
  rmse_value = unlist(lm_rmse_list)
)

# Convert change_day to a factor and set the levels in the desired order
lm_rmse_data$change_day <- factor(lm_rmse_data$change_day, levels = cols_change)


# Create a data frame containing the RMSE values and their corresponding change_ day labels
cat_rmse_data <- data.frame(
  change_day = cols_change,
  rmse_value = unlist(cat_rmse_list)
)

# Convert change_day to a factor and set the levels in the desired order
cat_rmse_data$change_day <- factor(cat_rmse_data$change_day, levels = cols_change)

# Create a scatter plot using ggplot2
ggplot(lm_rmse_data, aes(x = change_day, y = rmse_value)) +
  geom_point() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "RMSE of 45-day linear regression's predictions", x = "Future Trading Days", y = "RMSE Score")

ggplot(cat_rmse_data, aes(x = change_day, y = rmse_value)) +
  geom_point() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "RMSE of 45-day Catboost's predictions", x = "Future Trading Days", y = "RMSE Score")



# see daily average rmse
lm_rmse_data <- lm_rmse_data %>% mutate(row_number = row_number())

ggplot(lm_rmse_data, aes(x = change_day, y = rmse_value / row_number)) +
  geom_point() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Daily RMSE of 45-day linear regression's predictions", x = "Future Trading Days", y = "RMSE Score") 


cat_rmse_data <- cat_rmse_data %>% mutate(row_number = row_number())

ggplot(cat_rmse_data, aes(x = change_day, y = rmse_value / row_number)) +
  geom_point() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Daily RMSE of 45-day Catboost's predictions", x = "Future Trading Days", y = "RMSE Score") 


# An RMSE of 9.5 indicates that, on average, the predicted values are off by approximately 8 units from the actual values. So, for a particular prediction, we can expect it to be accurate within a range of plus or minus 9.5 units.
cat_rmse_data 
lm_rmse_data

