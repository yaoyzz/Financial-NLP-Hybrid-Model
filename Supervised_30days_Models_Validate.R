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

# split data into training and validation sets
# last 16 rows is abandoned, because there is no news records
# 213 = 180 + 16 + 15 + 2
validate <- df %>% slice((n()-400):(n()-16))
df <- df %>% slice(1:(n()-401))

validate_date = validate$date
validate_date
# set the random seed for reproducibility
set.seed(2023)

# filter out date
df = select(df, -date)
validate = select(validate, -date)

# split the data into training and testing sets
train_index <- createDataPartition(df$change_1day, p = 0.8, list = FALSE)
train <- df[train_index, ]
test <- df[-train_index, ]

# use grep() to find column names that start with change_
cols_change <- grep("^change_", names(df), value = TRUE)

cols_change[13:17]

# Initialize an empty list to store the models
multi_models <- list()

# smooth predict by 5-day average
# loop over outcome variables
for (i in 1:length(cols_change[28:32])) {
  
  # identify the predictor variables that are not in cols_change
  predictors <- setdiff(names(train), cols_change)
  
  # Prepare the data for CatBoost
  train_pool <- catboost.load_pool(data = train[predictors], label = train[[cols_change[i]]])
  test_pool <- catboost.load_pool(data = test[predictors], label = test[[cols_change[i]]])
  
  # Fit a CatBoost model using all predictor variables
  cat_model <- catboost.train(train_pool, params = list(iterations = 1000, # current best HP is 1000
                                                        learning_rate = 0.025, # 0.025
                                                        depth = 8, # 8
                                                        loss_function = 'RMSE'))
  #save the model 
  multi_models[[i]] <- cat_model
}


# Initialize an empty list to store the predictions
pred_list <- list()

# loop over models
for (i in 1:length(multi_models)) {
  # initialize an empty list to store the predictions for this model
  model_preds <- list()
  
  # loop over rows in validate dataset
  for (j in 1:nrow(validate)) {
    # extract the predictor variables for this row
    row_preds <- validate[j, setdiff(names(validate), cols_change)]
    
    # create a catboost.Pool object from the row predictors
    row_pool <- catboost.load_pool(data = as.matrix(row_preds))
    
    # make a prediction using the current model and row pool
    row_pred <- catboost.predict(multi_models[[i]], row_pool)
    
    # append the prediction to the list of predictions for this model
    model_preds <- append(model_preds, row_pred)
  }
  
  # append the list of predictions for this model to the overall prediction list
  pred_list <- append(pred_list, model_preds)
}

# convert the prediction list to a 2-dimensional matrix
pred_matrix <- matrix(unlist(pred_list), ncol = 5, byrow = TRUE)

pred_matrix

dim(pred_matrix)

# Initialize an empty list to store the averages
avg_list <- list()

# Loop over the matrix rows up to 193 (197 - 4)
for (i in 1:(nrow(pred_matrix) - 4)) {
  
  # Calculate the average for the current 5-element sequence
  current_avg <- (
      pred_matrix[i, 5] +
      pred_matrix[i + 1, 4] +
      pred_matrix[i + 2, 3] +
      pred_matrix[i + 3, 2] +
      pred_matrix[i + 4, 1]
  ) / 5
  
  # Append the average to the list
  avg_list <- append(avg_list, current_avg)
}
# Convert the list to a vector
avg_list <- unlist(avg_list)
avg_list[1:6]

length(avg_list)
length(validate_date)

# make validate prediction frame
pred_validate <- data.frame(date = validate_date[3:383],change_30days = avg_list)

# write the data into a csv file
write.csv(pred_validate, '30_days_change_prediction.csv',row.names = F)
