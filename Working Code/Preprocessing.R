library(stringr)
library(rvest)
library(dplyr)
library(tidyverse)

# import news and stock price data
setwd('C://Users//User//Desktop//5205//Final Project//News Data')
news = read.csv("AAPL_encoded_news_from_2010.csv")
price = read.csv("AAPL_Price_2009_12_2023_3.csv")
length(unique(news$created))
length(unique(price$Date))

#-----------------------Part 1 preprocess stock Data----------------------------
# read and select stock data
# select the columns and rename them
stock_cols = c('Date', 'Open', 'Close', 'Volume')
stock = select(price, all_of(stock_cols)) %>%
  rename("date" = "Date",
         "open" = "Open",
         "close" = "Close",
         "volume" = "Volume")

# populate 1 - 45 trading days price change in future
for (i in 1:45) {
    stock <- stock %>%
    mutate(!!paste0("change_", i, "day") := (lead(close, n = i)/close - 1) * 100)
}

# **Note** 2023-2-9 to 2023-3-24 is the range of days that miss future price changes
# **Note** 2023-3-2 is in the last row of news data

#---------------------------Part 2 Join tables----------------------------------
# Full join the News and stock price data
# Be aware that full join will leave many NA, but they will be dealt when we apply models.   
news_price <- full_join(stock, news, by= c('date'='created'), multiple = 'all' ) %>%
  arrange(date)

# clean memory
rm(list=setdiff(ls(), c("news_price")))



#---------------------------Clean the joined table------------------------------

# drop open and close columns
news_price <- select(news_price, -open, -close)

# For those rows when market is closed, but news were published, the na is filled with next trading day's observation
cols_to_fill <- c('volume', grep('^change_', names(news_price), value = TRUE))

news_price[cols_to_fill] <- news_price[cols_to_fill] %>% 
  tidyr::fill(everything(), .direction = 'down')

# For rows when market is open, but has no news on that day, the na is filled with last available observation
news_price <- news_price %>% 
  tidyr::fill(everything(), .direction = 'up')

# change the data type of all columns except "news_price"
news_price[, names(news_price) != "date"] <- lapply(news_price[, names(news_price) != "date"], as.numeric)

str(news_price)

# Replace Inf and -Inf values with 0
news_price[] <- lapply(news_price, function(x) ifelse(is.infinite(x), 0, x))

# Check the existance of na
colSums(is.na(news_price))


# fill na character-level embedding column with 0, not exist -> neutral
news_price <- news_price %>% 
  replace(is.na(.), 0)

# Check the existance of na
sum(colSums(is.na(news_price)))

# write the data into a csv file
write.csv(news_price, 'AAPL_price_news_2010_2023.csv',row.names = F)
dim(news_price)
