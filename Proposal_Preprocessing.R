library(stringr)
library(rvest)
library(dplyr)
library(tidyverse)
#--------------------------------Part 1 preprocess News Data--------------------------
getwd()
setwd("C://Users//User//Desktop//5205//Final Project//News Data")
news = read.csv("AAPL_benzinga_2020_to_2023_3.csv")

# select useful news columns
news_cols = c('created', 'title', 'body')
news = select(news, all_of(news_cols))

# exclude News that are irrelevant or mentions very little about Apple
set1 = grep("Apple", news$title)
set2 = grep("AAPL", news$title)
set3 = grep("APPLE", news$title)
set4 = grep("apple", news$title)
union(union(union(set1, set2), set3),set4)

news <- news[union(union(union(set1, set2), set3),set4),]

# delete the tags
for(i in 1:nrow(news)) {
  
  # Extract the first line from the body text
  html_text = news[i, 'body']
  #Read the crawled data into HTML text
  parsed_html = read_html(html_text)
  #Read the HTML text into clean text
  text = html_text(parsed_html)
  #Clean the text, and replace all badly formatted text
  clean_text = str_replace_all(text, "\\s+", " ")
  
  news[i, 'body'] = clean_text
}
dim(news)
write.csv(news, 'AAPL_benzinga_2020_to_2023_3.csv',row.names = F)

#--------------------------------Part 2 preprocess stock Data--------------------------
# read and select stock data
stock = read.csv("AAPL_yahoo_stock_price_from_2020.csv")

# select the columns and rename into lower case
stock_cols = c('Date', 'Open', 'Close', 'Volume')
stock = select(stock, all_of(stock_cols)) %>%
  rename("date" = "Date",
         "open" = "Open",
         "close" = "Close",
         "volume" = "Volume")

# calculate the 1 day / 5 day / 30 day percentage change
stock <- stock %>%
    mutate( change_1day = (close/open - 1) * 100 ) %>%
    mutate( change_5day = (close/lag(open, n =4) - 1) * 100 ) %>%
    mutate( change_30day = (close/lag(open, n =29) - 1) * 100 )


#--------------------------------Part 3 Join tables------------------------------------
# Full join the News and stock price data
# Be aware that full join will leave many NA, but they will be dealt when we apply models.   
news_price <- full_join(stock, news, by= c('date'='created'), multiple = 'all' ) %>%
  arrange(date)

# write the data into a csv file
write.csv(news_price, 'AAPL_price_news_from_2020.csv',row.names = F)
dim(news_price)
