library(stringr)
library(rvest)
library(dplyr)
library(tidyverse)
library(tidytext)
library(textstem)
library(readr)
library(tm)
library(tidyr)

setwd('C://Users//User//Desktop//5205//Final Project//News Data')

news = read.csv("AAPL_benzinga_2010_to_2023_3_26.csv")
length(unique(news$created))

#--------------------------------preprocess News Data--------------------------

# select useful news columns
news_cols = c('created', 'title', 'body')
news = select(news, all_of(news_cols))

# # exclude News that are irrelevant or mentions very little about Apple
# set1 = grep("Apple", news$title)
# set2 = grep("AAPL", news$title)
# set3 = grep("APPLE", news$title)
# set4 = grep("apple", news$title)
# 
# news <- news[union(union(union(set1, set2), set3),set4),]

# clean the text
clean_text_data <- function(data, column_name) {
  for (i in 1:nrow(data)) {
    # Extract the text from the specified column
    text = data[i, column_name]
    
    # Remove HTML tags
    clean_text = str_replace_all(text, "<[^>]+>", "")
    
    # Remove parentheses and their content
    clean_text = str_replace_all(clean_text, "\\([^\\(]*?\\)", "")
    
    # Remove all punctuation marks and $ sign
    clean_text = str_replace_all(clean_text, "[[:punct:]]|\\$", "")
    
    # Clean the text, and replace all badly formatted text
    clean_text = str_replace_all(clean_text, "\\s+", " ")
    
    # Assign the cleaned text back to the data frame
    data[i, column_name] = clean_text
  }
  
  return(data)
}
news <- news %>% clean_text_data("title") %>%
  clean_text_data("body")

dim(news)


# -------------------------------tf-idf----------------------------------------
# Define a function to calculate tf-idf values for a column
calc_tf_idf <- function(data, column) {
  # Preprocess the text data
    clean_data <- data %>%
    mutate_at(column, ~str_remove_all(., "[^[:alnum:]\\s]")) %>%
    mutate_at(column, ~str_remove_all(., "\\d+")) %>%
    unnest_tokens(word, !!sym(column)) %>%
    mutate(word = lemmatize_words(word)) %>%
    group_by(created) %>%
    anti_join(stop_words, by = "word")
  
  # Calculate Term Frequency (TF)
  tf <- clean_data %>%
    count(created, word, sort = TRUE) %>%
    rename(tf = n)
  
  # Calculate Inverse Document Frequency (IDF)
  idf <- clean_data %>%
    group_by(word) %>%
    summarise(docs = n_distinct(created)) %>%
    mutate(idf = log(nrow(data) / docs))
  
  # Calculate TF-IDF per article
  tf_idf <- tf %>%
    inner_join(idf, by = "word") %>%
    mutate(!!sym(paste0("tf_idf_", column)) := tf * idf) %>%
    group_by(created) %>%
    summarise(!!sym(paste0("tf_idf_", column)) := sum(!!sym(paste0("tf_idf_", column))))
  
  return(tf_idf)
}

# Calculate tf-idf for title and body columns
tf_idf_title <- calc_tf_idf(news, "title")
tf_idf_body <- calc_tf_idf(news, "body")

# Merge tf-idf data with news data
news_with_tf_idf <- tf_idf_title %>%
  left_join(tf_idf_body, by = "created")

# show dimention
dim(news_with_tf_idf)

# -------------------------------Word Embeddings--------------------------------
# Load required libraries
library(text2vec)
library(furrr)

# Preprocess the text data
preprocess_text <- function(data, column) {
    data %>%
    mutate_at(column, ~str_remove_all(., "[^[:alnum:]\\s]")) %>%
    mutate_at(column, ~str_remove_all(., "\\d+")) %>%
    unnest_tokens(word, !!sym(column)) %>%
    mutate(word = lemmatize_words(word)) %>%
    anti_join(stop_words, by = "word") %>%
    group_by(created)
}

clean_title <- preprocess_text(news, "title")
clean_body <- preprocess_text(news, "body")

# Create vocabulary
tokens <- bind_rows(clean_title, clean_body)
vocab <- create_vocabulary(it = itoken(tokens$word), ngram = c(1, 1))
vocab <- prune_vocabulary(vocab, term_count_min = 3)

# Train word embeddings
it <- itoken(tokens$word, preprocess_function = tolower, ids = tokens$created, progressbar = FALSE)
word_vectors <- create_tcm(it, vectorizer = vocab_vectorizer(vocab))
word_vectors <- word_vectors + 1e-9
glove <- GloVe$new(rank = 50, x_max = 10)
glove_fit <- glove$fit_transform(word_vectors, n_iter = 15)
embeddings <- as.matrix(glove_fit)

# Calculate mean word embeddings
calc_mean_embeddings <- function(clean_data, column, vocab, embeddings) {
  clean_data %>%
    mutate(word_index = match(word, vocab$term)) %>%
    filter(!is.na(word_index)) %>%
    mutate(embedding = map(word_index, ~embeddings[.x, ])) %>%
    group_by(!!sym("created")) %>%
    summarise(embedding = list(colMeans(do.call(rbind, embedding)))) %>%
    ungroup() %>%
    mutate(!!sym(paste0("mean_emb_", column)) := embedding) %>%
    select(-embedding)
}

mean_emb_title <- calc_mean_embeddings(clean_title, "title", vocab, embeddings)
mean_emb_body <- calc_mean_embeddings(clean_body, "body", vocab, embeddings)

# Merge mean word embeddings with news data
news_with_embeddings <- mean_emb_title %>%
  left_join(mean_emb_body, by = "created")

# Calculate mean,sum, max of the embeddings
news_with_embeddings$max_emb_title <- sapply(news_with_embeddings$mean_emb_title, max)
news_with_embeddings$max_emb_body <- sapply(news_with_embeddings$mean_emb_body, max)
news_with_embeddings$sum_emb_title <- sapply(news_with_embeddings$mean_emb_title, sum)
news_with_embeddings$sum_emb_body <- sapply(news_with_embeddings$mean_emb_body, sum)
news_with_embeddings$mean_emb_title <- sapply(news_with_embeddings$mean_emb_title, mean)
news_with_embeddings$mean_emb_body <- sapply(news_with_embeddings$mean_emb_body, mean)

# show dimention
dim(news_with_embeddings)

# -------------------------Character-level Embeddings--------------------------
# clean memory
rm(list=setdiff(ls(), c("news","news_with_embeddings","news_with_tf_idf")))

# import packages
library(reticulate)
library(keras)

create_char_embeddings <- function(data, column, maxlen = 100, embedding_dim = 50) {
  # Group by 'created' column and concatenate text
  data_grouped <- data %>% 
    group_by(created) %>%
    summarise(grouped_text = paste(!!sym(column), collapse = " "))
  
  # Tokenize characters
  tokenizer <- text_tokenizer(char_level = TRUE)
  fit_text_tokenizer(tokenizer, data_grouped$grouped_text)
  
  # Convert text to sequences
  sequences <- texts_to_sequences(tokenizer, data_grouped$grouped_text)
  
  # Pad sequences
  padded_sequences <- pad_sequences(sequences, maxlen = maxlen)
  
  # Create the embedding layer
  vocab_size <- length(tokenizer$word_index)
  embedding_layer <- layer_embedding(input_dim = vocab_size + 1, output_dim = embedding_dim, input_length = maxlen)
  
  # Return the tokenizer, padded sequences, and created column
  return(list(created = data_grouped$created, tokenizer = tokenizer, padded_sequences = padded_sequences))
}

title_embeddings <- create_char_embeddings(news, "title")
body_embeddings <- create_char_embeddings(news, "body")

news_with_embeddings$title_char_emb <- title_embeddings$padded_sequences
news_with_embeddings$body_char_emb <- body_embeddings$padded_sequences

# --------------------------Lexion sentiment------------------------------------
# clean memory
rm(list=setdiff(ls(), c("news","news_with_embeddings","news_with_tf_idf")))
# check current data size 
library(pryr)
object_size(news_with_embeddings)

library(textdata)
# Define a function to calculate the sentiment scores using different lexicons
calc_sentiment <- function(data, column) {
  sentiment_scores <- data %>%
    unnest_tokens(word, !!sym(column)) %>%
    inner_join(get_sentiments("afinn"), by = "word") %>%
    group_by(created) %>%
    summarise(afinn_score = sum(value)) %>%
    left_join(data %>%
                unnest_tokens(word, !!sym(column)) %>%
                inner_join(get_sentiments("bing"), by = "word") %>%
                mutate(value = ifelse(sentiment == "positive", 1, -1)) %>%
                group_by(created) %>%
                summarise(bing_score = sum(value)),
              by = "created") %>%
    left_join(data %>%
                unnest_tokens(word, !!sym(column)) %>%
                inner_join(get_sentiments("nrc"), by = "word") %>%
                count(created, sentiment) %>%
                spread(sentiment, n, fill = 0),
              by = "created")
  
  return(sentiment_scores)
}

# Calculate the sentiment scores for title and body columns
sentiment_title <- calc_sentiment(news, "title")
sentiment_body <- calc_sentiment(news, "body")

# Replace na with 0
sentiment_title <- sentiment_title %>%
  mutate(across(-created, replace_na, 0))

sentiment_body <- sentiment_body %>%
  mutate(across(-created, replace_na, 0))

# # Merge sentiment scores with the news data
# news_with_sentiment <- news_with_embeddings %>%
#   left_join(sentiment_title, by = "created") %>%
#   left_join(sentiment_body, by = "created", suffix = c("_title", "_body"))

news_with_sentiment <- sentiment_title %>%
  left_join(sentiment_body, by = "created")


# ---------------------------------Finalize------------------------------------
# clean memory
rm(list=setdiff(ls(), c("news","news_with_tf_idf","news_with_embeddings","news_with_sentiment")))

news_encode <- news_with_tf_idf %>%
  left_join(news_with_embeddings, by = "created") %>%
  left_join(news_with_sentiment, by = "created")

# Replace any missing values with 0, except for the 'created' column
news_encode <- news_encode %>%
  mutate(across(-created, ~replace_na(.x, 0)))

# Check if there are any missing values in the data frame
any_na <- any(is.na(news_encode))
print(any_na)

# write the data into a csv file
write.csv(news_encode, 'AAPL_encoded_news_from_2010.csv',row.names = F)
