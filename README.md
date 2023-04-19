# Financial-NLP-Hybrid-Model
Using Semisupervised learning models and Time Series Models to predict stock price using Benzinga News data. 

### Introduction
The conventional belief in the stock market is to “buy the rumor, sell the news”. You are
bound to make a fortune if you were right on the speculation. In our preliminary
research on conventional belief, we hypothesize that there is a strong tie and reaction to
the company-related news on the day of its release. We examined the fact by collecting
all news articles from Benzingas related to Apple, then breaking them down into a
dictionary and weighing each word. We used the percentage of the change (open and
close price) as our output. Then we compared whether there is a relationship between
the keywords and the price change percentage. We have also introduced a time series
model with various methods to benchmark the performance of models and found the
best models to predict the changes and the price of Apple stock.


### 01 Prepare Data    
Sources: 
• News data from Benzinga REST API
• Stock trading data from Yahoo Finance 

### 02 Preprocess
• Select relevant fields
• Combine dataset 
• Adding features

### 03 Natural Language Processing 
Techniques include 
• TFIDF - Get TF for each word for each article. Get IDF for each word for each article. Get the sum of TF x IDF for each article.
• Word-level Embeddings - Created a vocabulary of words. Trained word embeddings using GloVe (unsupervised learning algorithm). Calculated mean word embeddings for each article. Calculated the max, sum, and mean of the embeddings (vectors)
• Character-level Embeddings - Converted text data to sequences. Padded sequences to a fixed length. Created an embedding layer that maps the integer-encoded characters to dense vectors. 
• Sentiment Inference - Calculated sentiment scores using different lexicons. Calculated AFINN, Bing, and NRC sentiment scores per article. Replaced NA values with 0 in the sentiment scores data frame.

### 04 Regression Models
• Train, tune and validate the supervised learning model using text features as predictors and price movement as
dependent variable 
• Train the Linear Regression and CatBoost model 
• Use dependent variables including future change in one day to 30 days   

### 05 Time Series Model 
• Train, tune and validate the models on the dates and the responsive price movement
• Auto Arima
• Prophet
• Average 
• Exponential smoothing
• Holt Model
• Random Walk (Drift Model)

### 06 Projection and Inference
• Plot and compare the predictions created by different models and render the outcomes   

### Use Cases
• Use Time Series models to predict mid-term and long-term trends and generate profits.
• Use NLP supervised models for short-term directional prediction  in volatile market to generate profits

### Future Development
• Add more technical indicators as features such as moving average, MACD, RSI and KDJ to boost the supervised learning model.
• Automate the ETL process and deploy the models on stocks or other securities in various sectors to compare the performance under different market conditions.


