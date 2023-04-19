# Financial-NLP-Hybrid-Model
Using Semisupervised learning models and Time Series Models to predict stock price using Benzinga News data. 

Introduction
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


01 Prepare Data    
Sources: 
• News data from Benzinga REST API
• Stock trading data from Yahoo Finance 

02 Preprocess
• Select relevant fields
• Combine dataset 
• Adding features

03 Natural Language Processing 
Techniques include 
• Term frequency
• Word-level vectorization 
• Character-level vectorization  
• Sentiment classification  

04 Regression Models
• Train, tune and validate the supervised learning model using text features as predictors and price movement as
dependent variable 

05 Time Series Model 
• Train, tune and validate the models on the dates and the responsive price movement

06 Projection and Inference
• Plot and compare the predictions created by different models and render the outcomes   
