#Purpose: Build descriptive model examining significance of Tesla tweet sentiment and Tesla's stock prices

#Cleansing/Manipulation Packages:
pacman::p_load("dplyr","plyr","tidyr","stringr","lubridate","janitor","forcats","readr","qdap","tm")
#Graphing Packages:
pacman::p_load("ggplot2","esquisse","gridExtra","ggridges","igraph","dendextend","circlize","wordcloud","plotrix")
#ML Packages:
pacman::p_load("car","MLmetrics","estimatr","fpp","forecast","caret","ROCR","lift","randomForest","glmnet","MASS",
               "e1071","partykit","rpart","ROCR","lift","randomForest","xgboost","tensorflow", "keras","fpc","lda",
               "text2vec")
#Other Packages:
pacman::p_load("sqldf","readxl","geosphere", "pricesensitivitymeter")

#----- IMPORT DATA -----
tweets_sentiment <- read.csv("Queen's MMA\\MMA 823\\Project\\tweets_sentiment_price.csv", header=TRUE, sep = ",")

#----- EDA -----

#check out range of sentiment
summary(tweets_sentiment$overall_sentiment)

#change necessary data types
tweets_sentiment$date <- as.Date(tweets_sentiment$date)

str(tweets_sentiment)

#quick time series plot of stock price and overall sentiment
  #sentiment
ggplot(tweets_sentiment) +
  aes(x = date, y = overall_sentiment) +
  geom_line(size = 1L, colour = "#0c4c8a") +
  theme_minimal() +
  labs(title = "Daily Tweet Sentiment",
       x = "Date",
       y = "Sentiment")

  #stock price (same day closing)
ggplot(tweets_sentiment) +
  aes(x = date, y = TSLA_Price_close_same_day) +
  geom_line(size = 1L, colour = "#0c4c8a") +
  theme_minimal() +
  labs(title = "TSLA Closing Price",
       x = "Date",
       y = "Closing Price")

#----- DESCRIPTIVE MODEL -----

mod1 <- lm(TSLA_Price_open_next_day ~ overall_sentiment, tweets_sentiment)
summary(mod1)
plot(mod1)

#t-test (exact same results as single var model above)
cor.test(tweets_sentiment$TSLA_Price_open_next_day, tweets_sentiment$overall_sentiment)

#observations: Cor = 0.22, p-value is significant, 95% CI does not include 0

  #Based on the summary output of our descriptive model, it does appear that the overall sentiment of tweets
  #involving Tesla/Musk has a statistically significant association with Tesla's stock price, assuming a 
  #significance level of alpha = 0.05 (p-val = 1.71e-05)

  #Based on the findings from our descriptive model, it does appear that the sentiment of tweets involving Tesla/Musk
  #hold a positive relationship with Tesla's stock price (coefficient is positive). More specifically, the
  #coefficient estimate of 0.6177 indicates that as tweet sentiment increases by a value of 1, Tesla's opening stock
  #price on the following day appears to increase by approximately $0.62

  #R^2 = 0.04 indicates that the sentiment score explains 4% of stock price movement, we can certainly use it as a potential
  #factor in an APT model, along with some other other variables that appear to hold some degree of association
  #with stock movements.





