#MMA 860 Assignment 2

library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(sqldf)
library(readxl)

##QUESTION 2
#import datasets
df1 <- read_excel("Queen's MMA\\MMA 860\\Assignment 2\\MMA_860_Assignment_2_Data_v1_0.xlsx", 1)

#filter for first 25 observations
df1_filtered <- df1 %>%
  filter(Obs <= 25)

#regression model including experience and height
  #filtered dataset
fmod1 <- lm(Y ~ Experience + Height, df1_filtered)
summary(fmod1)

  #complete dataset
mod1 <- lm(Y ~ Experience + Height, df1)
summary(mod1)

  #observations: F-test of the model built from the complete
  #dataset is more significant (higher F-stat and lower
  #associated p-value), indicating that this model has
  #better explanatory power than the filtered dataset.
  #Additionally, Height comes back with a higher level of
  #evidence supporting its statistically significant
  #association with Y.

#regression model including experience and weight
  #filtered dataset
fmod2 <- lm(Y ~ Experience + Weight, df1_filtered)
summary(fmod2)

  #complete dataset
mod2 <- lm(Y~ Experience + Weight, df1)
summary(mod2)

  #observations: same idea as first pair

#regression model including experience, height, and weight
  #filtered dataset
fmod3 <- lm (Y ~ Experience + Height + Weight, df1_filtered)
summary(fmod3)

  #complete dataset
mod3 <- lm(Y ~ Experience + Height + Weight, df1)
summary(mod3)

  #observations: same idea as first pair