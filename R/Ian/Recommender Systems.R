#Purpose: Explore building recommender systems

#Cleansing/Manipulation Packages:
pacman::p_load("dplyr","tidyr","stringr","lubridate","janitor")
#Graphing Packages:
pacman::p_load("ggplot2","esquisse","gridExtra")
#ML Packages:
pacman::p_load("car","MLmetrics","estimatr","fpp","forecast","caret","ROCR","lift","randomForest","glmnet","MASS",
               "e1071","partykit","rpart","ROCR","lift","randomForest","xgboost","tensorflow", "keras", "recommenderlab")
#Other Packages:
pacman::p_load("sqldf","readxl","geosphere")

#----- Pre-processing -----

#import datasets
ratings <- read.csv("Queen's MMA\\MMA 831\\Week 4\\ratings.csv", header=TRUE, sep = ",")

#summary stats
str(ratings)
summary(ratings)

#drop timestamp
ratings <- ratings %>%
  dplyr::select(-timestamp)

#get into the format of a user-item affinity matrix using spread from tidyr
#ratings <- ratings %>%
#  spread(movieId, rating)

#get into the format of a user-item affinity matrix using spread from tidyr
ratings_matrix <- as(ratings,"realRatingMatrix")

#----- Collaborative Filtering Model -----

#let's train a simple model using UBCF (user-based collaborative filtering)
#note: could also use IBCF (item-based collaborative filtering)
rec_mod <- Recommender(ratings_matrix, method = "UBCF")

#this approach computes the cosine similarity between all user vectors
rec_mod <- Recommender(ratings_matrix, 
                       method = "UBCF", 
                       param=list(normalize = "Z-score", 
                                  method="Cosine",
                                  nn=5, 
                                  minRating=1))

#----- Model Scoring (use to recommend top N items) -----

  #recommend top 5 items for user 610
recommended_items_user610 <- predict(rec_mod, ratings_matrix["610",], n=5)

  #display user 610's top 5 recommended
as(recommended_items_user610, "list")

  #obtain the top 3
recommended_items_user610_top3 <- bestN(recommended_items_user610, n=3)

  #display the top 3
as(recommended_items_user610_top3, "list")

    #observations: these top N lists may include things that the user has already rated...

#Recommend items that the user has not engaged with
predicted_affinity_user610 <- predict(rec_mod, ratings_matrix["610",], type="ratings")

#see user 610's predicted affinity towards items they haven't engaged with
as(predicted_affinity_user610, "list")
  #observations: can see that movie_id's 1267, 2605, 2011 lead the way

#view the real affinity for the items obtained from the affinity matrix (these are user 610's observed ratings)
as(ratings_matrix["610",], "list")

#----- Evaluation -----

#create evaluation scheme - split 90:10 train/test
eval <- evaluationScheme(ratings_matrix[1:610], #recall we have 610 user_ids
                         method='split',
                         train=0.9,
                         given=15)

#fit recommender model based on UBCF to eval scheme
rec_eval_ubcf <- Recommender(getData(eval, "train"), "UBCF")

#fit recommender model based on IBCF to eval scheme
rec_eval_ibcf <- Recommender(getData(eval, "train"), "IBCF")

#making predictions on the test data set
pred_ubcf <- predict(rec_eval_ubcf, getData(eval, "known"), type="ratings")

pred_ibcf <- predict(rec_eval_ibcf, getData(eval, "known"), type="ratings")

#obtain and compare error metrics for both approaches
error_ubcf <- calcPredictionAccuracy(pred_ubcf, getData(eval, "unknown"))

error_ibcf <- calcPredictionAccuracy(pred_ibcf, getData(eval, "unknown"))

error <- rbind(error_ubcf, error_ibcf)
rownames(error) <- c("UBCF", "IBCF")
error
  #observations: we see that the UBCF (user based collaborative filtering) results in improved accuracy scores in this case

