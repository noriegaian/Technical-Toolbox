#Purpose: NLP Document Classification

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

#----- DATA IMPORT -----

#import resumes posted online for call centre job
headlines <- read.csv("Queen's MMA\\MMA 865\\Lecture 3\\all_3k_headlines.csv", header=TRUE, sep = ",")

#----- PREPROCESSING -----

#create cleaning function
headline.clean <- function(x) {
  x <- tolower(x)
  x <- removeWords(x, stopwords('en'))
  x <- removePunctuation(x)
  x <- stripWhitespace(x)
  return(x)
}

#create match.matrix function
match.matrix <- function(text.col, original.matrix=NULL, weighting=weightTf) {
  control <- list(weighting=weighting)
  training.col <- sapply(as.vector(text.col, mode="character"), iconv, to="UTF8", sub="byte")
  corpus <- VCorpus(VectorSource(training.col))
  matrix <- DocumentTermMatrix(corpus, control=control);
  if (!is.null(original.matrix)) {
    terms <- colnames(original.matrix[,which(!colnames(original.matrix) %in% colnames(matrix))])
    weight <- 0
    if (attr(original.matrix, "weighting") [2] == "tfidf")
      weight <- 0.000000001
    amat <- matrix(weight, nrow=nrow(matrix), ncol=length(terms))
    colnames(amat) <- terms
    rownames(amat) <- rownames(matrix)
    fixed <- as.DocumentTermMatrix(cbind(matrix[,which(colnames(matrix) %in% colnames(original.matrix))],amat), weighting=weighting)
    matrix <- fixed
  }
  matrix <- matrix[,sort(colnames(matrix))]
  gc()
  return(matrix)
}

#train/test split
train <- createDataPartition(headlines$y, p=0.5, list=F)
train.headlines <- headlines[train,]

test.headlines <- headlines[-train,]

#put training data through our cleaning function
clean.train <- headline.clean(train.headlines$headline)

#dtm
train.dtm <- match.matrix(clean.train, weighting=tm::weightTfIdf)

#turn into matrix
train.matrix <- as.matrix(train.dtm)
train.matrix <- Matrix(train.matrix, sparse=T)

#understand difference b/w train.dtm and sparse train: dimensions are the same but zero values become a period (lightweight for modeling)
dim(train.matrix)
train.matrix[1:5, 1:25]

##train glmnet model
#cross-validation
cv <- cv.glmnet(train.matrix, y=as.factor(train.headlines$y), alpha=1, 
                family='binomial', nfolds=10, intercept=F, type.measure='class')

#plot the interaction between lambda values and misclassification rates for the lasso regression model
plot(cv)

#make predictions
preds <- predict(cv, train.matrix, type='class', s=cv$lambda.1se)

#check AUC
train.auc <- roc(train.headlines$y, as.numeric(preds))
train.auc
  #observations: 0.919 (very good AUC)

#plot the ROC curve
plot(train.auc) #hideous plot...but whatever

#confusion matrix
confusion <- table(preds, train.headlines$y)
confusion
  #observations: 92.33% accuracy

##now let's make predictions on the new headlines (we've done cross-validation above)
#put through the test set in the cleaning data
clean.test <- headline.clean(test.headlines$headline)

#dtm
test.dtm <- match.matrix(clean.test, weighting=tm::weightTfIdf, original.matrix=train.dtm)

#convert to matrix
test.matrix <- as.matrix(test.dtm)
test.matrix <- Matrix(test.matrix)

#make predictions on test set
preds <- predict(cv, test.matrix, type='class', s=cv$lambda.min)

headline.preds <- data.frame(doc_row = rownames(test.headlines), class=preds[,1])

#check AUC
test.auc <- roc(test.headlines$y, as.numeric(preds))
test.auc
  #observations: AUC is now only 0.685...significantly worse though makes sense as this is brand new data

#see ROC plot
plot(train.auc, col="blue", main="RED = test, BLUE = train", adj=0)
plot(test.auc, add=TRUE, col="red", lty=2)
  #observations: we see the test ROC curve in red, train in blue

#confusion matrix
confusion <- table(headline.preds[,2], test.headlines$y)
confusion
sum(diag(confusion))/sum(confusion) #68% accuracy

##find the most impactful words
glmnet.coef <- as.matrix(coef(cv, s='lambda.min'))

glmnet.coef <- data.frame(words = row.names(glmnet.coef), glmnet_coefficients = glmnet.coef[,1])

glmnet.coef <- glmnet.coef[order(glmnet.coef$glmnet_coefficients, decreasing=T),]

glmnet.coef$words <- factor(glmnet.coef$words, levels=unique(glmnet.coef$words))

summary(glmnet.coef$glmnet_coefficients)

length(subset(glmnet.coef$glmnet_coefficients, glmnet.coef$glmnet_coefficients>0))
length(subset(glmnet.coef$glmnet_coefficients, glmnet.coef$glmnet_coefficients<0))

ggplot(glmnet.coef, aes(x=glmnet.coef$glmnet_coefficients)) +
  geom_line(stat='density', color='darkred', size=1)
  #observations: note that reviewing coefficient distributions is not that insightful...but helps illustrate how a lasso regression differes from typical linear regression

top.coef <- rbind(head(glmnet.coef,10), tail(glmnet.coef,10))
top.coef$impact <- ifelse(top.coef$glmnet_coefficients>0, "Positive", "Negative")

ggplot(top.coef, aes(x=glmnet_coefficients, y=words)) +
  geom_segment(aes(yend=words), xend=0, colour="grey50") +
  geom_point(size=3, aes(colour=impact))

