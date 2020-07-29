#Purpose: Implement K-Means and hierarchical clustering techniques

#Cleansing/Manipulation Packages:
pacman::p_load("dplyr","tidyr","stringr","lubridate","janitor")
#Graphing Packages:
pacman::p_load("ggplot2","esquisse","gridExtra")
#ML Packages:
pacman::p_load("car","MLmetrics","estimatr","fpp","forecast","caret","ROCR","lift","randomForest","glmnet","MASS",
               "e1071","partykit","rpart","ROCR","lift","randomForest","xgboost","tensorflow", "keras")
#Other Packages:
pacman::p_load("sqldf","readxl","geosphere")

#----- K-Means Clustering -----

#create dummy data
set.seed(2)
x = matrix(rnorm(50*2), ncol = 2)
x[1:25,1] = x[1:25,1]+3
x[1:25,2] = x[1:25,2]-4

#k-means with k=2
km_out = kmeans(x, 2, nstart = 20) #(object, k, # random initial sets)

#view the cluster assignments
km_out$cluster
  #observations: algorithm perfectly separated the observations into two clusters (even without any group information)

#view full summary
km_out
  #observations: tells us number of clusters and their size (25, 25)

#plot the data with our K-means clustering
plot(x, col=(km_out$cluster),
     main="K-Means Clustering Results with K=2",
     xlab="", ylab="", pch=20, cex=2)
  #observations: note that we obviously only had two variables...had we had more, we could perform PCA 
  #and then plot the first two principal components score vectors

#attempt with k=3
set.seed(4)
km_out2 = kmeans(x, 3, nstart = 20)
km_out2
  #observations: 3 clusters of sizes (10, 23, 17)

#plot the 3 clusters
plot(x, col=(km_out2$cluster),
     main="K-Means Clustering Results with K=3",
     xlab="", ylab="", pch=20, cex=2)

#test running kmeans with only nstart = 1 (one initial categorization)...shouldn't be good
set.seed(3)
km_out3 = kmeans(x, 3, nstart = 1)
km_out3$tot.withinss
  #view plot (observe that it's changed, looks like it may be a bit worse)
plot(x, col=(km_out3$cluster),
     main="K-Means Clustering Results with K=3",
     xlab="", ylab="", pch=20, cex=2)

km_out2$tot.withinss #compare to previous (w/ nstart = 20)
  #observations: 98 < 104 --> having a larger nstart improves results (minimizes chances of an unlucky initial set)

#----- Hierarchical Clustering -----

#using complete linkage
hc_complete <- hclust(dist(x), method="complete")

#using average (mean) linkage
hc_average <- hclust(dist(x), method="average")

#using single linkage
hc_single <- hclust(dist(x), method="single")

#view the resulting plots
par(mfrow=c(1,3))
plot(hc_complete, main="Complete Linkage", xlab="", sub="", cex=0.9)
plot(hc_average, main="Average Linkage", xlab="", sub="", cex=0.9)
plot(hc_single, main="Single Kinkage", xlab="", sub="", cex=0.9)
  #observations: complete and average linkage appear to be more evenly distributed (unsure if this matters)

#determine cluster labels with a given cut of the dendrogram
cutree(hc_complete, 2)
cutree(hc_average, 2)
cutree(hc_single, 2)
  #observations: complete linkage performed perfectly, average missed a handful, single linkage was awful

#can scale our variables before performing hierarchical clustering
xsc = scale(x)
par(mfrow=c(1,1))

hc_complete_scaled <- hclust(dist(xsc), method="complete")

plot(hc_complete_scaled, main="Hierarchical Clustering with Scaled Features")

cutree(hc_complete_scaled, 2)
  #observations: perfect results (although complete got perfect last time too)



