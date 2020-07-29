library(data.table)
library(dplyr)
library(ggplot2)
library(tidyr)
library(knitr)

########################################################################
##### STEP 1: LOADING DATASET
########################################################################
### Load Dataset - First, Lets we Load & Examine Dataset
df_data = read.csv('/Users/mm679j/Documents/R_Tutorial/RFM_and_clustering/ecom_cluster_data.csv')
glimpse(df_data)
summary(df_data)

########################################################################
##### STEP 2: DATA CLEANING
########################################################################

#Data cleaning -
#Delete all negative Quantity and Price. We also need to delete NA customer ID
df_data <- df_data %>% 
  mutate(Quantity = replace(Quantity, Quantity<=0, NA),
         UnitPrice = replace(UnitPrice, UnitPrice<=0, NA))

df_data <- df_data %>%
  drop_na()

#Recode variables - should do some recoding and convert character variables to factors.
df_data <- df_data %>% 
  mutate(InvoiceNo=as.factor(InvoiceNo), StockCode=as.factor(StockCode), 
         InvoiceDate=as.Date(InvoiceDate, '%m/%d/%Y %H:%M'), CustomerID=as.factor(CustomerID), 
         Country=as.factor(Country))

df_data <- df_data %>% 
  mutate(total_dolar = Quantity*UnitPrice)

glimpse(df_data)


########################################################################
##### STEP 3: CREATE R-F-M FEATURES
########################################################################
###To implement the RFM analysis, we need to further process the data set in by the following steps:
  #1. Find the most recent date for each ID and calculate the days to the now or some other date, to get the Recency data
  #2. Calculate the quantity of translations of a customer, to get the Frequency data
  #3. Sum the amount of money a customer spent and divide it by Frequency, to get the amount per transaction on average, that is the Monetary data.

df_RFM <- df_data %>% 
  group_by(CustomerID) %>% 
  summarise(recency=as.numeric(as.Date("2012-01-01")-max(InvoiceDate)),
            frequenci=n_distinct(InvoiceNo),
            monitery= sum(total_dolar)/n_distinct(InvoiceNo)) 

summary(df_RFM)

#preview dataframe
glimpse(df_RFM)

########################################################################
##### STEP 4: VISUALIZE / EXPLORE RFM DISTRIBUTIONS
########################################################################
## Look at distribution of R, F, and M
#recency
hist(df_RFM$recency, breaks =20)

#freq
hist(df_RFM$frequenci, breaks = 50)

hist(df_RFM$monitery, breaks = 50)

#skewness in M
df_RFM$monitery <- log(df_RFM$monitery)
hist(df_RFM$monitery)

########################################################################
##### STEP 5: DATA PREP & SCALING
########################################################################
#### Get ready for clustering
df_RFM2 <- df_RFM
row.names(df_RFM2) <- df_RFM2$CustomerID
df_RFM2$CustomerID = NULL

#only keep RFM and take customerID out of dataframe, so all that is left are the x-vars for calculating distance
df_RFM2
#each row is basically a vector of 3 (RFM) and we can then easily calculate some type of euclidean distance

#scale before we do clustering
df_RFM2 <- scale(df_RFM2)
summary(df_RFM2)
str(df_RFM2)

########################################################################
##### STEP 6: APPLY K-MEANS
########################################################################
#kmeans
kmeans_cluster <- kmeans(df_RFM2, 5)

#look at the output of the k-means function
str(kmeans_cluster)

#look at the cluster outputs
kmeans_cluster
#cluster sizes: 858, 625, 71, 2782 + shows you cluster means across R, F, and M

#some of the other details:
#cluster: a vector of integers (from 1:k) indicating the cluster to which each point is allocated.
#centers: a matrix of cluster centers.
#withiness: vector of within-cluster sum of squares, one component per cluster.
#tot.withiness: total within-cluster sum of squares. That is, sum(e).
#size: the number of points in each cluster.

########################################################################
##### STEP 6: FIND OPTIMAL # OF CLUSTERS - ELBOW METHOD
########################################################################
## Option 1: we write out own function to finding optimal # of clusters using Elbow method
# function to compute total within-cluster sum of square 
set.seed(123)
wss <- function(k) {
  kmeans(df_RFM2, k, nstart = 10 )$tot.withinss
}

# Compute and plot wss for k = 1 to k = 15
k.values <- 1:15
wss_values <- map_dbl(k.values, wss) # extract wss for 2-15 clusters

#plot wss values vs. # of clusters
plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

#look like optimal # of clusters is probably ~5-6

## Option 2: use pre-created functions from factoextra R package
#install.packages("factoextra")
library(factoextra)

#elbow method plot using fviz_nbclust function from factoextra R packlage
set.seed(123)
fviz_nbclust(df_RFM2, kmeans, method = "wss")

########################################################################
##### STEP 7: VISUALIZE CLUSTERING OUTPUTS 
########################################################################
# Again using precreated function from factoextra package makes our life a lot easier...
fviz_cluster(kmeans_cluster, data = df_RFM2)

# What this package does in the background
  # 1. Run PCA (see how Dim 1 explain 45% variance & Dim 2 explain 30%)
  # 2. Reduce N dim to 2 dimension (in our case its only reducing it from 3 to 2)
  # 3. Plot all data points with each cluster having a different colour


########################################################################
##### OPTIONAL: STEP 1-5 IS THE SAME, BUT RUN HIERARCHICAL CLUSTERING INSTEAD...
########################################################################

### You can also try running this using hierarchical clustering (although generally not a great idea when you have 1000s of members)

#Here's th boiler plate code:

#1 Normalize/Scale your input features
summary(df_RFM2) #here's the scaled df we used for the k-means

#2. calculate distance matrix
d = dist(df_RFM2)

#3. Run hclust algorithm on distance matrix
clustering = hclust(d, method = 'single')

#4. View cluster outputs
clustering

#5 Cut dendrogram at K=12
members = cutree(clustering,k = 12) #cut at 12 clusters
members[1:5] #look at first 5 members

#6. Profile the clusters to understand their characteristics
table(members)
aggregate(df_RFM[,2:4], by=list(members), mean)

########################################################################
########################################################################



