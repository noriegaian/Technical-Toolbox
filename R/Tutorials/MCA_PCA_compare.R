# load packages
require(FactoMineR)
require(ggplot2)

# load tea dataset (part of FactoMineR package) & keep only the top 6 columns for this demo
data(tea)
newtea = tea[, c("Tea", "How", "how", "sugar", "where", "always")]
head(newtea) #take a look 

############################################################
##### Run MCA - multiple correspondance analysis #####
mca1 = MCA(newtea, graph = FALSE)
summary(mca1)

############################################################
##### Run PCA #####
#first need to 1 hot encode
library(fastDummies)
dummy_df <- fastDummies::dummy_cols(newtea,remove_first_dummy = TRUE)

#now select numeric columns & run PCA
num_cols = unlist(lapply(dummy_df, is.numeric))
pca_df = dummy_df[ , num_cols]
pca_model=prcomp(pca_df)

############################################################
##### Comapare results of MCA vs PCA #####
summary(mca1)
summary(pca_df)

############################################################
##### Apply PCA  #####
#Looks like PCA is better, so we pick top 6 components & move on to next step...
pca_scored <- data.frame(pca_df, pca_model$x)
#pca_scored
keeps <- c("PC1","PC2", "PC3", "PC4", "PC5", "PC6")
pca_final = pca_scored[keeps]
summary(pca_final)

# old code - remove...
# number of categories per variable
cats = apply(newtea, 2, function(x) nlevels(as.factor(x)))
cats

