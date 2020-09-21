#Purpose: NLP Topic Modeling

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
wk.exp <- read.csv("Queen's MMA\\MMA 865\\Lecture 3\\1yr_plus_final4.csv", header=TRUE, sep = ",")

#import newspaper articles
text <- read.csv("Queen's MMA\\MMA 865\\Lecture 3\\Guardian_articles_11_14_2015_12_1_2015.csv", header=TRUE, sep = ",")

#import boston airbnb reviews (large corpora; can use fread instead of read.csv)
airbnb <- fread("Queen's MMA\\MMA 865\\Lecture 3\\Airbnb-boston_only.csv")

#----- TEXT CLUSTERING -----

#function for text preprocessing (same as usual)
clean.corpus <- function(corpus) {
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, c(stopwords("en"), "customer", "service", "customers", "calls")) #have added a few specific words
  return(corpus)
}

#create corpus
wk.source <- VCorpus(VectorSource(wk.exp$text))

#put through preprocessing function
wk.corpus <- clean.corpus(wk.source)

#convert to dtm
wk.dtm <- DocumentTermMatrix(wk.corpus, control = list(weighting = weightTfIdf))

#normalize the term vectors
wk.dtm.s <- scale(wk.dtm, scale=T)

##k-means (let's specify 3 clusters)
wk.clusters <- kmeans(wk.dtm.s, 3)

#barplot of cluster sizes
barplot(wk.clusters$size, main = 'k-means')
  #observations: notice that cluster 3 is extremely larger than the first 2

#plot our clusters
plotcluster(cmdscale(dist(wk.dtm)), wk.clusters$cluster)
  #observations: can pretty clearly see the large cluster, then 2 outliers for the other 2 clusters
  #realistically this is saying that the vast majority of the 50 obs in the wk.exp df are very similar...which
  #kind of makes sense given that they are all applicant resumes for the same job

#silhouette plot
dissimilarity.m <- dist(wk.dtm.s)
plot(silhouette(wk.clusters$cluster, dissimilarity.m))
  #observations: silhouette score of only avg 0.21...not very good cluster definition.
  #if the k-means algorithm was effective for this data, would expect to see 3 distinct groups of silhouettes
  #and a large average silhouette coefficient

#can extract prototypical terms from each cluster
library(clue)
work.clus.proto <- t(cl_prototypes(wk.clusters)) #note that in this ex this won't be all that useful due to the poor clustering

#can use a comparison cloud to quickly compare
comparison.cloud(work.clus.proto, max.words=100)

##spherical k-means (difference in how distances are measured - uses cosine over euclidean/manhattan); should always test multiple to see what works best with your data
library(skmeans)
wk.dtm <- DocumentTermMatrix(wk.corpus, control = list(weighting = weightTfIdf))

#again set # clusters to 3
soft.part <- skmeans(wk.dtm, 3, m = 1.2, control = list(nruns = 5, verbose = T)) #m parameter controls "fuzziness" of the cluster borders (1 is lowest value and indicates no fuzziness), run 5 times for better stability (don't get unlucky)

#barplot for cluster sizes
barplot(table(soft.part$cluster), main = 'Sperical k-means')
  #observations: much more even cluster distribution

#plot our clusters
plotcluster(cmdscale(dist(wk.dtm)), soft.part$cluster)

#now check out silhouette plot
plot(silhouette(soft.part))
  #observations: notice the 3 distinct silhouette shadows for each cluster, though still a tiny score (note our small sample though)
  #overall though, looks like using spherical k-means (cosine similarity) proved the better choice

#extract prototypical terms from each cluster and display in comp cloud
s.clus.proto <- t(cl_prototypes(soft.part))
comparison.cloud(s.clus.proto, max.words = 100)
  #observations: now some unique words appear

#examine closer the top 5 prototypical terms from each cluster
sort(s.clus.proto[,1], decreasing=T) [1:5]
sort(s.clus.proto[,2], decreasing=T) [1:5]
sort(s.clus.proto[,3], decreasing=T) [1:5]
  #observations: cluster 1 shows past work at Amazon with providing orders? cluster 2 with a large focus on team
  #and performance; cluster 3 with skills, multitasking, accomplishments

##k-mediod clustering (uses median instead of avg for cluster centres)
wk.dtm <- DocumentTermMatrix(wk.corpus, control = list(weighting = weightTfIdf))

#use pamk from fpc library for k-mediods
wk.mediods <- pamk(wk.dtm, krange = 2:4, critout = T) #krange specifies range of possible clusters (will select cluster size that maximizes silhouette scores)
  #observations: can see that 2 clusters results in the highest silhouette score

#silhouette plot
dissimilarity.m <- dist(wk.dtm)
plot(silhouette(wk.mediods$pamobject$clustering, dissimilarity.m))
  #observations: unfortunately, similar to k-means, 1 cluster is dominating

#----- EXPLORING STRING DISTANCE -----

library(stringdist)

stringdist('crabapple', 'apple', method = "lcs") #4 letters must change (i.e., be removed from crabapple to = apple)

#matching using string distance
match('apple', c('crabapple', 'pear')) #no match

amatch('apple', c('crabapple', 'pear'), maxDist=3, method='dl') #still no match because maxDist = 3 (4 required)

amatch('apple', c('crabapple', 'pear'), maxDist=4, method='dl') #once set to 4, we get the 1 expected match

ain('raspberry', c('berry', 'pear'), maxDist=4, method='dl') #ain provides boolean outcome

#string distance using hamming
stringdist('raspberry', c('berry', 'pear'), method='hamming') #hamming only allows for substitution (and there's no way to get raspberry to match either term through just substitution)

#string distance using osa
stringdist('raspberry', c('berry', 'pear'), method='osa') #osa allows for both substitution and deletion (4 moves for berry, 6 for pear)

#----- LDA TOPIC MODELING -----

#only examining the body column of the df (numerous unneeded columns); perform preprocessing (manual effort here)
articles <- iconv(text$body, "latin1", "ASCII", sub="")
articles <- gsub('http\\S+\\s*', '', articles)
articles <- bracketX(articles, bracket='all')
articles <- gsub("[[:punct:]]", "", articles)
articles <- removeNumbers(articles)
articles <- tolower(articles)
articles <- removeWords(articles, c(stopwords('en'), 'pakistan', 'gmt', 'england')) #notice some manually defined words

#quick function to ensure words are trimmed to have at least one character
blank.removal <- function(x) {
  x <- unlist(strsplit(x, ' '))
  x <- subset(x, nchar(x) > 0)
  x <- paste(x, collapse = ' ')
}
#apply function
library(pbapply)
articles <- pblapply(articles, blank.removal)

#testing strsplit function
ex.text <- c('Text mining is a              good time', 'Text mining is a good time')

strsplit(ex.text[1], ' ') #notice that it fails with the blank space (multiple empty tokens)
strsplit(ex.text[2], " ") #here it works perfectly (6 unique tokens - one for every word in sentence)

#examining our blank.removal function and how it can solve this issue
  #first uses strsplit which is nested inside the unlist function
char.vec <- unlist(strsplit(ex.text[1], " "))
char.vec #notice that problem still exists

  #next subsets the vector of words by # of characters > 0 (our blanks will be removed)
char.vec <- subset(char.vec, nchar(char.vec) > 0)
char.vec #now we have the proper tokens

  #lastly, just collapses the text back into a single sentence/document
char.vec <- paste(char.vec, collapse = " ")
char.vec

#feature extraction - lda package uses a function called lexicalize (as opposed to prior dtm methods)
  #test it out
ex.text <- c('this is a text document')
ex.text.lex <- lexicalize(ex.text)

ex.text.lex$documents #columns are words; 1st row represents position of word, 2nd row represents frequency

ex.text.lex$vocab #returns the individual words of in the document

#add a 2nd document
ex.text <- c('this is a text document', 'text mining a text document is great')

#put through lexicalize
ex.text.lex <- lexicalize(ex.text)

#check out matrix
ex.text.lex$documents #now returns two matrices (one per document); notice that 2nd row (2nd matrix) is all ones (despite text showing up twice) - this is a difference in lexicalize matrices vs standard dtm functions seen before
ex.text.lex$vocab #using index in row 1 (matrix 2) and vocab, can reconstruct 2nd sentence

#now let's apply it to our articles
documents <- lexicalize(articles)

#word counts
wc <- word.counts(documents$documents, documents$vocab)
doc.length <- document.lengths(documents$documents) #word count for all 50 articles

##fit the LDA model - specify parameters first
k <- 4 #number of topics that we'll identify
num.iter <- 25 #number of sampling repetitions (more reliable, similar idea to cross-validation)
alpha <- 0.02 #represents the prior document-topic distributions
eta <- 0.02 #sets the prior topic-term distributions
set.seed(1024)

fit <- lda.collapsed.gibbs.sampler(documents = documents$documents, K = k, vocab = documents$vocab, 
                                   num.iterations = num.iter, alpha = alpha, eta = eta, initial = NULL, 
                                   burnin = 0, compute.log.likelihood = TRUE)

#examine log likelihood visually
plot(fit$log.likelihoods[1,]) #looks like we've reached a stable convergence...this is good (shows improvement from the 25 iterations before leveling off)

#print top words for each of the 4 extracted topics
top.topic.words(fit$topics, 7, by.score=TRUE)
  #obsevations: topic 1 looks related to sports (ball, single, wicket, first, four)
  #topic 2 looks related to terrorist attacks (paris, attacks, isis, syria, police)
  #topic 3 looks related to ?
  #topic 4 looks related to ?

#identify the document that best represents each topic
top.topic.documents(fit$document_sums,1)
  #observations: document 6 best represents topic 1, etc.

#to plot, need to estimate document topic distributions using this function
theta <- t(pbapply(fit$document_sums + alpha, 2, function(x) x/sum(x)))
phi <- t(pbapply(t(fit$topics) + eta, 2, function(x) x/sum(x)))

#construct interactive visual
library(LDAvis)
article.json <- createJSON(phi = phi, theta = theta, doc.length = doc.length, vocab = documents$vocab, term.frequency = as.vector(wc))
  #this will pop open an interactive plot in browser (pretty cool!)

#assign each article to the most prominent topic - use custom function (essentially we want to see which topic words are most prevalent in a given document)
doc.assignment <- function(x) {
  x <- table(x)
  x <- as.matrix(x)
  x <- t(x)
  x <- max.col(x)
}

#this will print the topic assignments for the first 10 words from the 6th article
fit$assignments[[6]][1:10]
  #observations: 1st word assigned to topic 1, 3rd word to topic 4, etc.

#create table of assignments
table(fit$assignments[[6]][1:10])
  #observations: majority of these first 10 words in 6th article are from topic 1

#change table to matrix and transpose
t(as.matrix(table(fit$assignments[[6]][1:10])))

#return the column name containing the most frequent topic assignment
max.col(t(as.matrix(table(fit$assignments[[6]])))) #ends up getting assigned to topic 1

#now apply the function to all our articles
assignments <- unlist(pblapply(fit$assignments, doc.assignment))

#can recode topics to something more familiar
assignments <- recode(assignments, "1='Cricket1'; 2='Paris Attacks'; 3='Cricket2'; 4='Unknown'")

#create sequence of numbers corresponding to the articles, calculate polarity
article.ref <- seq(1:nrow(text))
article.pol <- polarity(articles)[[1]][3]

article.tree.df <- cbind(article.ref, article.pol, doc.length, assignments)
  #we have the articles, polarity scores, length, and topic!

#create treemap
library(treemap)
treemap(article.tree.df, index=c("assignments", 'article.ref'),
        vSize="doc.length", vColor="polarity", type="value", 
        title="Guardian Articles mentioning Pakistan", palette = c("red", "white", "green"))
  #observations: very cool; size is in relation to document length, see negative sentiment tied to Paris Attacks,
  #pretty positive for cricket

#----- TEXT VECTORIZATION W/ text2vec -----
#NOTE: text2vec is best used with large corpora and with some term weighting like TFIDF

#select 3 columns of interest from og df
airbnb <- data.table(review_id = airbnb$review_id, comments = airbnb$comments, review_scores_rating = airbnb$review_scores_rating)

#preprocessing
airbnb$comments <- removeWords(airbnb$comments, c(stopwords('en'), 'Boston'))
airbnb$comments <- removePunctuation(airbnb$comments)
airbnb$comments <- stripWhitespace(airbnb$comments)
airbnb$comments <- removeNumbers(airbnb$comments)
airbnb$comments <- tolower(airbnb$comments)

#tokenization
tokens <- strsplit(airbnb$comments, split = " ", fixed = T)

#create vocabulary
vocab <- create_vocabulary(itoken(tokens), ngram = c(1,1)) #notice we are allowing only single words (ngrams = 1)

#prune vocabulary (remove frequent terms and infrequent terms) - remove terms that appear < 5 times
vocab <- prune_vocabulary(vocab, term_count_min = 5)
  #observations: in vocab, we have a list of terms, their count, and document count

#create term co-occurrence matrix (TCM)
iter <- itoken(tokens)
vectorizer <- vocab_vectorizer(vocab) #grow_dtm = F tells function not to build a document term matrix
                                                                           #skip_grams_windows wil use rior and former 5 words around a target word for n-grams
tcm <- create_tcm(iter, vectorizer)

str(tcm)

#fit GloVe (outdated code in book - couldn't find solution online...)
glove = GlobalVectors$new(rank = 50, x_max = 10, learning_rate = 0.2)

fit.glove <- glove$fit_transform(tcm, num_iter = 15)

good.walks <- word.vect

