#Purpose: Text mining and string manipulation (Kwartler Chapter 2)

#Cleansing/Manipulation Packages:
pacman::p_load("dplyr","plyr","tidyr","stringr","lubridate","janitor","forcats","readr","qdap","tm")
#Graphing Packages:
pacman::p_load("ggplot2","esquisse","gridExtra","ggridges","igraph","dendextend","circlize","wordcloud","plotrix")
#ML Packages:
pacman::p_load("car","MLmetrics","estimatr","fpp","forecast","caret","ROCR","lift","randomForest","glmnet","MASS",
               "e1071","partykit","rpart","ROCR","lift","randomForest","xgboost","tensorflow", "keras")
#Other Packages:
pacman::p_load("sqldf","readxl","geosphere", "pricesensitivitymeter")

#----- DATA IMPORT -----

#import delta data (downloaded from www.tedkwartler.com)
text.df <- read.csv("Queen's MMA\\MMA 865\\Lecture 2\\oct_delta.csv", header=TRUE, sep = ",")

#import amazon data
amzn <- read.csv("Queen's MMA\\MMA 865\\Lecture 2\\amzn_cs.csv", header=TRUE, sep = ",")

#----- GENERAL STRING MANIPULATION -----

#check length of the texts
nchar(head(text.df$text))

#average length of the customer's reply
mean(nchar(text.df$text))
  #observations: About 92 characters

#practice using sub function (replaces first instance)
sub('thanks', 'thank you', text.df[1,5], ignore.case = T)
  #observations: this substitutes the "thanks" in the first response to "thank you"

#practice using gsub function (replaces all instances)
fake.text <- "R text mining is good but text mining in Python is also"

sub('text mining', 'tm', fake.text, ignore.case = F) #notice only first changes
gsub('text mining', 'tm', fake.text, ignore.case = F) #notice both are now subbed

gsub('&amp', '', text.df[5,5]) #gsub is useful for removing/replacing unwanted characters

#mgsub from qdap packages allows for multiple substituions in one command (as opposed to multiple gsub commands)
patterns <- c('good', 'also', 'text mining')
replacements <- c('great', 'just as suitable', 'tm')
mgsub(patterns, replacements, fake.text)

#use grep/grepl to search for "sorry" in customer responses (grep/grepl are good to identify the existence of something in a string)
grep('sorry', text.df$text, ignore.case = T)

sorry <- grepl('sorry', text.df$text, ignore.case=T)
sum(sorry)/nrow(text.df)
  #observations: 13% of the customer service responses included "sorry"

#can also search for multiple searches
grep(c('sorry | apologize'), text.df$text, ignore.case = T)

#check to see whether rep provided customers with a link or phone # more often
sum(grepl('http', text.df$text, ignore.case=T))/nrow(text.df) #4.28% of responses referenced a link

sum(grepl('[0-9]{3} | [0-9]{4}', text.df$text))/nrow(text.df) #11.33% of responses reference a phone #
  #observations: it appears that the customer reps are more likely (possibly trained) to get customers to call back

#stringi and string r can be used to find how many times something is in a string
stri_count(text.df$text, fixed = 'http')

str_detect(text.df$text, 'http')

#----- TEXT PREPROCESSING -----

#adding an id column
tweets <- data.frame(doc_id = seq(1:nrow(text.df)), text = text.df$text)

#wrapper for tolower function (errors if encounters a special character)
trytolower <- function(x) {
  #return NA when there is an error
  y = NA
  try_error = tryCatch(tolower(x), error = function(e) e)
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  return(y)
}

#define stopwords
custom.stopwords <- c(stopwords('english'), 'lol', 'smh', 'delta') #stopwords includes common english, we've added a few custom stopwords common to our responses (e.g., "delta")

#define function to obtain clean corpus (takes care of punctuation, whitespace, numbers, case)...order matters
clean.corpus <- function(corpus) {
  corpus <- tm_map(corpus, 
content_transformer(trytolower)) #first lower case all the words for commonality
  corpus <- tm_map(corpus, removeWords, #now remove defined stop words
custom.stopwords)
  corpus <- tm_map(corpus, removePunctuation) #lastly, remove punctuation, whitespace, numbers
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeNumbers)
  return(corpus)
}

#define our corpus (the 1377 tweets)
corpus <- VCorpus(DataframeSource(tweets))

#pipe through our clean.corpus function
corpus <- clean.corpus(corpus)

#view the first cleaned tweet
as.list(corpus)[1]
corpus[[1]][1]

#spellcheck
tm.definition <- 'Txt mining is the process of distilling actionable insyghts from text.' #purposefully misspelled

which_misspelled(tm.definition) #identifies our misspelled words

check_spelling_interactive(tm.definition) #goes through misspellings interactively suggesting/allowing corrections

#define function to check/fix spelling (sacrifice accuracy for speed here - much better than the above but not always correct)
fix.text <- function(myStr) {
  check <- check_spelling(myStr)
  splitted <- strsplit(myStr, split=' ')
  for (i in 1:length(check$row)) {
    splitted[[check$row[i]]][as.numeric(check$word.no[i])] = check$suggestion[i] #replaces with first suggestion
  }
  df <- unlist(lapply(splitted, function(x) paste (x, collapse = ' ')))
  return(df)
}

fix.text(tm.definition) #here we try it on our fake definition (notice actionable gets incorrectly changed but the other two changes are correct)

#----- TERM DOCUMENT MATRIX -----

#create tdm using term frequency
tdm <- TermDocumentMatrix(corpus, control = list(weighting = weightTf)) #set weighting to weightTf for term freq

#reclassify it as a matrix
tdm.tweets.m <- as.matrix(tdm) #expectedly it's very large...1377 columns (tweets) and 2631 rows (distinct terms after cleaning function)

#view a snippet (for speed purposese) of our matrix
tdm.tweets.m[2250:2255,1340:1342]
  #observations: notice that tweet 1340 included the word sorry

#calculate the term frequency (row-rise sum)
term.freq <- rowSums(tdm.tweets.m)

#create df for term frequency
freq.df <- data.frame(word=names(term.freq), frequency=term.freq)
  #observations: please, can, and sorry are the top 3 most frequent terms across all our tweets

#----- VISUALIZATION -----

#barplot looking at top 20 most frequent terms in our tweet corpus
freq.df <- freq.df[order(freq.df[,2], decreasing=T),]

freq.df$word <- factor(freq.df$word, levels = unique(as.character(freq.df$word)))

ggplot(freq.df[1:20,], aes(x=word, y=frequency)) +
  geom_bar(stat="identity") +
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=frequency), colour = "white", hjust=1.25, size = 5.0)

##in some cases, we may be interested in a specific frequent word (i.e., apologies) and what terms were associated with it
#create df of associated terms
associations <- findAssocs(tdm, 'apologies', 0.11) #we've set corr limit to 0.11 - this threshold can be adjusted
associations <- as.data.frame(associations)
associations$terms <- row.names(associations)
associations$terms <- factor(associations$terms, levels=associations$terms)

#visualize our association findings
ggplot(associations, aes(y=terms)) +
  geom_point(aes(x=apologies), data=associations, size=3) +
  theme_minimal() + 
  geom_text(aes(x=apologies, label=apologies), colour = "darkred", hjust = -0.5, size=4) +
  theme(text = element_text(size=12), axis.title.y=element_blank())
  #observations: "delay" has the highest corresponding association value (0.3) ("issues" a close 2nd)

##word networks (here we explore "refund"; only use with small corpora or subsets...otherwise too clustered and non-impactful)
refund <- tweets[grep("refund", tweets$text, ignore.case=T), ] #here are 7 tweets that included "refund"

#create corpus
refund.corpus <- VCorpus(DataframeSource) #selecting first 3 tweets

#put through the cleaning function
refund.corpus <- clean.corpus(refund.corpus)

#establish tdm
refund.tdm <- TermDocumentMatrix(refund.corpus, control = list(weighting = weightTf))

#create an adjacency matrix
refund.m <- as.matrix(refund.tdm) #convert into matrix first (similar to what we did in line 131)
refund.adj <- refund.m %*% t(refund.m) #here we have the square adjacency matrix
refund.adj <- graph.adjacency(refund.adj, weighted=TRUE, mode="undirected", diag=T) #set up object for igraph
refund.adj <- simplify(refund.adj)

#igraph
plot.igraph(refund.adj, vertex.shape="none", vertex.label.font=2, vertex.label.color="darkred",
            vertex.label.cex=0.7, edge.color="gray85")
title(main='@DeltaAssist "Refund" Word Network')
  #note that this is only based on 3 tweets (explains the 3 distinct network clusters)
  #close connection between "refund" and "apologies"

#or simply just use word_network_plot function from qdap (much easier...don't even need adjacency matrix)
word_network_plot(refund$text[1:3])
title(main='@DeltaAssist "Refund" Word Network')

#examine all 7 refund tweets
word_associate(tweets$text, match.string=c('refund'), stopwords=Top200Words, network.plot = T,
               cloud.colors = c('gray85', 'darkred'))
title(main='@DeltaAssist "Refund" Word Network')

##hierarchical dendrograms
#first reduce tdm by removing sparse terms (could have used this in above exercises too)
tdm2 <- removeSparseTerms(tdm, sparse=0.975)
#note: typical sparse thresholds range from 0.95-0.99 (TDM's are very sparse, i.e., lots of 0's)
#observations: we've gone from original 2631 terms to 43

#now create hierarchical clustering plot
hc <- hclust(dist(tdm2, method = "euclidean"), method = "complete") #we use euclidean distance metric w/ complete linkage
plot(hc, yaxt = 'n', main = "@DeltaAssist Dendrogram")
#observations: we see lots of apologetic wording and a distinct cluster asking for a direct message

#circular version of the dendrogram
hcd <- as.dendrogram(hc)
hcd <- color_labels(hcd, 4, col = c('#bada55', 'darkgrey', "black", 'darkred'))
hcd <- color_branches(hcd, 4, col = c('#bada55', 'darkgrey', "black", 'darkred'))
circlize_dendrogram(hcd, labels_track_height = 0.5, dend_track_height = 0.4)
  #observations: can see some associations (we see the dm request in red, 'team' and 'will' closely related)

##wordclouds (overused but can be effective)
#wordcloud function will plot ALL words (comparison.cloud and commonality.cloud plot subsets)
wordcloud(freq.df$word, freq.df$frequency, max.words=100, colors=c('black','darkred')) #max set to 100 words
  #observations: can see that the frequencies (please > can > sorry) match up with the word size

##comparison and commonality clouds (subsets of the full wordcloud when comparing two corpora)
#first we'll adjust the clean function slightly
trytolower <- function(x) {
  y = NA
  try_error = tryCatch(tolower(x), error = function(e) e)
  if(!inherits(try_error, 'error'))
    y = tolower(x)
  return(y)
}

custom.stopwords <- c(stopwords('english'), 'sorry', 'amp', 'delta', 'amazon') #removing delta and amazon + sorry

clean.vec <- function(text.vec) {
  text.vec <- trytolower(text.vec)
  text.vec <- removeWords(text.vec, custom.stopwords)
  text.vec <- removePunctuation(text.vec)
  text.vec <- stripWhitespace(text.vec)
  text.vec <- removeNumbers(text.vec)
  return(text.vec)
}

#bring in both dfs
delta <- read.csv("Queen's MMA\\MMA 865\\Lecture 2\\oct_delta.csv", header=TRUE, sep = ",")
amzn <- read.csv("Queen's MMA\\MMA 865\\Lecture 2\\amzn_cs.csv", header=TRUE, sep = ",")

#clean using our altered function
amzn.vec <- clean.vec(amzn$text)
delta.vec <- clean.vec(delta$text)

#collapse into single doc
amzn.vec <- paste(amzn.vec, collapse = " ")
delta.vec <- paste(delta.vec, collapse = " ")
all <- c(amzn.vec, delta.vec)
corpus <- VCorpus(VectorSource(all))

#create tdm
tdm <- TermDocumentMatrix(corpus)
tdm.m <- as.matrix(tdm)
colnames(tdm.m) = c("Amazon", "Delta")

#select colours
pal <- brewer.pal(8, "Purples")
pal <- pal[-(1:4)]

#commonality cloud
commonality.cloud(tdm.m, max.words = 200, random.order = FALSE, colors = pal)
  #observations: most common words between Delta and Amazon customer service include please, can, hear, team, etc.

#comparison cloud
comparison.cloud(tdm.m, max.words = 200, random.order = FALSE, title.size=1.0, colors=brewer.pal(ncol(tdm.m), "Dark2"))
  #observations: Delta in orange and Amazon in green; can see Amazon uses lots of links for feedback while Delta
  #seems big on phone numbers; "delivery" common to Amazon, "assistance" and "flight" common to Delta

#create a common words matrix
common.words <- subset(tdm.m, tdm.m[,1] > 0 & tdm.m[,2] > 0) #578 common words between Amazon and Delta

#calculate frequency differences per common word
difference <- abs(common.words[,1] - common.words[,2])

common.words <- cbind(common.words, difference)
common.words <- common.words[order(common.words[,3], decreasing = TRUE), ]
  #observations: looking at the df can clearly see the words that correspond to the largest text on the comparison cloud

#let's take the top 25 common terms with the most polarity
top25.df <- data.frame(x = common.words[1:25, 1], y = common.words[1:25, 2],
                       labels = rownames(common.words[1:25, ]))

#visualize
pyramid.plot(top25.df$x, top25.df$y,
             labels = top25.df$labels, gap = 24, top.labels = c("Amazon", "Words", "Delta"),
             main = "Words in Common", laxlab = NULL, raxlab = NULL, unit = NULL)

