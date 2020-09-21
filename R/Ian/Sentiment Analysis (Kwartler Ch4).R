#Purpose: NLP Sentiment Analysis

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
bos.airbnb <- read.csv("Queen's MMA\\MMA 865\\Lecture 3\\bos_airbnb_1k.csv", header=TRUE, sep = ",")

#import wizard of oz text
oz <- readLines("Queen's MMA\\MMA 865\\Lecture 3\\Wizard_Of_Oz.txt")

#----- SENTIMENT SCORING -----

#create a vector of new positive terms
new.pos <- c('rofl', 'lol')

#add to the other positive terms in qdap's subjectivity lexicon ("key.pol")
old.pos <- subset(as.data.frame(key.pol), key.pol$y == 1) #pulling only terms with a polarity value = 1

all.pos <- c(new.pos, old.pos[,1])

#create a vector of new negative terms
new.neg <- c('kappa', 'meh')

#add to the other negative terms in qdap's subjectivity lexicon
old.neg <- subset(as.data.frame(key.pol), key.pol$y == -1)

all.neg <- c(new.neg, old.neg[,1])

#now we combine both for full lexicon dictionary
all.polarity <- sentiment_frame(all.pos, all.neg, 1, -1)

#invoke the new lexicon
polarity('ROFL, look at that!', polarity.frame = all.polarity)
  #observations: 4 total words, avg polarity score = 0.5 (positive)
  #actual calculation (see lec 3 notes): 1 ("ROFL") / sqrt(4 words) = 1/2 = 0.5

polarity('whatever you say, kappa', polarity.frame = all.polarity)

#notice that without the specific lexicon specified, comes back as neutral since ROFL isn't found
polarity('ROFL, look at that!')

#apply polarity to the boston airbnb comments
bos.pol <- polarity(bos.airbnb$comments)

#plot distribution of polarity scores
ggplot(bos.pol$all, aes(x = polarity, y=..density..)) +
  theme_gdocs() +
  geom_histogram(binwidth = .25, fill = 'darkred', colour = 'grey60', size = .2) +
  geom_density(size = .75)
  #observations: normal distribution, centred around 1 (positive sentiment)

#append polarity scores to the df
bos.airbnb$polarity <- scale(bos.pol$all$polarity)

##Create a word cloud comparing positive and negative terms
#create subset of documents that are only positive or negative
pos.comments <- subset(bos.airbnb$comments, bos.airbnb$polarity > 0)
neg.comments <- subset(bos.airbnb$comments, bos.airbnb$polarity < 0)

#collapse and combine into a single character vector
pos.terms <- paste(pos.comments, collapse = " ")
neg.terms <- paste(neg.comments, collapse = " ")
all.terms <- c(pos.terms, neg.terms)
all.corpus <- VCorpus(VectorSource(all.terms)) #create corpus

#simple preprocessing of the corpus (IRL should use a more robust function as seen in Ch 2/3 Rscript)
  #note we are using TF-IDF here
all.tdm <- TermDocumentMatrix(all.corpus, control=list(weighting=weightTfIdf, removePunctuation = TRUE,
                                                       stopwords = stopwords(kind = 'en')))

#switch to matrix and label columns
all.tdm.m <- as.matrix(all.tdm)
colnames(all.tdm.m) <- c('positive', 'negative')

#create comparison cloud
comparison.cloud(all.tdm.m, max.words=100, colors=c('darkgreen','darkred'))
  #observations: + words include unique, enjoying, well equipped; - words include dirty, bad, automated
  #also a lot of mentions of names in positives - could hint at direct interaction with service rep

##symbol-based emoticons
#must escape Unicode by using "\" before the code
"\U2764" #heart
text <- "I am \U263A. I \U2764 ice cream"

#using mgsub, can create a function that searches for these emojis in text, and replaces them with relevant word
patterns <- c('\U2764', '\U263A')
replacements <- c('love', 'happy')
mgsub(patterns, replacements, text) #apply the substitution to our text

##punctuation-based emoticons
#load in qdap's dictionary
data(emoticon)
head(emoticon) #sneak peak

#important to be able to build out emoji lexicon from the standard (especially with social media)
meaning <- c('troubled face', 'crying')
emoji <- c('(>_<)', 'Q_Q')
new.emotes <- data.frame(meaning, emoji) #join them into a df
new.emotes <- dplyr::rename(new.emotes, "emoticon" = "emoji")
emoticon <- rbind(emoticon, new.emotes) #bind to existing lexicon

#test it
text <- "Text mining is so fun :-D. Other tm books make me Q_Q because they have academic examples!"

mgsub(emoticon[,2], emoticon[,1], text) #nice!

##tidytext approach to sentiment
library(tidytext)
library(textdata)

#manually compare and contrast lexicon differences from the sentiments df that comes with tidytext
data(sentiments)  #lexicon

afinn <- get_sentiments(lexicon = "afinn")
bing <- get_sentiments(lexicon = "bing")
nrc <- get_sentiments(lexicon = "nrc")

#preprocess and create document term matrix
oz.corp <- VCorpus(VectorSource(oz))

clean.corpus <- function(corpus){
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, stopwords('english'))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeNumbers)
  return(corpus)
}

  #run oz through preprocessing function
oz.corp <- clean.corpus(oz.corp)
oz.dtm <- DocumentTermMatrix(oz.corp)

#tidytext provides easy way to convert traditional tdm to a tidy format
oz.tidy <- tidy(oz.dtm)

#check out a snippet
oz.tidy[6810:6815, ]

#edit column names and change line num to numeric
colnames(oz.tidy) <- c('line_number', 'word', 'count')
oz.tidy$line_number <- as.numeric(oz.tidy$line_number)

#"joy" words in oz - perform inner join on oz to connect shared words with joy from the nrc lexicon
nrc.joy <- subset(nrc, nrc$sentiment == 'joy') #filter nrc lexicon to joy records

joy.words <- inner_join(oz.tidy, nrc.joy)
joy.words <- dplyr::count(joy.words, word)

#construct polarity timeline by joining on the bing lexicon
oz.sentiment <- inner_join(oz.tidy, bing)
oz.sentiment <- dplyr::count(oz.sentiment, sentiment, index=line_number)

#use spread to divide single sentiment column into positive & negative
oz.sentiment <- spread(oz.sentiment, sentiment, n, fill=0)

#add a polarity column and a column indicating if positive or negative
oz.sentiment$polarity <- oz.sentiment$positive - oz.sentiment$negative
oz.sentiment$pos <- if_else(oz.sentiment$polarity >= 0, "pos", "neg")

#barplot
ggplot(oz.sentiment, aes(x = index, y = polarity, fill = pos)) +
  geom_bar(stat="identity", position="identity", width=1) +
  theme_gdocs()

#adding smoothing
oz.smooth <- ggplot(oz.sentiment, aes(index, polarity))
oz.smooth + stat_smooth() + theme_gdocs()
  #observations: looks like there's a dip in polairty before index 3000 (could be conflict in the story at this point)
  #also see that the story ends on a positive note, resolution


