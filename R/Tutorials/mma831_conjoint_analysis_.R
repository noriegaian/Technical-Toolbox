#Conjoint Analysis
#See ref link: ://www.r-bloggers.com/conjoint-analysis-understand-your-customer-and-beat-the-competition/

########################################################################
##### STEP 1: DESIGN OF EXPERIMENT
########################################################################
#Design of Experiment
# Creating a fractional Design
#install.packages("DoE.base")
library(DoE.base)
test.design <-oa.design(nlevels =c(6,2,3,3,3,2,2))

#generates fractional factorial design
FracDesign = as.data.frame(test.design)

#if we want to make this human-readable, we need to give right names for attributes
names(FracDesign) <-c("Brand", "Cores", "RAM", "HardDrive","DSize","DQuality","TouchScreen")

#next same thing with levels...
levels(FracDesign$Brand) <-c("Apple", "Lenovo", "Acer", "Asus","Ethos", "Other")
levels(FracDesign$Cores) <-c("Dual Core", "Quad Core")
levels(FracDesign$RAM) <- c("4GB", "8 GB", "16 GB")
levels(FracDesign$HardDrive) <-c("256 GB", "512 GB", "1024 GB")
levels(FracDesign$DSize) <-c("12 Inch", "14 Inch", "15.2 Inch")
levels(FracDesign$DQuality) <-c("Normal", "HD")
levels(FracDesign$TouchScreen) <-c("Yes", "No")
rm(test.design) #remove obj from env

FracDesignßß

# Save design into an excel file
#install.packages("xlsx")
library(xlsx)
write.xlsx(FracDesign, "/Users/mm679j/Documents/R_Tutorial/Conjoint/ExperimentalDesign.xlsx");

FracDesign

##The table from above shows the fractional design that we will use for our conjoint analysis with one row corresponding to one run. The idea is that each person that participates in our conjoint analysis, will go through each run and rate the laptop.
#The more people participate, the better and more precise information we will have in order to estimate the market share and to understand our potential customers

#full factorial # of configs (or as this tutorial calls it "runs")
x=(6*2*3*3*3*2*2)
x


#Ethos will go fo an online sales strategy, it was very important to design the presentation of an alternative as realistic as possible.
#Another consideration is that it might be useful to add a description of all attributes and why they might be important, before the customer starts to rate the laptops
#we include a description of all attributes, the importance and the relevance. For instance, we would explain that high RAM might be important if you edit videos, edit high resolution images or process high amounts of data

#Measurement scale
#we decide to use a rating system like 1-9


########################################################################
##### STEP 2: DATA COLLECTION (WE USE SIMULATED DATA BELOW)
########################################################################

#Simulated data (i.e. collecting repsonses) 
#In reality this would happen through a market research survey using the fractional design
#### Data Collection (Create Dataset)
# Create basis
set.seed(123)
n <- 89 # number of participants
Data <- data.frame(Participant =1:89)
Data$Participant <-as.factor(Data$Participant)
for (run in 1:36) {
  Data[,paste("Run",as.character(run), sep = "")]<- sample(c(1:9), n, replace = TRUE)
}

# Shaping the data - adjusts scores to create a realistic distibution in rating etc.
Data[,c(6,11,17,28,33)] <-Data[,c(6,11,17,28,33)] + 2 # Improve Apple
Data[,c(8,13,14,15,18,35)] <-Data[,c(8,13,14,15,18,35)] - 2 # Decrease Ethos
Data[,c(2,4,5,7,8,11,12,13,16,18,19,25,28,29,31,32,33,37)]<- Data[c(2,4,5,7,8,11,12,13,16,18,19,25,28,29,31,32,33,37)] - 0.6 
Data[,c(2,3,5,9,11,13,15,16,19,23,26,30)]<- Data[, c(2,3,5,9,11,13,15,16,19,23,26,30)] + 0.9
Data[,c(2,3,6,9,10,13,18,19,20,21,22,23,25,28,29,31,33,35)]<- Data[,c(2,3,6,9,10,13,18,19,20,21,22,23,25,28,29,31,33,35)] + 1

Data[,-1] <- round(Data[,-1])
Data[,-1][Data[,-1] < 1] <- 1
Data[,-1][Data[,-1] > 9] <- 9

#look at the output;
#each participant asked to rate the full set of 36 - this is required if you are doing this 
str(Data)
Data


########################################################################
##### STEP 3: ESTIMATING MODEL COEFFICIENTS (PART WORTHS) 
########################################################################

### Estimating part-worth model (i.e. linear regression for each of the 89 participants)

### Estimating the Part-Worth Models
# Merging FracDesign and Data
#install.packages("data.table")
library(data.table)

Data$Participant <- NULL
Data <- transpose(Data)
rownames(Data) <- c(1:36)
Conjoint <- cbind(FracDesign, Data)

Conjoint

# Compute linear regression for each person 
#install.packages("rlist")
library(rlist)
Regressions <- list()

for (person in 8:ncol(Conjoint)) {
  model <- lm(Conjoint[,person]~ factor(Brand) + 
                factor(Cores) + 
                factor(RAM) + 
                factor(HardDrive) + 
                factor(DSize) + 
                factor(DQuality) + 
                factor(TouchScreen) , data =Conjoint)
  Regressions <- list.append(Regressions, model)
}

Regressions

########################################################################
##### STEP 4: AGGREGATING REGRESSION COEFFICIENTS (PART WORTHS)
########################################################################

#extract the coefficients the 89 indidvidual models into a dataframe & combine them;
vars <- c("Intercept",
          rep("Brand",6),
          rep("Cores",2),
          rep("RAM",3),
          rep("HardDrive", 3),
          rep("DSize",3),
          rep("DQuality",2),
          rep("TouchScreen",2))
lvls <- c("Intercept",
          as.character(levels(Conjoint$Brand)),
          as.character(levels(Conjoint$Cores)),
          as.character(levels(Conjoint$RAM)),
          as.character(levels(Conjoint$HardDrive)),
          as.character(levels(Conjoint$DSize)),
          as.character(levels(Conjoint$DQuality)),
          as.character(levels(Conjoint$TouchScreen)))

Results <-data.frame(Variable=vars,Levels=lvls)

for (person in 1:n) {
  c <- as.vector(Regressions[[person]]$coefficients)
  coef <-c(c[1],0,c[2:6],0,c[7],0,c[8:9],0,c[10:11],0,c[12:13],0,c[14],0,c[15])
  Results[,paste("Person",person,sep="")] <-round(coef, digits = 1)
}

str(Results)

#now we have this outputted into a df, we just want to average over all responses;

# Get averages and visualize them for each variable
Results[,"Average"] <-round(rowMeans(Results[,-c(1,2)]),digits = 1)

#preview:
str(Results$Average)


###########################################################################
##### STEP 5: VISUALIZE AGGREGATE PART-WORTHS FOR EACH ATTRIBUTE / LEVEL
###########################################################################

#Now visualize them;
#get coefficients for each Attribute & associated levels and create visualize them
#install.packages("ggplot2")
library(ggplot2)

# Brand
subs <- droplevels(subset(Results,Variable == "Brand")) #subset to get only the coefficients related to Brand, reindex neg #s to 0 to make it easy to read
subs$Levels <- reorder(subs$Levels,subs$Average) #reorder from min to max
if (min(subs$Average)<0) {
  subs$Average = subs$Average + abs(min(subs$Average)) #reindex neg # to 0-index so its easy to interpret
}
gg1 <- ggplot(data=subs,aes(x=Levels, y=Average, group=1)) +
  geom_line() +
  geom_point() +
  ggtitle("Brand")

# Cores
subs <- droplevels(subset(Results,Variable == "Cores"))
subs$Levels <- reorder(subs$Levels,subs$Average)
if (min(subs$Average)<0) {
  subs$Average <- subs$Average + abs(min(subs$Average))
}
gg2 <- ggplot(data=subs,aes(x=Levels, y=Average, group=1)) +
  geom_line() +
  geom_point() +
  ggtitle("Cores")

# DQuality
subs <- droplevels(subset(Results,Variable == "DQuality"))
subs$Levels <- reorder(subs$Levels,subs$Average)
if (min(subs$Average)<0) {
  subs$Average <- subs$Average + abs(min(subs$Average))
}
gg3 <- ggplot(data=subs,aes(x=Levels, y=Average, group=1)) +
  geom_line() +
  geom_point() +
  ggtitle("DQuality")

# DSize
subs <- droplevels(subset(Results,Variable == "DSize"))
subs$Levels <- reorder(subs$Levels,subs$Average)
if (min(subs$Average)<0) {
  subs$Average <- subs$Average + abs(min(subs$Average))
}
gg4 <- ggplot(data=subs,aes(x=Levels, y=Average, group=1)) +
  geom_line() +
  geom_point() +
  ggtitle("DSize")

# HardDrive
subs <- droplevels(subset(Results,Variable == "HardDrive"))
subs$Levels <- reorder(subs$Levels,subs$Average)
if (min(subs$Average)<0) {
  subs$Average <- subs$Average + abs(min(subs$Average))
}
gg5 <- ggplot(data=subs,aes(x=Levels, y=Average, group=1)) +
  geom_line() +
  geom_point() +
  ggtitle("HardDrive")

# RAM
subs <- droplevels(subset(Results,Variable == "RAM"))
subs$Levels <- reorder(subs$Levels,subs$Average)
if (min(subs$Average)<0) {
  subs$Average <- subs$Average + abs(min(subs$Average))
}
gg6 <- ggplot(data=subs,aes(x=Levels, y=Average, group=1)) +
  geom_line() +
  geom_point() +
  ggtitle("RAM")

# TouchScreen
subs <- droplevels(subset(Results,Variable == "TouchScreen"))
subs$Levels <- reorder(subs$Levels,subs$Average)
if (min(subs$Average)<0) {
  subs$Average <- subs$Average + abs(min(subs$Average))
}
gg7 <- ggplot(data=subs,aes(x=Levels, y=Average, group=1)) +
  geom_line() +
  geom_point() +
  ggtitle("TouchScreen")

#plot all these on teh same grid
#install.packages("gridExtra")
library(gridExtra)

grid.arrange(gg1, gg2, gg3, gg4, gg5,gg6, gg7, nrow=4, ncol=2)

#notice the part-worth utilities across these attributes/levels are all comparable
#for ex: biggest thing that would chance utility for students is going from 

#What is the best laptop?

