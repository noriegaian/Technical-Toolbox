
if("pacman" %in% rownames(installed.packages()) == FALSE) {install.packages("pacman")} # Check if you have universal installer package, install if not

pacman::p_load("caret","ROCR","lift","glmnet","MASS","e1071") #Check, install/call any of these packages

STCdata_A<-read.csv(file.choose(), na.strings=c(""," ","NA"), header=TRUE) # Load the datafile (numerical dates csv)

str(STCdata_A) # See if some data types were misclassified when importing data from CSV

# Fixing incorrectly classified data types:
STCdata_A$From.Grade <- as.factor(STCdata_A$From.Grade) #grade 8 vs grade 4 is a label, not numeric
STCdata_A$To.Grade <- as.factor(STCdata_A$To.Grade)
STCdata_A$Is.Non.Annual. <- as.factor(STCdata_A$Is.Non.Annual.)
STCdata_A$Days <- as.factor(STCdata_A$Days)
STCdata_A$Group.State <- as.factor(STCdata_A$Group.State)
#STCdata_A$Departure.Date <- as.Date(STCdata_A$Departure.Date, origin="1899-12-30")
#STCdata_A$Return.Date <- as.Date(STCdata_A$Return.Date, origin="1899-12-30")
#STCdata_A$Deposit.Date <- as.Date(STCdata_A$Deposit.Date, origin="1899-12-30")
#STCdata_A$Early.RPL <- as.Date(STCdata_A$Early.RPL, origin="1899-12-30")
#STCdata_A$Latest.RPL <- as.Date(STCdata_A$Latest.RPL, origin="1899-12-30")
#STCdata_A$Initial.System.Date <- as.Date(STCdata_A$Initial.System.Date, origin="1899-12-30")
STCdata_A$CRM.Segment <- as.factor(STCdata_A$CRM.Segment)
STCdata_A$Parent.Meeting.Flag <- as.factor(STCdata_A$Parent.Meeting.Flag)
STCdata_A$MDR.High.Grade <- as.factor(STCdata_A$MDR.High.Grade)
STCdata_A$School.Sponsor <- as.factor(STCdata_A$School.Sponsor)
STCdata_A$NumberOfMeetingswithParents <- as.factor(STCdata_A$NumberOfMeetingswithParents)
STCdata_A$SingleGradeTripFlag <- as.factor(STCdata_A$SingleGradeTripFlag)
#STCdata_A$FirstMeeting <- as.Date(STCdata_A$FirstMeeting, origin="1899-12-30")
#STCdata_A$LastMeeting <- as.Date(STCdata_A$LastMeeting, origin="1899-12-30")
STCdata_A$Retained.in.2012. <- as.factor(STCdata_A$Retained.in.2012.)
STCdata_A$Program.Code <- as.factor(STCdata_A$Program.Code)
STCdata_A$Travel.Type <- as.factor(STCdata_A$Travel.Type)
STCdata_A$Special.Pay <- as.factor(STCdata_A$Special.Pay)
STCdata_A$Poverty.Code <- as.factor(STCdata_A$Poverty.Code)
STCdata_A$Region <- as.factor(STCdata_A$Region)
STCdata_A$School.Type <- as.factor(STCdata_A$School.Type)
STCdata_A$MDR.Low.Grade <- as.factor(STCdata_A$MDR.Low.Grade)
STCdata_A$Income.Level <- as.factor(STCdata_A$Income.Level)
STCdata_A$SPR.Product.Type <- as.factor(STCdata_A$SPR.Product.Type)
STCdata_A$SPR.New.Existing <- as.factor(STCdata_A$SPR.New.Existing)
STCdata_A$SchoolGradeTypeLow <- as.factor(STCdata_A$SchoolGradeTypeLow)
STCdata_A$SchoolGradeTypeHigh <- as.factor(STCdata_A$SchoolGradeTypeHigh)
STCdata_A$SchoolGradeType <- as.factor(STCdata_A$SchoolGradeType)
STCdata_A$DepartureMonth <- as.factor(STCdata_A$DepartureMonth)
STCdata_A$GroupGradeTypeLow <- as.factor(STCdata_A$GroupGradeTypeLow)
STCdata_A$GroupGradeTypeHigh <- as.factor(STCdata_A$GroupGradeTypeHigh)
STCdata_A$GroupGradeType <- as.factor(STCdata_A$GroupGradeType)
STCdata_A$MajorProgramCode <- as.factor(STCdata_A$MajorProgramCode)
STCdata_A$SchoolSizeIndicator <- as.factor(STCdata_A$SchoolSizeIndicator)

# Create a custom function to fix missing values ("NAs") and preserve the NA info as surrogate variables
fixNAs<-function(data_frame){
  # Define reactions to NAs
  integer_reac<-0
  factor_reac<-"FIXED_NA"
  character_reac<-"FIXED_NA"
  date_reac<-as.Date("1900-01-01")
  # Loop through columns in the data frame and depending on which class the variable is, apply the defined reaction and create a surrogate
  
  for (i in 1 : ncol(data_frame)){
    if (class(data_frame[,i]) %in% c("numeric","integer")) {
      if (any(is.na(data_frame[,i]))){
        data_frame[,paste0(colnames(data_frame)[i],"_surrogate")]<-
          as.factor(ifelse(is.na(data_frame[,i]),"1","0"))
        data_frame[is.na(data_frame[,i]),i]<-integer_reac
      }
    } else
      if (class(data_frame[,i]) %in% c("factor")) {
        if (any(is.na(data_frame[,i]))){
          data_frame[,i]<-as.character(data_frame[,i])
          data_frame[,paste0(colnames(data_frame)[i],"_surrogate")]<-
            as.factor(ifelse(is.na(data_frame[,i]),"1","0"))
          data_frame[is.na(data_frame[,i]),i]<-factor_reac
          data_frame[,i]<-as.factor(data_frame[,i])
          
        } 
      } else {
        if (class(data_frame[,i]) %in% c("character")) {
          if (any(is.na(data_frame[,i]))){
            data_frame[,paste0(colnames(data_frame)[i],"_surrogate")]<-
              as.factor(ifelse(is.na(data_frame[,i]),"1","0"))
            data_frame[is.na(data_frame[,i]),i]<-character_reac
          }  
        } else {
          if (class(data_frame[,i]) %in% c("Date")) {
            if (any(is.na(data_frame[,i]))){
              data_frame[,paste0(colnames(data_frame)[i],"_surrogate")]<-
                as.factor(ifelse(is.na(data_frame[,i]),"1","0"))
              data_frame[is.na(data_frame[,i]),i]<-date_reac
            }
          }  
        }       
      }
  } 
  return(data_frame) 
}

STCdata_A<-fixNAs(STCdata_A) #Apply fixNAs function to the data to fix missing values
#observation: notice that # of variables went from 56 vars to 74 vars (18 surrogate dummies created)

table(STCdata_A$Group.State)# check for rare categories in group state
#observation: Notice that some states/countries have only a value of 1 (e.g., Cayman Islands and Bermuda)

# Create another a custom function to combine rare categories into "Other."+the name of the original variavle (e.g., Other.State)
# This function has two arguments: the name of the dataframe and the count of observation in a category to define "rare"
# Note that variables need to be of factor type for this function to work
combinerarecategories<-function(data_frame,mincount){ 
  for (i in 1 : ncol(data_frame)){
    a<-data_frame[,i]
    replace <- names(which(table(a) < mincount))
    levels(a)[levels(a) %in% replace] <-paste("Other",colnames(data_frame)[i],sep=".")
    data_frame[,i]<-a }
  return(data_frame) }


#Apply combinerarecategories function to the data and then split it into testing and training data.

STCdata_A<-combinerarecategories(STCdata_A,20) #combine categories with <20 values in STCdata into "Other"

set.seed(77850) #set a random number generation seed to ensure that the split is the same everytime
inTrain <- createDataPartition(y = STCdata_A$Retained.in.2012., #we want our random partition to be stratified - we want the proportion of retainment to be close to the same between the train and test set
                               p = 1888/2389, list = FALSE) #p splits the observations based on ~80/20 ratio
#inTrain vector tells us which rows will go into training (others will go into testing)
training <- STCdata_A[ inTrain,]
testing <- STCdata_A[ -inTrain,]

# Select the variables to be included in the "base-case" model
# First include all variables use glm(Retained.in.2012.~ ., data=training, family="binomial"(link="logit")) Then see which ones have "NA" in coefficients and remove those

model_logistic<-glm(Retained.in.2012.~ Special.Pay + 
                      To.Grade + Group.State + Is.Non.Annual. +
                      Tuition + FRP.Active + FRP.Cancelled + FRP.Take.up.percent. + 
                      Cancelled.Pax + Total.Discount.Pax + Initial.System.Date + 
                      Poverty.Code + CRM.Segment + School.Type + Parent.Meeting.Flag + 
                      MDR.Low.Grade + MDR.High.Grade + Total.School.Enrollment + 
                      EZ.Pay.Take.Up.Rate + School.Sponsor +  
                      SPR.New.Existing + FPP + FirstMeeting + LastMeeting + 
                      DifferenceTraveltoFirstMeeting + DepartureMonth  + MajorProgramCode + SingleGradeTripFlag + 
                      FPP.to.School.enrollment + FPP.to.PAX + SchoolSizeIndicator, data=training, family="binomial"(link="logit"))

summary(model_logistic) 

# to add surrogates paste this to the list of variables; note, it will run quite a bit slower
#Special.Pay_surrogate + Early.RPL_surrogate + Latest.RPL_surrogate + 
#Initial.System.Date_surrogate + CRM.Segment_surrogate + MDR.High.Grade_surrogate + 
#Total.School.Enrollment_surrogate + FirstMeeting_surrogate + 
#LastMeeting_surrogate + DifferenceTraveltoFirstMeeting_surrogate + 
#DifferenceTraveltoLastMeeting_surrogate + FPP.to.School.enrollment_surrogate

##The model clearly has too many variables, most of which are insignificant 

## Stepwise regressions. There are three aproaches to runinng stepwise regressions: backward, forward and "both"
## In either approach we need to specify criterion for inclusion/exclusion. Most common ones: based on information criterion (e.g., AIC) or based on significance  
model_logistic_stepwiseAIC<-stepAIC(model_logistic,direction = c("both"),trace = 1) #AIC stepwise, trace = 1 will display what the model is doing
#notice that the model's progressively get smaller (running faster) and AIC gets closer to 0
summary(model_logistic_stepwiseAIC) 

par(mfrow=c(1,4))
plot(model_logistic_stepwiseAIC) #Error plots: similar nature to lm plots
par(mfrow=c(1,1))
#remember plots 2 and 4 are most important
#Normal Q-Q looks good
#Don't observe any noticeable outliers from res vs lev

###Finding predicitons: probabilities and classification
logistic_probabilities<-predict(model_logistic_stepwiseAIC,newdata=testing,type="response") #Predict probabilities
logistic_classification<-rep("1",500) #create vector of 1's that matches test set length
#Predict classification using 0.6073 threshold. Why 0.6073 - that's the average probability of being retained in
#the data. Can view summary of testing$target for the observed proportion
logistic_classification[logistic_probabilities<0.6073]="0"
logistic_classification<-as.factor(logistic_classification)

###Confusion matrix  
confusionMatrix(logistic_classification,testing$Retained.in.2012.,positive = "1") #Display confusion matrix

####ROC Curve
logistic_ROC_prediction <- prediction(logistic_probabilities, testing$Retained.in.2012.)
logistic_ROC <- performance(logistic_ROC_prediction,"tpr","fpr") #Create ROC curve data
plot(logistic_ROC) #Plot ROC curve

####AUC (area under curve)
auc.tmp <- performance(logistic_ROC_prediction,"auc") #Create AUC data
logistic_auc_testing <- as.numeric(auc.tmp@y.values) #Calculate AUC
logistic_auc_testing #Display AUC value: 90+% - excellent, 80-90% - very good, 70-80% - good, 60-70% - so so, below 60% - not much value

#### Lift chart
plotLift(logistic_probabilities, testing$Retained.in.2012., cumulative = TRUE, n.buckets = 10) # Plot Lift chart

