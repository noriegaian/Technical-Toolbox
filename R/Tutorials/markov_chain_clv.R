library(markovchain)
library(dplyr)
library(expm)
library(diagram)
library(pracma)

#install these packages;
#install.packages("expm")
#install.packages("markovchain")
#install.packages("diagram")
#install.packages("pracma")


#current state as rows, future state as columns

#p1=0.8, p2=0.4, p3=0.1
stateNames <- c("Recent","TwoPlusMonths","ThreeMonths", "FourPlus")
tMatrix <- matrix(c(0.8, 0.2, 0, 0,
               0.6, 0, 0.4, 0,
               0.5, 0, 0, 0.5, 
               0.25, 0, 0, .75),
             nrow=4, byrow=TRUE)
row.names(tMatrix) <- stateNames;
colnames(tMatrix) <- stateNames;
tMatrix;

#fit the markov chain
regMC <- new("markovchain", states = stateNames,
                         transitionMatrix = tMatrix,
                         name = "Baseline")

#visualze what this looks like...
plot(regMC, node.size = 10)

#we also capture value in each state (i.e. something we do seperately from a RFM perspective)
Value_Matrix = c(40,30,10,3) #in dollars

#in the longer run as T--> infinity 
steadyStates(regMC) #give the steadyStates function our markov chain
#observations: 69% in state 1 (recent), 14% in two+ months, etc.

#longterm (as t goes to infinity) CLV
steadyStates(regMC)*Value_Matrix
sum(steadyStates(regMC)*Value_Matrix) #observations: worth $32

#multiply by value in each state (i.e. something we do seperately from a RFM perspective)

#12 months value
CLV_1yr=regMC^12
CLV_1yr[1]
#ovservations: close to steady state...

#from a single campaign perspective you can think of it as follows
#collect historical campaign resonse data and get campaign-specific transition matrix
stateNames <- c("Recent","TwoPlusMonths","ThreePlusMonths", "Churn") #we assume 4+ months as churn
#for people who recently purchased increase chance of ordering again from (.8 to .82)
#for people who ordered 2 months ago 0.60 to 0.65, and 3+/4+ months ago did not increase at all
Campaign <- matrix(c(0.82, 0.18, 0, 0,
                    0.65, 0, 0.35, 0,
                    0.5, 0, 0, 0.5, 
                    0.25, 0, 0, .75),
                  nrow=4, byrow=TRUE) #notice the slight change in effects compared to the initial matrix above
row.names(Campaign) <- stateNames;
colnames(Campaign) <- stateNames;
Campaign;
#observations: this is the transition probability matrix

#fit the markov chain
CampaignMC <- new("markovchain", states = stateNames,
             transitionMatrix = Campaign,
             name = "M")

#campaign value matrix - ex: costs us $1 to send mail, 10% off offer, you either comeback and spend $100 with us next month
Value_Matrix = c(40,30,10,3) #original value matrix
Responders = (Value_Matrix[1]*0.8)-1 #if reponse --> then State = Recent Purchaser
Non_Responders = -2 #if you dont response we just lost $2 & your state is the same

CampaignValue_Matrix = c(35,29,9,2) #do the math (i.e., 40*10%-1 where 40 is from value_matrix, 10% is offer, 1 is cost to send mail)
CampaignValue_Matrix

#campaign CLV
steadyStates(CampaignMC)*CampaignValue_Matrix

CampaignMC*regMC^11*CampaignValue_Matrix #campaign

regMC^12*Value_Matrix #non-treated group

Campaign_CLV = sum(steadyStates(CampaignMC)*CampaignValue_Matrix)
Campaign_CLV



#now $52 for each customer we targeted vs $32 before
#what if we gave 20% off & response was the same?

CampaignValue_Matrix2 = c(31,29,9,1)
CampaignTargetingMatrix=c(80,100,200,50)

CampaignTargetingMatrix = matrix( c(80,100,200,50), nrow=1, ncol=4) 

sum(CampaignTargetingMatrix*steadyStates(CampaignMC)*Campaign_Value_Matrix)
#=4562.6

#if we did nothing
sum(CampaignTargetingMatrix*steadyStates(regMC)*Value_Matrix)
#2766, so we generated $4562 in lifetime value (not adjusting for LTV)



