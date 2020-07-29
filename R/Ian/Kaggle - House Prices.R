#Purpose: Predict house prices as accurately as possible

#Cleansing/Manipulation Packages:
pacman::p_load("dplyr","tidyr","stringr","lubridate","janitor")
#Graphing Packages:
pacman::p_load("ggplot2","esquisse","gridExtra")
#ML Packages:
pacman::p_load("car","MLmetrics","estimatr","fpp","forecast","caret","ROCR","lift","randomForest","glmnet","MASS",
               "e1071","partykit","rpart","ROCR","lift","randomForest","xgboost","tensorflow", "keras")
#Other Packages:
pacman::p_load("sqldf","readxl","geosphere")

#import datasets
houses1 <- read.csv("Queen's MMA\\Kaggle\\House Prices\\train.csv", header=TRUE, sep = ",")
houses2 <- read.csv("Queen's MMA\\Kaggle\\House Prices\\test.csv", header=TRUE, sep = ",")

#----- EDA & PRE-PROCESSING -----

#merge datasets (remember test set starts at id1461 and >)
houses <- bind_rows(houses1, houses2)

str(houses)
summary(houses)

#examine house price distribution
ggplot(houses) +
  aes(x = SalePrice) +
  geom_histogram(bins = 30L, fill = "#0c4c8a") +
  theme_minimal()
  #observations: likely want a log transform here; very right skewed

#MSSubClass (see data dictionary for more detail)
houses$MSSubClass <- as.factor(houses$MSSubClass) #16 layers

  #boxplot
ggplot(houses) +
 aes(x = MSSubClass, y = SalePrice) +
 geom_boxplot(fill = "#0c4c8a") +
 theme_minimal()
  #observations: quite a bit of variation here - should be a useful feature...we also have useful features like stories and date built

#MSZoning
houses$MSZoning <- as.factor(houses$MSZoning)

  #boxplot
ggplot(houses) +
 aes(x = MSZoning, y = SalePrice) +
 geom_boxplot(fill = "#0c4c8a") +
 theme_minimal()

  #replace NA's in testing set...we have to make a decision here - can't impute but will replace w/ RL (most common)
houses <- houses %>%
  mutate(MSZoning = replace_na(MSZoning, 'RL'))
  #observations: again there is some variation here; C worst level; noticed 4 NA's in testing set

#BldgType
houses$BldgType <- as.factor(houses$BldgType)

  #boxplot
ggplot(houses) +
  aes(x = BldgType, y = SalePrice) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()

#LotFrontage
  #address the 486 NA's in LotFrontage by replacing them with the means of bldgtype
    #view mean lotfrontage by bldgtype
aggregate(houses$LotFrontage, by = list(BldgType = houses$BldgType), FUN = mean, na.rm = TRUE)

ggplot(houses) +
 aes(x = BldgType, y = LotFrontage) +
 geom_boxplot(fill = "#0c4c8a") +
 theme_minimal()

    #replace LotFrontage accordingly
houses <- houses %>%
  mutate(LotFrontage = if_else(is.na(LotFrontage) & BldgType == '1Fam', as.integer(74), LotFrontage),
         LotFrontage = if_else(is.na(LotFrontage) & BldgType == '2fmCon', as.integer(68), LotFrontage),
         LotFrontage = if_else(is.na(LotFrontage) & BldgType == 'Duplex', as.integer(71), LotFrontage),
         LotFrontage = if_else(is.na(LotFrontage) & BldgType == 'Twnhs', as.integer(24), LotFrontage),
         LotFrontage = if_else(is.na(LotFrontage) & BldgType == 'TwnhsE', as.integer(42), LotFrontage))

  #scatterplot
ggplot(houses) +
 aes(x = LotFrontage, y = SalePrice) +
 geom_point(size = 1L, colour = "#0c4c8a") +
 geom_smooth(span = 0.75) +
 theme_minimal()

#LotArea
ggplot(houses) +
 aes(x = LotArea, y = SalePrice) +
 geom_point(size = 1L, colour = "#0c4c8a") +
 geom_smooth(span = 0.75) +
 theme_minimal()
  #observations: notice positive relationship w/ both lotfrontage and lotarea; a few extreme outliers w/ lot area

#Street
houses$Street <- as.factor(houses$Street)

  #boxplot
ggplot(houses) +
  aes(x = Street, y = SalePrice) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()
  #observations: Paved looks like it's associated with slightly higher price (although only 12 gravel records)

#Alley
houses$Alley <- as.factor(houses$Alley)

  #fix NA's...note that per data dictionary NA's here mean that there is no alley
houses <- houses %>%
  mutate(Alley = if_else(is.na(Alley), 'None', as.character(Alley)))
houses$Alley <- as.factor(houses$Alley)

  #boxplot
ggplot(houses) +
  aes(x = Alley, y = SalePrice) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()
  #observations: Again, paved looks to be better than gravel, but big NA problem here

#LotShape
houses$LotShape <- as.factor(houses$LotShape)

  #boxplot
ggplot(houses) +
  aes(x = LotShape, y = SalePrice) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()
  #observations: no missing data; irregular lot shapes appear to be associated with higher prices

#LandCountour
houses$LandContour <- as.factor(houses$LandContour)

  #boxplot
ggplot(houses) +
  aes(x = LandContour, y = SalePrice) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()
  #observations: Banked houses appear to be associated with lower prices; hillside with higher; no missing data

#Utilities
houses$Utilities <- as.factor(houses$Utilities)

  #replace NA's with most common (all utilities)
houses <- houses %>%
  mutate(Utilities = if_else(is.na(Utilities), 'AllPub', as.character(Utilities)))
houses$Utilities <- as.factor(houses$Utilities)

  #boxplot: 
ggplot(houses) +
  aes(x = Utilities, y = SalePrice) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()
  #observations: Practically all records have all utilities; 2 NA's and 1 

#LotConfig
houses$LotConfig <- as.factor(houses$LotConfig)

  #boxplot
ggplot(houses) +
  aes(x = LotConfig, y = SalePrice) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()
  #observations: No missing data, eh bit of variation here

#LandSlope
houses$LandSlope <- as.factor(houses$LandSlope)

  #boxplot
ggplot(houses) +
  aes(x = LandSlope, y = SalePrice) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()
  #observations: No missing data; the slopier the property the more expensive

#Neighborhood
houses$Neighborhood <- as.factor(houses$Neighborhood)

  #boxplot
ggplot(houses) +
  aes(x = Neighborhood, y = SalePrice) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()
  #observations: No missing data; NoRidge, NridgHt, and StoneBr look to be the most expensive neighbourhoods

#Conditions
houses$Condition1 <- as.factor(houses$Condition1)
houses$Condition2 <- as.factor(houses$Condition2)

  #boxplots
ggplot(houses) +
  aes(x = Condition1, y = SalePrice) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()
ggplot(houses) +
  aes(x = Condition2, y = SalePrice) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()
  #observations: definitely some variation here; note that some levels will be combined into other (especially condition2)

#HouseStyle
houses$HouseStyle <- as.factor(houses$HouseStyle)

  #boxplot
ggplot(houses) +
  aes(x = HouseStyle, y = SalePrice) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()
  #observations: No missing data, typically higher price as stories increase

#OverallQual
  #scatterplot
ggplot(houses) +
  aes(x = OverallQual, y = SalePrice) +
  geom_point(size = 1L, colour = "#0c4c8a") +
  geom_smooth(span = 0.75) +
  theme_minimal()
  #observations: Likert scale, clearly a positive relationship

#OverallCond
  #scatterplot
ggplot(houses) +
  aes(x = OverallCond, y = SalePrice) +
  geom_point(size = 1L, colour = "#0c4c8a") +
  geom_smooth(span = 0.75) +
  theme_minimal()
  #observations: Likert scale, also appears to have a positive relationship, though not as clear as overall quality

#YearBuilt
  #scatterplot
ggplot(houses) +
  aes(x = YearBuilt, y = SalePrice) +
  geom_point(size = 1L, colour = "#0c4c8a") +
  geom_smooth(span = 0.75) +
  theme_minimal()
  #observations: Houses built more recently are associated with higher prices

#YearRemodAdd
  #scatterplot
ggplot(houses) +
  aes(x = YearRemodAdd, y = SalePrice) +
  geom_point(size = 1L, colour = "#0c4c8a") +
  geom_smooth(span = 0.75) +
  theme_minimal()
  #observations: Recent remodel dates are associated with higher prices

#RoofStyle
houses$RoofStyle <- as.factor(houses$RoofStyle)

  #boxplot
ggplot(houses) +
  aes(x = RoofStyle, y = SalePrice) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()
  #observations: no missing data; a bit of variation here

#RoofMatl
houses$RoofMatl <- as.factor(houses$RoofMatl)

  #boxplot
ggplot(houses) +
  aes(x = RoofMatl, y = SalePrice) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()
  #observations: many levels with very few records, going to be grouped into other eventually

#Exteriors
houses$Exterior1st <- as.factor(houses$Exterior1st)
houses$Exterior2nd <- as.factor(houses$Exterior2nd)

  #replace NA's with VinylSd (most common)
houses <- houses %>%
  mutate(Exterior1st = if_else(is.na(Exterior1st), 'VinylSd', as.character(Exterior1st)),
         Exterior2nd = if_else(is.na(Exterior2nd), 'VinylSd', as.character(Exterior2nd)))
houses$Exterior1st <- as.factor(houses$Exterior1st)
houses$Exterior2nd <- as.factor(houses$Exterior2nd)

  #boxplot
ggplot(houses) +
  aes(x = Exterior1st, y = SalePrice) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()
  #observations: alot of variability here, could be a useful feature

#MasVnrType
houses$MasVnrType <- as.factor(houses$MasVnrType)

  #fix NA's (24) - assign to None
houses <- houses %>%
  mutate(MasVnrType = if_else(is.na(MasVnrType), 'None', as.character(MasVnrType)))
houses$MasVnrType <- as.factor(houses$MasVnrType)

  #boxplot
ggplot(houses) +
  aes(x = MasVnrType, y = SalePrice) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()
  #observations: Stone masonry veneer appears to be associated with higher prices, followed by brick face

#MasVnrArea
  #replace NA's with 0
houses <- houses %>%
  mutate(MasVnrArea = replace_na(MasVnrArea, 0))

  #scatterplot
ggplot(houses) +
  aes(x = MasVnrArea, y = SalePrice) +
  geom_point(size = 1L, colour = "#0c4c8a") +
  geom_smooth(span = 0.75) +
  theme_minimal()
  #observations: clear positive relationship with masonry veneer area

#ExterQual
houses$ExterQual <- as.factor(houses$ExterQual)

  #boxplot
ggplot(houses) +
  aes(x = ExterQual, y = SalePrice) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()
  #observations: very strong feature here

#ExterCond
houses$ExterCond <- as.factor(houses$ExterCond)

  #boxplot
ggplot(houses) +
  aes(x = ExterCond, y = SalePrice) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()
  #observations: again, very strong feature, no NA's

#Foundation
houses$Foundation <- as.factor(houses$Foundation)

  #boxplot
ggplot(houses) +
  aes(x = Foundation, y = SalePrice) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()
  #observations: no missing data; poured concrete appears to be most associated with a higher price

#BsmtQual
houses$BsmtQual <- as.factor(houses$BsmtQual)

  #Assign NA's to none (no basement)
houses <- houses %>%
  mutate(BsmtQual = if_else(is.na(BsmtQual), 'None', as.character(BsmtQual)))
houses$BsmtQual <- as.factor(houses$BsmtQual)

  #boxplot
ggplot(houses) +
  aes(x = BsmtQual, y = SalePrice) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()
  #observations: good feature

#BsmtCond
houses$BsmtCond <- as.factor(houses$BsmtCond)

  #Assign NA's to none (no basement)
houses <- houses %>%
  mutate(BsmtCond = if_else(is.na(BsmtCond), 'None', as.character(BsmtCond)))
houses$BsmtCond <- as.factor(houses$BsmtCond)

  #boxplot
ggplot(houses) +
  aes(x = BsmtCond, y = SalePrice) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()
  #observations: good feature

#BsmtExposure
houses$BsmtExposure <- as.factor(houses$BsmtExposure)

  #Assign NA's to none (no basement)
houses <- houses %>%
  mutate(BsmtExposure = if_else(is.na(BsmtExposure), 'None', as.character(BsmtExposure)))
houses$BsmtExposure <- as.factor(houses$BsmtExposure)

  #boxplot
ggplot(houses) +
  aes(x = BsmtExposure, y = SalePrice) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()
  #observations: good feature, logical

#BsmtFinType1
houses$BsmtFinType1 <- as.factor(houses$BsmtFinType1)

  #Assign NA's to none (no basement)
houses <- houses %>%
  mutate(BsmtFinType1 = if_else(is.na(BsmtFinType1), 'None', as.character(BsmtFinType1)))
houses$BsmtFinType1 <- as.factor(houses$BsmtFinType1)

  #boxplot
ggplot(houses) +
  aes(x = BsmtFinType1, y = SalePrice) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()
  #observations: solid variation

#BsmtFinSF1
  #replace NA with 0
houses <- houses %>%
  mutate(BsmtFinSF1 = replace_na(BsmtFinSF1, 0))

  #scatterplot
ggplot(houses) +
  aes(x = BsmtFinSF1, y = SalePrice) +
  geom_point(size = 1L, colour = "#0c4c8a") +
  geom_smooth(span = 0.75) +
  theme_minimal()
  #observations: definite positive relationship...a couple very large outliers

#BsmtFinType2
houses$BsmtFinType2 <- as.factor(houses$BsmtFinType2)

  #Assign NA's to none (no basement)
houses <- houses %>%
  mutate(BsmtFinType2 = if_else(is.na(BsmtFinType2), 'None', as.character(BsmtFinType2)))
houses$BsmtFinType2 <- as.factor(houses$BsmtFinType2)

  #boxplot
ggplot(houses) +
  aes(x = BsmtFinType2, y = SalePrice) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()
  #observations: meh variance here

#BsmtFinSF2
  #replace NA with 0
houses <- houses %>%
  mutate(BsmtFinSF2 = replace_na(BsmtFinSF2, 0))

  #scatterplot
ggplot(houses) +
  aes(x = BsmtFinSF2, y = SalePrice) +
  geom_point(size = 1L, colour = "#0c4c8a") +
  geom_smooth(span = 0.75) +
  theme_minimal()
  #observations: barely a positive relationship for type 2 basement

#BsmtUnfSF
  #replace NA with 0
houses <- houses %>%
  mutate(BsmtUnfSF = replace_na(BsmtUnfSF, 0))

  #scatterplot
ggplot(houses) +
  aes(x = BsmtUnfSF, y = SalePrice) +
  geom_point(size = 1L, colour = "#0c4c8a") +
  geom_smooth(span = 0.75) +
  theme_minimal()
  #observations: definite positive relationship here (despite basement being unfinished)

#TotalBsmtSF
  #replace NA with 0
houses <- houses %>%
  mutate(TotalBsmtSF = replace_na(TotalBsmtSF, 0))

  #scatterplot
ggplot(houses) +
  aes(x = TotalBsmtSF, y = SalePrice) +
  geom_point(size = 1L, colour = "#0c4c8a") +
  geom_smooth(span = 0.75) +
  theme_minimal()
  #observations: again a clear positive relationship, couple large outliers

#Heating
houses$Heating <- as.factor(houses$Heating)

  #boxplot
ggplot(houses) +
  aes(x = Heating, y = SalePrice) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()
  #observations: Gas >

#HeatingQC
houses$HeatingQC <- as.factor(houses$HeatingQC)

  #boxplot
ggplot(houses) +
  aes(x = HeatingQC, y = SalePrice) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()
  #observations: Gas >

#CentralAir
houses$CentralAir <- as.factor(houses$CentralAir)

  #make dummy
houses <- houses %>%
  mutate(CentralAir = if_else(CentralAir == 'Y',1,0))
houses$CentralAir <- as.factor(houses$CentralAir)

  #boxplot
ggplot(houses) +
  aes(x = CentralAir, y = SalePrice) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()
  #observation: air conditioning associated with higher price

#Electrical
houses$Electrical <- as.factor(houses$Electrical)

  #Assign the NA to SBrkr (most common)
houses <- houses %>%
  mutate(Electrical = if_else(is.na(Electrical), 'SBrkr', as.character(Electrical)))
houses$Electrical <- as.factor(houses$Electrical)

  #boxplot
ggplot(houses) +
  aes(x = Electrical, y = SalePrice) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()
  #observations: standard electric is associated with higher price (likely also akin to newer houses)

#X1stFlrSF
  #scatterplot
ggplot(houses) +
  aes(x = X1stFlrSF, y = SalePrice) +
  geom_point(size = 1L, colour = "#0c4c8a") +
  geom_smooth(span = 0.75) +
  theme_minimal()
  #observations: definite positive relationship; no missing data

#X2ndFlrSF
  #scatterplot
ggplot(houses) +
  aes(x = X2ndFlrSF, y = SalePrice) +
  geom_point(size = 1L, colour = "#0c4c8a") +
  geom_smooth(span = 0.75) +
  theme_minimal()
  #observations: definite positive relationship; no missing data

#LowQualFinSF
  #scatterplot
ggplot(houses) +
  aes(x = LowQualFinSF, y = SalePrice) +
  geom_point(size = 1L, colour = "#0c4c8a") +
  geom_smooth(span = 0.75) +
  theme_minimal()
  #observations: not a very clear relationship, mostly zeroes; no missing data

#GrLivArea
  #scatterplot
ggplot(houses) +
  aes(x = GrLivArea, y = SalePrice) +
  geom_point(size = 1L, colour = "#0c4c8a") +
  geom_smooth(span = 0.75) +
  theme_minimal()
  #observations: definite positive relationship here

#BsmtFullBath
  #replace NA's with 0
houses <- houses %>%
  mutate(BsmtFullBath = replace_na(BsmtFullBath, 0))

  #scatterplot
ggplot(houses) +
  aes(x = BsmtFullBath, y = SalePrice) +
  geom_point(size = 1L, colour = "#0c4c8a") +
  geom_smooth(span = 0.75) +
  theme_minimal()
  #observations: logical

#BsmtHalfBath
  #replace NA's with 0
houses <- houses %>%
  mutate(BsmtHalfBath = replace_na(BsmtHalfBath, 0))

  #scatterplot
ggplot(houses) +
  aes(x = BsmtHalfBath, y = SalePrice) +
  geom_point(size = 1L, colour = "#0c4c8a") +
  geom_smooth(span = 0.75) +
  theme_minimal()
  #observations: logical

#FullBath
  #scatterplot
ggplot(houses) +
  aes(x = FullBath, y = SalePrice) +
  geom_point(size = 1L, colour = "#0c4c8a") +
  geom_smooth(span = 0.75) +
  theme_minimal()
  #observations: definite positive relationship

#HalfBath
  #scatterplot
ggplot(houses) +
  aes(x = HalfBath, y = SalePrice) +
  geom_point(size = 1L, colour = "#0c4c8a") +
  geom_smooth(span = 0.75) +
  theme_minimal()
  #observations: definite positive relationship

#BedroomAbvGr
  #scatterplot
ggplot(houses) +
  aes(x = BedroomAbvGr, y = SalePrice) +
  geom_point(size = 1L, colour = "#0c4c8a") +
  geom_smooth(span = 0.75) +
  theme_minimal()
  #observations: definite positive relationship

#KitchenAbvGr
  #scatterplot
ggplot(houses) +
  aes(x = KitchenAbvGr, y = SalePrice) +
  geom_point(size = 1L, colour = "#0c4c8a") +
  geom_smooth(span = 0.75) +
  theme_minimal()
  #observations: kind of suprising?

#KitchenQual
houses$KitchenQual <- as.factor(houses$KitchenQual)

  #assign NA to TA (most common)
houses <- houses %>%
  mutate(KitchenQual = if_else(is.na(KitchenQual), 'TA', as.character(KitchenQual)))
houses$KitchenQual <- as.factor(houses$KitchenQual)

  #boxplot
ggplot(houses) +
  aes(x = KitchenQual, y = SalePrice) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()
  #observation: great feature, logical relationship

#TotRmsAbvGrd
  #scatterplot
ggplot(houses) +
  aes(x = TotRmsAbvGrd, y = SalePrice) +
  geom_point(size = 1L, colour = "#0c4c8a") +
  geom_smooth(span = 0.75) +
  theme_minimal()
  #observations: no missing data; clear positive relationship with # rooms in house and sales price

#Functional
houses$Functional <- as.factor(houses$Functional)

  #assign NA's to Typ
houses <- houses %>%
  mutate(Functional = if_else(is.na(Functional), 'Typ', as.character(Functional)))
houses$Functional <- as.factor(houses$Functional)

  #boxplot
ggplot(houses) +
  aes(x = Functional, y = SalePrice) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()
  #observations: ehh variability

#Fireplaces
  #scatterplot
ggplot(houses) +
  aes(x = Fireplaces, y = SalePrice) +
  geom_point(size = 1L, colour = "#0c4c8a") +
  geom_smooth(span = 0.75) +
  theme_minimal()
  #observations: no missing data; clear positive relationship

#FireplaceQu
houses$FireplaceQu <- as.factor(houses$FireplaceQu)

  #assign NA's to none (i.e., no fireplace)
houses <- houses %>%
  mutate(FireplaceQu = if_else(is.na(FireplaceQu), 'None', as.character(FireplaceQu)))
houses$FireplaceQu <- as.factor(houses$FireplaceQu)

  #boxplot
ggplot(houses) +
  aes(x = FireplaceQu, y = SalePrice) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()
  #observations: typically some sort of fireplace beats no fireplace (i.e., higher price)

#GarageType
houses$GarageType <- as.factor(houses$GarageType)

  #assign NA's to None (i.e., no garage)
houses <- houses %>%
  mutate(GarageType = if_else(is.na(GarageType), 'None', as.character(GarageType)))
houses$GarageType <- as.factor(houses$GarageType)

  #boxplot
ggplot(houses) +
  aes(x = GarageType, y = SalePrice) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()
  #observations: Having a garage is associated with a higher price, builtin garage the highest price

#GarageYrBlt
  #impute NA's with YearRemodAdd
houses <- houses %>%
  mutate(GarageYrBlt = if_else(is.na(GarageYrBlt), YearRemodAdd, GarageYrBlt))

  #scatterplot
ggplot(houses) +
  aes(x = GarageYrBlt, y = SalePrice) +
  geom_point(size = 1L, colour = "#0c4c8a") +
  geom_smooth(span = 0.75) +
  theme_minimal()
  #observations: clear positive relationship, very random error in 2207 (test set)?

#GarageFinish
houses$GarageFinish <- as.factor(houses$GarageFinish)

  #assign NA's to None (i.e., no garage)
houses <- houses %>%
  mutate(GarageFinish = if_else(is.na(GarageFinish), 'None', as.character(GarageFinish)))
houses$GarageFinish <- as.factor(houses$GarageFinish)

  #boxplot
ggplot(houses) +
  aes(x = GarageFinish, y = SalePrice) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()
  #observations: Having a garage is associated with a higher price, finished garage the highest price

#GarageCars
  #replace NA with median of 2
houses <- houses %>%
  mutate(GarageCars = replace_na(GarageCars, 2))

  #scatterplot
ggplot(houses) +
  aes(x = GarageCars, y = SalePrice) +
  geom_point(size = 1L, colour = "#0c4c8a") +
  geom_smooth(span = 0.75) +
  theme_minimal()
  #observations: clear positive relationship

#GarageArea
  #replace NA with mean of 472.9
houses <- houses %>%
  mutate(GarageArea = replace_na(GarageArea, 472.9))

  #scatterplot
ggplot(houses) +
  aes(x = GarageArea, y = SalePrice) +
  geom_point(size = 1L, colour = "#0c4c8a") +
  geom_smooth(span = 0.75) +
  theme_minimal()
  #observations: clear positive relationship

#GarageQual
houses$GarageQual <- as.factor(houses$GarageQual)

  #assign NA's to None (i.e., no garage)
houses <- houses %>%
  mutate(GarageQual = if_else(is.na(GarageQual), 'None', as.character(GarageQual)))
houses$GarageQual <- as.factor(houses$GarageQual)

  #boxplot
ggplot(houses) +
  aes(x = GarageQual, y = SalePrice) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()
  #observations: Having a garage is associated with a higher price, levels are logical

#GarageCond
houses$GarageCond <- as.factor(houses$GarageCond)

  #assign NA's to None (i.e., no garage)
houses <- houses %>%
  mutate(GarageCond = if_else(is.na(GarageCond), 'None', as.character(GarageCond)))
houses$GarageCond <- as.factor(houses$GarageCond)

  #boxplot
ggplot(houses) +
  aes(x = GarageCond, y = SalePrice) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()
  #observations: Having a garage is associated with a higher price, levels are logical

#PavedDrive
houses$PavedDrive <- as.factor(houses$PavedDrive)

  #boxplot
ggplot(houses) +
  aes(x = PavedDrive, y = SalePrice) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()
  #observations: no missing data, paved driveways are associated with higher prices

#WoodDeckSF
  #scatterplot
ggplot(houses) +
  aes(x = WoodDeckSF, y = SalePrice) +
  geom_point(size = 1L, colour = "#0c4c8a") +
  geom_smooth(span = 0.75) +
  theme_minimal()
  #observations: slightly positive relationship

#OpenPorchSF
  #scatterplot
ggplot(houses) +
  aes(x = OpenPorchSF, y = SalePrice) +
  geom_point(size = 1L, colour = "#0c4c8a") +
  geom_smooth(span = 0.75) +
  theme_minimal()
  #observations: tough to distinguish here if there's a positive relationship, starts off, then levels out

#EnclosedPorch
  #scatterplot
ggplot(houses) +
  aes(x = EnclosedPorch, y = SalePrice) +
  geom_point(size = 1L, colour = "#0c4c8a") +
  geom_smooth(span = 0.75) +
  theme_minimal()
  #observations: positive relationship

#3SsnPorch
  #scatterplot
ggplot(houses) +
  aes(x = X3SsnPorch, y = SalePrice) +
  geom_point(size = 1L, colour = "#0c4c8a") +
  geom_smooth(span = 0.75) +
  theme_minimal()
  #observations: difficult to distinguish, minimal correlation?

#ScreenPorch
  #scatterplot
ggplot(houses) +
  aes(x = ScreenPorch, y = SalePrice) +
  geom_point(size = 1L, colour = "#0c4c8a") +
  geom_smooth(span = 0.75) +
  theme_minimal()
  #observations: slight positive relationship here

#PoolArea
  #scatterplot
ggplot(houses) +
  aes(x = PoolArea, y = SalePrice) +
  geom_point(size = 1L, colour = "#0c4c8a") +
  geom_smooth(span = 0.75) +
  theme_minimal()
  #observations: tough to distinguish

#PoolQC
houses$PoolQC <- as.factor(houses$PoolQC)

  #assign NA's (nearly all records) to None (i.e., no pool)
houses <- houses %>%
  mutate(PoolQC = if_else(is.na(PoolQC), 'None', as.character(PoolQC)))
houses$PoolQC <- as.factor(houses$PoolQC)

  #boxplot
ggplot(houses) +
  aes(x = PoolQC, y = SalePrice) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()
  #observations: having a pool is associated with higher sales price

#Fence
houses$Fence <- as.factor(houses$Fence)

  #assign NA's (nearly all records) to None (i.e., no fence)
houses <- houses %>%
  mutate(Fence = if_else(is.na(Fence), 'None', as.character(Fence)))
houses$Fence <- as.factor(houses$Fence)

  #boxplot
ggplot(houses) +
  aes(x = Fence, y = SalePrice) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()
  #observations: doesn't seem too significant

#MiscFeature
houses$MiscFeature <- as.factor(houses$MiscFeature)

  #assign NA's (nearly all records) to None (i.e., no fence)
houses <- houses %>%
  mutate(MiscFeature = if_else(is.na(MiscFeature), 'None', as.character(MiscFeature)))
houses$MiscFeature <- as.factor(houses$MiscFeature)

  #boxplot
ggplot(houses) +
  aes(x = MiscFeature, y = SalePrice) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()
  #observations: doesn't seem too significant

#MiscVal
  #scatterplot
ggplot(houses) +
  aes(x = MiscVal, y = SalePrice) +
  geom_point(size = 1L, colour = "#0c4c8a") +
  geom_smooth(span = 0.75) +
  theme_minimal()
  #observations: tough to interpret, not many misc features

#MoSold
houses$MoSold <- as.factor(houses$MoSold)

  #boxplot
ggplot(houses) +
  aes(x = MoSold, y = SalePrice) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()
  #observations: month sold doesn't appear to hold significance in terms of house price

#YrSold
  #scatterplot
ggplot(houses) +
  aes(x = YrSold, y = SalePrice) +
  geom_point(size = 1L, colour = "#0c4c8a") +
  geom_smooth(span = 0.75) +
  theme_minimal()
  #observations: little variation here, all within 5 year span

#SaleType
houses$SaleType <- as.factor(houses$SaleType)

  #assign NA to WD (most common)
houses <- houses %>%
  mutate(SaleType = if_else(is.na(SaleType), 'WD', as.character(SaleType)))
houses$SaleType <- as.factor(houses$SaleType)

  #boxplot
ggplot(houses) +
  aes(x = SaleType, y = SalePrice) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()
  #observations: quite a bit of variation here; new (just constructed and sold) seem to be the highest priced

#SaleCondition
houses$SaleCondition <- as.factor(houses$SaleCondition)

  #boxplot
ggplot(houses) +
  aes(x = SaleCondition, y = SalePrice) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()
  #observations: no missing data; partial surpisingly high

#combine low frequency factors
combinerarecategories<-function(data_frame,mincount){ 
  for (i in 1 : ncol(data_frame)){
    a<-data_frame[,i]
    replace <- names(which(table(a) < mincount))
    levels(a)[levels(a) %in% replace] <-paste("Other",colnames(data_frame)[i],sep=".")
    data_frame[,i]<-a }
  return(data_frame) }

#Apply combinerarecategories function to the data and then split it into testing and training data.

houses<-combinerarecategories(houses,10) #combine categories with <10 values in df into "Other"
#should change MSSubClass (Othersum=7), Utilities (Othersum=1), Condition1 (Othersum=15), Condition2 (Othersum=17),
#HouseStyle (Othersum=8), RoofStyle (Othersum=5), RoofMat1 (Othersum=20), Exterior1st (Othersum=13),
#Exterior2nd (Othersum=14), ExterCond (Othersum=3), Foundation (Othersum=5), BsmtCond (Othersum=5), heating (Othersum=18),
#HeatingQC (Othersum=3), Functional (Othersum=11), GarageQual (Othersum=8), GarageCond (Othersum=3), PoolQC (Othersum=10),
#MiscFeature (Othersum=10), SaleType (Othersum=29)

#----- FEATURE ENGINEERING -----

##NOTE: upon investigation, these didn't help (will comment out)

#Bathrooms by Sqfeet
houses <- houses %>%
  mutate(FullBath_per_Area = FullBath / GrLivArea)

  #scatterplot
#ggplot(houses) +
#  aes(x = FullBath_per_Area, y = SalePrice) +
#  geom_point(size = 1L, colour = "#0c4c8a") +
#  geom_smooth(span = 0.75) +
#  theme_minimal()

#Bedrooms by Sqfeet
houses <- houses %>%
  mutate(Bedrooms_per_Area = BedroomAbvGr / GrLivArea)

  #scatterplot
#ggplot(houses) +
#  aes(x = Bedrooms_per_Area, y = SalePrice) +
#  geom_point(size = 1L, colour = "#0c4c8a") +
#  geom_smooth(span = 0.75) +
#  theme_minimal()

#Total rooms by Sqfeet
houses <- houses %>%
  mutate(TotRms_per_Area = TotRmsAbvGrd / GrLivArea)

  #scatterplot
#ggplot(houses) +
#  aes(x = TotRms_per_Area, y = SalePrice) +
#  geom_point(size = 1L, colour = "#0c4c8a") +
#  geom_smooth(span = 0.75) +
#  theme_minimal()

#Year remodelled or built * overall quality rating
houses <- houses %>%
  mutate(QUal_by_Year = YearRemodAdd * OverallQual)

#scatterplot
#ggplot(houses) +
#  aes(x = QUal_by_Year, y = SalePrice) +
#  geom_point(size = 1L, colour = "#0c4c8a") +
#  geom_smooth(span = 0.75) +
#  theme_minimal()

#----- TRAIN/TEST SPLIT -----

#resplit into official train and test
houses1 <- houses %>%
  filter(Id < 1461)
houses2 <- houses %>%
  filter(Id >=1461)

#split the "official training set" into train/test set in order to train our model for deployment
sample <- sample.int(n = nrow(houses1), size = floor(0.7*nrow(houses1)), replace = F)
train <- houses1[sample, ]
test <- houses1[-sample, ]

#----- MULTIPLE REGRESSION MODEL ------

#Try basic linear regression first
mod1 <- lm(log(SalePrice) ~ ., train)

summary(mod1)

test$pred <- predict(mod1, test)

test <- test %>%
  mutate(pred = exp(pred))

RMSLE(test$pred, test$SalePrice)

  #observations: R2 = 0.9142, RMSLE = 0.134 --> projected 2180/5390 on Kaggle

#Try adding in transformations
mod2 <- lm(log(SalePrice) ~ MSSubClass + MSZoning + log(LotFrontage) + log(LotArea) + Street + Alley + LotShape + 
             LandContour + Utilities + LotConfig + LandSlope + Neighborhood + Condition1 + Condition2 + BldgType + 
             HouseStyle + OverallQual + OverallCond + YearBuilt + YearRemodAdd + RoofStyle + RoofMatl + 
             Exterior1st + Exterior2nd + MasVnrType + MasVnrArea + ExterQual + ExterCond + Foundation + 
             BsmtQual + BsmtCond + BsmtExposure + BsmtFinType1 + BsmtFinSF1 + BsmtFinType2 + BsmtFinSF2 + 
             BsmtUnfSF + TotalBsmtSF + Heating + HeatingQC + CentralAir + Electrical + log(X1stFlrSF) + 
             log(X2ndFlrSF+1) + LowQualFinSF + log(GrLivArea) + BsmtFullBath + BsmtHalfBath + FullBath + HalfBath + 
             BedroomAbvGr + KitchenAbvGr + KitchenQual + TotRmsAbvGrd + Functional + Fireplaces + FireplaceQu + 
             GarageType + GarageYrBlt + GarageFinish + GarageCars + log(GarageArea+1) + GarageQual + GarageCond + PavedDrive + 
             WoodDeckSF + OpenPorchSF + EnclosedPorch + X3SsnPorch + ScreenPorch + PoolArea + 
             PoolQC + Fence + MiscFeature + MiscVal + MoSold + YrSold + SaleType + SaleCondition, train)
                                                                                                      
summary(mod2)

test$pred <- predict(mod2, test)

test <- test %>%
  mutate(pred = exp(pred))

RMSLE(test$pred, test$SalePrice)

  #observations: R2 = 0.9227, RMSLE = 0.130 --> projected 1818/5390 on Kaggle

#Try Stepwise AIC regression for optimal feature selection
mod3 = step(lm(log(SalePrice) ~ MSSubClass + MSZoning + log(LotFrontage) + log(LotArea) + Street + Alley + LotShape + 
                 LandContour + Utilities + LotConfig + LandSlope + Neighborhood + Condition1 + Condition2 + BldgType + 
                 HouseStyle + OverallQual + OverallCond + YearBuilt + YearRemodAdd + RoofStyle + RoofMatl + 
                 Exterior1st + Exterior2nd + MasVnrType + MasVnrArea + ExterQual + ExterCond + Foundation + 
                 BsmtQual + BsmtCond + BsmtExposure + BsmtFinType1 + BsmtFinSF1 + BsmtFinType2 + BsmtFinSF2 + 
                 BsmtUnfSF + TotalBsmtSF + Heating + HeatingQC + CentralAir + Electrical + log(X1stFlrSF) + 
                 log(X2ndFlrSF+1) + LowQualFinSF + log(GrLivArea) + BsmtFullBath + BsmtHalfBath + FullBath + HalfBath + 
                 BedroomAbvGr + KitchenAbvGr + KitchenQual + TotRmsAbvGrd + Functional + Fireplaces + FireplaceQu + 
                 GarageType + GarageYrBlt + GarageFinish + GarageCars + log(GarageArea+1) + GarageQual + GarageCond + PavedDrive + 
                 WoodDeckSF + OpenPorchSF + EnclosedPorch + X3SsnPorch + ScreenPorch + PoolArea + 
                 PoolQC + Fence + MiscFeature + MiscVal + MoSold + YrSold + SaleType + SaleCondition, train), direction='both')

summary(mod3)

step_pred <- predict(mod3, test)

test <- test %>%
  mutate(pred = exp(step_pred))

RMSLE(test$pred, test$SalePrice)

  #observations: RMSLE = 0.122 --> projected 1100/5390

#----- RANDOM FOREST MODEL -----

#define random forest model
model_forest <- randomForest(SalePrice ~ ., data=train, 
                             type="regression",
                             importance=TRUE,
                             ntree = 125,         # hyperparameter: number of trees in the forest
                             mtry = 30,           # hyperparameter: number of random columns to grow each tree
                             nodesize = 5,       # hyperparameter: min number of datapoints on the leaf of each tree
                             maxnodes = 250,       # hyperparameter: maximum number of leafs of a tree
                             cutoff = c(0.5, 0.5) # hyperparameter: how the voting works; (0.5, 0.5) means majority vote
) 

#cross validation grid search for mtry
control <- trainControl(method="repeatedcv", number=5, repeats=1, search="grid")
set.seed(1)
tunegrid <- expand.grid(.mtry=c(25:30))
rf_gridsearch <- caret::train(SalePrice ~ ., data=train, method="rf", metric="RMSE", tuneGrid = tunegrid, trControl=control)
print(rf_gridsearch)
plot(rf_gridsearch)

plot(model_forest) # plots error as a function of number of trees in the forest; use print(model_forest) to print the values on the plot
print(model_forest)

#check variable importance
varImpPlot(model_forest) # plots variable importances; use importance(model_forest) to print the values
importance(model_forest)

#finding predicitons: probabilities and classification
forest_predictions<-predict(model_forest,newdata=test,type="response") #Predict quantities

test <- test %>%
  mutate(pred = forest_predictions)

RMSLE(test$pred, test$SalePrice)

  #observations: RMSLE = 0.148

#----- XGBOOST MODEL -----

house_matrix <- model.matrix(SalePrice ~ ., data = houses)[,-1]

  #removing the pred column in test df
test <- test %>%
  dplyr::select(-pred)

x_train <- model.matrix(SalePrice ~ ., data = train)[,-1]
x_test <- model.matrix(SalePrice ~ ., data = test)[,-1]

#grabbing the response var from original dataset
y_train <- train$SalePrice
y_test <- test$SalePrice

#define gradient boosted model
model_XGboost<-xgboost(data = data.matrix(x_train), 
                       label = as.numeric(as.character(y_train)), 
                       eta = 0.05,       # hyperparameter: learning rate  (typically want lower...must increase rounds)
                       max_depth = 3,  # hyperparameter: size of a tree in each boosting iteration (lower to help prevent overfitting)
                       nround=600,       # hyperparameter: number of boosting iterations
                       min_child_weight=4,
                       subsample=0.7,
                       colsample_bytree=0.6,
                       objective = "reg:linear"
)
model_XGboost

#Predict classification (for confusion matrix)
XGboost_prediction<-predict(model_XGboost,newdata=x_test, type="response")

test <- test %>%
  mutate(pred = XGboost_prediction)

RMSLE(test$pred, test$SalePrice)

  #observations: RMSLE = 0.120 --> 875/5390

#try tuning XGB parameters
default_param<-list(
  objective = "reg:linear",
  eta=0.05, #default = 0.3
  max_depth=3, #default=6
  nround=400
)

xgbcv <- xgb.cv(params = default_param, data = x_train, nrounds = 400, nfold = 5, showsd = T, 
                 stratified = T, print_every_n = 40, early_stopping_rounds = 10, maximize = F)

#----- SVM MODEL -----

#define SVM model
model_svm <- svm(SalePrice ~., data=train, probability=TRUE, gamma=0.001, cost=10)
summary(model_svm)
#observations: # of support vectors = 8262

#predict classification
svm_predictions<-predict(model_svm,newdata=test, probability=TRUE, type="response")

test <- test %>%
  mutate(pred = svm_predictions)

RMSLE(test$pred, test$SalePrice)

  #observations: RMSLE = 0.118 --> 660/5390 on Kaggle

#----- CREATE PREDICTIONS -----

#generate and export predictions
pred <- predict(mod3, houses2)

houses2_pred <- houses2 %>%
  mutate(SalePrice = exp(pred)) %>%
  dplyr::select(Id, SalePrice)

#RF
houses2_pred <- houses2 %>%
  mutate(SalePrice = pred) %>%
  dplyr::select(Id, SalePrice)

#XGB
  #need to replace the null SalePrice column with values (doesn't rly matter as we are removing it)
houses2 <- houses2 %>%
  mutate(SalePrice = if_else(is.na(SalePrice), 0,0))

houses2_xgb <- model.matrix(SalePrice ~ ., data = houses2)[,-1]

XGboost_prediction2 <- predict(model_XGboost,newdata=houses2_xgb, type="response")

houses2_pred <- houses2 %>%
  mutate(SalePrice = XGboost_prediction2) %>%
  dplyr::select(Id, SalePrice)

#SVM
houses2 <- houses2 %>%
  dplyr::select(-SalePrice)

svm_predictions2<-predict(model_svm,newdata=houses2, probability=TRUE, type="response")

houses2_pred <- houses2 %>%
  mutate(SalePrice = svm_predictions2) %>%
  dplyr::select(Id, SalePrice)

#export prediction results
write.csv(houses2_pred, file = paste0("Queen's MMA\\Kaggle\\House Prices\\SVM4_predictions.csv"), row.names = FALSE, na = "")


