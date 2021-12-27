#Purpose: Explore hypothesis testing in R

#Cleansing/Manipulation Packages:
pacman::p_load("dplyr","tidyr","stringr","lubridate","janitor")
#Graphing Packages:
pacman::p_load("ggplot2","esquisse","gridExtra")
#ML Packages:
pacman::p_load("car","MLmetrics","estimatr","fpp","forecast","caret","ROCR","lift","randomForest","glmnet","MASS",
               "e1071","partykit","rpart","ROCR","lift","randomForest","xgboost","tensorflow", "keras")
#Other Packages:
pacman::p_load("sqldf","readxl","geosphere")

#----- DATA IMPORT -----
ice_cream_data <- read_xlsx("Projects/Hypothesis Testing Data.xlsx", sheet = 1)

montreal_sales <- read_xlsx("Projects/Hypothesis Testing Data.xlsx", sheet = 2)

ab_prop_data <- read.csv("Projects/ab_test_proportions.csv")

#----- A) ICE CREAM DATA (two-sided two-sample t-test & paired t-test) -----

#Case: Data contains ice cream sales for chocolate and vanilla ice cream in Kingston. We think vanilla sells better 
  # than chocolate and want to test whether or not they sell the same

## 1) State hypotheses: <We should change our ordering practice> iff <the ice cream sales quantities for vanilla and 
  # chocolate are not the same>

  # Null Hypothesis (Ho): Mu_V = Mu_C

  # Alternative Hypothesis (Ha): Mu_V != Mu_C

## 2) Referring to the flow chart (see one note) the first thing we notice is that we're not dealing with proportions
  # (i.e., go right). We are dealing with a sample size of exactly 30, therefore by the CLT we can assume a normal
  # distribution. Lastly, note that we do not know the population variance, therefore we should go the t-test route.

## 3) Let's first make an assumption that the vanilla and chocolate sales are statistically independent. This infers
  # a two-sample t-test

t.test(x = ice_cream_data$Vanilla, 
       y = ice_cream_data$Chocolate, 
       alternative = "two.sided", #refer to hypothesis - testing for any difference or that one > than the other?
       paired = FALSE, #whether we want a paired t-test or not (FALSE when two samples are independent)
       var.equal = FALSE, #we'll assume unequal variance (typically doesn't make a big difference whether TRUE or FALSE)
       conf.level = 0.95 #we'll set to the default but if we wanted a different alpha we'd adjust here
       )

  #CONCLUSIONS: At a critical value of 0.05, we fail to reject the null hypothesis (pval = 0.1856). This would suggest
  # that there is no significant difference in ice cream sales between the two flavors.

## 4) Let's now assume that the data are in matched pairs by day. This infers a paired t-test

t.test(x = ice_cream_data$Vanilla,
       y = ice_cream_data$Chocolate,
       alternative = "two.sided",
       paired = TRUE,
       var.equal = FALSE,
       conf.level = 0.95)

  #CONCLUSIONS: At a critical value of 0.05, we are just able to reject the null hypothesis (pval = 0.0439). This
  # suggests that there is a statistically significant difference in sales between the two flavors.

#----- B) MONTREAL SALES (one-sided two-sample t-test) -----

#Case: Greener by design has been working in Montreal performing environmental audits for several years. Lately the 
  # owner has been concerned that the sales are falling and in particular that average daily revenue from audits in 
  # July are lower than those in June.  If sales are falling, she plans to abandon the market and will stop investing
  # in further advertising. Let's also assume an alpha of 0.10 - management is already wanting to pull out.

## 1) State hypotheses: <She should abandon the market> iff <the July sales are lower than June sales>

  # Null Hypothesis (Ho): Mu_July >= Mu_June

  # Alternative Hypothesis (Ha): Mu_July < Mu_June

## 2) Referring to the flow chart (see one note) the first thing we notice is that we're not dealing with proportions
  # (i.e., go right). We are dealing with a sample size of 21 which isn't enough to assume normality by the CLT.
  # Lastly, note that we do not know the population variance, therefore we should go the t-test route.

## 3) Sales data from June vs July feels statistically independent. This suggests using a two-sample t-test.
  # On the other hand, if we were comparing sales data from two different sites but for the exact same sales dates,
  # we would have been better off going with pairs (this is assumption we make with ice cream data in 1) above).

t.test(x = montreal_sales$July,
       y = montreal_sales$June,
       alternative = "less", # note that this is a one-sided test - specifically if July sales < June sales
       paired = FALSE, #data from the two samples are independent
       var.equal = FALSE,
       conf.level = 0.90 #this will give 90% CI - recall we are assuming alpha of 0.10 (won't change pvalue)
       )

  #CONCLUSIONS: At a critical value of 0.10, we are just barely able to reject the null hypothesis (pval = 0.0993).
  # This suggests that the July sales are significantly lower enough than the June sales to warrant the company to
  # abandon the market and stop investing in further advertising.

#----- C) AB PROPORTIONs DATA (two-sided proportions z-test) -----

    # see AB Testing.ipynb
#Case: You work on the product team at a medium-sized online ECOMM business. The UX designer created
  # a new version of the product page hoping that it will lead to a higher conversion rate.
  # Current conversion rate is about 13% on average and the team would consider an increase of 2% a
  # success (i.e., conversion rate up to 15%).

## 1) State hypotheses: <We consider the new design had impact> iff <conversion rate for treatment
  # group is different than control group>

  # Null Hypothesis (Ho): prop_c = prop_t

  # Alternative Hypothesis (Ha): prop_c != prop_t

## 2) Referring to the flow chart (see one note) the first thing we notice is that we're dealing with
  # a Bernoulli distribution since we're evaluating the proportions of people who clicked through and
  # purchased with the new web design vs the old design. We also know that n = 4720 and Pr(conversion)
  # = 0.13 (we'll assume the current conversion rate pre-testing). This easily passes the tests required
  # to assume normality by the CLT (np > 10 and n(1-p) > 10). As a result we can go the Z-test route.

  #create pivot table showing successes & failures (respectively) by group
count_table <- table(ab_prop_data$group, factor(ab_prop_data$converted, levels = c(1,0)))

count_table

prop.test(x = count_table, #table of successes & failures (respectively) by group
          alternative = 'two.sided', #refer to hypothesis (Ha is != in this case)
          conf.level = 0.95,
          correct = FALSE) #typically set to false?

  #CONCLUSIONS: At a critical value of 0.05, we fail to reject the null hypothesis (pval = 0.4278). We conclude that
  # the new web page design shows no significant difference in performance against the old design.



