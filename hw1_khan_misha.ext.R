#Misha Khan
#BANA 288 Predictive Analytics
#Spring 2021

#Homework #1: Introduction to Prediction Modeling

#Load data
options(stringsAsFactors = TRUE)
bank <- read.csv("~/Desktop/SPRING 2021/Predictive/Homework 1/hw1_universal_bank.csv")
names(bank)
str(bank)

################################################################################
#a. Write scripts to display at least 4 graphical statistical analyses
#of the Universal Bank data. Create at least one each: pie chart, 
#bar chart, histogram, and box plots.

#a.1 Pie Chart (Family Size)
table1 <- table(bank$Fam_Size)
slices <- table1
lbls <- c("One Member", "Two Members", "Three Members", "Four Members")
pct <- round(slices/sum(slices)*100)
pct
lbls <- paste(lbls, pct)
lbls <- paste(lbls, "%", sep = "")

pie(slices, 
    labels = lbls, 
    col = rainbow(length(lbls)), 
    main = "Family Size")

#a.2 Bar Chart (Education Level)
table2 <- table(bank$Education)

barplot(table2, 
        ylab = "Education Level", 
        xlab = "Count", 
        horiz = F, 
        col = c("Red","Green","SkyBlue"))

#a.3 Histogram (Age)
Ages <- bank$Age

hist(Ages, 
     main = "Histogram of Customer Ages", 
     col= "Orange")

#a.4 Box Plot (Income)
boxplot(bank$Income)

################################################################################
#b. Create a boxplot of the Average Credit Card Debt conditional on the
#customer’s acceptance of the bank’s offer.  Create a second “conditioned”
#chart of your choice as well.  Hint:  For the boxplot, use the “~” command
#after the variable of interest to link it to the one where the 
#conditioning occurs.

#Box Plot (Average Credit Card Debt vs Accepted Offer)
boxplot(Crdt_Crd_Avg~Acpt_Offer, data = bank, 
        xlab = "Accept Loan Offer (0= No, 1 = Yes)", 
        ylab = "Frequency",
        horizontal = F, 
        col = c("Yellow","Pink"))

#Box Plot (Income vs Education)
boxplot(Income~Education, data = bank, 
        horizontal = T, col = c("Red","Green","SkyBlue"))

################################################################################
#c1. Create a scatter plot with Income on the x-axis and Average Credit 
#Card Debt on the y-axis.  Add the least squares regression line to the plot.  
#In the context of this data set, what information does this chart convey?  

#Scatter Plot (Income vs Avg Credit Card Debt)
plot(bank$Income, bank$Crdt_Crd_Avg, 
     xlab="Income",
     ylab = "Average Credit Card Debt")

abline(lm(bank$Crdt_Crd_Avg ~ bank$Income), col = "red", lwd = 3)
lm(bank$Income ~ bank$Crdt_Crd_Avg)

#Interpretation: The plot of income vs average credit card debt shows several 
#points clustered on the left and scatters as we move to the right. Because of
#the upwards, positive slope of the regression line, we can assume that
#as income increases, credit card average also increases. However, the points
#are not quite linear so further modeling may be necessary.

#c2. Create a second scatter plot with Average Credit Card Debt on the x-axis 
#and Accept Offer on the y-axis.  Add the least squares regression line to 
#the plot.  What is the take-away from this chart?

#Scatter Plot (Avg Credit Card Debt vs Accept Offer)
plot(bank$Crdt_Crd_Avg, bank$Acpt_Offer, 
     xlab="Average Credit Card Debt",
     ylab = "Accept Loan Offer")

abline(lm(bank$Acpt_Offer ~bank$Crdt_Crd_Avg), col = "red", lwd =3)
lm(bank$Crdt_Crd_Avg ~ bank$Acpt_Offer)

#Interpretation: The plot of accept loan offer vs average credit card debt
#is not a useful visualization for analysis. Because accept loan offer is a 
#binary variable (0 = Not accept, 1 = Accept), it is difficult to determine
#a clear linear relationship between the two variables. The regression line is upwards,
#positive so we can assume that as average credit debt increases, accept loan
#offer goes towards the value of 1 however further modeling may be necessary.

################################################################################
#d.  In well-written sentences, provide at least three unique insights
#regarding the Universal Bank data based on the analyses completed in parts a-c.

#a.1 Pie Chart (Family Size): The family size is relatively equal for a family
#size of 1 (30%), 2 (25%), 3 (20%), or 4 (25%). 

#a.2 Bar Chart (Education Level): There is a higher number of individuals with education
#less than college than those who have a college or graduate degree.

#a.3 Histogram (Age): The histogram shows a normal distribution of the customer's
#age meaning that the average customer is 40-50. 

#b.1 Box Plot (Average Credit Card Debt vs Accepted Offer): There is a 
#higher mean of those who accept the loan offer than those who do not accept.
#More individuals are accepting the loan offer.



################################################################################
#e. Compute descriptive statistics, e.g., means, medians, standard deviations,
#etc., for all variables in the data set.  Hint:  Do some of the variables
#require transformation to be execute this step?
describe(bank) #Summary Statistics

#No, none of the variables require transformation because there are no 
#categorical variables.

################################################################################
#f. Compute the Pearson correlation for all pairs variables.  
#Given the goal of finding out which customers will accept the bank’s offer, 
#which three variables appear to be the most likely predictors?
round(cor(bank), digits = 4)
round(cor(bank), digits = 4)[15,] #Corr for Acpt_Offer

#Income (0.52), Crdt_Crd_Avg (0.38), CD_Account (0.34) are likely to be the 
#predictors for predicting if one accepts or denies the bank's offer.

################################################################################
#g. Fit a simple linear regression that predicts monthly average credit card 
#debt as a function of income.  What is the estimated regression model?   
#How well does this model fit?  Interpret the value of the slope 
#coefficient in this regression.
mod1 <- lm(bank$Crdt_Crd_Avg ~ bank$Income)
summary(mod1)

#Regression Model: Crdt_Crd_Avg = 0.110 + 0.025(Income)
#Multiple R-squared:  0.4132 
#Fit? Not a good model because only 41.32% of the model fits the observed data/
#41.32% of the varaibility in monthly credit card debt (y) is explained by
#a customer's income (x).

#Slope: Holding all other variables constant, if income increases by 1 unit ($1000),
#the monthly average credit card will also increase by 0.025 units ($24.72)

#Intercept: If income is 0 (X=0), then we predict the average credit card debt
#to be 0.110 ($110.36). 

#Check
yhat1 <- predict(mod1, bank)
yhat1[1:10]
round(bank$Crdt_Crd_Avg[1:10], digits= 4) #rounding does not work?
#Predict vs Observed is not close

################################################################################
#h. Fit a simple linear regression that predicts which customers will accept
#the bank’s offer of the personal loan product as a function of monthly
#average credit card debt.  How well does this model fit? 
#Interpret the value of the slope coefficient in the regression.
#Discuss the usefulness of this model.
mod2 <- lm(bank$Acpt_Offer ~ bank$Crdt_Crd_Avg)
summary(mod2)

#Regression Model: Acpt_Offer = -0.022 + 0.064(Crdt_Crd_Avg)
#Multiple R-squared:  0.1421
#Fit? Not a good model because only 14.21% of the model fits the observed data/
#14.21% of the variability is explained by whether a customer accepts or denies
#a loan offer (y) is explained by the monthly average credit card debt (x).

#Slope: Holding all other variables constant, if credit card average increases 
#by 1 unit ($1000), the acceptance of loans will increase by 0.064 units ($63.65).
#Intercept: If credit card average is 0 (X=0), then we predict that 
#the acceptance of loans will be -0.022 (-$21.81).

#Check
yhat1 <- predict(mod2, bank)
yhat1[1:10]
bank$Acpt_Offer[1:10]
#Predict vs Observed is not close

#Useful? Because of the low R squared, the model is not strong and needs improvement.
#However, because the Income variable has a low p value, income is significant
#in determining whether a customer will accept or deny a loan offer.


################################################################################
#i. For the models fit in parts h and i, provide and interpret a 99% prediction
#interval estimate for the dependent variable.  Assume that given values for
#income and credit card debt to use for prediction are $75,000 and $1,250,
#respectively.  Are these estimates useful?  Why or why not? 

#Part G
#incomepi <- data.frame(Income = 75)
#predict(mod1, data = incomepi)
#predict(mod1, data = incomepi, interval = "prediction", level = 0.99)

#mod1 <- lm(bank$Crdt_Crd_Avg ~ bank$Income, data = bank)
#incomepi <- data.frame(75)
#names(incomepi)[1] <- "Income"
#predict(mod1, incomepi)

predict(mod1, data.frame(Income = 75), interval = "prediction", level = 0.99)
#Income PI: (-1.582, 5.512), fit = 1.965

#Part H
#creditcardpi <- data.frame(Crdt_Crd_Avg = 1.25)
#predict(mod1, data = creditcardpi)
#predict(mod2, data = creditcardpi, interval = "prediction", level = 0.99)

#mod2 <- lm(bank$Acpt_Offer ~ bank$Crdt_Crd_Avg, data = bank)
#creditpi <- data.frame(1.25)
#names(creditpi)[1] <- "Crdt_Crd_Avg"
#predict(mod2, creditpi)

predict(mod2, data.frame(Crdt_Crd_Avg = 1.25), interval = "prediction", level = 0.99)
#Credit Card PI: (-0.667, 0.782), fit = 0.0578

#Useful? Not useful, both estimates have negative lower values which cannot be possible
#for the binary variable Acpt_Offer (0 or 1, can't be negative value).

################################################################################
#j. Suppose the goal was to predict whether a customer would accept the bank’s
#product offer (Acpt_Offer).   Provide 2 examples of statistical modeling
#methods would be used.  Justify this answer. 

#For binary variables, logistic regression is a useful statistical model
#to predict classification of whether a customer will accept a loan offer or not.

#KNN classification can also work to predict whether a customer will accept a 
#loan offer or not because it is a supervised learning algorithm
#that can output the labels by computing local probability.

