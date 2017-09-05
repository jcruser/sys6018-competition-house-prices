##### DATA CLEANING AND PREPARATION #####

library(tidyverse) #Load the core tidyverse packages: ggplot2, tibble, tidyr, readr, purrr, and dplyr

#Read in files:
train <- read_csv("train.csv") #Read in the comma separated value data file for training the model
test <- read_csv("test.csv") #Read in the csv data file for testing the model
sample <- read_csv("sample_submission.csv") #Read in the csv data file for sample submission

#Replace NA with "None" for all relevant columns in both training and testing sets
Nones <- c("Alley", "BsmtQual", "BsmtCond", "BsmtExposure", "BsmtFinType1", "BsmtFinType2", 
           "GarageType", "GarageFinish", "GarageQual", "GarageCond", "PoolQC", "Fence", "MiscFeature", "FireplaceQu")

for(i in 1:length(Nones)) {
  train[,Nones][i][is.na(train[,Nones][i])] = 'None'
}

for(i in 1:length(Nones)) {
  test[,Nones][i][is.na(test[,Nones][i])] = 'None'
}

#Subset to cross validate later
sub <- sample(1:1460,size=730) 
train.2 <- train[sub,]     #Select subset for training
validate <- train[-sub,]  #Set aside subset for validation


#Use factor to adjust the variables that are categorical
factors <- c("MSZoning","Street","LotShape","LandContour","Utilities","LotConfig","LandSlope",
             "Neighborhood","Condition1","Condition2","BldgType","HouseStyle","RoofStyle","RoofMatl",
             "Exterior1st","Exterior2nd","MasVnrType","ExterQual","ExterCond","Foundation","BsmtQual",
             "BsmtCond","BsmtExposure","BsmtFinType1", "BsmtFinType2","BsmtFinSF2","Heating","HeatingQC","CentralAir",
             "Electrical","KitchenQual","Functional","GarageType","GarageFinish","GarageQual","GarageCond",
             "PavedDrive","SaleType","SaleCondition","Alley","PoolQC", "Fence", "MiscFeature", "FireplaceQu")

train.2[factors] = lapply(train.2[factors], factor)
test[factors] = lapply(test[factors], factor) #Also do this for the test set
validate[factors] = lapply(validate[factors], factor) #Also for the validation set

#Change all columns with integers to the numeric class
train.2[ , (!names(train.2) %in% factors)] = lapply(train.2[ , (!names(train.2) %in% factors)], as.numeric)
test[ , (!names(test) %in% factors)] = lapply(test[ , (!names(test) %in% factors)], as.numeric)
validate[ , (!names(validate) %in% factors)] = lapply(validate[ , (!names(validate) %in% factors)], as.numeric)

sapply(train.2, class) #Check classes to make sure everything worked correctly

#Create mode function
Mode <- function(x, na.rm) {
  xtab <- table(x)
  xmode <- names(which(xtab == max(xtab)))
  if (length(xmode) > 1) xmode <- ">1 mode"
  return(xmode)
}

#Replace all NA values in "factor" columns with the mode of that column
for (var in 1:ncol(train.2)) {  
  if (lapply((train.2[,var]), class)=="factor") {
    train.2[is.na(train.2[,var]),var] <- Mode(train.2[,var], na.rm = TRUE)
  }
}

#Repeat for test set
for (var in 1:ncol(test)) {  
  if (lapply((test[,var]), class)=="factor") {
    test[is.na(test[,var]),var] <- Mode(train.2[,var], na.rm = TRUE)
  }
}

#Repeat for validation set
for (var in 1:ncol(validate)) {  
  if (lapply((validate[,var]), class)=="factor") {
    validate[is.na(validate[,var]),var] <- Mode(train.2[,var], na.rm = TRUE)
  }
}

#Replace all NA values in "numeric" columns with the mean of that column 
#(repeat for test and validation sets)
for (var in 1:ncol(train.2)) {
  if (lapply((train.2[,var]), class)=="numeric") {
    train.2[is.na(train.2[,var]),var] <- sapply(train.2[,var], mean, na.rm=TRUE)
  }
}

for (var in 1:ncol(test)) {
  if (lapply((test[,var]), class)=="numeric") {
    test[is.na(test[,var]),var] <- sapply(train.2[,var], mean, na.rm=TRUE)
  }
}

for (var in 1:ncol(validate)) {
  if (lapply((validate[,var]), class)=="numeric") {
    validate[is.na(validate[,var]),var] <- sapply(train.2[,var], mean, na.rm=TRUE)
  }
}

#Change names of columns that have numbers in them
names(train.2)[names(train.2) == '1stFlrSF'] <- 'FirstFlrSF'
names(train.2)[names(train.2) == '2ndFlrSF'] <- 'SecFlrSF'
names(train.2)[names(train.2) == '3SsnPorch'] <- 'TriSsnPorch'

names(test)[names(test) == '1stFlrSF'] <- 'FirstFlrSF'
names(test)[names(test) == '2ndFlrSF'] <- 'SecFlrSF'
names(test)[names(test) == '3SsnPorch'] <- 'TriSsnPorch'

names(validate)[names(validate) == '1stFlrSF'] <- 'FirstFlrSF'
names(validate)[names(validate) == '2ndFlrSF'] <- 'SecFlrSF'
names(validate)[names(validate) == '3SsnPorch'] <- 'TriSsnPorch'





##### PARAMETRIC APPROACH WITH NUMERIC VARIABLES OF INTEREST TO START #####

#Run a linear regression model with a few numeric variables of interest
train.lm1 <- lm(SalePrice~LotArea+YearBuilt+TotalBsmtSF+FirstFlrSF+SecFlrSF+FullBath+HalfBath+TotRmsAbvGrd+PoolArea+YrSold, data=train.2)
summary(train.lm1)

#EXAMPLE OUTPUT (FROM ONE RUN):
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#  (Intercept)  -2.420e+06  2.394e+06  -1.011 0.312466    
#  LotArea       5.702e-01  1.883e-01   3.028 0.002547 ** 
#  YearBuilt     8.036e+02  7.481e+01  10.743  < 2e-16 ***
#  TotalBsmtSF   4.678e+01  7.339e+00   6.374 3.29e-10 ***
#  FirstFlrSF    1.001e+02  9.462e+00  10.583  < 2e-16 ***
#  SecFlrSF      9.399e+01  7.545e+00  12.457  < 2e-16 ***
#  FullBath     -1.205e+03  4.708e+03  -0.256 0.798127    
#  HalfBath     -2.516e+03  4.543e+03  -0.554 0.579957    
#  TotRmsAbvGrd -2.805e+03  1.717e+03  -1.634 0.102646    
#  PoolArea      1.931e+02  5.400e+01   3.576 0.000372 ***
#  YrSold        4.155e+02  1.189e+03   0.350 0.726782    

#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 42320 on 719 degrees of freedom
#Multiple R-squared:  0.7445,	Adjusted R-squared:  0.741 
#F-statistic: 209.5 on 10 and 719 DF,  p-value: < 2.2e-16

#Re-run with fewer variables, based on significance from last run
train.lm2 <- lm(SalePrice~LotArea+YearBuilt+TotalBsmtSF+FirstFlrSF+SecFlrSF+PoolArea, data=train.2)
summary(train.lm2) #All have high significance

validpreds <- predict(train.lm2, newdata = validate)
validate$predictions <- validpreds

for (i in 1:nrow(validate)) {
  ((abs((validate[i,"predictions"])-(validate[i,"SalePrice"])))/(validate[i,"predictions"]))*100 -> validate[i,"PercentOff1"]
}
AvPercentOff.lm1 <- mean(validate$PercentOff1)
AvPercentOff.lm1 #17.07737 % off of true sale price on average 
#Of note, this came down as far as 14% on other runs

predict(train.lm2, newdata = test) #Use predict function to apply the linear model to the test data
mypreds.lm <- data.frame(predict(train.lm2, newdata = test))  #Put these values into a dataframe

colnames(mypreds.lm)[1] <- "SalePrice"

Id = 1461:2919
mypreds.lm$Id <- Id
mypreds.lm1 <- mypreds.lm[,c(2,1)] 

#FIRST LM METHOD -- SCORE = 0.22 on Kaggle
write.table(mypreds.lm1, file = "eih2nn_houses_lm1.csv", row.names=F, sep=",") #Write out to a csv





##### ANOTHER PARAMETRIC APPROACH WITH ALL VARIABLES TO START #####

train.lm3 <- lm(SalePrice~MSSubClass+MSZoning+LotFrontage+LotArea+Street+LotShape+LandContour+
                   LotConfig+LandSlope+Neighborhood+Condition1+Condition2+BldgType+HouseStyle+OverallQual+
                   OverallCond+YearBuilt+YearRemodAdd+RoofStyle+RoofMatl+Exterior1st+Exterior2nd+MasVnrType+MasVnrArea+
                   ExterQual+ExterCond+Foundation+BsmtQual+BsmtCond+BsmtExposure+BsmtFinType1+BsmtFinSF1+BsmtFinType2+
                   BsmtFinSF2+BsmtUnfSF+TotalBsmtSF+Heating+HeatingQC+CentralAir+Electrical+FirstFlrSF+SecFlrSF+LowQualFinSF+
                   GrLivArea+BsmtFullBath+BsmtHalfBath+FullBath+HalfBath+BedroomAbvGr+KitchenAbvGr+KitchenQual+TotRmsAbvGrd+
                   Functional+Fireplaces+GarageType+GarageYrBlt+GarageFinish+GarageCars+GarageArea+GarageQual+GarageCond+
                   PavedDrive+WoodDeckSF+OpenPorchSF+EnclosedPorch+TriSsnPorch+ScreenPorch+PoolArea+MiscVal+MoSold+YrSold+
                   SaleType+SaleCondition, data=train.2)
summary(train.lm3) #See selected variables below for which ones consistently had a significance code above 0.001 

train.lm4 <- lm(SalePrice~LotArea+OverallQual+OverallCond+YearBuilt+RoofMatl+
                  ExterQual+BsmtFinSF1, data=train.2)
summary(train.lm4)

train.lm5 <- lm(SalePrice~LotArea+OverallQual+ExterQual+BsmtFinSF1, data=train.2)
summary(train.lm5)
mse.lm5 <- mean(train.lm5$residuals^2)
mse.lm5 #1860451135...

validpreds2 <- predict(train.lm5, newdata = validate)
validate$predictions2 <- validpreds2

for (i in 1:nrow(validate)) {
  ((abs((validate[i,"predictions2"])-(validate[i,"SalePrice"])))/(validate[i,"predictions2"]))*100 -> validate[i,"PercentOff2"]
}
AvPercentOff.lm2 <- mean(validate$PercentOff2)
AvPercentOff.lm2 #16.89892 % off of true sale price on average 
#This got as low as 16.3% on other runs, but was generally comparable to the 
#results from the other linear model approach

#WRITE UP TEST PREDITIONS
predict(train.lm5, newdata = test) #Use predict function to apply the linear model to the test data
mypreds.lm2 <- data.frame(predict(train.lm5, newdata = test))  #Put these values into a dataframe

colnames(mypreds.lm2)[1] <- "SalePrice"

Id = 1461:2919
mypreds.lm2$Id <- Id
mypreds.lm3 <- mypreds.lm2[,c(2,1)] 

mypreds.lm3[757,2] <- 0

#SECOND LM METHOD -- SCORE = 0.25 on Kaggle (worse score, as expected)
write.table(mypreds.lm3, file = "eih2nn_houses_lm2.csv", row.names=F, sep=",") #Write out to a csv


