#KAGGLE HOUSE PRICES COMPETITION
#Team = Competition 1-8
#Elizabeth Homan, eih2nn
#Jenn Cruser, jc4pg
#Boh Young Suh, bs6ea

#FINAL WORK PRIOR TO 9/5/17
#####################################################

#Install (if necessary) and load the core tidyverse packages: ggplot2, tibble, tidyr, readr, purrr, and dplyr
library(tidyverse) 
#Additionally, install/load the class and caret packages to run the knn functions 
library(class)  
library(caret)

#Read in files:
train <- read_csv("train.csv") #Read in the comma separated value data file for training the model
test <- read_csv("test.csv") #Read in the csv data file for testing the model
sample <- read_csv("sample_submission.csv") #Read in the csv data file for sample submission (for reference)

#####################################################

##### DATA CLEANING AND PREPARATION #####

#Replace NA with "None" for all relevant columns in both training and testing sets
Nones <- c("Alley", "BsmtQual", "BsmtCond", "BsmtExposure", "BsmtFinType1", "BsmtFinType2", 
           "GarageType", "GarageFinish", "GarageQual", "GarageCond", "PoolQC", "Fence", "MiscFeature", "FireplaceQu")

for(i in 1:length(Nones)) {
  train[,Nones][i][is.na(train[,Nones][i])] = 'None'
}

for(i in 1:length(Nones)) {
  test[,Nones][i][is.na(test[,Nones][i])] = 'None'
}

#Subset to validate prediction models later
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
test[factors] = lapply(test[factors], factor) #Repeat for the test set
validate[factors] = lapply(validate[factors], factor) #Repeat for the validation set

#Change all columns with integers to the numeric class (using lapply function)
train.2[ , (!names(train.2) %in% factors)] = lapply(train.2[ , (!names(train.2) %in% factors)], as.numeric)
test[ , (!names(test) %in% factors)] = lapply(test[ , (!names(test) %in% factors)], as.numeric)
validate[ , (!names(validate) %in% factors)] = lapply(validate[ , (!names(validate) %in% factors)], as.numeric)

sapply(train.2, class) #Check classes to make sure everything worked correctly (should have only numeric and factor)

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
for (var in 1:ncol(train.2)) {
  if (lapply((train.2[,var]), class)=="numeric") {
    train.2[is.na(train.2[,var]),var] <- sapply(train.2[,var], mean, na.rm=TRUE)
  }
}

#Repeat for test and validation sets
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

#Change names of columns with numbers as the first character (this creates a problem when listing vars of interest for linear models)
names(train.2)[names(train.2) == '1stFlrSF'] <- 'FirstFlrSF'
names(train.2)[names(train.2) == '2ndFlrSF'] <- 'SecFlrSF'
names(train.2)[names(train.2) == '3SsnPorch'] <- 'TriSsnPorch'

names(test)[names(test) == '1stFlrSF'] <- 'FirstFlrSF'
names(test)[names(test) == '2ndFlrSF'] <- 'SecFlrSF'
names(test)[names(test) == '3SsnPorch'] <- 'TriSsnPorch'

names(validate)[names(validate) == '1stFlrSF'] <- 'FirstFlrSF'
names(validate)[names(validate) == '2ndFlrSF'] <- 'SecFlrSF'
names(validate)[names(validate) == '3SsnPorch'] <- 'TriSsnPorch'

#####################################################

##### PARAMETRIC APPROACH #####

#Create a linear model that takes into account all of the variables, from both the numeric and factor classes

train.lm <- lm(SalePrice~MSSubClass+MSZoning+LotFrontage+LotArea+Street+LotShape+LandContour+
                   LotConfig+LandSlope+Neighborhood+Condition1+Condition2+BldgType+HouseStyle+OverallQual+
                   OverallCond+YearBuilt+YearRemodAdd+RoofStyle+RoofMatl+Exterior1st+Exterior2nd+MasVnrType+MasVnrArea+
                   ExterQual+ExterCond+Foundation+BsmtQual+BsmtCond+BsmtExposure+BsmtFinType1+BsmtFinSF1+BsmtFinType2+
                   BsmtFinSF2+BsmtUnfSF+TotalBsmtSF+Heating+HeatingQC+CentralAir+Electrical+FirstFlrSF+SecFlrSF+LowQualFinSF+
                   GrLivArea+BsmtFullBath+BsmtHalfBath+FullBath+HalfBath+BedroomAbvGr+KitchenAbvGr+KitchenQual+TotRmsAbvGrd+
                   Functional+Fireplaces+GarageType+GarageYrBlt+GarageFinish+GarageCars+GarageArea+GarageQual+GarageCond+
                   PavedDrive+WoodDeckSF+OpenPorchSF+EnclosedPorch+TriSsnPorch+ScreenPorch+PoolArea+MiscVal+MoSold+YrSold+
                   SaleType+SaleCondition, data=train.2)

summary(train.lm) #Using this summary, select out all variables with a majority of subvariables showing significance over .001

#Over many runs, these seem to be LotArea, OverallQual, OverallCond, YearBuilt, RoofMatl, ExterQual, and BsmtFinSF1...
train.lm <- lm(SalePrice~LotArea+OverallQual+OverallCond+YearBuilt+RoofMatl+
                  ExterQual+BsmtFinSF1, data=train.2)

summary(train.lm) #YearBuild and RoofMatl do not appear to have high significance in this grouping

train.lm <- lm(SalePrice~LotArea+OverallQual+ExterQual+BsmtFinSF1, data=train.2)

summary(train.lm) #All variables have the highest possible significance in this final version of the model

mse.lm <- mean(train.lm$residuals^2) #Find mean square error
mse.lm #1553522361 (but this varies depending on the slice of the training data)

validpreds <- predict(train.lm, newdata = validate) #Use predict function to use this model on the validation set
validate$predictions <- validpreds #Create a new column in the validation dataframe with these predictions

#Create another column and fill with the percent difference in sale price between the predicted numbers and true numbers
for (i in 1:nrow(validate)) {
  ((abs((validate[i,"predictions"])-(validate[i,"SalePrice"])))/(validate[i,"predictions"]))*100 -> validate[i,"PercentOff"]
}

AvPercentOff.lm <- mean(validate$PercentOff) #Find mean % difference between predicted and real sale prices in the validation set
AvPercentOff.lm #This gets as low as 15.5% on certain slices of the training data

#Create predictions for test set
predict(train.lm, newdata = test) #Use the predict function to apply the above linear model to the test data
mypreds.lm <- data.frame(predict(train.lm, newdata = test))  #Put these values into a dataframe

colnames(mypreds.lm)[1] <- "SalePrice" #Assign the column the appropriate name

Id = 1461:2919 #Create a variable ID with the appropriate numbers for the kaggle submission (1461-2912)
mypreds.lm$Id <- Id #Add this column to the newest dataframe
mypreds.lm <- mypreds.lm[,c(2,1)] #Switch the columns so that ID is the first column

mypreds.lm[757,2] <- 0 #Change the one negative prediction to 0, as Kaggle will not accept negative values for sale prices

write.table(mypreds.lm, file = "houses_lm.csv", row.names=F, sep=",") #Write out to a csv

#FIRST LM METHOD -- SCORE = 0.25 on Kaggle

#####################################################

#After a few runs and tests using only variables with a majority of subvariables showing significance higher than .001,
#we decided to try including all variables with any subvariables showing significance higher than .001 (**).
#All variables of this nature are listed below and were used to train our second model (train.lm2).

train.lm2 <- lm(SalePrice~LotArea+Neighborhood+OverallQual+OverallCond+
                   YearBuilt+RoofMatl+ExterQual+BsmtQual+BsmtExposure+
                   BsmtUnfSF+FirstFlrSF+SecFlrSF+KitchenQual+PoolArea+ScreenPorch, data=train.2)

summary(train.lm2) #At least one subvariable for each variable in this set shows a high significance value
#on most, but not every, run with different slices of the training data

#Of note, we were unable to use validation for this model due to variables that are not consistent across both
#the training and validations sets (i.e. subvariables with too few observations across the whole set)

mypreds.lm2 <- data.frame(predict(train.lm2, newdata = test))  #Use predict function to create predictions for the test set
#using the new linear model and place these predictions into a new dataframe

colnames(mypreds.lm2)[1] <- "SalePrice" #Assign the column the appropriate name
mypreds.lm2$Id <- Id #Add ID column
mypreds.lm2 <- mypreds.lm2[,c(2,1)] #Switch order of columns

write.table(mypreds.lm2, file = "houses_lm2.csv", row.names=F, sep=",") #Write out to a csv

#SECOND LM METHOD -- SCORE = 0.158 on Kaggle (BEST SCORE)

######################################################################

##### NON-PARAMETRIC APPROACH #####

#For our non-parametric approach, we decided to use KNN...

#First, we prepared our data sets by selecting out numeric variables only

train.3 <- train.2[ , (!names(train.2) %in% factors)] #Select only columns with numeric values
test.2 <- test[ , (!names(test) %in% factors)] #Repeat for test set
validate.2 <- validate[ , (!names(validate) %in% factors)] #Repeat for validation set

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 1) #Create a training control
set.seed(22) #Set the seed to a random number 

#Of note, we repeated this with different seeds several times to make sure we hadn't picked one that would produce a rare K result

knn.fit <- train(SalePrice ~., data = train.3, method = "knn",  #Train a model using knn, with 10 runs of different K values
                 trControl=trctrl,
                 preProcess = c("center", "scale"),
                 tuneLength = 10)

knn.fit #See output below...

#k-Nearest Neighbors 
#730 samples
#36 predictor

#Pre-processing: centered (36), scaled (36) 
#Resampling: Cross-Validated (10 fold, repeated 1 times) 
#Summary of sample sizes: 657, 658, 656, 656, 658, 657, ... 
#Resampling results across tuning parameters:
  
#k   RMSE      Rsquared 
#5  40689.33  0.7563691
#7  39811.12  0.7712492
#9  39649.86  0.7788696
#11  39864.37  0.7829757
#13  39496.73  0.7902631
#15  39503.36  0.7951439
#17  39648.64  0.8001914
#19  39942.76  0.8007253
#21  40129.84  0.8027504
#23  40359.44  0.8021267

#RMSE was used to select the optimal model using  the smallest value.

#The final value used for the model was k = 13.

validate <- subset(validate, select = -c(predictions, PercentOff)) #Overwrite df to exclude columns created for previous linear model
validate.2 <- subset(validate, select = -c(SalePrice)) #Create new validation df without the real sale prices

knn.validate <- predict(knn.fit, newdata = validate.2) #Use KNN model for the validation set and put into a vector
validate$knnpredictions <- knn.validate #Add vector values into a new column in the validation dataframe

#Find percent difference from real sale prices... 
for (i in 1:nrow(validate)) {
  ((abs((validate[i,"knnpredictions"])-(validate[i,"SalePrice"])))/(validate[i,"SalePrice"]))*100 -> validate[i,"PercentOff"]
}

AvPercentOffKNN <- mean(validate$PercentOff) #Evaluate the mean percentage difference
AvPercentOffKNN #11.4% off of true sale prices in validation set (on average)

knn.preds <- predict(knn.fit, newdata = test.2) #Use KNN model for test data
knn.preds <- data.frame(predict(knn.fit, newdata = test.2)) #Put predictions into a new dataframe

colnames(knn.preds)[1] <- "SalePrice" #Assign column name
knn.preds$Id <- Id #Add ID values
knn.preds <- knn.preds[,c(2,1)] #Switch column order

write.table(knn.preds, file = "houses_knn.csv", row.names=F, sep=",") #Write out to a csv

#KNN PREDICTIONS -- SCORE = 0.185 on Kaggle

#####################################################

#We repeated the KNN approach with K=9 (as this was also frequent when using other seeds/slices of the subsetted training data)
#...It produced essentially the same result as K=13 on kaggle
