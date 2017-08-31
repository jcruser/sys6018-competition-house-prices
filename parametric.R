#Parametric Approach


##### DATA CLEANING AND PREPARATION #####

library(tidyverse) #Load the core tidyverse packages: ggplot2, tibble, tidyr, readr, purrr, and dplyr

#Read in files:
train <- read_csv("train.csv") #Read in the comma separated value data file for training the model
test <- read_csv("test.csv") #Read in the csv data file for testing the model
sample <- read_csv("sample_submission.csv") #Read in the csv data file for sample submission

#Drop columns that have too many NAs on visual inspection of data
drop <- c("Alley","PoolQC", "Fence", "MiscFeature","FireplaceQu")
train.2 <- train[ , !(names(train) %in% drop)]
test.2 <- test[ , !(names(test) %in% drop)]

sub <- sample(1:1460,size=730) #Subset to cross validate
train.3 <- train.2[sub,]     #Select subset for training
validate <- train.2[-sub,]  #Set aside subset for validation

sapply(train.3, class) #Look at classes of each variable

#Use factor to adjust the variables that are categorical
factors <- c("MSZoning","Street","LotShape","LandContour","Utilities","LotConfig","LandSlope",
             "Neighborhood","Condition1","Condition2","BldgType","HouseStyle","RoofStyle","RoofMatl",
             "Exterior1st","Exterior2nd","MasVnrType","ExterQual","ExterCond","Foundation","BsmtQual",
             "BsmtCond","BsmtExposure","BsmtFinType1", "BsmtFinType2","BsmtFinSF2","Heating","HeatingQC","CentralAir",
             "Electrical","KitchenQual","Functional","GarageType","GarageFinish","GarageQual","GarageCond",
             "PavedDrive","SaleType","SaleCondition")

train.3[factors] = lapply(train.3[factors], factor)
test.2[factors] = lapply(test.2[factors], factor) #Also do this for the test set
validate[factors] = lapply(validate[factors], factor) #Also for the validation set

#Change all columns with integers to the numeric class
train.3[ , (!names(train.3) %in% factors)] = lapply(train.3[ , (!names(train.3) %in% factors)], as.numeric)
test.2[ , (!names(test.2) %in% factors)] = lapply(test.2[ , (!names(test.2) %in% factors)], as.numeric)
validate[ , (!names(validate) %in% factors)] = lapply(validate[ , (!names(validate) %in% factors)], as.numeric)

sapply(train.3, class) #Check classes again to make sure everything worked correctly

#Create mode function
Mode <- function(x, na.rm) {
  xtab <- table(x)
  xmode <- names(which(xtab == max(xtab)))
  if (length(xmode) > 1) xmode <- ">1 mode"
  return(xmode)
}

#Replace all NA values in "factor" columns with the mode of that column
for (var in 1:ncol(train.3)) {  
  if (lapply((train.3[,var]), class)=="factor") {
    train.3[is.na(train.3[,var]),var] <- Mode(train.3[,var], na.rm = TRUE)
  }
}

for (var in 1:ncol(test.2)) {  
  if (lapply((test.2[,var]), class)=="factor") {
    test.2[is.na(test.2[,var]),var] <- Mode(train.3[,var], na.rm = TRUE)
  }
}

for (var in 1:ncol(validate)) {  
  if (lapply((validate[,var]), class)=="factor") {
    validate[is.na(validate[,var]),var] <- Mode(train.3[,var], na.rm = TRUE)
  }
}

#Replace all NA values in "numeric" columns with the mean of that column
for (var in 1:ncol(train.3)) {
  if (lapply((train.3[,var]), class)=="numeric") {
    train.3[is.na(train.3[,var]),var] <- sapply(train.3[,var], mean, na.rm=TRUE)
  }
}

for (var in 1:ncol(test.2)) {
  if (lapply((test.2[,var]), class)=="numeric") {
    test.2[is.na(test.2[,var]),var] <- sapply(train.3[,var], mean, na.rm=TRUE)
  }
}

for (var in 1:ncol(validate)) {
  if (lapply((validate[,var]), class)=="numeric") {
    validate[is.na(validate[,var]),var] <- sapply(train.3[,var], mean, na.rm=TRUE)
  }
}

#Change names of columns that have numbers in them
names(train.3)[names(train.3) == '1stFlrSF'] <- 'FirstFlrSF'
names(train.3)[names(train.3) == '2ndFlrSF'] <- 'SecFlrSF'
names(train.3)[names(train.3) == '3SsnPorch'] <- 'TriSsnPorch'

names(test.2)[names(test.2) == '1stFlrSF'] <- 'FirstFlrSF'
names(test.2)[names(test.2) == '2ndFlrSF'] <- 'SecFlrSF'
names(test.2)[names(test.2) == '3SsnPorch'] <- 'TriSsnPorch'

names(validate)[names(validate) == '1stFlrSF'] <- 'FirstFlrSF'
names(validate)[names(validate) == '2ndFlrSF'] <- 'SecFlrSF'
names(validate)[names(validate) == '3SsnPorch'] <- 'TriSsnPorch'




##### PARAMETRIC APPROACH #####

#Run a linear regression model with a few numeric variables of interest
train.lm1 <- lm(SalePrice~LotArea+YearBuilt+TotalBsmtSF+FirstFlrSF+SecFlrSF+FullBath+HalfBath+TotRmsAbvGrd+PoolArea+YrSold, data=train.3)
summary(train.lm1)

#Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  -1.722e+06  2.205e+06  -0.781   0.4349    
#LotArea       5.138e-01  1.158e-01   4.435 1.06e-05 ***
#YearBuilt     7.598e+02  6.448e+01  11.784  < 2e-16 ***
#TotalBsmtSF   5.654e+01  6.080e+00   9.299  < 2e-16 ***
#FirstFlrSF    8.909e+01  8.603e+00  10.356  < 2e-16 ***
#SecFlrSF      9.332e+01  7.533e+00  12.388  < 2e-16 ***
#FullBath     -3.706e+03  4.327e+03  -0.856   0.3920    
#HalfBath     -4.256e+03  4.332e+03  -0.982   0.3262    
#TotRmsAbvGrd -2.914e+03  1.644e+03  -1.772   0.0768 .  
#PoolArea     -2.587e+01  4.721e+01  -0.548   0.5839    
#YrSold        1.151e+02  1.096e+03   0.105   0.9164    

#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 38900 on 719 degrees of freedom
#Multiple R-squared:  0.751,	Adjusted R-squared:  0.7475 
#F-statistic: 216.8 on 10 and 719 DF,  p-value: < 2.2e-16

#Re-run with fewer variables, based on significance from last run
train.lm2 <- lm(SalePrice~LotArea+YearBuilt+TotalBsmtSF+FirstFlrSF+SecFlrSF, data=train.3)
summary(train.lm2) #All have high significance

mse.lm1 <- mean(train.lm2$residuals^2)
mse.lm1 #1501209518...

predict(train.lm2, newdata = test.2) #Predict using the wt and year variables, as in p3.lm2
mypreds.lm <- data.frame(predict(train.lm2, newdata = test.2))  #Put these values into a vector

colnames(mypreds.lm)[1] <- "SalePrice"

Id = 1461:2919
mypreds.lm$Id <- Id
mypreds.lm1 <- mypreds.lm[,c(2,1)] 

#FINAL FIRST PREDICTIONS -- SCORE = 0.22 on Kaggle
write.table(mypreds.lm1, file = "eih2nn_houses_lm1.csv", row.names=F, sep=",") #Write out to a csv






#### NON PARAMETRIC APPROACH? ####

#Just playing around...

#train.lg1 <- glm(SalePrice~MSSubClass+MSZoning+LotFrontage+LotArea+Street+LotShape+LandContour+
#Utilities+LotConfig+LandSlope+Neighborhood+Condition1+Condition2+BldgType+HouseStyle+OverallQual+
#OverallCond+YearBuilt+YearRemodAdd+RoofStyle+RoofMatl+Exterior1st+Exterior2nd+MasVnrType+MasVnrArea+
#ExterQual+ExterCond+Foundation+BsmtQual+BsmtCond+BsmtExposure+BsmtFinType1+BsmtFinSF1+BsmtFinType2+
#BsmtFinSF2+BsmtUnfSF+TotalBsmtSF+Heating+HeatingQC+CentralAir+Electrical+FirstFlrSF+SecFlrSF+LowQualFinSF+
#GrLivArea+BsmtFullBath+BsmtHalfBath+FullBath+HalfBath+BedroomAbvGr+KitchenAbvGr+KitchenQual+TotRmsAbvGrd+
#Functional+Fireplaces+GarageType+GarageYrBlt+GarageFinish+GarageCars+GarageArea+GarageQual+GarageCond+
#PavedDrive+WoodDeckSF+OpenPorchSF+EnclosedPorch+TriSsnProch+ScreenPorch+PoolArea+MiscVal+MoSold+YrSold+
#SaleType+SaleCondition, data=train.3, family = "binomial")

