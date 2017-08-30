#Parametric Approach

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
validate <- train.2[-sub,]  #Set aside subset for cross-validation

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

#Change all columns with integers to the numeric class
train.3[ , (!names(train.3) %in% factors)] = lapply(train.3[ , (!names(train.3) %in% factors)], as.numeric)
test.2[ , (!names(test.2) %in% factors)] = lapply(test.2[ , (!names(test.2) %in% factors)], as.numeric)

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

#Replace all NA values in "numeric" columns with the mean of that column
for (var in 1:ncol(train.3)) {
  if (lapply((train.3[,var]), class)=="numeric") {
    train.3[is.na(train.3[,var]),var] <- sapply(train.3[,var], mean, na.rm=TRUE)
  }
}
