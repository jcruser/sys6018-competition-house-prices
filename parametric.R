#Parametric Approach

library(tidyverse) #Load the core tidyverse packages: ggplot2, tibble, tidyr, readr, purrr, and dplyr

#Read in files:
train <- read_csv("train.csv") #Read in the comma separated value data file for training the model
test <- read_csv("test.csv") #Read in the csv data file for testing the model
sample <- read_csv("sample_submission.csv") #Read in the csv data file for sample submission

#Drop columns that have too many NAs on visual inspection of data
drop <- c("Alley","PoolQC", "Fence", "MiscFeature","Fireplace")
train.2 <- train[ , !(names(train) %in% drop)]

factors <- c("MSZoning","Street","LotShape","LandContour","Utilities","LotConfig","LandSlope",
             "Neighborhood","Condition1","Condition2","BldgType","HouseStyle","RoofStyle","RoofMatl",
             "Exterior1st","Exterior2nd","MasVnrType","ExterQual","ExterCond","Foundation","BsmtQual",
             "BsmtCond","BsmtExposure","BsmtFinType1","BsmtFinSF2","Heating","HeatingQC","CentralAir",
             "Electrical","KitchenQual","Functional","GarageType","GarageFinish","GarageQual","GarageCond",
             "PaveDrive","SaleType","SaleCondition")

#Change into factors *****

#Fill in NA values with mean of column
for(i in 1:ncol(train.2)){
  train.2[is.na(train.2[,i]), i] <- mean(train.2[,i], na.rm = TRUE)
}
