#Parametric Approach

library(tidyverse) #Load the core tidyverse packages: ggplot2, tibble, tidyr, readr, purrr, and dplyr

#Read in files:
train <- read_csv("train.csv") #Read in the comma separated value data file for training the model
test <- read_csv("test.csv") #Read in the csv data file for testing the model
