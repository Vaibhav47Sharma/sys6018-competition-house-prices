library(tidyverse)
library(dplyr)
library(magrittr)
library(Amelia)

# Reading in the data
setwd("../")
df.train = read_csv("data/all/train.csv")

# Create artificial train test dataset to make sure that dataset has all factors contained in 
# test
#   Read in test dataset
df.test = read_csv("data/all/test.csv")
#   Artificially created target variable to ncol matches
df.test = add_column(df.test, SalePrice = rep(0, nrow(df.test)))
#   Indicator variable for both train and test
df.train = add_column(df.train, TrainInd = rep(1, nrow(df.train)))
df.test = add_column(df.test, TrainInd = rep(0, nrow(df.test)))

#   creating train test table
df.train.test = rbind(df.train, df.test)

# Finding variables with missing values
missmap(df.train.test, main = "Missing Map")
str(df.train.test)

# PoolQC is mostly missing and we will delete this variable
df.train.test$PoolQC = NULL

# MiscFrature is mostly missing and we will delete this variable
df.train.test$MiscFeature = NULL

# Alley is mostly missing and we will delete this variable
df.train.test$Alley = NULL

# Fence is mostly missing and we will delete this variable
df.train.test$Fence = NULL

# Checking what percentage of FireplaceQu is missing
mean(is.na(df.train.test$FireplaceQu))
# Half of the FireplaceQu
# [1] 0.486468

# Deleting FireplaceQu for now
df.train.test$FireplaceQu = NULL

# Finding which variables have missing values
missing = colMeans(is.na(df.train.test))
missing = missing[missing!=0] 

# We note that SaleType has a missing value

# Checking what these mising values are
names(missing)
# [1] "MSZoning"     "LotFrontage"  "Utilities"    "Exterior1st"  "Exterior2nd"  "MasVnrType"   "MasVnrArea"  
# [8] "BsmtQual"     "BsmtCond"     "BsmtExposure" "BsmtFinType1" "BsmtFinSF1"   "BsmtFinType2" "BsmtFinSF2"  
# [15] "BsmtUnfSF"    "TotalBsmtSF"  "Electrical"   "BsmtFullBath" "BsmtHalfBath" "KitchenQual"  "Functional"  
# [22] "GarageType"   "GarageYrBlt"  "GarageFinish" "GarageCars"   "GarageArea"   "GarageQual"   "GarageCond"  
# [29] "SaleType"   
# So there are thirty missing variables after deleting the major missing variables

# Taking the character variables and making them factors
character.variables = sapply(df.train.test, class)
character.variables = character.variables[character.variables == 'character']
df.train.test[, names(character.variables)] = lapply(df.train.test[, names(character.variables)], factor)

############################################################################################################
# Should come back and do imputation here
# For now, fit regression model without missing variables
df.train.test = df.train.test %>% select(-c(names(missing)))

# Now that schema and features are consistent, split back into train and test
df.train = df.train.test %>% filter(TrainInd == 1)
df.test = df.train.test %>% filter(TrainInd == 0)

# Dropping Survived and Train Ind from df.test
df.test = df.test %>% select(-TrainInd, -SalePrice)

# Dropping TrainInd from df.train
df.train = df.train %>% select(-TrainInd)
df.train$Id = NULL

# Imputation and class change for factor variables

df.train.test.dummy = df.train.test[, names(character.variables)]
df.train.test.dummy[is.na(df.train.test.dummy)] = "Unknown"
df.train.test[, names(character.variables)] = lapply(df.train.test.dummy, factor)

# Imputation and class change for numeric variables
numeric.vecs = which(sapply(df.train.test, is.numeric))
df.numeric.imputed = df.train.test %>%
  transmute_at(vars(numer.vecs), funs(func={ifelse(is.na(.), mean(., na.rm=T), .)}))

df.train.test[, names(numeric.vecs)] = df.numeric.imputed
table(is.na(df.train.test))
