# This script assumes the existence of several R objects including:
# df.train
# df.test
# df.train.ohe
# df.test.ohe

# parameter tuning

library(assertthat)
library(tidyverse)

# Test crossvalidation for parameter tuning
cross.val(ycol = df.train$SalePrice, train = as.data.frame(df.train.ohe), folds = 10L, k = 5L)

# test grid search for parameter tuning
param.grid <- expand.grid(k = c(2L,3L,5L,8L,13L,21L), distweight = c(T, F), dm = c("euclid", "chisq"))
pg <- expand.grid(k = c(5L, 10L), distweight = c(F), dm = c("euclid"))

KNN.grid.search(ycol = df.train$SalePrice, train = as.data.frame(df.train.ohe), folds = 10L, grid = pg)

# Get KNN predictions for submission

preds <- KNN.Predict(test = df.test.ohe, ycol = df.train$SalePrice, train = df.train.ohe, k=10L)
preds2 <- KNN.Predict(test = df.test.ohe[,coefs$name[2:106]], ycol = df.train$SalePrice, train = df.train.ohe[,coefs$name[2:106]], k=10L)
preds3 <- KNN.Predict(test = df.test.ohe[,coefs$name[2:106]], ycol = df.train$SalePrice, train = df.train.ohe[,coefs$name[2:106]], k=10L, distweight=T)
preds4 <- KNN.Predict(test = df.test.ohe[,coefs$name[2:106]], ycol = df.train$SalePrice, train = df.train.ohe[,coefs$name[2:106]], k=10L, distweight=F, dm = "chisq")
# using standardization but not normalization
preds6 <- KNN.Predict(test = df.test.ohe[,coefs$name[2:106]], ycol = df.train$SalePrice, train = df.train.ohe[,coefs$name[2:106]], k=10L, distweight=F, dm = "chisq")
preds7 <- KNN.Predict(test = df.test.ohe[,imp$varname[1:10]], ycol = df.train$SalePrice, train = df.train.ohe[,imp$varname[1:10]], k=10L, distweight=F, dm = "chisq")
preds8 <- KNN.Predict(test = df.test.ohe[,imp$varname[1:25]], ycol = df.train$SalePrice, train = df.train.ohe[,imp$varname[1:25]], k=10L, distweight=F, dm = "chisq")
preds9 <- KNN.Predict(test = df.test.ohe[,imp$varname[1:30]], ycol = df.train$SalePrice, train = df.train.ohe[,imp$varname[1:30]], k=10L, distweight=F, dm = "chisq")
preds10 <- KNN.Predict(test = df.test.ohe[,imp$varname[1:15]], ycol = df.train$SalePrice, train = df.train.ohe[,imp$varname[1:15]], k=10L, distweight=F, dm = "weighted", w =(imp$Overall[1:15]))
preds11 <- KNN.Predict(test = df.test.ohe[,imp$varname[1:10]], ycol = df.train$SalePrice, train = df.train.ohe[,imp$varname[1:10]], k=10L, distweight=F, dm = "weighted", w =(imp$Overall[1:10]))
preds12 <- KNN.Predict(test = df.test.ohe[,imp$varname[1:7]], ycol = df.train$SalePrice, train = df.train.ohe[,imp$varname[1:7]], k=10L, distweight=F, dm = "weighted", w =(imp$Overall[1:7]))

ids <- read_csv("data/all/test.csv")
ids <- ids$Id

# Prep submission files
sol1 <- data.frame(Id = ids, SalePrice = preds, stringsAsFactors = F)
sol2 <- data.frame(Id = ids, SalePrice = preds2, stringsAsFactors = F)
sol3 <- data.frame(Id = ids, SalePrice = preds3, stringsAsFactors = F)
sol4 <- data.frame(Id = ids, SalePrice = preds4, stringsAsFactors = F)
sol5 <- data.frame(Id = ids, SalePrice = exp(p), stringsAsFactors = F)
sol6 <- data.frame(Id = ids, SalePrice = preds6, stringsAsFactors = F)
sol7 <- data.frame(Id = ids, SalePrice = preds7, stringsAsFactors = F)
sol8 <- data.frame(Id = ids, SalePrice = preds8, stringsAsFactors = F)
sol9 <- data.frame(Id = ids, SalePrice = preds9, stringsAsFactors = F)
sol10 <- data.frame(Id = ids, SalePrice = preds10, stringsAsFactors = F)
sol11 <- data.frame(Id = ids, SalePrice = preds11, stringsAsFactors = F)
sol12 <- data.frame(Id = ids, SalePrice = preds12, stringsAsFactors = F)

# Write out submission files
write_csv(sol1, path= "data/submissions/KNN1.csv")
write_csv(sol2, path= "data/submissions/KNN2.csv")
write_csv(sol3, path= "data/submissions/KNN3.csv")
write_csv(sol4, path= "data/submissions/KNN4.csv")
write_csv(sol5, path= "data/submissions/KNN5_caret.csv")
write_csv(sol6, path= "data/submissions/KNN6.csv")
write_csv(sol7, path= "data/submissions/KNN7.csv")
write_csv(sol8, path= "data/submissions/KNN8.csv")
write_csv(sol9, path= "data/submissions/KNN9.csv")
write_csv(sol10, path= "data/submissions/KNN10.csv")
write_csv(sol11, path= "data/submissions/KNN11.csv")
write_csv(sol12, path= "data/submissions/KNN12.csv")


# try using caret for comparison of R's KNN package to our own implementation

tc.lm <- trainControl(
        method = "boot",
        number= 10,
        repeats = 1)
mname <- "knn"

model.knn <- train(x = as.data.frame(df.train.ohe)[,-291], 
                   y = log(df.train$SalePrice), 
                   method = "knn", 
                   trControl = tc.lm, 
                   preProcess =c("center", "scale", "zv", "nzv", "knnImpute")
                   #na.action = "na.omit"
                   )
model.knn$bestTune
summary(model.knn)
model.knn

p <- predict(object = model.knn, newdata = as.data.frame(df.test.ohe))



# train random forest model to find estimate of feature importance for feature selection
library(randomForest)

temp <- df.train.ohe[complete.cases(df.train.ohe),]
model.rf <- randomForest::randomForest(SalePrice ~ ., data = temp, ntree = 100, importance = T)

tc.lm <- trainControl(
  method = "boot",
  number= 5,
  repeats = 1)
mname <- "knn"

model.rf <- train(x = as.data.frame(df.train.ohe)[,-291], 
                   y = log(df.train$SalePrice), 
                   method = "rf", 
                   trControl = tc.lm, 
                   preProcess =c("center", "scale", "zv", "nzv", "knnImpute"),
                   importance=T
                   #na.action = "na.omit"
)

# Get best predictors and place in ordered data frame
imp <- varImp(model.rf$finalModel)
imp$varname <- rownames(imp)
imp <- imp %>% arrange(desc(Overall))


# create list of data frames of various sizes for more extensive grid search
df.train.list <- list()
trtmp <- as.data.frame(df.train.ohe[,as.character(imp$varname)])
for (i in 1:5) {
  df.train.list[[i]] <- as.data.frame(df.train.ohe[,1:(i*10)])
}


# Run expandgrid a lot to do parameter tuning
outputlist <- list()
for (i in 1:5) {
  # This returned bad data for some reason - ignore result
  outputlist[i] <- KNN.grid.search(ycol = df.train$SalePrice, train = df.train.list[[i]], folds = 10L, grid = param.grid)
}
save(outputlist, file="outputlist_gridsearch.RData")

# Run search of parameter space using likely values since above grid search has bug apparently,
# when run with the multiple sets of predictor data frames
cross.val(ycol = df.train$SalePrice, train = df.train.list[[1]], folds = 10L, k = 10L) #0.1121
cross.val(ycol = df.train$SalePrice, train = df.train.list[[2]], folds = 10L, k = 10L) #0.1042
cross.val(ycol = df.train$SalePrice, train = df.train.list[[3]], folds = 10L, k = 10L) #0.0966 *
cross.val(ycol = df.train$SalePrice, train = df.train.list[[4]], folds = 10L, k = 10L) #0.0971 *

cross.val(ycol = df.train$SalePrice, train = df.train.list[[1]], folds = 10L, k = 5L)  #0.117
cross.val(ycol = df.train$SalePrice, train = df.train.list[[2]], folds = 10L, k = 5L)  #0.108

cross.val(ycol = df.train$SalePrice, train = df.train.list[[2]], folds = 10L, k = 15L)#0.1063

cross.val(ycol = df.train$SalePrice, train = df.train.list[[2]], folds = 10L, k = 10L, dm = "weighted", w = (1/imp$Overall[1:20])) #0.1031
cross.val(ycol = df.train$SalePrice, train = df.train.list[[2]], folds = 10L, k = 10L, dm = "weighted", w = (imp$Overall[1:20]))   #0.1042

cross.val(ycol = df.train$SalePrice, train = df.train.list[[3]], folds = 10L, k = 10L, dm = "weighted", w = (1/imp$Overall[1:30])) #0.0967
cross.val(ycol = df.train$SalePrice, train = df.train.list[[3]], folds = 10L, k = 10L, dm = "weighted", w = (imp$Overall[1:30]))   #0.0963



