#KNN Implementation

library(assertthat)
library(tidyverse)

Dist.euclidean.vector <- function(x, y, accept.na = TRUE) {
  #Calculates the euclidean distance between two vectors
  assertthat::assert_that(is.numeric(x))
  assertthat::assert_that(is.numeric(y))
  assertthat::assert_that(length(x)==length(y))
  assertthat::assert_that(length(x)>0)
  if (!accept.na) {
    assertthat::assert_that(sum(is.na(x)) == 0)
    assertthat::assert_that(sum(is.na(y)) == 0)
  }
  return (sqrt(sum((x - y) ^ 2, na.rm = TRUE)))
}

Dist.hamming.vector <- function(x, y, accept.na = TRUE) {
  #Calculates the hamming distance between two vectors
  assertthat::assert_that(is.numeric(x))
  assertthat::assert_that(is.numeric(y))
  assertthat::assert_that(length(x)==length(y))
  assertthat::assert_that(length(x)>0)
  if (!accept.na) {
    assertthat::assert_that(sum(is.na(x)) == 0)
    assertthat::assert_that(sum(is.na(y)) == 0)
  }
  difference <- x - y
  difference <- difference[!is.na(difference)]
  return (length(difference[difference != 0]))
}

Dist.chi.vector <- function(x, y, accept.na = TRUE) {
  #Calculates the chi squared distance between two vectors
  assertthat::assert_that(is.numeric(x))
  assertthat::assert_that(is.numeric(y))
  assertthat::assert_that(length(x)==length(y))
  assertthat::assert_that(length(x)>0)
  if (!accept.na) {
    assertthat::assert_that(sum(is.na(x)) == 0)
    assertthat::assert_that(sum(is.na(y)) == 0)
  }
  sum.vector <- x + y
  euclid.distance.vector <- (x - y) ^ 2
  sum.vector <- ifelse(sum.vector != 0, euclid.distance.vector / sum.vector, 0)
  
  return (sum(sum.vector, na.rm = TRUE)/2)
}

Normalize.vector <- function(x, accept.na = TRUE) {
  #Function to normalize the vector. Uses the formula (x-xmin) / (xmax - xmin)
  assertthat::assert_that(is.numeric(x))
  assertthat::assert_that(length(x)>0)
  if (!accept.na) {
    assertthat::assert_that(sum(is.na(x)) == 0)
  }
  
  #Removing all the NA values
  x <- x[!is.na(x)]
  x.max <- max(x)
  x.min <- min(x)
  
  #Max - Min, this is the denominator
  x.diff <- x.max - x.min
  x <- (x - x.min)/ x.diff
  
  return(x)
}

Standardize.vector <- function(x, accept.na = TRUE) {
  #Function to standardize the vector. Uses a simple formula (x-mean(x)) / sd(x)
  assertthat::assert_that(is.numeric(x))
  assertthat::assert_that(length(x)>0)
  if (!accept.na) {
    assertthat::assert_that(sum(is.na(x)) == 0)
  }
  
  #Removing all the NA values
  x <- x[!is.na(x)]
  mean <- sum(x)/ length(x)
  sd <- sd(x)
  
  #Applying the transformation
  x <- (x - mean)/ sd
  return(x)
}

DistKNN <- function(x, y, dm = "euclid") {
  if (dm == "euclid") {
    return(Dist.euclidean.vector(x, y))
  } else if (dm == "chisq") {
    return(Dist.chi.vector(x, y))
  } else {
    stop("Distance metric name not recognized")
  }
}

FindKNearestNeighbors <- function(x, ycol, train, k, distweight = F, ...) {
  #x is the test vector of length ncol(train)
  #ycol is a vector of response values of same length as nrow(train)
  #train is the full matrix of training data
  #k is the number of nearest neighbors
  #dm is the name of the distance method
 
  assert_that(is.matrix(train))
  assert_that(is.numeric(x))
  assert_that(is.numeric(ycol))
  assert_that(is.integer(k))
  assert_that(length(ycol) == nrow(train))
  assert_that(length(x) == ncol(train))
  assert_that(k >= 1 & k < nrow(train))

  distances <- numeric(nrow(train))
  response.values <- numeric(nrow(train)) #this assumes continuous response
  
  #Find distance of each neighbor
  for (i in 1:nrow(train)) {
    distances[i] <- DistKNN(x, as.numeric(train[i,]))
    response.values[i] <- ycol[i]
  }
  
  #Get top k neighbors
  ord <- order(distances)
  top.k.resp <- response.values[ord][1:k]
  top.k.dist <- distances[ord][1:k]
  
  #Find (weighted) average output
  if (distweight == F) {
    outp <- mean(top.k.resp)
  } else {
    if (all(top.k.dist) == 0) {
      #Weighting irrelevant, ignore.
      outp <- mean(top.k.resp)
    }
    if (any(top.k.dist == 0)) {
      #Prevent divide by 0
      top.k.dist[which(top.k.dist == 0)] <- min(top.k.dist[top.k.dist != 0])
    }
    #Mean weighted by inverse of distance
    outp <- sum(top.k.resp*(1/top.k.dist))/sum((1/top.k.dist))
  }
  return(outp)
}

KNN.Predict <- function(test, ycol, train, k, ...) {
  #test is the matrix of predictors to be used for prediction
  #ycol is a vector of response values of same length as nrow(train)
  #train is the matrix of training data predictors
  #k is the number of nearest neighbors
  
  assert_that(is.matrix(train))
  assert_that(is.matrix(test))
  assert_that(is.numeric(ycol))
  assert_that(is.integer(k))
  assert_that(length(ycol) == nrow(train))
  assert_that(ncol(test) == ncol(train))
  assert_that(k >= 1 & k < nrow(train))
  
  predictions <- numeric(nrow(test))
  for (i in 1:nrow(test)) {
    x <- as.numeric(test[i,])
    predictions[i] <- FindKNearestNeighbors(x, ycol, train, k, ...)
  }
  return(predictions)
}

cross.val <- function(ycol, train, folds, ...) {
  #This function takes a training data set and uses cross-validation to 
  #measure the mean accuracy of a KNN model using given parameters.
  assert_that(is.integer(folds))
  assert_that(is.data.frame(train))
  assert_that(is.numeric(ycol))
  assert_that(folds > 1 & folds < nrow(train)/2)
  assert_that(length(ycol) == nrow(train))
  require(tidyverse)
  #assert_that(is.integer(k))
  
  RMSEs <- numeric(folds)
  
  #create folds
  len <- nrow(train)
  train.folds <- split(train, rep(1:folds, each=ceiling(len/folds), length.out=len))
  y.folds <- split(ycol, rep(1:folds, each=ceiling(len/folds), length.out=len))
    
  for (i in 1:folds) {
    #create test and train sets
    test.subset <- train.folds[[i]]
    train.inds <- (1:folds)[-i]
    train.subset <- bind_rows(train.folds[train.inds])
    ycol.test.subset <- y.folds[[i]]
    ycol.train.subset <- unlist(y.folds[train.inds])
    
    #get predictions
    preds <- KNN.Predict(test = as.matrix(test.subset), ycol = ycol.train.subset, train = as.matrix(train.subset), ...)
    
    #measure accuracy
    RMSE <- sqrt(sum((preds - ycol.test.subset)^2)/len)
    RMSEs[i] <- RMSE
  }
  
  outp <- c(mean(RMSEs), sd(RMSEs))
  names(outp) <- c("Mean RMSE", "SD of RMSE")
  
  return(outp)
}
  
  
  
  
  