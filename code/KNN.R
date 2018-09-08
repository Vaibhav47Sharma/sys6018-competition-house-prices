# KNN Implementation

library(assertthat)

Dist.euclidean.vector <- function(x, y) {
  # Calculates the euclidean distance between two vectors
  assertthat::assert_that(is.numeric(x))
  assertthat::assert_that(is.numeric(y))
  assertthat::assert_that(length(x)==length(y))
  assertthat::assert_that(length(x)>0)
  assertthat::assert_that(sum(is.na(x)) == 0)
  assertthat::assert_that(sum(is.na(y)) == 0)
  return (sqrt(sum((x - y) ^ 2)))
}

Dist.hamming.vector <- function(x, y) {
  # Calculates the hamming distance between two vectors
  assertthat::assert_that(is.numeric(x))
  assertthat::assert_that(is.numeric(y))
  assertthat::assert_that(length(x)==length(y))
  assertthat::assert_that(length(x)>0)
  assertthat::assert_that(sum(is.na(x)) == 0)
  assertthat::assert_that(sum(is.na(y)) == 0)
  difference <- x - y
  return (length(difference[difference != 0]))
}

Dist.chi.vector <- function(x, y) {
  # Calculates the chi squared distance between two vectors
  assertthat::assert_that(is.numeric(x))
  assertthat::assert_that(is.numeric(y))
  assertthat::assert_that(length(x)==length(y))
  assertthat::assert_that(length(x)>0)
  assertthat::assert_that(sum(is.na(x)) == 0)
  assertthat::assert_that(sum(is.na(y)) == 0)
  
  sum.vector <- x + y
  euclid.distance.vector <- (x - y) ^ 2
  sum.vector <- ifelse(sum.vector != 0, euclid.distance.vector / sum.vector, 0)
  
  return (sum(sum.vector)/2)
}

DistKNN <- function(x, y, dm = "euclid") {
  if (dm == "euclid") {
    return(dist.euclidean.vector(x, y))
  #} else if (dm == "chisq") {
  #  return(dist.chisq.vector(x, y))
  } else {
    stop("Distance metric name not recognized")
  }
}

FindKNearestNeighbors <- function(x, y, train, k, dm = "euclid", distweight = F) {
  # x is the test vector of length ncol(train)
  # y is a vector of response values of same length as nrow(train)
  # train is the full matrix of training data
  # k is the number of nearest neighbors
  # dm is the name of the distance method
 
  assert_that(is.matrix(train))
  assert_that(is.numeric(x))
  assert_that(is.numeric(y))
  assert_that(is.integer(k))
  assert_that(length(y) == nrow(train))
  assert_that(length(x) == ncol(train))
  assert_that(k >= 1 & k < nrow(train))

  distances <- numeric(nrow(train))
  response.values <- numeric(nrow(train)) # this assumes continuous response
  
  # Find distance of each neighbor
  for (i in 1:nrow(train)) {
    distances[i] <- DistKNN(x, as.numeric(train[i,]))
    response.values[i] <- y[i]
  }
  
  # Get top k neighbors
  ord <- order(distances)
  top.k.resp <- response.values[ord][1:k]
  top.k.dist <- distances[ord][1:k]
  
  # Find (weighted) average output
  if (distweight == F) {
    outp <- mean(top.k.resp)
  } else {
    if (all(top.k.dist) == 0) {
      # Weighting irrelevant, ignore.
      outp <- mean(top.k.resp)
    }
    if (any(top.k.dist == 0)) {
      # Prevent divide by 0
      top.k.dist[which(top.k.dist == 0)] <- min(top.k.dist[top.k.dist != 0])
    }
    # Mean weighted by inverse of distance
    outp <- sum(top.k.resp*(1/top.k.dist))/sum((1/top.k.dist))
  }
  return(outp)
}
