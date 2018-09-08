# KNN Implementation

library(assertthat)

dist.euclidean.vector <- function(x, y) {
  # Calculates the euclidean distance between two vectors
  assertthat::assert_that(is.numeric(x))
  assertthat::assert_that(is.numeric(y))
  assertthat::assert_that(length(x)==length(y))
  assertthat::assert_that(length(x)>0)
  assertthat::assert_that(sum(is.na(x)) == 0)
  assertthat::assert_that(sum(is.na(y)) == 0)
  return (sqrt(sum((x - y) ^ 2)))
}

dist.hamming.vector <- function(x, y) {
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


