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



