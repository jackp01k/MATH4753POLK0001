#' My function of z scores
#'
#' Produces a vector of z scores for a given data set
#'
#' This is a part of the lab 4 requirement, but it is from lab 2
#'
#' @param x a dataset
#'
#' @return a list of vectors
#' @export
#'
#' @examples
#' x = 1:57 ; zvals(x)
zvals <- function(x) {
  z = (x-mean(x))/sd(x)
}