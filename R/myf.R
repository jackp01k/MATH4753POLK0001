#' My function of squares and cubes
#'
#' Produce list of squares and cubes
#'
#' This is a part of the introduction to package making
#'
#' @param x a vector
#'
#' @return a list of vectors
#' @export
#'
#' @examples
#' x = 1:4;myf(x)
myf = function(x) {
  obj1 = x^2 # square a vector
  obj2 = x^3
  
  list(square = obj1, cube = obj2)
}