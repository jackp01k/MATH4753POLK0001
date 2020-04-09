#' My function to create a 95% confidence interval
#'
#' This is part of the lab 11 function requirement
#'
#' @param x a vector representation of a sample
#'
#' @return a confidence interval
#' @export
#'
#' @examples
#' x = rnorm(30, mean =10, sd = 12); myci(x)
myci <- function (x) {
  #calculate mean
  m = mean(x)

  #calculate sd
  s = sd(x)

  #statement of alpha
  a = 0.95

  #calculate number of samples
  l = length(x)

  t=qt(a, l)
  z=qnorm(a)

  ci = c()
  if(length(x) >= 30) {
    ci[1]=round(mean(d)-z*sd(d)/sqrt(l), 4)
    ci[2]=round(mean(d)+z*sd(d)/sqrt(l), 4)

    #output Z
    paste("Using Z, the ci is", ci, sep = " ")
  }
  else {
    ci[1]=round(mean(d)-t*sd(d)/sqrt(l),4)
    ci[2]=round(mean(d)+t*sd(d)/sqrt(l),4)

    #output T
    paste("Using T, the ci is", ci, sep = " ")
  }
}
