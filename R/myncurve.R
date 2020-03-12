#' My function of creating a normal distribution plots
#'
#' Produces cumulative normal density plots and its area.
#'
#' This is a part of the lab 6 function requirement
#'
#' @param mu, the mean
#' @param sigma, the standard deviation
#' @param a, a sample point
#'
#' @return a probability and a density plot
#' @export
#'
#' @examples
#' myncurve(0,1,2)
myncurve <- function(mu, sigma, a) {
  subtitle.m = paste("mean=", mu, sep = " ")
  subtitle.s = paste("sd=", sigma, sep = " ")
  subtitle = paste(subtitle.m, subtitle.s, "\n")
  curve(dnorm(x,mean = mu, sd = sigma), xlim = c(mu-3*sigma, mu+3*sigma),
        xlab = "",
        ylab = "Normal Density",
        main = "myncurve Normal Distribution",
        sub = subtitle)
  xcurve = seq(-1000,a,length = 10000)
  ycurve = dnorm(xcurve, mean = mu, sd = sigma)
  polygon(c(-1000, xcurve, a), c(0, ycurve,0), col = "red", lwd = 3)

  prob = pnorm(a, mu, sigma, TRUE)
  prob = round(prob, 4)
  prob
}
