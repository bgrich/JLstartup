#' Gaussian Fit
#'
#' \code{fitG} calculates the Gaussian fit
#'
#' This function calculated the Gaussian fit of the provided data.


fitG <- function(x, y, mu, sig, scale, yoff){

  f = function(p){
    d = p[3] * sqrt(2 * pi) * p[2] * dnorm(x, mean = p[1], sd = p[2]) + p[4]
    sum((d - y) ^ 2)
  }

  optim(p = c(mu, sig, scale, yoff), f)
}







