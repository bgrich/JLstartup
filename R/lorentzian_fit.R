#' Gaussian Fit
#'
#' \code{fitG} calculates the Gaussian fit
#'
#' This function calculates the Gaussian fit of the provided data. It uses
#' \code{\link[stats]{dnorm}} calculate the Gaussian profile and optimizes
#' it. This version contains a y-offset optimization.
#'
#' @param x A numeric vector. The x-axis of the data to be fit.
#' @param y A numeric vector. The y-axis of the data to be fit.
#' @param mu A numeric. A starting guess for the mean of the Gaussian.
#' @param sig A numeric. A starting guess for the standard deviation of the
#' Gaussian.
#' @param scale A numeric. A starting guess for the ampltidue scaling of the
#' Gaussian.
#' @param yoff A numeric. A starting guess for the offset from the y-axis.
fitCauchy <- function(x, y, center, hwhm, height, yoff){

  f = function(p){
    d = p[3] * pi * p[2] * dcauchy(x, location = p[1], scale = p[2]) + p[4]
    sum((d - y) ^ 2)
  }

  optim(c(center, hwhm, height, yoff), f)
}

fitCauchy2 <- function(x, y, center, hwhm, height){

  f = function(p){
    d = p[3] * pi * p[2] * dcauchy(x, location = p[1], scale = p[2])
    sum((d - y) ^ 2)
  }

  optim(c(center, hwhm, height), f)
}