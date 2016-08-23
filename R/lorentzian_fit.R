#' Lorentzian/Cauchy Fit
#'
#' \code{fitCauchy} calculates the Lorentzian/Cauchy fit
#'
#' This function calculates the Lorentzian/Cauchy fit of the provided data. It
#' uses \code{\link[stats]{dcauchy}} to calculate the Lorentzian/Cauchy
#' profile and optimizes it. This version contains a y-offset optimization.
#'
#' @param x A numeric vector. The x-axis of the data to be fit.
#' @param y A numeric vector. The y-axis of the data to be fit.
#' @param center A numeric. A starting guess for the center of the Lorentzian.
#' @param hwhm A numeric. A starting guess for the half-width at half-max of
#' the Lorentzian.
#' @param height A numeric. A starting guess for the ampltidue scaling of the
#' Lorentzian.
#' @param yoff A numeric. A starting guess for the offset from the y-axis.
fitCauchy <- function(x, y, center, hwhm, height, yoff){

  f = function(p){
    d = p[3] * pi * p[2] * dcauchy(x, location = p[1], scale = p[2]) + p[4]
    sum((d - y) ^ 2)
  }

  optim(c(center, hwhm, height, yoff), f)
}

#' @rdname fitCauchy
fitCauchy2 <- function(x, y, center, hwhm, height){

  f = function(p){
    d = p[3] * pi * p[2] * dcauchy(x, location = p[1], scale = p[2])
    sum((d - y) ^ 2)
  }

  optim(c(center, hwhm, height), f)
}