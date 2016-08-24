#' Dipole-dipole Cusp Line Shape
#'
#' Returns a cusp-like line shape
#'
#' \code{dcusp} provides the cusp-like line shape used in
#' \href{http://journals.aps.org/pra/abstract/10.1103/PhysRevA.93.042505}{Dipole-dipole resonance line shapes in a cold Rydberg gas} by Richards and Jones.
#'
#' @param x a numeric. The position at which the distribution is calculated.
#' @param width a numeric. The width of the line shape. The width parameter is
#' related to the full-width at half-maximum (FWHM) by width = FWHM / 3.09.
#' @param xoff a numeric. The offset of the distribution along the x-axis.
dcusp <- Vectorize(function(x, width, xoff){
  A <- width / abs(x - xoff)
  if (A == Inf) {
    1 / 2
  } else {
    (A/2) * (gsl::Ci(A) * sin(A) + (pi / 2 - gsl::Si(A)) * cos(A))
  }
})

#' Cusp Line Shape Fit
#'
#' Performs a fit to the cusp line shape \code{\link{dcusp}}
#'
#' These functions perform fits of provided data to the cusp line shape
#' \code{\link{dcusp}} using \code{\link[stats]{optim}}. The base version
#' of \code{fitCusp} using a max iteration number of 300 in \code{optim}.
#' \code{fitCusp2} uses a max iteration number of 1000. \code{fitCusp3}
#' also uses a max iteration number of 1000, but removes the y-offset.
#'
#' @param x a numeric vector. The x coordinates to be fit.
#' @param y a numeric vector. The y coordinates to be fit.
#' @param height a numeric. An initial guess to the height of the distribution.
#' @param width a numeric. An initial guess to the width of the distribution.
#' @param xoff a numeric. An initial guess to the x-offset of the
#' distribution.
#' @param yoff a numeric. An initial guess to the y-offset of the
#' distribution.
fitCusp <- function(x, y, height, width, xoff, yoff){

  f = function(xdat, ydat, par){
    d = 2 * par[1] * dcusp(xdat, width = par[2], xoff = par[3]) + par[4]
    sum((d - ydat) ^ 2)
  }

  optim(par = c(height, width, xoff, yoff),
        f,
        control = list(maxit = 300),
        xdat = x,
        ydat = y)
}

#' @rdname fitCusp
fitCusp2 <- function(x, y, height, width, xoff, yoff){

  f <- function(xdat, ydat, par){
    d = 2 * par[1] * dcusp(xdat, width = par[2], xoff = par[3]) + par[4]
    sum((d - ydat) ^ 2)
  }

  optim(par = c(height, width, xoff, yoff),
        f,
        control = list(maxit = 1000),
        xdat = x,
        ydat = y)
}

#' @rdname fitCusp
fitCusp3 <- function(x, y, height, width, xoff){

  f <- function(xdat, ydat, par){
    d = 2 * par[1] * dcusp(xdat, width = par[2], xoff = par[3])
    sum((d - ydat) ^ 2)
  }

  optim(par = c(height, width, xoff),
        f,
        control = list(maxit = 1000),
        xdat = x,
        ydat = y)
}