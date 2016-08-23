#' Derivative
#'
#' Take the first derivative of y(x)
#'
#' This function takes the first derivative of an arbitrary function y(x).
#' The spacing of the x-points do not need to be uniform for the function to
#' work. The function computes the derivative by calculating
#' y' = (y2 - y1)/(x2 - x1).
#'
#' @param x a numeric vector. The x values for the arbitrary function y(x).
#' @param y a numeric vector. The y values for the arbitrary function y(x).
Deriv1 <- function(x, y){
  y.prime <- diff(y) / diff(x)
  x.prime <- x[-length(x)] + diff(x) / 2
  list(x = x.prime,
       y = y.prime)
}

peaks <- function(series, span = 3){
  z <- embed(series, span)
  s <- span %/% 2
  v <- max.col(z, "first") == 1 + s   # take first if a tie
  result <- c(rep(FALSE, s), v)
  result <- result[1:(length(result) - s)]
  result
}