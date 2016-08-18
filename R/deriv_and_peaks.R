# First derivative.  Adjust x values to be center of interval.
# Spacing of x-points need not be uniform
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