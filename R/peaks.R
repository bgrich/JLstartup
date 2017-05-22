#' Peaks
#'
#' \code{peaks} determines local maxima
#'
#' @param series A numeric vector. The data to search for local maxima.
#'
#' @export


peaks <- function(series,span=3)
{
  z <- embed(series, span)
  s <- span%/%2
  v <- max.col(z, "first") == 1 + s   # take first if a tie
  result <- c(rep(FALSE,s),v)
  result <- result[1:(length(result)-s)]
  result
}