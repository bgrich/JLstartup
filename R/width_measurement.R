#' Width Measurement (FWHM and HWHM)
#'
#' Returns the width of a given spectrum.
#'
#' These functions provide the full-width at half-maximum or the half-width
#' at half-maximum depending on the function. Given a two column matrix
#' describing the x and y positions of a spectrum, the function finds the
#' location of the maximum and finds the spot just below the half-max point
#' on either side of the peak. The function \code{\link[stats]{approx}} is
#' used to determine the actual location of the half-max point on either
#' side of the maximum. The separation of these two points is then computed
#' provided the full maximum. For the half-width functions, onlye the right
#' side is computed in \code{HWHMright} and only the left side is computed
#' in \code{HWHMleft}.
#'
#' @param spectrum a numeric matrix. The x and y coordinates of the function
#' whose width is to be measured.
FWHM <- function(spectrum){
  i <- which.max(spectrum[, 2])
  n <- length(spectrum[, 2])
  left <- ifelse(i < 1, 1, i)
  right <- ifelse(i > n, n, i)

  hm <- spectrum[i, 2] / 2

  while (left > 1 && spectrum[left, 2] > hm) {
    left <- left - 1
  }
  while (right < n && spectrum[right, 2] > hm) {
    right <- right + 1
  }

  xleft <- approx(x = spectrum[left:(left + 1), 2], y = spectrum[left:(left + 1), 1], xout = hm)$y
  xright <- approx(x = spectrum[(right - 1):right, 2], y = spectrum[(right - 1):right, 1], xout = hm)$y

  return(abs(xleft - xright))
}

#' @rdname FWHM
HWHMleft <- function(spectrum){
  i <- which.max(spectrum[, 2])
  maxpos <- spectrum[i, 1]
  n <- length(spectrum[, 2])
  left <- ifelse(i < 1, 1, i)
  right <- ifelse(i > n, n, i)

  hm <- spectrum[i, 2] / 2

  while (left > 1 && spectrum[left, 2] > hm) {
    left <- left - 1
  }

  xleft <- approx(x = spectrum[left:(left + 1), 2], y = spectrum[left:(left + 1), 1], xout = hm)$y

  return(abs(xleft - maxpos))
}

#' @rdname FWHM
HWHMright <- function(spectrum){
  i <- which.max(spectrum[, 2])
  maxpos <- spectrum[i, 1]
  n <- length(spectrum[, 2])
  left <- ifelse(i < 1, 1, i)
  right <- ifelse(i > n, n, i)

  hm <- spectrum[i, 2] / 2

  while (right < n && spectrum[right, 2] > hm) {
    right <- right + 1
  }

  xright <- approx(x = spectrum[(right - 1):right, 2], y = spectrum[(right - 1):right, 1], xout = hm)$y

  return(abs(maxpos - xright))
}

WidthMeasure <- function(spectrum) {
  width <- FWHM(spectrum)
  if (is.na(width)) {
    width <- 2 * HWHMleft(spectrum)
    if (is.na(width)) {
      width <- 2 * HWHMright(spectrum)
    }
  }
  width
}
