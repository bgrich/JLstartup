#' Width Measurement (FWHM and HWHM)
#'
#' Returns the width of a given spectrum.
#'
#' These functions provide
##Determines the Full-Width at Half-Maximum (FWHM) of a peak

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

#Determines the Half-Width at Half-Maximum from the maximum to the half-max on the left side (lower index side)

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

#Determines the Half-Width at Half-Maximum from the maximum to the half-max on the right side (higher index side)

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
