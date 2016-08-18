#' MOT Density Calculation
#'
#' \code{MOTDen} calculates the density of Rydberg atoms in the MOT.
#'
#' This function calculates the density of Rydberg atoms in the MOT for a
#' given MOT trace. It takes a data frame and, using
#' \code{\link[caTools]{trapz}} and \code{\link{fitG}}, computes the
#' integral of the MOT fluorescence and it's full-width at half-maximum
#' (FWHM). To get the total integral of the MOT, it is assumed that the MOT is
#' a Gaussian in all dimensions. It then calculates the total number of atoms
#' by converting the integral of the MOT fluorescence into the power and
#' dividing that by the power per photon emitted. The number of Rydberg atoms
#' takes the total number of atoms and divides it by 3.5. This is a compromise
#' between cases where 1/3 of the atoms enter the Rydberg state and 1/4 of
#' the atoms enter the Rydberg state.
#'
#' The function returns the density in atoms/cm\eqn{^{-1}}.
#'
#' Note: This is only for excitation to a single Rydberg state! For
#' excitation to multiple states, the number of Rydberg atoms compared to the
#' total number of atoms needs to be adjusted.
#'
#' This is an R implementation of the pcamera function/script written by Mary
#' Kutteruf for Matlab.

MOTDen <- function(MOTDataFrame){
  #Converts the MOT file into a dplyr data frame
  MOT <- tibble::as_tibble(MOTDataFrame)

  #Adds a column with the index
  MOT <- MOT %>%
    dplyr::mutate(index = c(1:length(Time)))

  #Convert time from seconds to microseconds
  MOT$Time <- MOT$Time * 1e6

  #Filters the MOT data frame such that only rows with index greater or equal to 140 are included
  MOT <- filter(MOT, index >= 140)

  initialXOff <- MOT$Time[which.max(MOT$Signal)]
  initialYOff <- mean(MOT$Signal[which(MOT$Time > 22)])
  InitialA <- max(MOT$Signal)

  #Performs a Gaussian fit on the MOT trace (Originally 20,0.2,0.05,0.03)
  GaussFit <- fitG(MOT$Time, MOT$Signal, initialXOff, 0.2, InitialA, initialYOff)

  #Saves the fit parameters to specific variables
  A <- GaussFit$par[3]
  c <- GaussFit$par[2]
  xoffset <- GaussFit$par[1]
  yoffset <- GaussFit$par[4]

  #Creates a curve for the gaussian fit
  FitCurve <- A * sqrt(2 * pi) * c * dnorm(MOT$Time, mean = xoffset, sd = c) + yoffset

  #Plots the original data as points and the fitted curve as an overlaid line
  plot(MOT$Time, MOT$Signal)
  lines(MOT$Time, FitCurve, col = 2)

  #Subtracts the y offset value from the fitted curve
  FitWOBackground <- FitCurve - yoffset

  #Selected the starting and ending indices for the peak to be integrated
  startind <- min(which(FitWOBackground > 0.05 * max(FitWOBackground))) - 15
  endind <- max(which(FitWOBackground > 0.05 * max(FitWOBackground))) + 15
  range <- c(startind:endind)

  #Performs an trapezoidal integration of the fitted curve without the y offset
  rI <- trapz(MOT$Time[range], FitWOBackground[range])

  #rI is just the integral of a single line of the MOT as read by the camera.
  #To get the integral of the entire MOT, we assume that the camera line is
  #some surface at y=0 and that the MOT is a symmetric Gaussian.
  #We can determine the integral of the MOT in the xdirection by scaling
  #normalizing the fitted curve to 1 and integrating once more.
  rDeltax <- trapz(MOT$Time[range], FitWOBackground[range] / max(FitWOBackground))

  #We get the total integral by multiplying the integral in the x direction and in the y (rDeltax and rI respectively)
  rItot <- rDeltax * rI

  #The total number of atoms
  Na <- 1.19e6 * rItot  #Calibration from May 2015
  #   Na <- 3.88e5*rItot #New calibration as of November 2014
  #   Na <- 1.07e6*rItot

  #The total number of Rydberg atoms. For three levels (ground state, intermediate state and Rydberg State)
  #the total number of Rydberg atoms will be between 1/3 and 1/4 the total atoms.
  #1/3.5 is a compromise between those two numbers
  NRyd <- Na / 3.5

  #Half-width at half-max of the Gaussian in cm
  w <- sqrt(2 * log(2)) * c * 0.07

  #Volume of the MOT
  V <- 4 * pi * w ^ 3 / 3

  #Rydberg Density
  den <- NRyd/V

  den
}