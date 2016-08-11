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