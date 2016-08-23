#Dipole-dipole cusp form distribution
dcusp <- Vectorize(function(x, width, xoff){
  A <- width / abs(x - xoff)
  if (A == Inf) {
    1 / 2
  } else {
    (A/2) * (Ci(A) * sin(A) + (pi / 2 - Si(A)) * cos(A))
  }
})

##Fit for Cusp Lineshape
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

#Fit for cusp lineshape with no y offset
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