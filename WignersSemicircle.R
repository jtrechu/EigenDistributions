library(ggplot2)

##Random Sample Generator of Gaussian Orthogonal Ensembles
rGOE <- function(n,sigma,seed = NA){
  if (!(is.na(seed))) {set.seed(seed)}
  H <- matrix(rnorm(n^2),nrow=n,ncol = n)
  GOE <- sigma/sqrt(2*n)*(H+t(H))
  return(GOE)
}
##Numerical Simulation for Wigner's Semicircle Law
sigma = 6
n = 300
H = rGOE(n,sigma,123)
eigenvalues <- (eigen(H,only.values = T)$values)


semicircle <- function(x) {
  y <- sqrt(pmax(0, 4*sigma^2- x^2)) / (sigma^2 * 2*pi)
  return(y)
}
hist(eigenvalues,freq=F,main = paste("Numerical verification of Wigner's Semicircle Law for n =", as.character(n)) )
curve(semicircle, from = min(eigenvalues)-5, to = max(eigenvalues)+5, add = TRUE, col = "red", lwd = 2)

