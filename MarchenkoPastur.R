##Numerical Simulation for Marchenko-Pastur's Semicircle Law
N<-2800
n <- 2900
M <- diag(1/n,nrow=N)
W <- rWishart(1,n,M)[,,1]
eigenvalues <- (eigen(W,only.values = T)$values)
lmax <- (1+sqrt(n/N))^2
lmin <- (1-sqrt(n/N))^2
marcenko <- function(x) {
  y <- sqrt(pmax(0, (lmax-x)*(x-lmin))) / (2*pi*x*(n/N))
  return(y)
}

hist(eigenvalues,freq=F,main = paste("Numerical verification of Marcenko-Pastur Law for n =", as.character(n)) )
curve(marcenko, from = lmin, to = lmax, add = TRUE, col = "red", lwd = 2)


