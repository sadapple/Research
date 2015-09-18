## This script tries to implement the LPD proposed by Tony Cai
## the fastclime package
install.packages("fastclime")
library(fastclime)
# Load the package
L = fastclime.generator(n=200,d=50,graph="hub")
# Generate data with hub structures
out = fastclime(L$data,0.1)
# Estimate the solution path
plot(out)
# Plot the sparsity level information from the graph path
out1 = fastclime.lambda(out$lambdamtx, out$icovlist, 0.2)
# Finds the estimated path corrsponding to lambda=0.2


## paralp function
A=matrix(c(-1,-1,0,1,-2,1),nrow=3)
b=c(-1,-2,1)
c=c(-2,3)
b_bar=c(1,1,1)
c_bar=c(1,1)
paralp(c,A,b,c_bar,b_bar)

## matrix generate
Posdef <- function (n, ev = runif(n, 0, 50))
{
  Z <- matrix(ncol=n, rnorm(n^2))
  decomp <- qr(Z)
  Q <- qr.Q(decomp)
  R <- qr.R(decomp)
  d <- diag(R)
  ph <- d / abs(d)
  O <- Q %*% diag(ph)
  Z <- t(O) %*% diag(ev) %*% O
  return(Z)
}

## examples
n <- 50
delta <- seq(1,n)
Sig <- Posdef(n)
A <- cbind(Sig,-Sig)
A <- rbind(A,-A)
# b <- c(rep(1,n),rep(-1,n))  ## b correspond to the constraint vector in ROAB problem
b <- c(delta,-delta)
b_bar <- rep(1,2*n)
c <- rep(-1,2*n)
c_bar <- rep(0,2*n)

a <- 20
beta <- paralp(c,A,b,c_bar,b_bar)
beta1 <- paralp(c,A,b,c_bar,b_bar,lambda = a)
betaplus <- beta[1:n]
betaminus <- beta[(n+1):(2*n)]
beta1plus <- beta1[1:n]
beta1minus <- beta1[(n+1):(2*n)]


test <- (betaplus>0)&(betaminus>0)
sum(beta)-sum(beta1)
