## Function to simulate a p.d. matrix

## Use package
##Sig<-genPositiveDefMat(dim=3)
##Sig

# Alternative: Generating a random positive-definite matrix with user-specified positive eigenvalues
# If eigenvalues are not specified, they are generated from a uniform distribution

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

 

## Simulation
Sig <- Posdef(n=200)

## Solve initial problem
library(quadprog)

ptm1 <- proc.time()

beta_0 <- solve.QP(Dmat=Sig,dvec=rep(0,nrow(Sig)),Amat=matrix(rep(1,nrow(Sig))),bvec=1,meq=1)
beta <- beta_0$solution
beta
beta.mat <- beta
lamda <- 0
lamda.vec <- 0

## The initial active set
act <- which(beta!=0)
nonact <- which(beta==0)
act
test <- Sig[c(act,nonact),c(act,nonact),drop=FALSE]
test1 <- Sig[act,act,drop=FALSE]
test21 <- Sig[nonact,act,drop=FALSE]

## Determine the initial subgradient of the solution
beta1 <- beta[act]
beta2 <- beta[nonact]
beta1
beta2

sub <- beta
sub[which(beta>0)] <- 1
sub[which(beta<0)] <- -1
sub1 <- sub[act]
sub2 <- sub[nonact]
sub.mat <- sub


## while (all(sub1==1)==FALSE){
### Between 0->nonzero, nonzero->0, find which would occur first and which specific coordinate does the job
repeat {


idq <- rep(1,length(act))
idpq <- rep(1,length(nonact))
Sig.inv <- solve(test1)
a <- as.numeric(t(idq)%*%Sig.inv%*%sub[act])
b <- as.numeric(t(idq)%*%Sig.inv%*%idq)
avec <- Sig.inv%*%sub[act]
bvec <- Sig.inv%*%idq
c <- as.vector((a/b)*bvec-avec)
d <- (a/b)*idpq-as.vector(test21%*%c)-sub[nonact]


if (length(act)!=length(beta)){

## Find the index giving smallest delta lamda under nonzero->0 situation

selt1 <- (-2)*beta[act]/c
pos.index1 <- which(selt1>0)
selt1.pos <- selt1[which(selt1>0)]
delta.lamda1 <- selt1.pos[which.min(selt1.pos)]
index1 <- act[pos.index1[which.min(selt1.pos)]]

## Find the index giving smallest delta lamda under 0->nonzero situation
selt2 <- rep(1,length(nonact))
for (i in 1:length(nonact)) {
  if (d[i]>0&&d[i]>=(1-sub2[i])){     #### d[i]>(1-sub2[i]) would be more accurate
     selt2[i] <- lamda/(d[i]/(1-sub2[i])-1)
  } else if (d[i]<0&&(-d[i]>=(1+sub2[i]))) {
     selt2[i] <- lamda/(-d[i]/(1+sub2[i])-1)
  }
  else {selt2[i] <- Inf}
}
delta.lamda2 <- min(selt2)
index2 <- nonact[which.min(selt2)]


##### Determine which situation happen and update beta, lamda, subgradient and the active set

if (delta.lamda1>delta.lamda2){
  delta.lamda <- delta.lamda2;
  index <- index2
 } else {
  delta.lamda <- delta.lamda1;
  index <- index1
 }
##
} else {

  selt <- (-2)*beta[act]/c
  pos.index <- which(selt>0)
  selt.pos <- selt[which(selt>0)]
  delta.lamda <- selt.pos[which.min(selt.pos)]
  index <- act[pos.index[which.min(selt.pos)]]
}

## updating beta and subgradient
beta[act] <- beta[act]+1/2*delta.lamda*c
beta.mat <- cbind(beta.mat,beta)
sub[nonact] <- sub[nonact]+(delta.lamda/(delta.lamda+lamda))*d ##Problematic?
sub.mat <- cbind(sub.mat,sub)


## update lamda and the active set
lamda <- lamda+delta.lamda
lamda.vec <- c(lamda.vec,lamda)
if (length(act)!=length(beta)){
    if (delta.lamda1>delta.lamda2){
      act <- sort(c(act,index))
      nonact <- nonact[-which(nonact==index)]
    } else {
      act <- act[-which(act==index)]
      nonact <- sort(c(nonact,index))
    }
} else {
  act <- act[-which(act==index)]
  nonact <- sort(c(nonact,index))
}


## other updates
test <- Sig[c(act,nonact),c(act,nonact),drop=FALSE]
test1 <- Sig[act,act,drop=FALSE]
test21 <- Sig[nonact,act,drop=FALSE]

sub1 <- sub[act]
sub2 <- sub[nonact]

beta1 <- beta[act]
beta2 <- beta[nonact]


if (all(beta1>0)==TRUE) break
}

delta.lamda1
d
nonact
delta.lamda2
lamda.vec

### Ploting

plot(rep(0,nrow(beta.mat)),beta.mat[,1],pch=21,cex=.5,xlim=c(0,lamda.vec[length(lamda.vec)]),xlab=expression(lambda),ylab="Coefficients")
for (i in 1:nrow(beta.mat)){
  lines(lamda.vec,beta.mat[i,])
}

for (i in 1:nrow(beta.mat)){
  points(lamda.vec,beta.mat[i,],pch=21,cex=.5,col="red")
}

for (i in 1:length(lamda.vec)){
  abline(v=lamda.vec[i],col="blue")
}


proc.time() - ptm1


## Comparison with the Matrix Update version, avoid to use solve() to find inverse
par(mfrow = c(2,1))
