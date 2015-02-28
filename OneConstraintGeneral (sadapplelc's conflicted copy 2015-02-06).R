## Function to simulate a p.d. matrix
# Generating a random positive-definite matrix with specified positive eigenvalues
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

## Function to update the inverse via Woodbury Formula when a index is entering the active set

InvUpdate1 <- function(A,Inv,act,nonact,index){

    index.vec <- A[act,index,drop=FALSE] # the covariance matrix values for the (act,index) block
    d <- length(act)
    vec1 <- c(rep(0,d),1)
    block1 <- cbind(diag(1,d),-Inv%*%index.vec)
    M1 <- rbind(block1,vec1)

    vec2 <- c(rep(0,d),1/(A[index,index]-as.numeric(t(index.vec)%*%Inv%*%index.vec)))
    block2 <- cbind(Inv,rep(0,d))
    M2 <- rbind(block2,vec2)

    Inv1 <- M1%*%M2%*%t(M1)
    k <- which(sort(c(act,index))==index)
    if (k==(d+1)){
        B <- Inv1
    } else {
        Inv1[c(k:(d+1)),] <- Inv1[c((d+1),k:d),]
        Inv1[,c(k:(d+1)) ] <- Inv1[,c((d+1),k:d)]
        B <- Inv1
    }
    return(B)
}

## Function to update the inverse via Woodbury Formula when a index is leaving the active set

InvUpdate2 <- function(A,Inv,act,nonact,index){
    index.vec <- A[-c(nonact,index),index,drop=FALSE]
    k <- which(act==index)
    block1 <- Inv[-k,-k,drop=FALSE] # One line resolves the previous issue
    block2 <- (block1%*%index.vec%*%t(index.vec)%*%block1)/(A[index,index]+as.numeric(t(index.vec)%*%block1%*%index.vec))
    B <- block1-block2
    return(B)
}


## Load quadratic prog package

library(quadprog)

## construct special case
library(lars)
a1 <- 1
a2 <- matrix(1,nrow=1)
m1 <- lars(a2,a1,"lasso",trace=TRUE,normalize = FALSE,intercept=FALSE)
## lambda=1,take alpha=1/6
a1 <- c(1,1)
a2 <- matrix(c(1,0,1/3,1/6),nrow=2)
m2 <- lars(a2,a1,"lasso",trace=TRUE,normalize = FALSE,intercept=FALSE)
## lambda=  0.05882353, take alpha= 0.005
a1 <- c(1,1,1)
a2 <- matrix(c(1,0,0,1/3,1/6,0,0.01,0.01,0.005),nrow=3)
m3 <- lars(a2,a1,"lasso",trace=TRUE,normalize = FALSE,intercept=FALSE)
## lambda = 0.002247191, take alpha =0.0001
a1 <- c(1,1,1,1)
a2 <- matrix(c(1,0,0,0,1/3,1/6,0,0,0.01,0.01,0.005,0,0.0002,0.0002,0.0002,0.0001),nrow=4)
m4 <- lars(a2*100,a1*10,"lasso",trace=TRUE,normalize = FALSE,intercept=FALSE)
## lambda=4.771448e-02, take alpha=0.002
a1 <- c(rep(1,5))
a2 <-  matrix(c(1,0,0,0,0,1/3,1/6,0,0,0,0.01,0.01,0.005,0,0,0.0002,0.0002,0.0002,0.0001,0,rep(0.004,4),0.002),nrow=5)
m5 <- lars(a2,a1,"lasso",trace=TRUE,normalize = FALSE,intercept=FALSE)


## Simulation

# Sig <- Posdef(n=10)

Sig <- t(a2)%*%a2 # Design matrix for complexity extreme case


ptm <- proc.time()

## Solve initial problem

# mu <- sample(c(1:nrow(Sig)),nrow(Sig),replace = FALSE) # specify the constraint vector
#mu <- runif(nrow(Sig),0.5,2) 
  eta <- 1000
  mu <- t(a2)%*%a1/eta ## for complexity
  eigen(Sig-eta*mu%*%t(mu)) # check positive definite or not


nonsparse_ind <- which(mu==max(mu))
sparse_ind <- which(mu!=max(mu))

beta_0 <- solve.QP(Dmat=Sig,dvec=rep(0,nrow(Sig)),Amat=matrix(mu),bvec=1,meq=1)
beta <- beta_0$solution
beta.mat <- beta
lamda <- 0
lamda.vec <- 0

## The initial active set
act <- which(beta!=0)
nonact <- which(beta==0)
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

## Other initialized values

muq <- mu[act]
mupq <- mu[nonact]
Sig.inv <- solve(test1)
a <- as.numeric(t(muq)%*%Sig.inv%*%sub[act])
b <- as.numeric(t(muq)%*%Sig.inv%*%muq)
avec <- Sig.inv%*%sub[act]
bvec <- Sig.inv%*%muq
c <- as.vector((a/b)*bvec-avec)
d <- (a/b)*mupq-as.vector(test21%*%c)-sub[nonact]


### Between 0->nonzero, nonzero->0, find which would occur first and which specific coordinate does the job

repeat {

if (length(act)!=length(beta)){

## Find the index giving smallest delta lamda under nonzero->0 situation

selt1 <- (-2)*beta[act]/c
pos.index1 <- which(selt1>0)
selt1.pos <- selt1[which(selt1>0)]
delta.lamda1 <- selt1.pos[which.min(selt1.pos)]
index1 <- act[pos.index1[which.min(selt1.pos)]]

if (length(selt1.pos)==0) {
    delta.lamda1 <- Inf           ## consider the case which all indices in act can not make beta reach zero in this step 
}




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


##### Determine which situation happens and update beta, lamda, subgradient and the active set
## This part is not rigorous need to modify
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
beta[index] <- 0  ## Force the index component of the beta to be zero, otherwise floating point issue might be severe
beta.mat <- cbind(beta.mat,beta)
sub[nonact] <- sub[nonact]+(delta.lamda/(delta.lamda+lamda))*d ##Problematic?
sub.mat <- cbind(sub.mat,sub)


## update lamda, the inverse matrix and the active set

lamda <- lamda+delta.lamda
lamda.vec <- c(lamda.vec,lamda)
if (length(act)!=length(beta)) {
    if (delta.lamda1>delta.lamda2) {

        ## Inverse Matrix Update via Sherman Morrison Formula

        Sig.inv <- InvUpdate1(A=Sig,Inv=Sig.inv,act=act,nonact=nonact,index=index)

        ## Update active set
        act <- sort(c(act,index))
        nonact <- nonact[-which(nonact==index)]
    } else {
        ## Inverse Matrix Update

        Sig.inv <- InvUpdate2(A=Sig,Inv=Sig.inv,act=act,nonact=nonact,index=index)


        ## Update active set
        act <- act[-which(act==index)]
        nonact <- sort(c(nonact,index))
    }
} else {
        ## Inverse Matrix Update

        Sig.inv <- InvUpdate2(A=Sig,Inv=Sig.inv,act=act,nonact=nonact,index=index)


        ## Update active set
        act <- act[-which(act==index)]
        nonact <- sort(c(nonact,index))

}


## Other Matrices Update

test21 <- Sig[nonact,act,drop=FALSE]
# Sig.inv <- solve(test1)  ## Direct Approach for the Inverse Update


## Other updates

sub1 <- sub[act]
sub2 <- sub[nonact]

beta1 <- beta[act]
beta2 <- beta[nonact]


## idq <- rep(1,length(act))
## idpq <- rep(1,length(nonact))
## a <- as.numeric(t(idq)%*%Sig.inv%*%sub[act])
## b <- as.numeric(t(idq)%*%Sig.inv%*%idq)
## avec <- Sig.inv%*%sub[act]
## bvec <- Sig.inv%*%idq
## c <- as.vector((a/b)*bvec-avec)
## d <- (a/b)*idpq-as.vector(test21%*%c)-sub[nonact]


muq <- mu[act]
mupq <- mu[nonact]
a <- as.numeric(t(muq)%*%Sig.inv%*%sub[act])
b <- as.numeric(t(muq)%*%Sig.inv%*%muq)
avec <- Sig.inv%*%sub[act]
bvec <- Sig.inv%*%muq
c <- as.vector((a/b)*bvec-avec)
d <- (a/b)*mupq-as.vector(test21%*%c)-sub[nonact]




## Stopping rule


if (all(beta[sparse_ind]==0)==TRUE) break

## Modifications
# if (all(isTRUE(all.equal(beta[sparse_ind],0)))==TRUE) break  
# if (all(beta1>0)==TRUE) break  # previous stopping rule for all 1 constraint


}

## Ploting

## Original approach 
## plot(rep(0,nrow(beta.mat)),beta.mat[,1],pch=21,cex=.5,xlim=c(0,lamda.vec[length(lamda.vec)]),xlab=expression(lambda),ylab="Coefficients")
## for (i in 1:nrow(beta.mat)){
##   lines(lamda.vec,beta.mat[i,])
## }

## for (i in 1:nrow(beta.mat)){
##   points(lamda.vec,beta.mat[i,],pch=21,cex=.5,col="red")
## }

## for (i in 1:length(lamda.vec)){
##   abline(v=lamda.vec[i],col="blue")
## }

## Plot in equal width
y.max <- max(apply(beta.mat,2,max))
y.min <- min(apply(beta.mat,2,min))
y.med <- max(c(y.max,abs(y.min)))^(1)
plot(rep(0,nrow(beta.mat)),log(abs(beta.mat[,1]^(1))+1)*sign(beta.mat[,1]),pch=21,cex=.5,xlim=c(0,length(lamda.vec)-1),ylim =c(-12,20),xlab="kinks",ylab="Coefficients",main="Regularization Path, p=4")
for (i in 1:nrow(beta.mat)){
  lines(c(0:(length(lamda.vec)-1)),log(abs(beta.mat[i,]^(1))+1)*sign(beta.mat[i,]))
}

for (i in 1:nrow(beta.mat)){
  points(c(0:(length(lamda.vec)-1)),log(abs(beta.mat[i,]^(1))+1)*sign(beta.mat[i,]),pch=21,cex=.5,col="red")
}

abline(h=0,col="blue")  ## draw the horizontal zero line


proc.time() - ptm
