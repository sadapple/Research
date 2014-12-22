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


## Comparison
## Two approach to compute the inverse of a submatrix with the inverse information of the whole matrix 

InvUpdate1 <- function(A,act,nonact,index){ # Directly use solve() function

    act <- act[-which(act==index)]
    nonact <- sort(c(nonact,index))
    C <- solve(A[act,act,drop=FALSE])
    return(C)
}

InvUpdate2 <- function(A,act,nonact,index){ # Sherman Formula
    
    k <- which(act==index)
    index.vec <- A[-c(nonact,index),index,drop=FALSE]
    Inv <- solve(A[act,act,drop=FALSE])
    ## Inv[,c(o1,o2)] <- Inv[,c(o2,o1)]    # These two line of codes fail to keep the order structure of the set act.
    ## Inv[c(o1,o2),] <- Inv[c(o2,o1),]    # Main problems detected here
    
    block3 <- Inv[-k,-k,drop=FALSE] # One line resolves the previous issue
    block4 <- (block3%*%index.vec%*%t(index.vec)%*%block3)/(A[index,index]+as.numeric(t(index.vec)%*%block3%*%index.vec))
    B <- block3-block4
    return(B)
}


A <- matrix(c(5,2,2,1),nrow = 2)
A <- Posdef(n=10)
act <- c(1:7)
nonact <- c(8:10)
index <- 3

InvUpdate1(A=A,act=act,nonact = nonact,index=index)
InvUpdate2(A=A,act=act,nonact = nonact,index=index)



### simulation
## Scene 1

o2 <- d1+1
Inv1[,c(o1,o2)] <- Inv1[,c(o2,o1)]
Inv1[c(o1,o2),] <- Inv1[c(o2,o1),]
Inv <- Inv1

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


## Scene 2
d1 <- length(act)
o1 <- which(act==index)
o2 <- d1
index.vec2 <- A[-c(nonact,index),index,drop=FALSE]
Inv[,c(o1,o2)] <- Inv[,c(o2,o1)]
Inv[c(o1,o2),] <- Inv[c(o2,o1),]

block3 <- Inv[c(1:(d1-1)),c(1:(d1-1))]
block4 <- (block3%*%index.vec2%*%t(index.vec2)%*%block3)*(1/(A[index,index]+as.numeric(t(index.vec2)%*%block3%*%index.vec2)))
Inv <- block3-block4

InvUpdate2 <- function(A,Inv,act,nonact,index){
    index.vec <- A[-c(nonact,index),index,drop=FALSE]
    k <- which(act==index)
    block1 <- Inv[-k,-k,drop=FALSE] # One line resolves the previous issue
    block2 <- (block1%*%index.vec%*%t(index.vec)%*%block1)/(A[index,index]+as.numeric(t(index.vec)%*%block1%*%index.vec))
    B <- block1-block2
    return(B)
}


## Inv <- Inv[c(1:(d1-1)),c(1:(d1-1))]- (Inv[c(1:(d1-1)),c(1:(d1-1))]%*%index.vec2%*%t(index.vec2)%*%Inv[c(1:(d1-1)),c(1:(d1-1))])*(1/(A[index,index]+as.numeric(t(index.vec2)%*%Inv[c(1:(d1-1)),c(1:(d1-1))]%*%index.vec2)))


