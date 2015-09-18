## 8.3.
## Replacing Inf by the columnmax in a matrix, then replace NA by column-mean
x <- matrix(c(3,1,Inf,2,Inf,6,7,Inf,NA,NA,Inf,NA,6,7,8),nrow=5) # A sample matrix
## x <- matrix(c(NA,NA,NA,Inf,Inf,Inf,NA,4,2,Inf,6,3),nrow = 3)    

y <- is.infinite(x)
z <- is.na(x)
x[y] <- -Inf
col.max <- apply(x,MARGIN = 2,FUN = max,na.rm=TRUE) # column-max without Inf
tmp <- matrix(col.max,nrow=nrow(x),ncol=ncol(x),byrow = TRUE)
x[y] <- tmp[y]
col.mean <- colMeans(x,na.rm = TRUE)    # column-mean after replace Inf by column-max
tmp1 <- matrix(col.mean,nrow=nrow(x),ncol=ncol(x),byrow = TRUE)
x[z] <- tmp1[z]                         # Now the matrix x is what you need

## 8.15
getwd()
x <- read.table("excess return.txt", header = TRUE)
pr.x <- princomp(x,cor=TRUE)
pr.x0 <- princomp(x,cor=FALSE)
