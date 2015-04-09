## load data from mulltest
install.packages("multtest") ## failed, package removed from CRAN

## so I tried Bioconductor
source("http://bioconductor.org/biocLite.R")
biocLite("multtest")

## find the pooled covariance matrix and the mean difference vector
data(golub)
index <- c(1:27)
X0 <- golub[,index]
X1 <- golub[,-index]
mu0 <- apply(X0, 1, mean)
mu1 <- apply(X1, 1, mean)
X0.center <-  X0- mu0 
X1.center <-  X1 - mu1 
Sig <- (X0.center%*%t(X0.center)+X1.center%*%t(X1.center))/ncol(golub)
mu <- mu0 -mu1

min(eigen(Sig)$values)
## make matrix Sig p.d.


## orgininal dat
leuk <- read.table("/media/Learn/Dropbox/Research/datasets/leukemia_Golub/data/data_set_ALL_AML_independent.txt",sep="\t",quote="",header=T,row.names=NULL,comment.char="")


