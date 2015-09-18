## 5.28
rw <- function(nsteps) {
    steps <- sample(c(-1,1),nsteps,
                    replace=TRUE)
    cumsum(steps)
}

## 5.11.
## dist of sample median
x <- replicate(10000,median(rnorm(15))) ## simulate 10000 sample median of norm size 15

plot(density(sqrt(15)*x))  

z <- seq(-5,5,length.out = 200)

lines(z,dnorm(z,sd=sqrt(pi/2)),type = "l",col = "red") ## comparing with the asymptotic density
j
## 5.9. 565HW11
chooseCRANmirror(graphics=FALSE)
install.packages("dlm")


###### Kalman Filter
y  <-  scan("lt.txt")
y <- matrix(y,nrow = length(y))

k <- 1
m <- 1
n <- length(y)
ZZ  <-  matrix(1)
HH  <-  matrix(0.25)
TT  <-  matrix(1)
QQ  <-  matrix(0.01)
## sp0 <- rep(0,m)
## Sp0 <- array(10^7,c(m,m))
sp0 <- 0.2
Sp0 <- matrix(2.25)

sf <- array(0,c(n,m))
sp <- array(0,c(n,m))
Sf <- list()
Sp <- list()
v <- array(0,c(n,k))
V <- list()
## Initialization
sp[1,] <- sp0
Sp[[1]] <- Sp0+QQ
loglik <- 0

for (t in 1:n){
    v[t,] <- y[t,] - ZZ%*%sp[t,]
    V[[t]] <- ZZ%*%Sp[[t]]%*%t(ZZ) + HH
    sf[t,] <- sp[t,] + Sp[[t]]%*%t(ZZ)%*%solve(V[[t]])%*%v[t,]
    Sf[[t]] <- Sp[[t]] - Sp[[t]]%*%t(ZZ)%*%solve(V[[t]])%*%ZZ%*%Sp[[t]]
    if (t < n){
        sp[t+1,]  <-  TT%*%sf[t,]
        Sp[[t+1]]  <-  TT%*%Sf[[t]]%*%t(TT) + QQ
    }
    loglik <- -log(2*pi)/2 - log(det(V[[t]]))/2 - t(v[t,])%*%solve(V[[t]])%*%v[t,]/2 + loglik
}

library(dlm)
ltm <- dlm(FF = 1, V = 0.25, GG = 1, W = 0.01, m0 = 0.2, C0 = 2.25)
dlmLL(y,ltm)


## 5.4. 565HW9
###### lynx data
x <- log(lynx)/log(10)
acf(x)
pacf(x)

n <- length(x)
p <- 3
d <- 2
m <- n%/%10+p
X <- array(0,c(n-p,(p+1)))
xd <- x[(p+1-d):(n-d)]
index <- order(xd)
xdo <- xd[index]
for (i in 1:(n-p)){
    ind <- index[i]+p-d+d
    X[i,] <- x[ind:(ind-p)]  ## Note the decreasing index here
}

res <- array(0,c(n-p,2))
AR <- array(0,c(n-p,p+1,3))
for (i in m:(n-p-1)){
    fit <- lm(X[1:i,1]~X[1:i,2:(p+1)])
    xnew <- as.vector(X[i+1,2:(p+1)])
    xnew <- c(1,xnew)
    pred <- sum(fit$coef[1:(p+1)]*xnew)
    fit.s <- summary(fit)
    res[i+1,1] <- X[i+1]-pred
    temp <- 1+t(xnew)%*%fit.s$cov%*%(xnew)
    res[i+1,2] <- res[i+1,1]/sqrt(temp)
                                        #res[i+1,2] <- res[i+1,2]/fit.s$sigma
    AR[i+1,,1] <- fit.s$coef[,1]
    AR[i+1,,2] <- fit.s$coef[,2]
    AR[i+1,,3] <- fit.s$coef[,3]
}

res.m <- lm(res[(m+1):(n-p),2]~X[(m+1):(n-p),2:(p+1)])
res.s <- summary(res.m)
res.s
F <- (sum(res[(m+1):(n-p),2]^2)-res.s$sigma^2*(n-p-m-p-1))/(p+1)/res.s$sigma^2
F
1-pf(F,p+1,(n-p-m-p-1))

plot(xdo[(m+1):(n-p-m)],res[(m+1):(n-p-m),1],main="Ordinary Predictive Residuals")
plot(xdo[(m+1):(n-p-m)],res[(m+1):(n-p-m),2],main="Standardized Predictive Residuals")
plot(xdo[(m+1):(n-p-m)],AR[(m+1):(n-p-m),3,3],main="t-ratios of Lag 1")

abline(v= c(2.5,2.65,3.1))  ## Subjective Judgement


######### AIC selection

tmp1 <- xdo[c(24,26,27,28)]
tmp2 <- xdo[c(41:44)]
tmp3 <- xdo[c(62:65)]
aicall <- array(0,c(length(tmp1),length(tmp1),length(tmp1)))

for (i in 1:length(tmp1)) {
    for (j in 1:length(tmp2)) {
        for (k in 1:length(tmp3)) {

            t1 <- tmp1[i]
            t2 <- tmp2[j]
            t3 <- tmp3[k]
            num1 <- length(which(xd <= t1 ))
            num2 <- length(which((xd > t1) & (xd <= t2)))
            num3 <- length(which((xd > t2) & (xd <=t3)))
            num4 <- length(which(xd > t3))
            ind1 <- 1:num1
            ind2 <- (num1+1):(num1+num2)
            ind3 <- (num1+num2+1):(num1+num2+num3)
            ind4 <- (num1+num2+num3+1):(num1+num2+num3+num4)

            ## Regime 1

            fit1 <- lm(X[ind1,1]~X[ind1,2:(p+1)])
            aicall[i,j,k] <- aicall[i,j,k] + AIC(fit1)

            ## Regime 2

            fit2 <- lm(X[ind2,1]~X[ind2,2:(p+1)]) 
            aicall[i,j,k] <- aicall[i,j,k] + AIC(fit2)

            ## Regime 3

            fit3 <- lm(X[ind3,1]~X[ind3,2:(p+1)]) 
            aicall[i,j,k] <- aicall[i,j,k] + AIC(fit3)

            ## Regime 4

            fit4 <- lm(X[ind4,1]~X[ind4,2:(p+1)]) 
            aicall[i,j,k] <- aicall[i,j,k] + AIC(fit4)


        }
    }
}

ind.min <- which(aicall == min(aicall),arr.ind = TRUE) ## Find the array index achieving the minimum AIC. Here ind.min == c(4,1,3)


##### Fit AR model

t1 <- tmp1[ind.min[1]]
t2 <- tmp2[ind.min[2]]
t3 <- tmp3[ind.min[3]]

jpeg("ThresholdValue.jpeg",width = 500,height = 480)    ## Plotting Threshold
plot(xdo[(m+1):(n-p-m)],AR[(m+1):(n-p-m),3,3],main="t-ratios of Lag 1")
abline(v= c(t1,t2,t3))  ## Subjective Judgement
dev.off()

num1 <- length(which(xd <= t1 ))
num2 <- length(which((xd > t1) & (xd <= t2)))
num3 <- length(which((xd > t2) & (xd <=t3)))
num4 <- length(which(xd > t3))
ind1 <- 1:num1
ind2 <- (num1+1):(num1+num2)
ind3 <- (num1+num2+1):(num1+num2+num3)
ind4 <- (num1+num2+num3+1):(num1+num2+num3+num4)

## Regime 1

fit1 <- lm(X[ind1,1]~X[ind1,2:(p+1)])
fit1.sum <- summary(fit1)

## Regime 2

fit2 <- lm(X[ind2,1]~X[ind2,2:(p+1)]) 
fit2.sum <- summary(fit2)

## Regime 3

fit3 <- lm(X[ind3,1]~X[ind3,2:(p+1)]) 
fit3.sum <- summary(fit3)

## Regime 4

fit4 <- lm(X[ind4,1]~X[ind4,2:(p+1)]) 
fit4.sum <- summary(fit4)



#### Refined model

fit1 <- lm(X[ind1,1]~X[ind1,c(2,4)])
fit1.sum <- summary(fit1)

## Regime 2

fit2 <- lm(X[ind2,1]~X[ind2,c(2,4)]) 
fit2.sum <- summary(fit2)

## Regime 3

fit3 <- lm(X[ind3,1]~X[ind3,c(2,3)]) 
fit3.sum <- summary(fit3)

## Regime 4

fit4 <- lm(X[ind4,1]~X[ind4,c(2,3)]) 
fit4.sum <- summary(fit4)


###### parametric bootstrap

coef <- array(0,c(4,4))
coef[1,1:4] <- c(fit1$coef[1:2],0,fit1$coef[3])
coef[2,1:4] <- c(fit2$coef[1:2],0,fit2$coef[3])
coef[3,1:4] <- c(fit3$coef,0)
coef[4,1:4] <- c(fit4$coef,0)

sigma <- c(fit1.sum$sigma*sqrt((num1-3)/num1),fit2.sum$sigma*sqrt((num2-3)/num2),fit3.sum$sigma*sqrt((num3-3)/num3),fit4.sum$sigma*sqrt((num3-3)/num3))

tar <- function(x){
    xx <- c(1,x)
    if (x[d] < tmp1[ind.min[1]]){
        y <- coef[1,]%*%xx+rnorm(1,0,sigma[1])
    }
    else if (x[d] < tmp2[ind.min[2]]) {
        y <- coef[2,]%*%xx+rnorm(1,0,sigma[2])
    }
    else if (x[d] < tmp3[ind.min[3]]) {
        y <- coef[3,]%*%xx+rnorm(1,0,sigma[3])
    }
    else {
        y <- coef[4,]%*%xx+rnorm(1,0,sigma[4])
    }
}

h <- 3
B <- 5000
BS <- array(0,c(h,B))
for (i in 1:B){
    temp <- x[n:(n-2)]
    BS[1,i] <- tar(temp)
    temp <- c(BS[1,i],temp)
    temp <- temp[1:3]
    BS[2,i] <- tar(temp)
    temp <- c(BS[2,i],temp)
    temp <- temp[1:3]
    BS[3,i] <- tar(temp)
}
mean(BS[1,])
mean(BS[2,])
mean(BS[3,])

