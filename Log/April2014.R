## 4.30.
chooseCRANmirror(graphics=FALSE)
install.packages("cluster")


## 4.28.
x <- read.csv("tmp.txt",header = FALSE) ## Grades for the 2nd TA midterm
x <- as.matrix(x)[,1]
mean(x)
median(x)

## simulate SARIMA model
library(forecast)
?simulate.Arima
?Arima

## Notes of functions
complete.cases() ; na.omit()  ## remove NA cases
merge() ## merge data.frame
g <- c("M","F","F","I","M","M","F")
split(1:7,g)  ## splitting

## get()
a=c(1,2,3)
b=c(4,5,6)
for(n in c("a","b")) print(n)
for(n in c("a","b")) print(get(n))
## Q: how about print(scan())

pmin() ## parallel

x=c(1,2,3) ## cat()
cat(x,sep=c(".","\n","\n"))

## string functions, like grep()
grep("Pole",c("Equator","North Pole","South Pole")) ## pattern matching
grep("pole",c("Equator","North Pole","South Pole"))
regexpr("uat","Equator")  ## return index of first match
gregexpr("iss","Mississippi") ## return indices of all matches


substring("Equator",3,5)
strsplit("6-16-2011",split="-")

## printing functions
paste("q",i,".pdf",sep="")
sprintf("q%d.pdf",i)

## locator()
hist(c(12,5,13,25,16))
locator(1)

## polygon
f <- function(x) return(1-exp(-x))
curve(f,0,2)
polygon(c(1.2,1.4,1.4,1.2),c(0,0,f(1.3),f(1.3)),col="gray")
polygon(c(1.2,1.4,1.4,1.2),c(0,0,f(1.3),f(1.3)),density=10)

## rev(), could be used to plot symmetrically
a=c(1,2)
rev(a)

## sub() and gsub()
sub(',','','1,222,333.12')
gsub(',','','1,222,333.12')

## readLines()

## Combination of simple functions
a = rnorm(1000)
length(which(a<1&a>0))

m=matrix(1:12,3,4)
which(m%%3==0,arr)
which(m%%3==0,arr.ind=TRUE)

##  order()
x=c(1,2,1,3,2,5)
y=c(1,3,2,4,2,6)
z=c(1:6)
a=rbind(x,y,z)
ii=order(x,y,z)
a[,ii]
iii=order(-x,y,z)
a[,iii]

## order() with NA values
a=c(1,3,2,NA)
b=c(NA,2,4,7)
c=cbind(a,b)
ii=order(a,b)
c[ii,]
iii=order(a,b,na.last=FALSE)
c[iii,]
iiii=order(a,b,na.last=NA)
c[iiii,]

## Common mistakes
for (i in 1:3) {
  print("select i")
}  ## Wrong
for (i in 1:3) {
  print(paste("select", i))
}

## Generate multi dist like normal, wishart
## bivariate normal
rbivariate <- function(mean.x = 0, sd.x=1, mean.y=0, sd.y=1, r=0, iter=100) {
    z1 <- rnorm(iter)
    z2 <- rnorm(iter)
    x <- sqrt(1-r^2)*sd.x*z1 + r*sd.x*z2 + mean.x
    y <- sd.y*z2 + mean.y
    return(list(x,y))
}

tmp <- rbivariate(r=0.9,iter=3000)
x <- tmp[[1]]
y <- tmp[[2]]

## Simulation Study for Prediction Dist of multi normal
library(MASS)
library(mgcv)
n <- 3
p <- 2
rho <- 0
m <- c(0,0)
Sig <- matrix(c(1,rho,rho,1),2,2)

Wgen <- function(n,m,Sig){
    tmp <- mvrnorm(n=n,mu = m,Sigma = Sig)
    S <- (n-1)*cov(tmp)
    svdt <- svd(S)
    s0 <- svdt$u%*%diag(1/(sqrt(svdt$d)))%*%t(svdt$v)
    s1 <- solve(t(chol(S)))
    z0 <- 1/sqrt(1+1/n)*(mvrnorm(n=1,mu=m,Sigma=Sig)-colMeans(tmp))
    w0 <- s0%*%z0
    w1 <- s1%*%z0
    return(cbind(w0,w1))
}

## Ploting
pdf("w0w1.pdf")
par(mfrow = c(5,2))
rho=c(-0.9,-0.4,0,0.4,0.9)

for (r in rho){
    Sig <- matrix(c(1,r,r,1),2,2)
    whole <- replicate(n=10000,Wgen(n,m,Sig)) ## The original sample size 1000 is not enough
    w0 <- whole[,1,]
    w1 <- whole[,2,]
    plot(w0[1,],w0[2,],xlim = c(-1000,1000),ylim = c(-1000,1000),main=c("r=",r))
    plot(w1[1,],w1[2,],xlim = c(-1000,1000),ylim = c(-1000,1000),main=c("r=",r))
    ## plot(w0[1,],w0[2,],xlim = c(-100,100),ylim = c(-500,500))
    ## plot(w1[1,],w1[2,],xlim = c(-100,100),ylim = c(-500,500))
}

dev.off()

## 4.26. Caching the mean of a vector
makeVector <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}

cachemean <- function(x, ...) {
        m <- x$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
}

tmp <- rnorm(500,0,1)
tmp0 <- makeVector(tmp)
cachemean(tmp0)


## 4.24.
install.packages("dlm")
tmp <- read.table("/media/LC/Dropbox/Current/TimeSeries/Data/d-geohlc.txt")

## 4.22. R HW from Cousera
cube <- function(x, n) {
        x^3
}
cube(3)

f <- function(x) {
        g <- function(y) {
                y + z
        }
        z <- 4
        x + g(x)
}
z <- 10
f(3)

x <- 5
y <- if(x < 3) {
        NA
} else {
        10
}

## 4.20.
tmp <- read.csv("hw1_data.csv",header=TRUE)
OZ <- tmp[,1]
sum(is.na(OZ))
mean(OZ,na.rm=TRUE)
tmp1 <- tmp[tmp$Ozone>31 & tmp$Temp>90,]
mean(tmp1$Solar.R,na.rm = TRUE)
tmp2 <- tmp[tmp$Month==6,]
mean(tmp2$Temp,na.rm = TRUE)
tmp3 <- tmp[tmp$Month==5,]
max(tmp3$Ozone,na.rm = TRUE)

## 4.19.
## sum a margin of a array according to some rule
chooseCRANmirror(graphics = FALSE)
install.packages("abind")
newarray <- abind(lapply(seq(1,20000,20), function(i){Reduce("+",lapply(i:(i+19),function(x){a[x,,]}))}), along=0)
## other functions worth considering
addmargins(a,1)
colSums(a,dim=1)
## Note that you can specify the dim of the above func

## 4.15.
?persp
example(persp)

## 4.12.
## what does the partial argument do in the sort function
v <- runif(1e7)
system.time(a <- sort(v, decreasing=TRUE)[10000])
system.time(b <- -sort(-v, partial=10000)[10000])
a==b


## 4.11.
## Install packages from source
## In Windows, need to install Rtools and Rbatch first
getwd()
install.packages("f:/Dropbox/Workspace/R/Rcode/tclust_1.1-03.tar.gz",repos = NULL,type = "source")
library(tclust)


## 4.7.
## Sort and split a data frame according to several variables
## split a data frame by a0 while sort b in descending order
x <- read.table(textConnection("
a0 a1  b
1   1  1 10
2   1  2  9
3   1  3  8
4   1  4  7
5   1  5  6
6   1  6  5
7   1  7  4
8   1  8  3
9   1  9  2
10  1 10  1
11  2 11 10
12  2 12  9
13  3 13 10
14  3 14  9
15  3 15  8
16  3 16  7
17  3 17  6
18  3 18  5"))

library(plyr)
x <- x[order(x$a0,-x$b),]
ddply(x,.(a0),function(df) df[seq(min(5,nrow(df))),]) ## Note the symbol "." here!

## This function is similar to '~' in that it is used to capture the
## name of variables, not their current value.  This is used
## throughout plyr to specify the names of variables (or more
## complicated expressions).


## Note the difference here
y <- data.frame(a=c(1,2,3,4,5),b=c(1,1,2,2,3),c=c(3,2,1,4,9))
order(y$b,y$c)
order(y$b,-y$c)


## simulate dist of median
?replicate
x <- replicate(10000,median(rnorm(15))) ## sample median of norm size 15
## hist(x,density = 5)
plot(density(sqrt(15)*x))
z <- seq(-5,5,length.out = 200)
lines(z,dnorm(z,sd=sqrt(pi/2)),type = "l",col = "red")
## two plots are very close
## alternative would be use curve()


## 4.4.
## Some calculation for Hastie's course
exp(-6+0.05*40+3.5)/(1+exp(-6+0.05*40+3.5))


## 4.2.
## logical operators: |,||,&,&&,xor,!
## construct truth tables
x <- c(NA, FALSE, TRUE)
names(x) <- as.character(x)
outer(x, x, "&") ## AND table
outer(x, x, "|") ## OR  table
y <- c(F,T,T)

## clustering
chooseCRANmirror(graphics = FALSE)
install.packages("cluster")
library(cluster)
data(ruspuni)

## 565 HW7
library(astsa)
data(prodn)
PI <- prodn
acf(PI)

dPI <- diff(PI)
plot(dPI)
acf(dPI)
acf(dPI,type = "covariance",lag.max = 50)
pacf(dPI,lag.max = 50)

dsPI <- diff(dPI,lag = 12)
plot(dsPI)
acf(dsPI,lag.max = 50)
pacf(dsPI,lag.max = 50)
EACF(dsPI,12,12)  ## for regular part, c(0,1,4) seems fine
## Model Fitting
fit1 <- arima(PI,order = c(0,1,4),seasonal = list(order=c(2,1,1),period=12))
fit2 <- arima(PI,order = c(0,1,4),seasonal = list(order=c(1,1,1),period=12))
fit3 <- arima(PI,order = c(0,1,4),seasonal = list(order=c(2,1,0),period=12))
fit4 <- arima(PI,order = c(0,1,4),seasonal = list(order=c(3,1,0),period=12))
fit5 <- arima(PI,order = c(0,1,4),seasonal = list(order=c(0,1,2),period=12))
fit6 <- arima(PI,order = c(0,1,4),seasonal = list(order=c(4,1,0),period=12))
fit7 <- arima(PI,order = c(0,1,4),seasonal = list(order=c(4,1,1),period=12))
fit8 <- arima(PI,order = c(2,1,0),seasonal = list(order=c(0,1,3),period=12)) ## AIC is better than fit6 and fit1

## Diagnostic
tsdiag(fit1,gof.lag = 200)
tsdiag(fit2,gof.lag = 200)
tsdiag(fit3,gof.lag = 200)
tsdiag(fit4,gof.lag = 200)
tsdiag(fit5,gof.lag = 200)
tsdiag(fit6,gof.lag = 200)
tsdiag(fit7,gof.lag = 200)
tsdiag(fit8,gof.lag = 200)
## From above, fit1,6,8 seems good

## Rolling Forecast Performance
## From both approach, fit6 seems better
## source("f:/Dropbox/Current/TimeSeries/Code/rolling.forecast.R")
source("rolling.forecast.R")
rolling.forecast(prodn,1,start=length(prodn)-20,order=c(0,1,4),seasonal=list(order=c(2,1,1),period=12),include.mean=F)  ## Smallest among first five
rolling.forecast(prodn,1,start=length(prodn)-20,order=c(0,1,4),seasonal=list(order=c(1,1,1),period=12),include.mean=F)
rolling.forecast(prodn,1,start=length(prodn)-20,order=c(0,1,4),seasonal=list(order=c(2,1,0),period=12),include.mean=F)
rolling.forecast(prodn,1,start=length(prodn)-20,order=c(0,1,4),seasonal=list(order=c(3,1,0),period=12),include.mean=F)
rolling.forecast(prodn,1,start=length(prodn)-20,order=c(0,1,4),seasonal=list(order=c(0,1,2),period=12),include.mean=F)
rolling.forecast(prodn,1,start=length(prodn)-20,order=c(0,1,4),seasonal=list(order=c(4,1,0),period=12),include.mean=F) ## smallest among seven
rolling.forecast(prodn,1,start=length(prodn)-20,order=c(0,1,4),seasonal=list(order=c(4,1,1),period=12),include.mean=F)
rolling.forecast(prodn,1,start=length(prodn)-20,order=c(2,1,0),seasonal=list(order=c(0,1,3),period=12),include.mean=F) ## not better than fit6 or fit1

fit <- arima(PI[1:353],order = c(0,1,4),seasonal = list(order=c(2,1,1),period=12))
predict(fit,n.ahead = 12)
predict(fit1,n.ahead = 12)

## 565 HW6
# P4
qdef <- read.table("f:/Dropbox/Current/TimeSeries/Data/q-gdpdef.txt",header = TRUE)[,4]
qdef <- ts(qdef)
plot(qdef)
logqd <- log(qdef)
d1logqd <- diff(logqd)
plot(d1logqd)

# Dickey Fuller test
library(tseries)
library(fUnitRoots)
library(astsa)
library(TSA)
adfTest(d1logqd,lag=12,type = "ct") ## How to select the lag here and how to choose the type?
adf.test(d1logqd)

d2logqd <- diff(d1logqd)
adfTest(d2logqd,lag=12,type = "ct")

# Modelling
acf(d2logqd)
pacf(d2logqd)
eacf(d2logqd,ar.max = 20,ma.max = 20)
fit1 <- arima(d2logqd,order=c(3,0,3))
fit2 <- arima(d2logqd,order=c(3,0,2))
fit3 <- arima(d2logqd,order=c(3,0,1))
fit4 <- arima(d2logqd,order=c(1,0,7))
fit5 <- arima(d2logqd,order=c(7,0,2))

# P3 How to simulate a arima with difference and intercept
x4 <- arima.sim(model=list(order=c(1,1,1),ar=0.8,ma=0.6),n=399,rand.gen=rnorm,sd=5) ## How to specify the mean directly
dx4 <- arima.sim(model=list(ar=0.8,ma=0.6),n=400,rand.gen=rnorm,sd=5)+1.6  ## The way I know
x4 <- cumsum(dx4)
out4 <- arima(x4,order=c(1,1,1),xreg = 1:length(x4))
fore4 <- predict(out4,n.ahead = 12,newxreg =(400+1):(400+12))
plot(x4[(400-23):400],type="l",lty=1,xlim=c(0,36),ylim=c(1200,1700))
lines(25:36,fore4$pred,type="l",lty=2)
lines(25:36,fore4$pred-1.96*fore4$se,type="l",lty=3)
lines(25:36,fore4$pred+1.96*fore4$se,type="l",lty=3)
