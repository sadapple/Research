## 3.31.
?with
?get
get("%o%")
?pmax

## 3.29. Matrix Techniques
## Find the eigenvectors of a matrix

# Generating p.d. matrices
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

M <- Posdef(4,ev=c(1,2,3,4))
x <- c(1,1.5,0.7,4)

FindEig <- function(x,Mat,n){
                    i <- 0
                    if ((is.vector(x)==TRUE)&
                        (is.matrix(Mat)==TRUE)&
                        (length(x)==nrow(Mat))){
                        while(i<=n){
                            x <- Mat%*%x/sqrt(sum((Mat%*%x)^2))
                            i=i+1
                        }
                        return(x)
                    } else {
                        return("unconformal argument inputs or unequal dimesions")
                    }
                }
eig1 <- FindEig(x,M,20)  ## The largest eigenvector
M%*%eig1/eig1   ## Should expect close to the largest eigenvalue of M
sub1 <- M%*%eig1%*%t(eig1)
M1 <- M-sub1
eig2 <- FindEig(x,M1,30) ## Find the second largest eigenvector
M%*%eig2/eig2  ## close to the second largest eigenvalue of M


## Gram Schmidt orthogonalization procedure for columns of a matrix
 Gram <- function(M){
                  if (is.matrix(M)==TRUE){
                      p <- nrow(M)
                      q <- ncol(M)
                      G <- matrix(rep(0,p*q),nrow = p)
                      G[,1] <- M[,1]
                      for (i in 2:q){
                          P <- matrix(rep(0,p*(i-1)),nrow=p)   ## Alternative G[,i] <- M[,i], this approach doesn't construct P
                          for (j in 1:(i-1)){
                              P[,j] <- (M[,i]%*%G[,j]/G[,j]%*%G[,j])*G[,j]  ##  G[,i] <- G[,i]-(M[,i]%*%G[,j]/G[,j]%*%G[,j])*G[,j]
                          }
                          G[,i] <- M[,i]-rowSums(P)   ## Drop this line if follow the alternative
                      }
                      return(G)
                  } else {
                      return("The input is not a matrix")
                  }
              }


# Alternative for comparison

 Gram1 <- function(M){
                  if (is.matrix(M)==TRUE){
                      p <- nrow(M)
                      q <- ncol(M)
                      G <- matrix(rep(0,p*q),nrow = p)
                      G[,1] <- M[,1]
                      for (i in 2:q){
                          G[,i] <- M[,i]
                          for (j in 1:(i-1)){
                              G[,i] <- G[,i]-(M[,i]%*%G[,j]/G[,j]%*%G[,j])*G[,j]
                          }
                      }
                      return(G)
                  } else {
                      return("The input is not a matrix")
                  }
              }


G <- Gram(M)
G1 <- Gram1(M)
# Checking
round(t(G)%*%G,3)
round(t(G1)%*%G1,3)
round(diag(G-G1),3)

## Comparing system time
k <- 400
M <- Posdef(400,runif(min=0.8,max=10,n=400))
system.time(G <- Gram(M))
system.time(G1 <- Gram1(M))   ## Gram1 seems faster when n is large, but if use the Gram1 version of Sun Die, my Gram function seems faster



## 3.28.
## Comparing some apply function
X <- sapply(3:5,seq)
sapply(X,fivenum)
mapply(function(x,y) seq_len(x)+y,
       c(a=1,b=2,c=3),
       c(A=10,B=0,C=-10))
(X <- list(list(a=pi, b=list(c=1:1)), d="a test"))
rapply(X,function(x) x)


## 3.22.
############# Subsetting Technique
a <- matrix(1:9, nrow = 3)
colnames(a) <- c("A", "B", "C")
a[0, -2]
# Arrays in R are stored in column-major order
x <- outer(1:5,1:5,FUN = "+")
x[c(4,15)]

# Subsetting with matrix index
vals <- outer(1:5, 1:5, FUN = "paste", sep = ",")
select <- matrix(ncol = 2, byrow = TRUE, c(
  1, 1,
  3, 1,
  2, 4
))
vals[select]

## Subsetting Data Frames
df <- data.frame(x = 1:3, y = 3:1, z = letters[1:3])
df[df$x == 2, ]
df[c(1, 3), ]
# There are two ways to select columns from a data frame
# Like a list:
df[c("x", "z")]
# Like a matrix
df[, c("x", "z")]

# There's an important difference if you select a single column:
# matrix subsetting simplifies by default, list subsetting does not.
str(df["x"])
str(df[, "x"])

# Caution
mtcars[1:20]
mtcars[1:20,]

## Exercises
# Fixing errors
mtcars[mtcars$cyl = 4, ]
mtcars[-1:4, ]
mtcars[mtcars$cyl <= 5]
mtcars[mtcars$cyl == 4 | 6, ]  # mtcars[mtcars$cyl == c(4,6), ]

# ? Diff
x <- 1:5; x[NA]
x[NA_real_]

# Form upper triangular matrix
x <- outer(1:5, 1:5, FUN = "*")
x[upper.tri(x)] ## Only give you a vector, Why? Recall the matrix subsetting rule

(m2 <- matrix(1:20, 4, 5))
lower.tri(m2)
m2[lower.tri(m2)] <- NA
m2

# is.na
df <- c(NA,NA,1,2,3)
df[is.na(df)] <- 0

## The [[ operator and recursively subsetting
a <- list(a = 1, b = 2)
a[[1]]
a[["a"]]

# If you do supply a vector it indexes recursively
b <- list(a = list(b = list(c = list(d = 1))))
b[[c("a", "b", "c", "d")]]
# Same as
b[["a"]][["b"]][["c"]][["d"]]

## Diff between $ and [[
## x$y is equivalent to x[["y", exact = FALSE]].
var <- "cyl"
# Doesn't work - mtcars$var translated to mtcars[["var"]]
mtcars$var
# Instead use [[
mtcars[[var]]
# But $ does partial matching
x <- list(abc = 1)
x$a
# If you want to avoid this behaviour you can set options(warnPartialMatchDollar = TRUE) -
# but beware that this is a global option and may affect behaviour in other code you have loaded (e.g. packages).

## Extract information from linear models
mod <- lm(mpg ~ wt, data = mtcars)
summary(mod)

### Subsetting and Assignment
x <- 1:5
x[c(1, 2)] <- 2:3
x
#> [1] 2 3 3 4 5

# The length of the LHS needs to match the RHS
x[-1] <- 4:1
x
#> [1] 2 4 3 2 1

# Note that there's no checking for duplicate indices
x[c(1, 1)] <- 2:3
x
#> [1] 3 4 3 2 1

# You can't combine integer indices with NA
x[c(1, NA)] <- c(1, 2)
#> Error: NAs are not allowed in subscripted assignments
# But you can combine logical indices with NA
# (where they're treated as false).
x[c(T, F, NA)] <- 1
x
#> [1] 1 4 3 1 1

# This is mostly useful when conditionally modifying vectors
df <- data.frame(a = c(1, 10, NA))
df$a[df$a < 5] <- 0
df$a
#> [1]  0 10 NA

## Indexing with a blank can be useful in conjunction with assignment
mtcars[] <- lapply(mtcars, as.integer)
mtcars <- lapply(mtcars, as.integer)

## Removing components or Addding components to a list
x <- list(a = 1, b = 2)
x[["b"]] <- NULL
str(x)
#> List of 1
#>  $ a: num 1

y <- list(a = 1)
y["b"] <- list(NULL)
str(y)
#> List of 2
#>  $ a: num 1
#>  $ b: NULL

#### Subsetting Applications
### Matching and Merging(integer subsetting)
grades <- c(1, 2, 2, 3, 1)

info <- data.frame(
  grade = 3:1,
  desc = c("Excellent", "Good", "Poor"),
  fail = c(F, F, T)
)
## We want to duplicate the info table so that we have a row for each value in grades.
# Using match
id <- match(grades, info$grade)
info[id, ]
#>     grade      desc  fail
#> 3       1      Poor  TRUE
#> 2       2      Good FALSE
#> 2.1     2      Good FALSE
#> 1       3 Excellent FALSE
#> 3.1     1      Poor  TRUE

# Using rownames
rownames(info) <- info$grade
info[as.character(grades), ]
#>     grade      desc  fail
#> 1       1      Poor  TRUE
#> 2       2      Good FALSE
#> 2.1     2      Good FALSE
#> 3       3 Excellent FALSE
#> 1.1     1      Poor  TRUE

## If you have multiple columns to match on, you'll need to first collapse them to a single column (with interaction(), paste(), or plyr::id()).
## You can also use merge() or plyr::join(), which do the same thing for you

### Random samples/bootstrap (integer subsetting)
df <- data.frame(x = rep(1:3, each = 2), y = 6:1, z = letters[1:6])

# Randomly reorder
df[sample(nrow(df)), ]
# Select 3 random rows
df[sample(nrow(df), 3), ]
# Select 10 bootstrap samples
df[sample(nrow(df), 10, rep = T), ]

### Ordering
## For two or more dimensions, order() and integer subsetting makes it easy to order either the rows or columns of an object.

# Randomly reorder df
df2 <- df[sample(nrow(df)), 3:1]
df2[order(df2$x), ]
df2[, order(names(df2))]

## More concise, but less flexible, functions are available for sorting vectors, sort(), and data frames, plyr::arrange().


### Expanding aggregated counts (integer subsetting)

df <- data.frame(x = c(2, 4, 1), y = c(9, 11, 6), n = c(3, 5, 1))
rep(1:nrow(df), df$n)
#> [1] 1 1 1 2 2 2 2 2 3
df[rep(1:nrow(df), df$n), ]
#>     x  y n
#> 1   2  9 3
#> 1.1 2  9 3
#> 1.2 2  9 3
#> 2   4 11 5
#> 2.1 4 11 5
#> 2.2 4 11 5
#> 2.3 4 11 5
#> 2.4 4 11 5
#> 3   1  6 1

### Removing columns from data frame (character subsetting)
df <- data.frame(x = 1:3, y = 3:1, z = letters[1:3])
df$z <- NULL

df <- data.frame(x = 1:3, y = 3:1, z = letters[1:3])
df[c("x", "y")]

df[setdiff(names(df), "z")]

### Selecting rows based on a condition (logical subsetting)
mtcars[mtcars$cyl == 4 & mtcars$gear == 4, ]


## 3.21.
?pmax
x <- matrix(c(1,2,3,4),nrow=2)
pmax(x)
x <- sort(rnorm(100));  cH <- 1.35
pmin(cH, quantile(x)) # no names
pmin(quantile(x), cH) # has names
plot(x, pmin(cH, pmax(-cH, x)), type = "b", main =  "Huber's function")

# rle()
x <- c(1:5, 5:3)
## sort into descending order; first more efficiently:
stopifnot(sort(x, decreasing = TRUE) == rev(sort(x)))
stopifnot(rev(1:7) == 7:1)  #- don't need 'rev' here

# sweep()
require(stats) # for median
med.att <- apply(attitude, 2, median)
sweep(data.matrix(attitude), 2, med.att)  # subtract the column medians

# split and expand.grid
?expand.grid


## 3.20.
## unlist()
x <- list(1:3, "a", c(T, F, T), c(2.3, 5.9))
unlist(x)
## Coersion Egs
c(1, F)
c("a", 1)
c(list(1), "a")
c(T, 1L)
1 == "1"
"one" < 2

## Factors
x <- factor(c("a", "b", "b", "a"))
x
class(x)
levels(x)
# You can't use values that are not in the levels
x[2] <- "c"
x
# NB: you can't combine factors
c(factor("a"), factor("b"))

## Factor issues  when read in data
# Reading in "text" instead of from a file here:
z <- read.csv(text="value\n12\n1\n.\n9")
typeof(z$value)
#> [1] "integer"
as.numeric(z$value)
#> [1] 3 2 1 4
# Oops, that's not right: 3 1 2 4 are the levels of a factor, not the values we read in!
class(z$value)
#> [1] "factor"
# We can fix it now:
as.numeric(as.character(z$value))
#> Warning: NAs introduced by coercion

#> [1] 12  1 NA  9
# Or change how we read it in:
z <- read.csv(text="value\n12\n1\n.\n9", na.strings=".")
typeof(z$value)
#> [1] "integer"
class(z$value)
#> [1] "integer"
z$value
#> [1] 12  1 NA  9
# Perfect! :)

## structure() subtlty
structure(1:5, comment = "my attribute") # Why comment attribute is not shown?

## 3.19.

## Palindrome
## Warm up
x <- y <- 1:9
data <- expand.grid(x=x,y=y)
print(data)
z <- data$x * data$y
#
z <- matrix(z,ncol=9)

set.seed(1)
x <- round(runif(10),2)
print(x)
order(x)
x[order(x)[1]]
which.min(x)
x[which.min(x)]
x[order(x)]
y <- 1:10
data <- data.frame(x,y)
class(data)
head(data)
data[1,]
data[,1]
data$x
data[order(data$x),]

# Project Euler 4
# The reverse function

reverse <- function(n) {
    reversed <- 0
    while (n > 0) {
        reversed <- 10 * reversed + n %% 10
        n <- n%/%10
    }
    return(reversed)
}

# Search palindrome

x <- y <- 999:100
data <- expand.grid(x=x,y=y)
data$prod <- data$x * data$y
data <- data[order(data$prod,decreasing=T),]
head(data)

value <- data$prod
for (i in 1:length(value)) {
    isequal <- (value[i] == reverse(value[i]))
    if (isequal) {
        print(data[i,])
        break
    }
}



## 3.18.
## 587 HW3
x = c( -10,(-3):5,10)
y = c(0,0,1,0,0,1,0,1,1,1,1)

ef =function(b,z=x,ns=100)
{
   n = length(x); px = 1/(1+ exp(-b*z) )
   y = array( (runif(n*ns) <=px)*1, c(n,ns) )
   for(i in 1:ns) n[i] = glm(y[,i]~z-1,family=binomial)$coef
   mean(n)
}

z=NULL
z[1] <- glm(y~x-1,family="binomial")$coef
for(i in 1:100) z[i+1]=z[i]-1/i*(ef(z[i])-z[1])

## dev.new()
pdf("587hw3.pdf")
ts.z <- ts(z)
plot(ts.z)
dev.off()


## 3.12.

## 565 HW5

## 565 HW6.1
library(tseries)
library(astsa)
library(fBasics)
source("f:/Dropbox/Current/TimeSeries/Code/EACF.R")
s6 <- as.matrix(read.table("f:/Dropbox/Current/TimeSeries/Data/S6.dat"))
s6 <- ts(as.vector(s6))
plot(s6)
acf(s6)
pacf(s6)
ds6 <- diff(s6)
plot(ds6)
adf.test(ds6)

d2s6 <- diff(ds6)
adf.test(d2s6)
plot(d2s6)
acf(d2s6)
pacf(d2s6)
EACF(d2s6)
m1 <- arima(d2s6,order = c(3,0,1))
m2 <- arima(d2s6,order = c(0,0,0),seasonal = list(order=c(1,0,0),period=4))

d3s6 <- diff(d2s6)
adf.test(d3s6)
plot(d3s6)
acf(d3s6)
pacf(d3s6)
EACF(d3s6)
m1 <- arima(d3s6,order = c(0,0,0),seasonal = list(order=c(1,0,0),period=4))

## 3.10.

## Apply families
# divide all values by 2
m <- matrix(c(1:10, 11:20), nrow = 10, ncol = 2)
apply(m, 1:2, function(x) x/2)
m[,1:2]/2  # Alternative


x <- 1:4
lapply(x,runif)
lapply(x,runif,min=0,max=10)
x <- list(a=matrix(1:4,nr=2),b=matrix(1:6,nr=3))
lapply(x,function(elt) elt[,1] )  # Use Anonymous funtion to extract the first column of each componet of the list
x <- c(rnorm(10),runif(10),rnorm(10,1))
f <- gl(3,10)
tapply(x,f,mean)
tapply(x,f,mean,simplify = F)
tapply(x,f,range)
split(x,f)
## by()
attach(iris)
by(iris[, 1:4], Species, colMeans)
## It provides a way to split your data by factors and do calculation on each subset.


## Common Idiom, use split followed by lapply
library(datasets)
s <- split(airquality,airquality$Month)
sapply(s,function(x) colMeans(x[,c("Ozone","Solar.R","Wind")],na.rm = T))

x <- rnorm(10)
f1 <- gl(2,5)
f2 <- gl(5,2)
interaction(f1,f2)
split(x,list(f1,f2),drop = T)
str(split(x,list(f1,f2)))
str(split(x,list(f1,f2),drop = T))

## Vectorization via mapply()
noise <- function(x,mean,sd) {rnorm(x,mean,sd)}
mapply(noise,1:5,5:1,sd=2)


## Others to study
vapply()
replicate(10,rnorm(10))
rapply()


## Math functions
integrate(dnorm,-Inf,Inf)
integrand <- function(x){ 1/((x+1)*sqrt(x))}
integrate(integrand,lower=0,upper=Inf)  # converge slowly

x <- 1:10
class(t(t(x)))  # form a column vector
## Inner product could be calculated in many ways, like outer().
## But when the matrix is big, crossprod() function would be more efficient

## Use outer() to do 3-D plot
f <- function(x,y) { r <- sqrt(x^2+y^2); 10*sin(r)/r}
x <- seq(-1,1,length.out = 100)
y <- seq(-1,1,length.out = 100)
z <- outer(x,y,f)

## About matrices
x <- matrix(rnorm(25),nrow=5)
diag(c(1,2,3))
diag(x)
lower.tri(x,diag = T)
x[upper.tri(x)] <- 0
x
eigen(x)
solve(x)

## Comparing the time for apply mean and colMeans()
m <- 1000; n <- 3000
A <- matrix(1:(m*n),m,n)
system.time(B1 <- matrix(apply(A,2,mean),n,by=T))
system.time(B2 <- matrix(colMeans(A),n,by=T))

## Combinatorics
choose(5,3)
combn(5,3)
factorial(10)

## Differentiation
f1 <- expression(sin(x)*x)
D(f1,"x")

## Find roots for univariate functions
f <- function(x){sin(x)}
uniroot(f,c(5,10))  ## Return an error saying valus at end points not of opposite sign

########## Dealing with chars
tolower("MikTeX")
toupper("MikTeX")
casefold("Leila",upper=T)
nchar(month.name[1])

## Regular Expressions
grep("J.",month.abb)
grep("l.J",month.abb)
grep("J.l",month.abb)

## Select chars from string
substr("abcdef",2,4)
substring("abcdef",1:6,6:1)
substring("abcdef",1:6,1:6)

## layout()
layout(matrix(c(2,1,3,4),nrow = 2,byrow = T))
hist(rnorm(25),col="VioletRed")
hist(rnorm(25),col="VioletRed")
hist(rnorm(25),col="VioletRed")
hist(rnorm(25),col="VioletRed")



## 3.9.

## Auto.arima
library(forecast)
help("auto.arima")
?auto.arima

hs300 <- read.table("F:/Dropbox/Workspace/R/data/hs300-data.csv")
x <- as.matrix(hs300)
y <- (x[2:length(x)]-x[1:(length(x)-1)])/x[1:(length(x)-1)]

hsPrice <- ts(x)
auto.arima(hsPrice)

## 3.6. set Rprofile and Bootstrap


# help(Startup)
## Example of Rprofile.site
 local({
   # add MASS to the default packages, set a CRAN mirror
   old <- getOption("defaultPackages"); r <- getOption("repos")
   r["CRAN"] <- "http://my.local.cran"
   options(defaultPackages = c(old, "MASS"), repos = r)
   ## (for Unix terminal users) set the width from COLUMNS if set
   cols <- Sys.getenv("COLUMNS")
   if(nzchar(cols)) options(width = as.integer(cols))
 })

# Forecast
install.packages("forecast")

library(forecast)
gas <- ts(read.csv("http://robjhyndman.com/data/gasoline.csv", header=FALSE)[,1],
          freq=365.25/7, start=1991+31/7/365.25)
bestfit <- list(aicc=Inf)
for(i in 1:25)
{
  fit <- auto.arima(gas, xreg=fourier(gas, K=i), seasonal=FALSE)
  if(fit$aicc < bestfit$aicc)
    bestfit <- fit
  else break;
}
fc <- forecast(bestfit, xreg=fourierf(gas, K=12, h=104))
plot(fc)




# simple Bootstrap

library(boot)                           # necessary

simpleboot<-function(x,stat,reps=1000) {
cat("Bootstrapping can go wrong!\n")
 cat("This simple function will not show you warning messages.\n")
 cat("Check results closely and be prepared to consult a statistician.\n")
 if(stat=="max" | stat=="min") { warning("Bootstrap is likely to fail for minima and maxima") }
 require(boot)
 eval(parse(text=eval(substitute(paste("p.func<-function(x,i) ",stat,"(x[i])",sep=""),list(stat=stat)))))
 myboots<-boot(x,statistic=p.func,R=reps,stype="i")
 hist(myboots$t,breaks=25,main="EDF from bootstrap",xlab=stat)
 suppressWarnings(return(list(replicates=reps,point.estimate=myboots$t0,normal.ci=c(boot.ci(myboots)$normal[2],boot.ci(myboots)$normal[3]),
percent.ci=c(boot.ci(myboots)$percent[4],boot.ci(myboots)$percent[5]),
bca.ci=c(boot.ci(myboots)$bca[4],boot.ci(myboots)$bca[5]))))
}

# example:
mydata<-rchisq(25,df=3)
simpleboot(mydata,"median")


## 3.5. 565 Hw4

getwd()
setwd("f:/Dropbox/Current/Financial Time Series/Data")
library(tseries)
library(astsa)
library(fBasics)

unrate <- read.table("m_unrate.txt",header=TRUE)
head(unrate)
colnames(unrate)
unrate <- ts(unrate[,4])
plot(unrate)
## Box.test(unrate,"Ljung")
acf(unrate) # Need to take difference


unrate.diff <- diff(unrate)
plot(unrate.diff)
acf(unrate.diff)
pacf(unrate.diff)

## 3.4. selecting out the maximum of rows

data=matrix(rnorm(100),10,10)
data_max=apply(data,1,max)
data_result=ifelse(data>=data_max,1,0)
data_result

