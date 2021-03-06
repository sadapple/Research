## 4.28

## install Rstan
Sys.setenv(MAKEFLAGS = "-j4")
source('http://mc-stan.org/rstan/install.R', echo = TRUE, max.deparse.length = 2000)
install_rstan()


## 4.27
alpha.1 <- rbeta(2000,295,308)
alpha.2 <- rbeta(2000,289,333)
dif <- alpha.2-alpha.1
hist(dif,xlab="alpha2-alpha1",yaxt="n",cex=2,breaks = seq(-.13,.08,.01))
print(mean(dif>0))

## 4.20
## 401 project
data(pressure)
# write the data into a txt file
write.table(pressure,file="C:/Users/CHUANLIU/Dropbox/extra/Teaching/2015-Spring-Stat-401/pressure.txt")
# Celsius to Fahren
pressure$temperature <- 1.8*pressure$temperature+32
# simple linear regression
fit1 <- lm(temperature~pressure,data = pressure)
fit2 <- lm(temperature~-1+pressure,data = pressure)
plot(fit2)

coef2 <- fit2$coefficients
plot(pressure$pressure,pressure$temperature)
abline(a=0,b=coef2[1],col="red")

# multiple regression
pressure$pressure_square <- pressure$pressure^2
pressure$pressure_cubic <- pressure$pressure^3
fit3 <- lm(temperature~ pressure+pressure_square+pressure_cubic,data = pressure)
plot(fit3)

## 4.15
## alternative implementation for split()
a = rnorm(5000)  #The vector
group = sample(1:100,5000,replace=T)  #group labels


self_split = function(a,group)
{
    ind = order(group)
    a = a[ind]
    group=group[ind]
    len = table(group)
    clen = cumsum(len)
    n = length(clen)
    indi = c(1,clen[1:(n-1)]+1)
    indj = clen
    inds = cbind(indi,indj)#beginning and ending index for each group

    ans = apply(inds,1,function(x,a) a[x[1]:x[2]],a)
    return(ans)
}
#### combine different pieces together
## use match() directly on list
a = list()
for (i in 1:100) a[[i]] = rnorm(sample(50:100,1))
b = unlist(a)
b = sample(b,length(b))
system.time((ans = lapply(a,match,b)))

## combine list elements into a single vector then use match() for only once
self_match = function(a,b)
{
    len = sapply(a,length)
    aa = unlist(a)
    ans = match(aa,b)
    ans = split(ans,rep(1:length(len),len))
    return(ans)
}

system.time((ans = self_match(a,b)))



## 4.14
x <- c(3,3,4,5,6,6,7,8,8,9)
y <- c(9,5,12,9,14,16,22,18,24,22)
plot(x,y,col="red")
lm(y~x)
summary(lm(y~x))


## 4.8
## partial sum of sin(k^2)
n <- 100000
x <- seq(1:n)
y <- cumsum(sin(x^2))
plot(y,type = "l",col="red")
## The above partial sum doesn't seem to be bounded.

## partial sum of sin(k)
n <- 1000
x <- seq(1:n)
y <- cumsum(sin(x))
plot(y,type = "l",col="red")
## Bounded, could be proved via trigometric product to sum formula

## 4.7
## Ploting t distributions
# Display the Student's t distributions with various
# degrees of freedom and compare to the normal distribution

x <- seq(-4, 4, length=100)
hx <- dnorm(x)

degf <- c(1, 3, 8, 30)
colors <- c("red", "blue", "darkgreen", "gold", "black")
labels <- c("df=1", "df=3", "df=8", "df=30", "normal")

plot(x, hx, type="l", lty=2, xlab="x value",
  ylab="Density", main="Comparison of t Distributions")

for (i in 1:4){
  lines(x, dt(x,degf[i]), lwd=2, col=colors[i])
}

legend("topright", inset=.05, title="Distributions",
  labels, lwd=2, lty=c(1, 1, 1, 1, 2), col=colors)


## 4.5
## Verify the correlation between sample variance and mean for general population with finite 3rd moment
## simulate a distribution with nonzero skewness
VarMeanCor <- function(n,m){
    data <- replicate(m, rexp(n,1))
    VarData <- apply(data,2,var)
    MeanData <- apply(data,2,mean)
    return(var(VarData,MeanData))

}
n <- 100
m <- 10000
## since for exp(1), the 3rd central moment is 2, so we should expect when m tends to infinity
## VarMeanCor(n,m) would tend to 2/n
