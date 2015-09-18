
# 2014.Feb ----------------------------------------------------------------

## Fitting hs300 data
hs300 <- read.table("hs300-data.csv")
x <- as.matrix(hs300)
y <- (x[2:length(x)]-x[1:(length(x)-1)])/x[1:(length(x)-1)]

hsPrice <- ts(x)
# Qusetion: How to specify the date when it is not very regular?
hsPrice <- ts(x,start=2002,end=2014,frequency=260)


plot(hsPrice)
hsRet <- ts(y)
hsRet.1 <- ts(y^2)
hsRet.2 <- diff(hsRet,diff=20)
plot(hsRet)
plot(hsRet.1)


# Testing W.N.
Box.test(hsRet,lag=10,type="Ljung-Box")

# Testing Stationarity
adf.test(hsRet)
adf.test(hsRet.1)
stationarity(hsRet)
stationarity(hsRet.1)


z <- ts(hsRet.1[1:2048])
plot(z)
z_spec <- hwtos2(z) # Having problems
plot(z_spec)


# ACF,PACF
hsRet_acf <- acf(hsRet)
hsRet_acf <- acf(hsRet,lag.max=252)
str(hsRet_acf)
hsRet_acf$acf

# Fitting a trend to the hsRet T.S.
hsRet.trend = filter(c(rep(hsRet[1],120),hsRet,rep(hsRet[2919],120)),sides=2,rep(1/241,241))[121:3039]
hsRet.trend = ts(hsRet.trend)
plot(hsRet.trend)
plot(hsRet-hsRet.trend,type="o",ylab="Number",main="Residuals")
abline(h=0)

par(mfrow=c(1,2))
plot(hsRet)
plot(hsRet.trend)


## Geometrical topology

breaks = quantile(volcano, seq(0, 1, length.out=256))
cols = colorRampPalette(c("#55FFFF", "grey10"))(255)
image(volcano, col=cols, breaks=breaks, useRaster=TRUE, axes=T, asp=T)


## Transforming a Gaussian white noise
w = ts(rnorm(501))
plot(w)
x = ts(w[-1]*w[-501])
plot(x)
acf(x)
acf(x,type="covariance")

y = x^2
plot(y)
acf(y)
acf(y,type="covariance")
acf.y = acf(y)
head(acf.y$acf)

# 2014.Jan ----------------------------------------------------------------


# split()
x=rnorm(10)
f1=gl(2,5)
f2=gl(5,2)

split(x,list(f1,f2)) # Don't quite understand the result

# mapply()
mapply(rnorm,1:5,1:5,2)

# Gifts from boys to girls
boys = c(22, 29, 31, 32, 33, 35, 36, 37, 41, 42, 47, 52, 55, 56)
girls = setdiff(22:58, boys)
set.seed(20120307)
g.rearrange = sample(girls)
b.rearrange = rep(sample(boys), length = length(girls))
sort(paste(b.rearrange, "->", g.rearrange))





# 2013 --------------------------------------------------------------------


6.27. ##studying PCA
er10=read.table(file="excess return.txt",header=TRUE)

er10.pca=princomp(er10)
pdf("04_er10_pca.pdf",height=6,width=12)
layout(t(1:2),widths=c(1,1))
par(mar=c(3,4,1,0))
screeplot(er10.pca,main="",type="barplot") ## type="lines"
er10.pca.corr=princomp(er10,cor=TRUE)
screeplot(er10.pca.corr,main="")
dev.off()

er10.pca=princomp(er10)
er10.pca$loadings ## default cutoff seems .1 ???
er10.cov=cov(er10)
svd(er10.cov)$v
er10.pca.corr=princomp(er10,cor=TRUE)
er10.pca.corr$loadings


6.19.
4*49/46*qf(0.1,4,46,lower.tail=FALSE)

6.11.
replicate(100,runif(100))

5.26
a<-c(2,3,4)
sum(a)
cumsum(a)
## 3 ways to round
round(), floor(), ceiling()
5.22.
#compute all possible determinants for 2*2 integer matrices whose value is from[0,9]
d<-outer(0:9,0:9)
fr<-table(outer(d,d,"-")) ## Tricky!
plot(as.numeric(names(fr)),fr,type="h",xlab=,ylab=)

#Generalized transpose of arrays
aperm()

# Math operations for matrices
crossprod()
x<-matrix(1:12,ncol=4,nrow=3)
diag(x)
#Note if x is a scalar, then diag(x) give you a identity matrix
solve(A,b) #stable than solve(A)%*%b
x%*%solve(A,x) ## could be used to express quadratic forms
#Note: what does x%*%x mean!

eigen()
svd(M)

lsfit()
qr() ##could use it to solve linear equations and LSE
qr.coedf(),qr.fitted(),qr.resid()

##form partition matriced
cbind(1,X,Y)

#Frequency tables for factors
table(statef) & tapply(statef,statef,length)

factor(cut(incomes,breaks=35+10*(0:7)))-> incomef
table(incomef,statef)
##create lists and data frames
#how to use attach() and detach(), what is search path?
search()

##a way to find hazard function value
pnorm(t,,lower.tail=FALSE,log=TRUE)

## Analyse one population data
summary() fivenum() stem() hist()
attach(faithful)
stem(eruptions)
hist(eruptions)
hist(eruptions,seq(1.6,5.2,0.2),prob=TRUE)
lines(density(eruptions,bw=0.1))
rug(eruptions) # to see real data points
plot(ecdf(eruptions),do.points=FALSE,verticals=TRUE)
long<-eruptions[eruptions>3]
plot(ecdf(long),do.points=FALSE,verticals=TRUE)
x<-seq(3,5.4,0.01)
lines(x,pnorm(x,mean=mean(long),sd=sqrt(var(long))),lty=3)

par(pty="s")
qqnorm(long)
qqline(long)
x<-rt(250,df=5)
qqnorm(x)
qqline(x)
qqplot(qt(ppoints(250),df=5),x,xlab="QQ plot for t dsn")
qqline(x)
#Testing for normal, note the test assumptions
shapiro.test(long)
ks.test(long,"pnorm",mean=mean(long),sd=sqrt(var(long)))
#assume normality, then we could do t test
var.test()
t.test(,,var.equal=TRUE)
#Nonparametric tests, note their assumptions
wilcox.test

##### How to vitualize the diff of two samples?
boxplot()
plot(ecdf())



5.9.
num<- function(n){
  n<- round(n) ## can I use is.integer() here?
   if(n<=0)
    print("not positive int")
  else {
    repeat{
      if (n==1) break
      else if (n%%2==0) {n<- (n/2)}
      else n<- 3*n+1
    }
    # could use while() to perform the loop, but I think it's not a good idea
    print("computation success")
    return(n)
  }
}
num(-1000)
num(1300)




5.8.
## Approximating roots using disection
root.appr<- function(f,a,b,eps=1e-5){
  if(f(a)*f(b)>0)
    list(fail="fail")
  else {
    repeat{
      if(abs(b-a)<eps) break
      x<- (a+b)/2
      if(f(a)*f(x)<=0) b<-x else a<-x
      ## note here the condition "f(a)*f(x)<0" is not enough
    }
    list(root=(a+b)/2,fun=f(x))
  }
}

f<- function(x) x^3+1
root.appr(f,-1,0,1e-7)

3.25.
c(1, c(2, c(3, 4)))
numeric <- c(1, 2.5, 4.5)
# Note the L suffix which distinguishes numeric from integers
integer <- c(1L, 6L, 10L)
typeof(2),typeof(2L)

##Coersion in R
c("a", 1)
c(1, T, F)
# Total number of TRUEs
sum(mtcars$cyl == 4)
# Proportion of TRUEs
mean(mtcars$cyl == 4)

## List
x <- list(1:3, "a", c(T, F, T), c(2.3, 5.9))
str(x)
x <- list(list(list(list())))
str(x)
is.recursive(x)

is.list(mtcars)
names(mtcars)
str(mtcars$mpg)

mod <- lm(mpg ~ wt, data = mtcars)
is.list(mod)
names(mod)
str(mod$qr)

## Attributes
y <- 1:10
attr(y, "comment") <- "This is a vector"
attr(y, "comment")
str(attributes(y))


## combining two matrices to form a partition matrix
require(Matrix)
x = matrix(1:9, 3, 3)
y = matrix(9:1, 3, 3)
bdiag(x, y)

##plotting Cauchy density
a=rcauchy(10000,location=0, scale=1)
plot(density(a),xlim=c(-40,40))

curve(dcauchy, -4, 4)
curve(dnorm, -4, 4)
curve(sin,-4,4)

##all possible comibnations of choosing 3 from 8 distinct objects
combn(1:8, 3)

## ploting a heart in R
help(points)
TestChars <- function(sign=1, font=1, ...)
{
  if(font == 5) { sign <- 1; r <- c(32:126, 160:254)
  } else if (l10n_info()$MBCS) r <- 32:126 else r <- 32:255
  if (sign == -1) r <- c(32:126, 160:255)
  par(pty="s")
  plot(c(-1,16), c(-1,16), type="n", xlab="", ylab="",
       xaxs="i", yaxs="i")
  grid(17, 17, lty=1)
  for(i in r) try(points(i%%16, i%/%16, pch=sign*i, font=font,...))
}
TestChars()
try(TestChars(sign=-1))
TestChars(font=5)

#plottings begin here
dat<- data.frame(t=seq(0, 2*pi, by=0.1) )
xhrt <- function(t) 16*sin(t)^3
yhrt <- function(t) 13*cos(t)-5*cos(2*t)-2*cos(3*t)-cos(4*t)
dat$y=yhrt(dat$t)
dat$x=xhrt(dat$t)
with(dat, plot(x,y, type="l"))
with(dat, polygon(x,y, col="hotpink"))
points(c(10,-10, -15, 15), c(-10, -10, 10, 10), pch=169, font=5) # adding small hearts

library(Cairo)

clubs <- expression(symbol('\247'))
hearts <- expression(symbol('\251'))
diamonds <- expression(symbol('\250'))
spades <- expression(symbol('\252'))
csymbols <- c(clubs, hearts, diamonds, spades)

plot( 0, xlim=c(0,5), ylim=c(0,2), type="n" )
clr <- c("black", "red", "red", "black")
for (i in 1:4) {
  hline <- function( yloc, ... )
    for (i in 1:length(yloc))
      lines( c(-1,6), c(yloc[i],yloc[i]), col="gray")
  hline(0.9);
  hline(1.0);
  hline(1.1);
  hline(1.2)
  text( i, 1, csymbols[i], col=clr[i], cex=5 )
  text( i, 0.5, csymbols[i], col=clr[i] ) }

# Also try this
plot(1,1)
text(x=1+0.2*cos(seq(0, 2*pi, by=.5)),
     y=1+0.2*sin(seq(0, 2*pi, by=.5)),
     expression(symbol('\251') ) )

#Other plotting approach
plot(1, 1, pch = "???", cex = 20, xlab = "", ylab = "", col = "firebrick3")


##Pascal Triangle

m = matrix(0,8,8)
diag(m)=1
m[,1]=1

for (i in 3:nrow(m)) {
  for (j in 2:(i-1)) {
    m[i,j] <- (m[i-1,j-1]*m[i-1,j]+1)/m[i-2,j-1]
  }
}

##Multi-If syntax
