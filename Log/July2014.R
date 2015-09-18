## 7.14.
x <- rnorm(10,1,2)
ecdf(x)
## plot(ecdf(x),ylab = "Fn(x)",verticals = FALSE)
plot(ecdf(x),ylab = "Fn(x)",verticals = TRUE,do.p=FALSE)
lines(x,pnorm(x,mean(x),sd(x)))


## 7.22

