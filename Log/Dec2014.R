## 12.12
## Apply family notes
## lapply, sapply, tapply(split according to the levels of factors, not necessarily one factor), mapply(parallel version, for a set of arguments)
## split followed by an lapply
lapply(split(x,f),mean)

##
x <- rnorm(10)
f1 <- gl(2,5)
f2 <- gl(5,2)

str(split(x,list(f1,f2)))
str(split(x,list(f1,f2),drop = TRUE))

## mapply

mapply(rep,1:4,4:1)
