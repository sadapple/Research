###2.21
x <- c(3:5, 11:8, 8 + 0:5)
ux <- unique(x)
#generating factors
gl()

#####2.15.
x <- c('hello','world','I','love','R')
for (n in x) {
  print(n)
}  ## Note this part!

x <- seq(from=1,to=2,length.out=10)
print(x)
round(x) 
x > 1.5
all(x>1.5)
any(x>1.5)

# 如何自定义一个求圆面积的函数
myfunc <- function(r) {
  area <- pi*r^2
  return(area)
}
print(myfunc(4))

# 同时求四个不同半径圆的面???
r <- c(2,2,4,3)
sapply(X=r,FUN=myfunc)


## how to determine a prime number or not

# Project Euler 3
# 找到600851475143这个数的最大质因子
# 先建立一个函数以判断某个数是否为质数

findprime  <- function(x) {
  if (x %in% c(2,3,5,7)) return(TRUE)
  if (x%%2 == 0 | x==1) return(FALSE)
  xsqrt <- round(sqrt(x))
  xseq <- seq(from=3,to=xsqrt,by=2)
  if (all(x %% xseq !=0)) return(TRUE)
  else return(FALSE)
}

# 列出1???100的质数，看函数对不对
x = 1:100
x[sapply(x,findprime)]

# 寻找最大的质因???
n <- 97
for (i in seq(from=3, to=round(sqrt(n)), by=2)) {
  if (findprime(i) & n %% i == 0) {
    n <- n / i
    prime.factor <- i       
    if (i >= n)
      break
  }
}
print(prime.factor) ## If n is prime itself, then there would be no result.

######2.14.

x<- runif(100)
y<- rnorm(100)+5*x
m1<- lm(y~x)
mode(x);attributes(m1)
methods(class=lm) #Find what function can deal with the 'lm' class object 
mode(m1);class(m1);names(m1)
m1$residuals;residuals(m1)## Sometimes there might be some differeces 
str(m1)

##distinction between nchar()&length()
x<-"abcdefg"
nchar(x);length(x)
paste();strsplit();substr()
gsub();sub()
grep();grepl()
## Vectorization in R
f.data <- data.frame(x=rnorm(10),y=runif(10))
lapply(f.data,FUN=function(x) list(median=median(x),sd=sd(x)))

sapply(f.data,FUN=function(x) list(median=median(x),sd=sd(x)))

head(iris)
attach(iris)
tapply(Sepal.Width,INDEX=Species,FUN=mean)

# others
aggregate();tapply();by();

replicate();
game <- function() {
  n <- sample(1:6,2,replace=T)
  return(sum(n))
}
replicate(n=10000,game())

# Vectorize() 它能将一个不能进行向量化运算的函数进行转化，使之具备向量化运算功???

###### Recursion Structure
# if(condition) {expr1} else {expr2}
x <- 1:100
y <- rep(T,100)
for (i in 3:100) {
  if (all(i%%(2:(i-1))!=0)){
    y[i] <- TRUE
  } else {y[i] <- FALSE
  }
}
print(x[y])
## all()函数的作用是判断一个逻辑序列是否全为真，%%的作用是返回余数。在if/else语句中一个容易出现的错误就是else没有放在｝的后面

# an example

craps <- function() {
  #returns TRUE if you win, FALSE otherwise
  initial.roll <- sum(sample(1:6,2,replace=T))
  if (initial.roll == 7 || initial.roll == 11) return(TRUE)
  while (TRUE) {
    current.roll <- sum(sample(1:6,2,replace=T))
    if (current.roll == 7 || current.roll == 11) {
      return(FALSE)
    } else if (current.roll == initial.roll) {
      return(TRUE)
    }
  }
}
mean(replicate(10000, craps()))

##? what does while(TRUE) mean? Interesting~

## 查错
cat();
iter <- stats::rpois(1, lambda=10)
## print an informative message
cat("iteration = ", iter <- iter + 1, "\n")
#另一种避免出错的方法是尽量使用函数。但是在使用函数时需要注意的问题是输入参数的不可预测性???
stop('your message here.')
#对函数进行调试的重要工具是browser()，它可以使我们进入调试模式逐行运行代码。在函数中的某一行插入browser()后，在函数执行时会在这一行暂停中断，并显示一个提示符。此时我们可以在提示符后输入任何R语言的交互式命令进行检查调试。输入n则会逐行运行程序，并提示下一行将运行的语句。输入c会直接跳到下一个中断点。而输入Q则会直接跟出调试模式???
#debug()函数和browser()是相似的，如果你认为某个函数，例如fx(x)，有问题的话，使用debug(fx)即可进入调试模式。它本质上是在函数的第一行加入了browser，所以其它提示和命令都是相同的???

#trace(fx1,fx2),该函数可以让我们在调用fx1函数时自动调用fx2，通常我们将browser作为fx2???
#setBreakpoint(filename,linenumber),该函数可在程序的某一行设置中断点???
#traceback()，在程序出现报错时，可使用该函数回溯程序调用过的函数???

##2013.2.6. Euler Project 2
i <-2;x <-1:2

while (x[i]<4e6) {
  x[i+1]<-x[i]+x[i-1]
  i=i+1
}

x<-x[-i];
i<-1
sumFib <-0
while (x[i]<4e6) {
  if(x[i]%%2==0){
    sumFib <- sumFib+x[i]
  }  
  i=i+1
}  # The code could run and give the correct result, but one error return.

# Alternatively, we can use 
sum(x[x%%2==0])

##?ṩ??????????????Ҫx <- x[-i]??һ??
x = 1:2
for (i in 2:1000) {
  x[i+1] = x[i-1] + x[i]
  i = i + 1
  if (x[i] > 4e6) break
}
sum(x[x %% 2 == 0])

## 12.6.
1:10
10:1
x <- 1:10
print(x)
sum(x)
x > 5
x[x > 5]
x > 5 & x < 8
x > 8 | x < 3
10 %% 3
9 %% 3














x %% 3
x %% 3 == 0
x[x %% 3 == 0]

x<-1:999
sum(x[x%%3==0|x%%5==0])


