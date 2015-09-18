
# 2.22. Learn Ploting in various packages -------------------------------------------------------------------

# check lattice package information
library(lattice)
package ?lattice
library(help=lattice)
data(environmental)
?environmental
head(environmental)

xyplot(ozone ~ radiation,data=environmental)
xyplot(ozone ~ radiation,data=environmental,main="Ozone vs. Radiation")
xyplot(ozone ~ temperature,data=environmental)
summary(environmental$temperature)
temp.cut <- equal.count(environmental$temperature,4)
temp.cut

xyplot(ozone ~ radiation | temp.cut, data=environmental, layout=c(2,2), as.table=TRUE, pch=20,
       panel = function(x,y,...) {
         panel.xyplot(x,y,...)
         fit <- lm(y ~ x) # linear
         panel.abline(fit,lwd=2)

       })

xyplot(ozone ~ radiation | temp.cut, data=environmental, layout=c(2,2), as.table=TRUE, pch=20,
       panel = function(x,y,...) {
         panel.xyplot(x,y,...)
         panel.loess(x,y) # nonlinear

       })

wind.cut <- equal.count(environmental$wind,4)
wind.cut
xyplot(ozone ~ radiation | temp.cut * wind.cut, data=environmental, as.table=TRUE, pch=20,
       panel = function(x,y,...) {
         panel.xyplot(x,y,...)
         panel.loess(x,y) # nonlinear

       })

# other plots
splom(~ environmental)
histogram(~ ozone | temp.cut * wind.cut, data = environmental)



# 2.21. -------------------------------------------------------------------


## The "drop" argument in subset()
aa <- matrix(rnorm(4),2,2)
colnames(aa) <- c("male","female")
bb <- subset(aa,select=male)
cc <- subset(aa,select=male,drop=T)
class(bb);class(cc)

df <- data.frame(a = factor(letters[1:20]), b = 1:20)
# subset the data.frame
ssdf <- subset(df, a %in% c("a", "b", "c"), drop=FALSE)
ssdf$a

ssdf2 <- subset(df, a %in% c("a", "b", "c"), drop=TRUE)
# Note that the result is still the same:
ssdf2$a
# One more step to resolve
droplevels(ssdf$a)



## Data frame in a data frame
require(reshape2)
df <- data.frame(ID=11:13, FOO=c('a|b','b|c','x|y'))
df = transform(df, FOO = colsplit(FOO, pattern = "\\|", names = c('a', 'b')))
df
str(df)
# Resolution
df=as.data.frame(as.list(df))

## the %in% operator
1:6 %in% 0:36
0:37 %in% 0:36

all(1:6 %in% 0:36)
any(1:6 %in% 0:36)


## Read and Write interaction
write.table(x=data.frame(v=1:10),file=fifo("myfifo"))
read.table(file=fifo("myfifo"))

# Socket(support Windows)

con1 <- socketConnection(port = 8888, server = TRUE, blocking=TRUE)
write.table(x=data.frame(v=1:10), file=con1)
close(con1)

con2 <- socketConnection(port = 8888)
read.table(file=con2)
close(con2)

#端口号是自己设定的，如果不是超级用户，只能设置1024之后的号码。
#blocking 参数设置读写阻塞状态。

#初始化 socket时，出现停止，不是死机，而是正在listening，等待另一个程序（客户端）建立链接。
#运行示例代码，可以先打开两个R，先在一个里面粘贴服务端代码运行，再在另一个里面粘贴客户端代码。

# 1. 对于write.table函数，设置col.names=FALSE
# 而不是对read.table函数设置。
# 2. 另外，repeat没有必要包含con1及con2语句，这样每次都得重新建立TCP连接，效率太低了。
# repeat只包含write.table或read.table语句就可以了。
# 3. 适度考虑blocking参数的使用。
# 4. 如果非常强调效率，还是使用fifo，这个不涉及网络通信，效率比socket高。

# 读比写快，所以读了很多空白，就出错了。
# 在流模式中，在读端设置blocking，同时告诉read.table一次读多少行。

#服务端代码
con1<-socketConnection(port=8888,server=TRUE);
repeat{
  write.table(x=data.frame(v=1:10),file=con1,col.names=FALSE);
}
close(con1)

#客户端代码
t=0
con2<-socketConnection(port=8888,blocking=TRUE);
repeat{
  t=t+1;
  cat("t",t,"\n")
  A=read.table(file=con2, nrows=10);
  print(A);
}
close(con2)

# socket是网络通信的接口，从它的角度来看，所有的数据都是“流”。
# 接受数据的时候，
# 要么采用“不阻塞”的形式：有数据，不管多少就接受；没有数据就立即返回0。
# 要么采用“阻塞”的形式：所有数据读完，或接受到指定数量的数据才返回，这个数量要你来指定。
# nrows=10，指定了数据的文本行数，便于返回。

#   楼主感兴趣，并有余力的话，可以扩展了解一下 socket 及其它 Unix 编程的细节。推荐 APUE 这本书（最新版是2013版。）
# 此书旧版，昔日在图书馆，伴我度过了无数悠闲的午后时光，日后终于练成了和本专业无关的屠龙大法。
# 以前还有一兄弟，偶然由于发文章的出版社要求排版，结果练成了 LaTeX 大法。我辈中人，皆由沉迷于工具，走火入魔，蹉乎！


# 2.20. -------------------------------------------------------------------

Sys.setlocale(,"ENG")
Sys.setlocale(,"CHS")
step()
basicStats() # fBasics
normalTest()
na.omit()


# 2.17. -------------------------------------------------------------------


# Palindrome


# End ---------------------------------------------------------------------







