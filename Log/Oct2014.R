## 10.26
x <- c(5,12,21,12)
xf <- factor(x)
str(xf)
unclass(xf)
attr(xf,"levels")

## 10.23
## ifelse()
## ## ifelse returns a value with the same shape as test which is filled
## with elements selected from either yes or no depending on whether the
## element of test is TRUE or FALSE.

mydata<- matrix(c(1:12),nrow=3)
ifelse(is.null(nrow(mydata)),mydata <- data.frame(t(mydata)),mydata <- mydata)



## 10.9
## use of which
x<-c(0,2,4,5,1,5)
which(x %in% c(0,2,1))

## 10.6
x <- list(abc=123,you="me",att=matrix(c(1,2,3,4),nrow=2))

## Game of craps
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

## 10.8
## Blackjack simulation

# 从13张扑克牌中抽一张牌的函数，用n和m来控制大小牌的抽取概率
card.select <- function(n=1,m=6) {
 x <- sample(1:13,1,prob=rep(c(n,m),c(9,4))/sum(rep(c(n,m),c(9,4))))
 # J，Q，K都被转为10点
 x <- ifelse(x>10,10,x)
 return(x)
}

# 完成一次二十一点游戏的函数，point为是否继续要牌的判断阀值
game <- function(point) {
  # 最开始得到的两张牌
  select <- c(card.select(),card.select())
  # 将原始序列select中的1点转成11点，求牌的点数和
  Ato11 <- select
  Ato11[Ato11==1] <- 11
  card.sum <- sum(Ato11)
  # 进入条件循环，以判断是否应该继续要牌
  #若牌点数和大于阀值，则不再要牌
  while (card.sum <= point) {
    select <- c(select, card.select())
    Ato11 <- select
    Ato11[Ato11==1] <- 11
    card.sum <- sum(Ato11)
 }
  # 若点数和大于21且其中有A，则A看作1点，用原始的序列求和
  if (card.sum > 21 && 1 %in% select) {
  card.sum <- sum(select)
  }
  # A转为1点后，再次条件循环，以判断是否应该继续拿牌
  while (card.sum <= point) {
    select <- c(select, card.select())
    card.sum <- sum(select)
 }
  # 若点数大于21点，则爆牌为0
  y <-ifelse((card.sum<=21),card.sum,0)
 return(y)
# cat('select=',select,'\n','return=',y,'\n')
}

# 与庄家博弈，赌场规定庄家以16为阀值，玩家应以12为阀值，等待庄家爆掉
player <- replicate(100000,game(12))
dealer <- replicate(100000,game(16))
#结果有赢输平三种情况
result <-ifelse(player > dealer,1,ifelse(player < dealer,-1,0))
# 若二者均爆牌且平局，设玩家输
result[player==0 & result==0] <- -1
# 将平的情况略去以方便进行二项检验
result.no.tie <- result[result!=0]
# 二项检验
binom.test(length(result.no.tie[result.no.tie==1]),length(result.no.tie))
# 观察玩家连赢或连输的游程
table(rle(result))
# 计算累积利润
profit <- cumsum(result)
# 绘图以观察利润变化
q <- ggplot(data.frame(profit,index=1:length(profit)),aes(index,profit))
q + geom_line(colour='lightskyblue4')

# 考查在不同的大小牌比值的情况下，玩家获胜的概率
odd.10 <-  win <- rep(0,10)
for (i in 1:10) {
    card.select <- function(n=1,m=i) {
        x <- sample(1:13,1,prob=rep(c(n,m),c(9,4))/sum(rep(c(n,m),c(9,4))))
        x <- ifelse(x>10,10,x)
        return(x)
    }
    odd.10[i] <- 4*i/9
    player <- replicate(100000,game(12))
    dealer <- replicate(100000,game(16))
    result <-ifelse(player > dealer,1,ifelse(player < dealer,-1,0))
    result[player==0 & result==0] <- -1
    result.no.tie <- result[result!=0]
    win[i] <- length(result.no.tie[result.no.tie==1])/length(result.no.tie)
}

d <- ggplot(data.frame(odd.10,win),aes(odd.10,win))
d + geom_line(colour='lightskyblue4',size=1) +
    geom_point(colour='red4',size=3.5) +
    geom_hline(y=0.5,linetype=2)
