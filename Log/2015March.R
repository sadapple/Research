## 3.29
## color in R
n <- 10
mycolors <- rainbow(n)
pie(rep(1,n),labels=mycolors,col=mycolors)
mygrays <- gray(0:n/n)
pie(rep(1,n),labels=mygrays,col=mygrays)

## 3.28
## sapply and vapply

## 3.26
## make up P4
(0.2*0.05)/(0.8*0.85+0.2*0.05)

## 3.24
x <- rnorm(100)
qqnorm(x)
## 3.12
## 401 midterm score analysis
mid <- read.csv(file = "/media/Learn/Dropbox/extra/Teaching/2015-Spring-Stat-401/401grades.csv",header = T)
mid.score <- mid[,3]
mid.score <- mid.score[-66]
mid.score <- as.vector(na.omit(mid.score))

mid.mean <- mean(mid.score,na.rm = T)
mid.median <- median(mid.score,na.rm = T)
mid.variance <- var(mid.score,na.rm = T)
qqnorm(mid.score)   ## from this plot, we see the density should be left skewed as shown in the later histgram plot
mid.hist <- hist(mid.score,breaks=10,col="red",xlab = "Midterm score")
# add a normal curve
xfit<-seq(min(mid.score),max(mid.score),length=40)
yfit<-dnorm(xfit,mean=mean(mid.score),sd=sd(mid.score))
yfit <- yfit*diff(mid.hist$mids[1:2])*length(mid.score) # why multiply the last two terms?
lines(xfit, yfit, col="blue", lwd=2)

# kernel density plot
mid.density <- density(mid.score,bw = 5,kernel = "gaussian",na.rm = T)
plot(mid.density)
polygon(mid.density,col = "red",border = "blue")


## 3.10 generate matrices
k <- 10
p <- 20
tmp <- matrix(rep(0,k*k),nrow=k)
tmp[col(tmp)-row(tmp)!=0] <- 0.5
tmp <- rbind(tmp,matrix(rep(0,k*(p-k)),nrow=p-k))
tmp <- cbind(tmp,matrix(rep(0,p*(p-k)),nrow=p))
mat <- tmp+diag(1,p)

## 3.9
## 401 midterm
# P1
x <- c(9, 12, 10, 11, 11, 7, 12, 6, 11, 4, 10, 10, 11, 9, 10, 7, 10, 8, 8, 9, 8, 9, 11, 9, 8, 6, 10, 6, 8, 11)
mean(x)
median(x)
var(x)
y <- sort(x)
boxplot(x)
# P2
x <- c(0,3,5,8,9)
y <- c(1,2,4,3,5)
cor(x,y)

# P7
pnorm(-2)
pnorm(-0.4)
qnorm(0.002)
# P8
pnorm(-15/16)
qnorm(0.95,101.5,1.6)


## 3.2
## install Rmosek
## have to manual configure the PKG_MOSEKHOME variable since the automatic way doesn't work
install.packages("Rmosek", type="source", INSTALL_opts="--no-multiarch",
repos="http://download.mosek.com/R/7",
configure.vars="PKG_MOSEKHOME=/home/sadapplelc/mosek/7/tools/platform/linux64x86 PKG_MOSEKLIB=mosek64")

                                        # getting started
require(Rmosek)
library(help="Rmosek")  ## find all functions of Rmosek
help("mosek", package="Rmosek")  ## a short manual

## linear optimization eg
lo1 <- list()
lo1$sense <- "max"
lo1$c <- c(3,1,5,1)
lo1$A <- Matrix(c(3,1,2,0,
                  2,1,3,1,
                  0,2,0,3), nrow=3, byrow=TRUE, sparse=TRUE)
lo1$bc <- rbind(blc = c(30,15,-Inf),
                buc = c(30,Inf,25))
lo1$bx <- rbind(blx = c(0,0,0,0),
                bux = c(Inf,10,Inf,Inf))
r <- mosek(lo1)

## mix integer linear programming eg
modelfile <- system.file(package="Rmosek", "extdata", "milo1.opf")
r_read <- mosek_read(modelfile)
r_solve <- mosek(r_read$prob)

################ dplyr usage
library("dplyr")

## 5 verbals
## filter-choose rows
## select-choose columns
## arrange-order rows
## mutate-make new columns 1:1
## summarize-make new colunns N:1

set.seed(1234)
rows <- 8
d <- data.frame(shape = sample(c("circle", "square"),
                               rows, replace = TRUE),
                color = sample(c("red", "blue"),
                               rows, replace = TRUE),
                area = runif(rows, min = 1, max = 10))

filter(d, shape == "circle")
filter(d,
       ifelse(shape == "circle", area < 2, area > 3))

select(d, shape, area)
select(d, shape:area)
select(d, -color)
select(d, contains("sh"))

arrange(d, shape, color)
arrange(d, -area)  # from big to small

mutate(d, new.color = sample(rainbow(8)),
          perimeter = ifelse(shape == "square",
                             4 * area^0.5,
                             2 * (pi * area)^0.5),
          side.length = ifelse(shape == "square",
                               perimeter / 4,
                               NA))

d.by.col <- group_by(d, color)
d.by.col
summarize(d.by.col,
          tot.area = sum(area))

d %>%        ## %>% pipe operator
  group_by(shape, color) %>%
  summarize(max.area = max(area))

## larger data frame eg
library(babynames)
d <- tbl_df(babynames)
d
# Q:What were the most common names in the 1980s for each sex?
d %>%
  filter(year >= 1980 & year < 1990) %>%
  group_by(sex, name) %>%
  summarize(count = sum(n)) %>%
  mutate(position = rank(-count)) %>%
  filter(position <= 10) %>%
  arrange(sex, position)
                                        # compare readability between two approaches
d %>%
  filter(year >= 1980 & year < 1990) %>%
  group_by(sex, name) %>%
  summarize(count = sum(n)) %>%
  mutate(position = rank(-count)) %>%
  filter(position <= 10) %>%
  arrange(sex, position)

arrange(
  filter(
    mutate(
      summarize(
        group_by(
          filter(d, year >= 1980 & year < 1990),
          sex, name
        ),
        count = sum(n)
      ),
      position = rank(-count)
    ),
    position <= 10
  ),
  sex, position
)

