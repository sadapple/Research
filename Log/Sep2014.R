## 9.28
x <- sample( c( "dog", "cat", "hamster", "goldfish"), size = 1000, prob=c( 0.25, 0.25, 0.25, 0.25 ) ,replace=TRUE )
table(x)
## search patterns of all the objects in R
ls( pattern="a." , all.names=TRUE)

## 9.25
## Precisions in R
options(digits=6)
a=rnorm(1000,59,3)
b=rnorm(1000,152,4)
a
b
c=round(a,3)
d=round(b,3)
c
d
## save R objects
save(list = ls(all = TRUE), file = ".RData")
## Using scan() to read data
data <- t(matrix(scan("fileName",sep=’,’), 5, 364))
## filter missing values
x[!is.na(x)]

## some other useful functions
stack()
unstack()
merge()
aggregate()
unique()
## rug() show density on side of the plot
x <- rnorm(50)
plot(sort(x))
rug(x,side = 2)







#### before 9.25

## Find the index of all different values in a vector
x = c(2,2,2,2,3,3,3,2,1,2,3,2,2,2,1,1,3,2,1,2,1,2,1,1,1,1,1,1,1,1,1,1,1,1)
lapply(1:length(unique(x)),function(i) which(x == i))

##### Scotland Referendum via Bayes

# Data latest polls
polls = NULL
polls <- data.frame( rbind(
  Opinium = c(43, 47, 1156),
  Survation = c(44, 48, 1000),
  ICM =  c(41, 45, 1175)
))

# set up for decimals
polls[, 1:2] <- polls[, 1:2]/100

# weighting polls
wtd.polls <- rbind(polls, c(apply(polls[,1:2],2, weighted.mean, polls[,3]), sum(polls[,3])))

names(wtd.polls) <- c("Yes","No","n")

wtd.polls$Others = 1-wtd.polls$Yes-wtd.polls$No

# Adjusting for the undecideds for the final results
wtd.polls[5,] <- data.frame(wtd.polls[4,1:2] + wtd.polls[4,1:2]/ sum(wtd.polls[4,1:2])*wtd.polls[4,4],n=wtd.polls[4,3],Others=0)

##################################################
## If we have more than two categories (Yes, No, Uncertain), Beta distribution won't work properly.
## The following is the main function, which I used to randomly draw data from a dirichlet distribution.
##################################################
prob2win = function(x=wtd.polls, j=5, export=1){
  p=gtools::rdirichlet(100000, x[j,'n']*c(x[j,'Yes'],x[j,'No'], 1-x[j,'Yes']-x[j,'No'])+1
  )
  if(export==1){
    mean(p[,1] < p[,2])
  } else {
    return(p)
  }
}


(No.win.probs = prob2win(j=5,export=1) )

## set simulated data for determining parameters and graphing
p = prob2win(j=5,export=2)

## Get the marginal distribution. Since we have two classes the Dirichlet is distributed as a Beta.
p = cbind(p, p[,2]-p[,1])

## Draw a histogram of simulated data
hist(p[,4], col="gray", nclass=50, main="Histogram of the Yes/No Differences", xlab="Yes/No Difference")
abline(v=0, col=c('red'), lwd=2, lty=c(1))

## In this case I ran the function a few time to set the start value close to what I believe be the output.
## So, you have to adjust the shape parameters (shape1 and shape2) manually; it's much of a trial and error exercise,
## but you can use the function beta.par from SciencesPo package to estimate these parameters using \mu and \sigma^2.
library(MCMCpack)
fit1 = fitdistr(p[,1], "beta",
                  start=list(shape1=3,shape2=3))

fit2 = fitdistr(p[,2], "beta",
                    start=list(shape1=3,shape2=3))

library(png)
#Replace the directory and file information with your info
png <- readPNG(file.choose())

#Get the plot information so the image will fill the plot box, and draw it
lim <- par()
rasterImage(png, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])
grid()

## Draw densities of simulated data
curve(dbeta(x,fit1$estimate[1],fit1$estimate[2]), ylim=c(0,50), xlim=c(.40,.60), col='NA', lty=1, lwd=3, ylab="Density", xlab=expression("Posterior "*pi(theta)*""), main="Distribution of the Yes/No Preference Referendum 2014", sub=paste("Probability that NO wins: ", round(No.win.probs,3) ) ) ## yes 1

curve(dbeta(x,fit1$estimate[1],fit1$estimate[2]), xlim=c(.43,.52), col='green', lty=1, add=TRUE, lwd=8) ## yes 1

curve(dbeta(x,fit2$estimate[1],fit2$estimate[2]), add=TRUE, col='magenta', xlim=c(.49,.56), lty=1, lwd=8) ## No 2

abline(v=c(median(p[,1]), median(p[,2])), col=c('green','magenta'), lwd=2, lty=c(2,2))
legend("topright",c("Yes","No"), lwd=2, col=c('green','magenta'), lty=c(1,1))
