## Rstan installation
chooseCRANmirror(graphics = FALSE)
install.packages("rstan", dependencies = TRUE )
library(rstan)

## Stat 211 Quiz2 problem 1
temp <- c(88.6,93.3,80.6,69.7,69.4,79.6,80.6,76.3,71.6,84.3,75.2,82.0,83.3,82.6,83.5)
chirp <- c(20,19.8,17.1,14.7,15.4,15.0,16.0,14.4,16.0,18.4,15.5,17.1,16.2,17.2,17.0)

model <- lm(chirp ~ temp)

newobs <- data.frame(temp = c(80))
predict(model,newobs)  ## result: 16.64486

## Simulate log Poisson

x <- rpois(10000000,lambda = 1/2)
x.log <- log(x)

# is.finite(x.log) : extract the finite elements
x.logFin <- x.log[is.finite(x.log)]

# check variance
var(x)
var(x.logFin)

# consider log(Poisson+1)
# not the right approach
x <- rpois(100000,lambda = 1/2)+1
x.log <- log(x)

var(x)
var(x.log)

x <- rpois(100000,lambda = 1/2)
x.log <- log(x)
x.log[is.infinite(x.log)] <- 0
var(x.log)
