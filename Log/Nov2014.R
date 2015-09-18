## 11.10
## game of craps
cases <- function(p){
    return(p^2/(p+2/9))
}

x <- c(1/36,2/36,3/36,4/36,5/36,5/36,4/36,3/36,1/36)
2/9+sum(sapply(x,cases))                # game of craps winning prob

## 11.3
## Read and Write data
read.table
read.csv
write.table
write.csv

data <- readLines('http://en.wikipedia.org/wiki/Main_Page',n=10) # Read the first ten lines of the html file of the wiki main page

## Read multiple files 
doc.names <- dir("/Users/chuanliu/Dropbox/Workspace/R/Rcode")
doc.path <- sapply(doc.names,function(names) paste("/Users/chuanliu/Dropbox/Workspace/R/Rcode",names,sep='/'))
doc <- sapply(doc.path, function(doc) readLines(doc))

## String Manipulation

paste()
strsplit()
substr()
gsub()
grep()
grepl()                                 # return logic value

## eg extract the sender's email address

# read the email content
data <- readLines('data')
# Determine whether the oject is a character vector
class(data)
# find the line with "From:"
email <- data[grepl('From:',data)]
# separate the string by space, resulting in a character list
from <- strsplit(email,' ')
# convert the list into vector
from <- unlist(from)
# find the element with "@", this is the sender's email address
from <- from[grepl('@',from)]


## Vectorization in R

## apply
m.data <- matrix(rnorm(100),ncol=10)
apply(m.data,1,mean)

## lapply
f.data <- data.frame(x=rnorm(10),y=runif(10))
lapply(f.data,FUN=function(x) list(median=median(x),sd=sd(x))）

## sapply
sapply(f.data,FUN=function(x) list(median=median(x),sd=sd(x)))
class(test)

## tapply
head(iris)
attach(iris)
tapply(Sepal.Width,INDEX=Species,FUN=mean)
## by(), aggregate() are similar

## replicate
## simulate the sum of two dices
game <- function() {
    n <- sample(1:6,2,replace=T)
    return(sum(n))
}
x <- replicate(n=10000,game())
mean(x)
## Vectorize()


