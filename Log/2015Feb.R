## 2.2
## Formatting problem
a <- c('A' ,'B' ,'C', 'a' ,'b' ,'c' ,'d' ,'e' ,'f')  
d <- data.frame(c1=a[1], c2=a[2], c3=a[3], c4=a[-(1:3)])  ## Use auto-complete rule of vectors

## Logistic regression vs LDA


## 2.10
## Wolf data

wolf.weight = c(71,57,84,93,101,84,88,117,90,86,71,71,86,93,86,77,68,106,73)
wolf.length = c(134,123,129,143,148,127,136,146,143,142,124,125,139,140,133,122,125,123,122)
plot(wolf.weight,wolf.length,xlim=c(20,150),ylim=c(100,160))


## 2.18
x <- read.table("dexter_train.data", fileEncoding="UTF-16")

## 2.26
## Arrays
## The values in the data vector give the values in the array in the same order as they would
## occur in FORTRAN, that is “column major order,” with the first subscript moving fastest and
## the last subscript slowest.

a <- rnorm(1500)
dim(a) <- c(3,5,100)

## construct a matrix to index an array
## Negative indices are not allowed in index matrices. NA and zero values are allowed: rows in the
## index matrix containing a zero are ignored, and rows containing an NA produce an NA in the
## result.
## Index matrices must be numerical: any other form of matrix (e.g. a logical or character
## matrix) supplied as a matrix is treated as an indexing vector.

i <- array(c(1:3,3:1),dim=c(3,2))
a[i]
j <- array(c(1:3,3:1),dim=c(2,3))
a[j]

# Mixed vector and array arithmetic Rules
##   The expression is scanned from left to right.
## • Any short vector operands are extended by recycling their values until they match the size
## of any other operands.
## • As long as short vectors and arrays only are encountered, the arrays must all have the same
## dim attribute or an error results.
## • Any vector operand longer than a matrix or array operand generates an error.
## • If array structures are present and no error or coercion to vector has been precipitated, the
## result is an array structure with the common dim attribute of its array operands.



## outer product of arrays

b <- array(rnorm(20),dim=c(4,5))
dim(a %o% b)

outer(a,b,"*")

## Example: Determinant of a 2 by 2 single-digit matrix
d <- outer(0:9, 0:9)
fr <- table(outer(d, d, "-"))
plot(as.numeric(names(fr)), fr, type="h", xlab="Determinant", ylab="Frequency")

## Generalized transpose of an array

## The function aperm(a, perm) may be used to permute an array, a. The argument perm must be
## a permutation of the integers {1, . . . , k}, where k is the number of subscripts in a


