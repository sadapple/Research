## 5.25

install.packages("C:/Users/sadap_000/mixAK_4.0-5.zip", repos = NULL, type="source")
data(Enzyme)

## 5.24
## How to replace or delete a particular character in all the entries of a data frame
 # generate data frame
a<-paste(1:100000,"SquareM",sep="")
b<-paste(100000:1,"rmb",sep="")
ab<-data.frame(a,b)

# different approaches comparison
library(stringr)
system.time(stringr::str_replace(a, "SquareM", ""))
system.time(gsub("SquareM", "", a))
system.time(substr(a, 1, nchar(a) - 7)) ## the shortest time

## 5.23
## sorting a data frame
x=c(3,1,5,4,9)
y=c(2,8,4,6,7)
mydata=data.frame(x=x,y=y)
newdata <- mydata[order(x),] ## The order() function!
## 5.22
## eg for explaining scoping rule and environments
# DES.R:  R routines for discrete-event simulation (DES)

# each event will be represented by a data frame row consisting of the
# following components:  evnttime, the time the event is to occur;
# evnttype, a character string for the programmer-defined event type;
# optional application-specific components, e.g.
# the job's arrival time in a queuing app

# a global list named "sim" holds the events data frame, evnts, and
# current simulated time, currtime; there is also a component dbg, which
# indicates debugging mode

# forms a row for an event of type evntty that will occur at time
# evnttm; see comments in schedevnt() regarding appin
evntrow <- function(evnttm,evntty,appin=NULL) {
   rw <- c(list(evnttime=evnttm,evnttype=evntty),appin)
   return(as.data.frame(rw))
}

# insert event with time evnttm and type evntty into event list;
# appin is an optional set of application-specific traits of this event,
# specified in the form a list with named components
schedevnt <- function(evnttm,evntty,appin=NULL) {
   newevnt <- evntrow(evnttm,evntty,appin)
   # if the event list is empty, set it to consist of evnt and return
   if (is.null(sim$evnts)) {
      sim$evnts <<- newevnt
      return()
   }
   # otherwise, find insertion point
   inspt <- binsearch((sim$evnts)$evnttime,evnttm)
   # now "insert," by reconstructing the data frame; we find what
   # portion of the current matrix should come before the new event and
   # what portion should come after it, then string everything together
   before <-
      if (inspt == 1) NULL else sim$evnts[1:(inspt-1),]
   nr <- nrow(sim$evnts)
   after <- if (inspt <= nr) sim$evnts[inspt:nr,] else NULL
   sim$evnts <<- rbind(before,newevnt,after)
}

# binary search of insertion point of y in the sorted vector x; returns
# the position in x before which y should be inserted, with the value
# length(x)+1 if y is larger than x[length(x)]; could be changed to C
# code for efficiency
binsearch <- function(x,y) {
   n <- length(x)
   lo <- 1
   hi <- n
   while(lo+1 < hi) {
      mid <- floor((lo+hi)/2)
      if (y == x[mid]) return(mid)
      if (y < x[mid]) hi <- mid else lo <- mid
   }
   if (y <= x[lo]) return(lo)
   if (y < x[hi]) return(hi)
   return(hi+1)
}

# start to process next event (second half done by application
# programmer via call to reactevnt())
getnextevnt <- function() {
   head <- sim$evnts[1,]
   # delete head
   if (nrow(sim$evnts) == 1) {
      sim$evnts <<- NULL
   } else sim$evnts <<- sim$evnts[-1,]
   return(head)
}

# simulation body
# arguments:
#    initglbls:  application-specific initialization function; inits
#      globals to statistical totals for the app, etc.; records apppars
#      in globals; schedules the first event
#    reactevnt: application-specific event handling function, coding the
#       proper action for each type of event
#    prntrslts:  prints application-specific results, e.g. mean queue
#       wait
#    apppars:  list of application-specific parameters, e.g.
#      number of servers in a queuing app
#    maxsimtime:  simulation will be run until this simulated time
#    dbg:  debug flag; if TRUE, sim will be printed after each event
dosim <- function(initglbls,reactevnt,prntrslts,maxsimtime,apppars=NULL,
      dbg=FALSE) {
   sim <<- list()
   sim$currtime <<- 0.0  # current simulated time
   sim$evnts <<- NULL  # events data frame
   sim$dbg <<- dbg
   initglbls(apppars)
   while(sim$currtime < maxsimtime) {
      head <- getnextevnt()
      sim$currtime <<- head$evnttime  # update current simulated time
      reactevnt(head)  # process this event
      if (dbg) print(sim)
   }
   prntrslts()
}

## DES application : M/M/1 queue, arrival rate 0.5, service rate 1.0

# initializes global variables specific to this app
mm1initglbls <- function(apppars) {
    mm1glbls <<- list()
                                        # simulation parameters
    mm1glbls$arrvrate <<- apppars$arrvrate
    mm1glbls$srvrate <<- apppars$srvrate
                                        # server queue, consisting of arrival times of queued jobs
    mm1glbls$srvq <<- vector(length=0)
                                        # statistics
    mm1glbls$njobsdone <<- 0 # jobs done so far
    mm1glbls$totwait <<- 0.0 # total wait time so far
                                        # set up first event, an arrival; the application-specific data for
                                        # each event will consist of its arrival time, which we need to
                                        # record in order to later calculate the job's residence time in the
                                        # system
    arrvtime <- rexp(1,mm1glbls$arrvrate)
    schedevnt(arrvtime,"arrv",list(arrvtime=arrvtime))
}


                                        # application-specific event processing function called by dosim()
                                        # in the general DES library
mm1reactevnt <- function(head) {
    if (head$evnttype == "arrv") { # arrival
                                        # if server free, start service, else add to queue (added to queue
                                        # even if empty, for convenience)
        if (length(mm1glbls$srvq) == 0) {
            mm1glbls$srvq <<- head$arrvtime
            srvdonetime <- sim$currtime + rexp(1,mm1glbls$srvrate)
            schedevnt(srvdonetime,"srvdone",list(arrvtime=head$arrvtime))
        } else mm1glbls$srvq <<- c(mm1glbls$srvq,head$arrvtime)
                                        # generate next arrival
        arrvtime <- sim$currtime + rexp(1,mm1glbls$arrvrate)
        schedevnt(arrvtime,"arrv",list(arrvtime=arrvtime))
    } else { # service done
                                        # process job that just finished
                                        # do accounting
        mm1glbls$njobsdone <<- mm1glbls$njobsdone + 1
        mm1glbls$totwait <<-
            mm1glbls$totwait + sim$currtime - head$arrvtime
                                        # remove from queue
        mm1glbls$srvq <<- mm1glbls$srvq[-1]
                                        # more still in the queue?
        if (length(mm1glbls$srvq) > 0) {
                                        # schedule new service
            srvdonetime <- sim$currtime + rexp(1,mm1glbls$srvrate)
            schedevnt(srvdonetime,"srvdone",list(arrvtime=mm1glbls$srvq[1]))
        }
    }
}

mm1prntrslts <- function() {
    print("mean wait:")
    print(mm1glbls$totwait/mm1glbls$njobsdone)
}


# the call
 dosim(mm1initglbls,mm1reactevnt,mm1prntrslts,10000.0, list(arrvrate=0.5,srvrate=1.0))
# should return a value of about 2 (may take a while)


## 5.20
## Recursion in R
## quick sort eg
qs <- function(x){
    if (length(x)<=1) return(x)
    pivot <- x[1]
    rest <- x[-1]
    small <- rest[pivot>rest]
    big <- rest[pivot<=rest]
    small <- qs(small)
    big <- qs(big)
    return(c(small,pivot,big))
}

## 5.18
## verify Cabrera's statment about p.d. matrix
## simulate a p.d. matrix
## then switch the sign for one row/column, then among all the eigenvalues, exactly one will become negative.

Posdef <- function (n, ev = runif(n, 0, 50))
{
  Z <- matrix(ncol=n, rnorm(n^2))
  decomp <- qr(Z)
  Q <- qr.Q(decomp)
  R <- qr.R(decomp)
  d <- diag(R)
  ph <- d / abs(d)
  O <- Q %*% diag(ph)
  Z <- t(O) %*% diag(ev) %*% O
  return(Z)
}

n <- 3
tmp <- Posdef(n,ev = runif(n,1,1000))
tmp1 <- tmp
eigen(tmp)
ind <- sample(n,1)
tmp1[ind,] <- -tmp[ind,]
eigen(tmp)$values
eigen(tmp1)$values
