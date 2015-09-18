#### NMA
## discrete version with logit link

#Random effects model for multi-arm trials (any number of arms)
model{
for(i in 1:22){
         w[i,1] <-0
	     delta[i,1]<-0
	     for (k in 1:na[i])  {
	            HbA1c7[i,k] ~ dbin(p[i,k],n[i,k])      # binomial likelihood
		  	  logit(p[i,k])<-mu[i] + delta[i,k]

		}
		 for (k in 2:na[i]) {
                 delta[i,k] ~ dnorm(md[i,k],taud[i,k])             # trial-specific LOR distributions
                 md[i,k] <-  dd[ntreat[i,k]] - dd[ntreat[i,1]]  + sw[i,k]
				    +gamma01*(HasTZD[i,k]-HasTZD.bar)*step(1.1 - (ntreat[i,1]))
																			                   # mean of LOR distributions
                  taud[i,k] <- prec*2*(k-1)/k                                    #precision of LOR distributions
                  w[i,k] <- (delta[i,k]  - dd[ntreat[i,k]] + dd[ntreat[i,1]])
				    +gamma01*-1*(HasTZD[i,k]-HasTZD.bar)*step(1.1 - (ntreat[i,1]))

                  sw[i,k] <-sum(w[i,1:k-1])/(k-1) }                 # cumulative adjustment for multi-arm trials
		}

HasTZD.bar<-0

gamma01 ~ dnorm(0.0,0.000001)

#Give priors for differences
	dd[1]<-0
	for (k in 2:14){dd[k] ~ dnorm(0,0.000001) }

tau<-1/prec
tau.sqrt<-sqrt(tau)

prec <- 1/(sd*sd)
sd ~ dunif(0,1)

# Unconstrained baseline effect (study-specific baselines)
 for (j in 1:22) {
 mu[j] ~ dnorm(0.0,0.000001)
	 }

#Absolute log odds on Treatment A based on trials in which it was used
for (i in 1:22){

mu1[i] <- exp(mu[nstudy[i]])*n[i,1]*(step(ntreat[i,1]-1))*(step(1-ntreat[i,1]))
tot[i] <- n[i,1]*(step(ntreat[i,1]-1))*(step(1-ntreat[i,1]))
                  }

m<- log(sum(mu1[])/sum(tot[]))

# Absolute pr(success) Treatments B,C,D based on T[1] and the
#   MEAN  Relative treatment effects
for (k in 1:14)   { logit(T[k])<- m +dd[k] }

#All pairwise log odds ratios and odds ratios
for (c in 1:14-1){
	for (k in (c+1):14){
                lor[c,k] <- dd[k] - dd[c]
		log(or[c,k]) <- lor[c,k]
		lorprob[c,k] <- step(lor[c,k])
		}
	}
}




## continuous version with normal setup
#Random effects model for multi-arm trials (any number of arms)
model{
for(i in 1:29){
         w[i,1] <-0  # what does w[i,k] store?
	     delta[i,1]<-0
	     for (k in 1:na[i])  {
			prec.HbA1c[i,k] <- n[i,k]/HbA1c.var[i,k]    ## what does prec.HbA1c define, precision
			HbA1c[i,k]~dnorm(theta[i,k],prec.HbA1c[i,k])
		  	  theta[i,k]<-mu[i] + delta[i,k]

	  }
		 for (k in 2:na[i]) {
                 delta[i,k] ~ dnorm(md[i,k],taud[i,k])             # trial-specific LOR distributions
                 md[i,k] <-  dd[ntreat[i,k]] - dd[ntreat[i,1]]  + sw[i,k]
				    +gamma01*(HbA1cBase[i,k]-HbA1cBase.bar)*step(1.1 - (ntreat[i,1])) # why specify the step function like this? same gamma for all treatment? HbA1cBase depend on treatment index?
																			                   # mean of LOR distributions
                 taud[i,k] <- prec*2*(k-1)/k   # vary with index k
                  w[i,k] <- (delta[i,k]  - dd[ntreat[i,k]] + dd[ntreat[i,1]])
				    +gamma01*-1*(HbA1cBase[i,k]-HbA1cBase.bar)*step(1.1 - (ntreat[i,1])) # -1?

 #adjustment, multi-arm RCTs
                  sw[i,k] <-sum(w[i,1:k-1])/(k-1) }                 # cumulative adjustment for multi-arm trials


  }


HbA1cBase.bar<-8.182
gamma01 ~ dnorm(0.0,0.000001)

#Give priors for differences
	dd[1]<-0
	for (k in 2:15){dd[k] ~ dnorm(0,0.000001) }

tau<-1/prec
tau.sqrt<-sqrt(tau)

prec <- 1/(sd*sd)
sd ~ dunif(0,4)

# Unconstrained baseline effect (study-specific baselines)
 for (j in 1:29) {
 mu[j] ~ dnorm(0.0,0.000001)
	 }

#Absolute log odds on Treatment A based on trials in which it was used
for (i in 1:29){
mu1[i] <- mu[nstudy[i]]*n[i,1]*(step(ntreat[i,1]-1))*(step(1-ntreat[i,1]))
tot[i] <- n[i,1]*(step(ntreat[i,1]-1))*(step(1-ntreat[i,1]))
                  }

m<- sum(mu1[])/sum(tot[])


# Absolute pr(success) Treatments B,C,D based on T[1] and the
#   MEAN  Relative treatment effects
for (k in 1:15)   { T[k]<- m +dd[k] }

#All pairwise comparisons
for (c in 1:15-1){
	for (k in (c+1):15){
		meandif[c,k] <- dd[k] - dd[c]
		meandifprob[c,k] <- step(meandif[c,k])
		}
	}

}





## 7.12
## compare ACF and PACF between models
## arima(p,d,q)
x <- arima.sim(list(order = c(3,1,0), ar = c(0.6,0.2,0.1)), n = 500)
acf(x,lag.max = 50)
pacf(x,lag.max = 50)

x <- arima.sim(list(order = c(0,1,3), ma = c(0.6,-0.2,0.1)), n = 500)
acf(x,lag.max = 50)
pacf(x,lag.max = 50)

## ar(p)
x <- arima.sim(list(order = c(2,0,0), ar = c(0.75,0.2)), n = 500)
acf(x,lag.max = 50)   ## decays exponentially
pacf(x,lag.max = 50)

x <- arima.sim(list(order = c(3,0,0), ar = c(-0.55,-0.35,0.5)), n = 500)
acf(x,lag.max = 50)   ## decays exponentially
pacf(x,lag.max = 50)



## ma(q)
