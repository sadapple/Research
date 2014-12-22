###################### Markowitz portfolio selection
library(quadprog)

p=6
rf=.5
mu=rnorm(p,2,sqrt(1))
beta=cbind(rep(1,p),c(rep(1,p/2),rep(-1,p/2)))
## beta=rep(1,p)
X=matrix(rnorm(20*2),ncol=2)%*%t(beta)+matrix(rnorm(20*p,0,.5),ncol=p)
Sigma=cor(X)
Sigma=diag(abs(mu)^(1/6))%*%Sigma%*%diag(abs(mu)^(1/6))  ## ??

########### no constraint
A=as.numeric(t(rep(1,p))%*%solve(Sigma)%*%rep(1,p))
B=as.numeric(t(rep(1,p))%*%solve(Sigma)%*%mu)
C=as.numeric(t(mu)%*%solve(Sigma)%*%mu)
D=as.numeric(A*C-B^2)
mu.rf=mu-rf
B.rf=as.numeric(t(rep(1,p))%*%solve(Sigma)%*%mu.rf)
C.rf=as.numeric(t(mu.rf)%*%solve(Sigma)%*%mu.rf)
rm.mu=rf+C.rf/B.rf
rm.sigma=abs(rm.mu-rf)/sqrt(C.rf)
r.mu=B/A+(-2000:2000)/100
r.sigma=sqrt((C-2*r.mu*B+r.mu^2*A)/D)
rf.mu=((rf*1000):((B/A+20)*1000))/1000
rf.sigma=(abs(rf.mu-rf))/sqrt(C.rf)

########### no short sale
rp.mu=((min(mu)*1000):(max(mu)*1000))/1000
rp.mu=rp.mu[-1]
rp.sigma=rp.mu
W.p=array(0,dim=c(length(rp.mu),p))
A.mat=cbind(rep(1,p),mu,diag(1,p))
for (i in 1:length(rp.mu)){
  temp = solve.QP(Dmat=Sigma,dvec=rep(0,p),Amat=A.mat,bvec=c(1,rp.mu[i],rep(0,p)),meq=2)
  rp.sigma[i] = sqrt(temp$value*2)
  W.p[i,] = t(temp$solution)
}
rp.gmv.sigma=min(rp.sigma)
rp.gmv.mu=rp.mu[which(rp.sigma==rp.gmv.sigma)]

rfp.mu=((rf*1000):(max(mu)*1000))/1000
rfp.sigma=rfp.mu
W.fp=array(0,dim=c(length(rfp.mu),p))
A.mat=cbind(mu-rf,-rep(1,p),diag(1,p))
for (i in 1:length(rfp.mu)){
  temp=solve.QP(Dmat=Sigma,dvec=rep(0,p),Amat=A.mat,bvec=c(rfp.mu[i]-rf,-1,rep(0,p)),meq=1)
  rfp.sigma[i] = sqrt(temp$value*2)
  W.fp[i,] = t(temp$solution)
}

########### plot
par(mar=c(4,4,.5,.5))
plot(r.sigma,r.mu,xlab="Volatility",ylab="Expected return",type="n",col="blue",xlim=c(0,max(max(rp.sigma),rm.sigma+.2)),ylim=c(min(B/A-(rm.mu+.5-B/A),0),rm.mu+.5))#,xaxt="n",yaxt="n")
legend("bottomleft",c("no constraint","no short sale"),col=c("blue","black"),lty=c(1,1),bg="gray90")
lines(r.sigma[r.mu>=B/A],r.mu[r.mu>=B/A],col="blue")
lines(r.sigma[r.mu<B/A],r.mu[r.mu<B/A],col="blue",lty=2)
abline(h=B/A,lty=3,col="blue")
abline(v=sqrt(1/A),lty=3,col="blue")
lines(rf.sigma,rf.mu,col="blue")
points(rm.sigma,rm.mu,pch=20,col="blue")
axis(2, at = rf, labels = expression(r[f]),cex.axis=1.3,tick=FALSE)
abline(v=0,lty=3)
abline(h=rf,lty=3)

lines(rp.sigma[rp.mu>=rp.gmv.mu],rp.mu[rp.mu>=rp.gmv.mu])
lines(rp.sigma[rp.mu<rp.gmv.mu],rp.mu[rp.mu<rp.gmv.mu],lty=2)
abline(h=max(mu),lty=3)
abline(h=min(mu),lty=3)
abline(h=rp.gmv.mu,lty=3)
abline(v=rp.gmv.sigma,lty=3)
lines(rfp.sigma,rfp.mu)
W.fp.sum=apply(W.fp,MARGIN=1,sum)
aa=min(which(W.fp.sum==1))
points(rfp.sigma[aa],rfp.mu[aa],pch=20)

## num.rep=10000
## W=matrix(rnorm(num.rep*(p-1),1/p,1/p),ncol=p-1)
## W.sum=apply(W,MARGIN=1,sum)
## W=cbind(W,1-W.sum)
## mu.w=W%*%mu
## var.w=apply(W%*%Sigma*W,MARGIN=1,sum)
## var.w=var.w^(1/2)
## points(var.w,mu.w,pch=".",col="blue",xlim=c(0,max(var.w)))

num.rep=10000
W=matrix(rexp(num.rep*p),ncol=p)
W.sum=apply(W,MARGIN=1,sum)
W=W/W.sum
mu.w=W%*%mu
var.w=apply(W%*%Sigma*W,MARGIN=1,sum)
var.w=var.w^(1/2)
points(var.w,mu.w,pch=".")


pdf("06_mps.pdf",height=6,width=12)
layout(t(c(1,2)),widths=c(1,1))
dev.off()

pdf("06_mps_01.pdf",height=8,width=12)
dev.off()

pdf("06_mps_02.pdf",height=8,width=12)
dev.off()

pdf("06_mps_03.pdf",height=8,width=12)
dev.off()


###################### no-short-selling costraints
library(mvtnorm)

p=300
N=360
sigma1=.4 #0, .2, .4
sigma2=.2 #0, .2
num.rep=100

var=array(0,dim=c(num.rep,7))
colnames(var)=c("GMV","NSS","SAM.NSS","SAM","FAC","FAC.NSS","EW")
for (i in 1:num.rep){
  beta=cbind(rnorm(p,1,sigma1),rnorm(p,0,sigma2))
  Psi=diag(exp(rnorm(p,.8,.7)))
  Sigma=beta%*%t(beta)+Psi
  R=rmvnorm(N,sigma=Sigma)
  Sigma.sam=cov(R) ## sample covariance matrix
  Sigma.svd=svd(Sigma.sam)
  beta.hat=Sigma.svd$v[,1:2]%*%diag(sqrt(Sigma.svd$d[1:2]))
  beta.mat=beta.hat%*%t(beta.hat)
  Psi.hat=diag(diag(Sigma.sam-beta.mat))
  Sigma.fac=beta.mat+Psi.hat ## 2-factor model by PCA
  
  ########### GMVP
  w=solve.QP(Dmat=Sigma,dvec=rep(0,p),Amat=cbind(rep(1,p)),bvec=1,meq=1)$solution
  var[i,1]=t(w)%*%Sigma%*%w
  
  ########### GMVP, NSS
  A.mat=cbind(rep(1,p),diag(1,p))
  var[i,2]=solve.QP(Dmat=Sigma,dvec=rep(0,p),Amat=A.mat,bvec=c(1,rep(0,p)),meq=1)$value*2
  
  ## ########### sample covariance matrix, sample gmv, NSS
  ## var[i,3]=solve.QP(Dmat=Sigma.sam,dvec=rep(0,p),Amat=A.mat,bvec=c(1,rep(0,p)),meq=1)$value*2
  
  ########### sample covariance matrix, GMVP, NSS
  w=solve.QP(Dmat=Sigma.sam,dvec=rep(0,p),Amat=A.mat,bvec=c(1,rep(0,p)),meq=1)$solution
  var[i,3]=t(w)%*%Sigma%*%w
  
  ########### sample covariance matrix, GMVP
  ## a=Sys.time() ###### it is faster to use solve.QP then solve it explicitly
  w=solve.QP(Dmat=Sigma.sam,dvec=rep(0,p),Amat=cbind(rep(1,p)),bvec=1,meq=1)$solution
  ## as.numeric(Sys.time()-a)
  ## a=Sys.time()
  ## dir=solve(Sigma.sam)%*%rep(1,p)
  ## w=1/sum(rep(1,p)*dir)*dir
  ## as.numeric(Sys.time()-a)
  var[i,4]=t(w)%*%Sigma%*%w
  
  ########### 2-factor model, GMVP
  w=solve.QP(Dmat=Sigma.fac,dvec=rep(0,p),Amat=cbind(rep(1,p)),bvec=1,meq=1)$solution
  var[i,5]=t(w)%*%Sigma%*%w
  
  ########### 2-factor model, GMVP, NSS
  w=solve.QP(Dmat=Sigma.fac,dvec=rep(0,p),Amat=A.mat,bvec=c(1,rep(0,p)),meq=1)$solution
  var[i,6]=t(w)%*%Sigma%*%w
  
  ########### equally weighted portfolio
  var[i,7]=t(rep(1,p))%*%Sigma%*%rep(1,p)/p^2
  
  cat(i,", ")
  if (i %% 20 == 0){cat("\n")}
}

boxplot(var,ylab="Variance")

pdf("06_nss_01.pdf",height=8,width=12)
p=300
N=360
sigma1=.4
sigma2=.2
num.rep=100
par(mar=c(2.2,4,.5,.5))
boxplot(var.1,ylab="Variance",ylim=c(0,.4))
dev.off()

pdf("06_nss_02.pdf",height=8,width=12)
p=300
N=360
sigma1=0
sigma2=0
num.rep=100
par(mar=c(2.2,4,.5,.5))
boxplot(var.2,ylab="Variance",ylim=c(1.0,1.15))
dev.off()

pdf("06_nss_03.pdf",height=8,width=12)
p=30
N=60
sigma1=.4
sigma2=.2
num.rep=100
par(mar=c(2.2,4,.5,.5))
boxplot(var.3,ylab="Variance")
dev.off()

pdf("06_nss_04.pdf",height=8,width=12)
p=30
N=60
sigma1=0
sigma2=0
num.rep=100
par(mar=c(2.2,4,.5,.5))
boxplot(var.4,ylab="Variance",ylim=c(1.0,2.0))
dev.off()


pdf("06_nss.pdf",height=12,width=12)
layout(1:4,heights=c(1,1,1,1.1))
par(mar=c(.5,4,.5,.5))
boxplot(var.4,ylab="Variance",ylim=c(1.0,2.0),xaxt="n")
boxplot(var.3,ylab="Variance",xaxt="n")
boxplot(var.2,ylab="Variance",ylim=c(1.0,1.15),xaxt="n")
par(mar=c(2.2,4,.5,.5))
boxplot(var.1,ylab="Variance",ylim=c(0,.4))
dev.off()
