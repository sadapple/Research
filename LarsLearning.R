boston=read.table("boston.txt",header=T,sep="\t")
attach(boston)
boston.X=as.matrix(boston[,(9:21)])
boston.y=as.vector(boston[,8])

############ forward selection
boston.ls=lm(CMEDV~CRIM+ZN+INDUS+CHAS+NOX+RM+AGE+DIS+RAD+TAX+PTRATIO+B+LSTAT,data=boston)
summary(boston.ls)
boston.ls.0=lm(CMEDV~1,data=boston)
boston.ls.forward=step(boston.ls.0,scope=list(lower=~1,upper=~CRIM+ZN+INDUS+CHAS+NOX+RM+AGE+DIS+RAD+TAX+PTRATIO+B+LSTAT),k=0,direction="forward",data=boston)
boston.ls.forward$coefficients
############ backward selection
boston.ls.backward=step(boston.ls,scope=list(lower=~1,upper=~CRIM+ZN+INDUS+CHAS+NOX+RM+AGE+DIS+RAD+TAX+PTRATIO+B+LSTAT),k=2000,direction="backward",data=boston) ###### set a large k to force backward moves
boston.ls.backward$coefficients

###################### load the lars package
library(lars)
## boston.y=boston.y-mean(boston.y)
## boston.X=scale(boston.X,center=TRUE,scale=TRUE)
## w=sqrt(dim(boston.X)[1]-1)
## boston.X=scale(boston.X,center=FALSE,scale=rep(w,dim(boston.X)[2]))
## summary(lm(boston.y~boston.X-1))

boston.lasso=lars(boston.X,boston.y,type="lasso",trace=FALSE,normalize=TRUE,intercept=TRUE)
boston.lars=lars(boston.X,boston.y,type="lar",trace=FALSE,normalize=TRUE,intercept=TRUE)
boston.fs=lars(boston.X,boston.y,type="forward.stagewise",trace=FALSE,normalize=TRUE,intercept=TRUE)
boston.lasso
boston.lars
boston.fs


###################### plot solution paths, the coefficients are for the standardized X matrix
plot(boston.lasso,xvar="step",breaks=TRUE,plottype="coefficients")
plot(boston.lasso,xvar="norm",breaks=TRUE,plottype="coefficients")
plot(boston.lasso,xvar="arc.length",breaks=TRUE,plottype="coefficients")
plot(boston.lasso,xvar="df",breaks=TRUE,plottype="coefficients")

plot(boston.lars,xvar="arc.length")

par(mfrow=c(1,2))
plot(boston.lars,xvar="arc.length")
plot(boston.lasso,xvar="arc.length")


###################### extract estimated coefficients
predict(boston.lasso,s=3,type="coefficients",mode="step") ##s=s-1 for our notation
coef(boston.lasso,s=3,mode="step")
###################### fitted values
predict(boston.lasso,boston[1,9:21],s=3,type="fit",mode="step")
predict(boston.lasso,t(boston.X[1,]),s=3,type="fit",mode="step") ## new input has to be a row vector
predict(boston.lasso,boston[1,9:21],s=.5,type="fit",mode="fraction")
predict(boston.lasso,boston[1,9:21],s=2,type="fit",mode="lambda")
predict(boston.lasso,boston[1,9:21],s=10,type="fit",mode="norm")
###################### get all fitted values on the training set
boston.lasso.fitted=predict(boston.lasso,boston.X,s=3,type="fit",model="step")
boston.lasso.fitted=predict(boston.lasso,boston[,9:21],s=3,type="fit",model="step")

#################################################################
###### You have to be careful when using the lars function. #####
###### It gives coefficients in term of the original input  #####
###### matrix X (boston.X here). If necessary, you need     #####
###### to figure out the right intercept by yourself.       #####
#################################################################

###################### class illustration with less predictors
###################### LSTAT RM PTRATIO  B CHAS CRIM DIS TAX NOX ZN
boston0.X=as.matrix(boston[,c("LSTAT","RM","PTRATIO","B","CHAS","CRIM","DIS","TAX","NOX","ZN")])
boston0.lasso=lars(boston0.X,boston.y,type="lasso",trace=FALSE,normalize=TRUE,intercept=TRUE)
boston0.lars=lars(boston0.X,boston.y,type="lar",trace=FALSE,normalize=TRUE,intercept=TRUE)
boston0.fs=lars(boston0.X,boston.y,type="forward.stagewise",trace=FALSE,normalize=TRUE,intercept=TRUE)
boston0.lasso
boston0.lars
boston0.fs

pdf("02_boston_lars.pdf",height=8,width=12)
plot(boston0.lars,xvar="arc.length")
dev.off()

pdf("02_boston_lars_lasso.pdf",height=8,width=12)
par(mfrow=c(1,2))
plot(boston0.lars,xvar="step")
plot(boston0.lasso,xvar="step")
dev.off()

detach(boston)

###################### example on the sign change of LARS coefficients
x1=c(1,0,0,0,0)
x1=(x1-mean(x1))/(sd(x1)*sqrt(4))
x2=c(1,0,-.5,0,0)
x2=(x2-mean(x2))/(sd(x2)*sqrt(4))
x3=c(0,1,0,0,0)
x3=(x3-mean(x3))/(sd(x3)*sqrt(4))

X3=cbind(x1,x2,x3)
w3=solve((t(X3)%*%X3),rep(1,3))
y=rnorm(5)
y=(lm(y~X3))$residuals+X3%*%w3+2*x1+.5*x2
y=y-mean(y)
cor(y,X3)
m=lm(y~X3-1)
summary(m)

###### 1st step
plot(0,0,xlim=c(0,1),ylim=c(0,1),type="n",xlab=expression(gamma[1]),ylab="correlations",main="Step 1")
legend("topright",c("x1","x2","x3"),col=c("black","blue","yellow"),lty=c(1,1,1),bg="gray90")
abline(0,0)
abline(cor(y,x1),-1)
abline(cor(y,x2),-t(x2)%*%x1,col="blue")
abline(cor(y,x3),-t(x3)%*%x1,col="yellow")
