library(MASS)
library(Matrix)
library(lattice)
library(stats)
n = 3
k = 10000

pdf("w0w1.pdf")
par(mfrow = c(3,2))
r = c(-0.9,0,0.9)
mu = c(1,2)
for (rho in r )
{

    Sigma = matrix(c(1,rho,rho,1),ncol=2)

    w0 = matrix(0, ncol = 2, nrow = k)
    w1 = matrix(0, ncol = 2, nrow = k)
    for (j in 1:k)
        {
            x = mvrnorm(n,mu,Sigma)
            z = mvrnorm(1,mu,Sigma)
            xbar = c(mean(x[,1]),mean(x[,2]))
            S = matrix(0,ncol=2,nrow=2)
            for (i in 1:n)
                {
                    S = S + (x[i,]-xbar)%*%t(x[i,]-xbar)
                }

            ##Compute w0

            s = sqrt(det(S))
            t = sqrt(sum(diag(S))+2*s)
            S2 = S/t + diag(c(s,s))/t
            w0[j,] =(1+1/n)^{-1/2}*solve(S2)%*%(z-xbar)


            ##Compute w1

            lS = t(chol(S))
            w1[j,] =(1+1/n)^{-1/2}*solve(lS)%*%(z-xbar)
        }

    plot(w0[,1],w0[,2],xlim = c(-1000,1000),ylim = c(-1000,1000),main = c("rho=",rho))
    plot(w1[,1],w1[,2],xlim = c(-1000,1000),ylim = c(-1000,1000),main = c("rho=",rho))

                                        #hist(w0[,1])
                                        #hist(w1[,1])
                                        #ff0 = kde2d(w0[,1],w0[,2])
                                        #contour(ff0,xlim = c(-1000,1000),ylim = c(-1000,1000))
                                        #ff1 = kde2d(w1[,1],w1[,2])
                                        #contour(ff1,xlim = c(-1000,1000),ylim = c(-1000,1000))
}
dev.off()
