# Exercise 5.7
rm(list=ls())

x.bar <- matrix(c(4.640,
                  45.400,
                  9.965),ncol=1)
S <- matrix(c(2.879, 10.010, -1.810,
              10.010, 199.788, -5.640,
              -1.810, -5.640, 3.628),byrow=TRUE,ncol=3)
Sinv <- matrix(c(.586,-.022,.258,
                 -.022,.006,-.002,
                 .258,-.002,.402),byrow=TRUE,ncol=3)

p <- 3
n <- 20
alpha <- 0.05

a1 <- matrix(c(1,0,0),ncol=1)
a2 <- matrix(c(0,1,0),ncol=1)
a3 <- matrix(c(0,0,1),ncol=1)

# Simultaneous, using result 5-3 (see example 5.4)
l1 <- t(a1)%*%x.bar - sqrt(
  (p*(n-1))/
    (n*(n-p))*
    qf(1-alpha,p,n-p)*
    t(a1)%*%S%*%a1)
u1 <- t(a1)%*%x.bar + sqrt((p*(n-1))/(n*(n-p))*qf(1-alpha,p,n-p)*t(a1)%*%S%*%a1)

l2 <- t(a2)%*%x.bar - sqrt((p*(n-1))/(n*(n-p))*qf(1-alpha,p,n-p)*t(a2)%*%S%*%a2)
u2 <- t(a2)%*%x.bar + sqrt((p*(n-1))/(n*(n-p))*qf(1-alpha,p,n-p)*t(a2)%*%S%*%a2)

l3 <- t(a3)%*%x.bar - sqrt((p*(n-1))/(n*(n-p))*qf(1-alpha,p,n-p)*t(a3)%*%S%*%a3)
u3 <- t(a3)%*%x.bar + sqrt((p*(n-1))/(n*(n-p))*qf(1-alpha,p,n-p)*t(a3)%*%S%*%a3)

l1;u1
l2;u2
l3;u3

# Bonferonni, using (5-29)
alpha.b <- alpha/(2*p)
t.bonf <- qt(1-alpha.b,df=n-1)

(l1b <- x.bar[1,1] - t.bonf * sqrt(S[1,1]/n))
(u1b <- x.bar[1,1] + t.bonf * sqrt(S[1,1]/n))
(l2b <- x.bar[2,1] - t.bonf * sqrt(S[2,2]/n))
(u2b <- x.bar[2,1] + t.bonf * sqrt(S[2,2]/n))
(l3b <- x.bar[3,1] - t.bonf * sqrt(S[3,3]/n))
(u3b <- x.bar[3,1] + t.bonf * sqrt(S[3,3]/n))

source("R/general.R")
eigellipse(X.mean=x.bar,eig=eigen(S[1:2,1:2]),n=n,p=p,center=FALSE,axes=FALSE,xl=c(2,7),yl=c(30,60),alpha=0.05)
# Simultaneous
abline(v=l1,lty=3)
abline(v=u1,lty=3)
abline(h=l2,lty=3)
abline(h=u2,lty=3)
# Bonferroni
abline(v=l1b,lty=2)
abline(v=u1b,lty=2)
abline(h=l2b,lty=2)
abline(h=u2b,lty=2)
