# Example 4.2
e1 <- matrix(c(1/sqrt(2),1/sqrt(2)),ncol=1)
e2 <- matrix(c(1/sqrt(2),-1/sqrt(2)),ncol=1)
eigenvectors <- cbind(e1,e2)

angle <- atan(eigenvectors[2,1]/eigenvectors[1,1]) # sohcahtoa

library(plotrix)
mu <- c(0,0)

s11 <- 1
s12 <- 0.75

plot(0,pch='',ylab='',xlab='',xlim=c(-5,5),ylim=c(-5,5))
c50 <- qchisq(0.50,df=2)
lengths50 <- c(c50*sqrt(s11+s12),c50*sqrt(s11-s12))
draw.ellipse(x=mu[1],y=mu[2],a=lengths50[1],b=lengths50[2],angle=angle,deg=FALSE)

c90 <- qchisq(0.90,df=2)
lengths90 <- c(c90*sqrt(s11+s12),c90*sqrt(s11-s12))
draw.ellipse(x=mu[1],y=mu[2],a=lengths90[1],b=lengths90[2],angle=angle,deg=FALSE)

################################
x.bar <- matrix(c(4.640,
                  45.400,
                  9.965),ncol=1)
S <- matrix(c(2.879, 10.010, -1.810,
              10.010, 199.788, -5.640,
              -1.810, -5.640, 3.628),byrow=TRUE,ncol=3)
p <- 3
n <- 20
alpha <- 0.05

confidenceEllipse(X.mean=x.bar,eig=eigen(S[1:2,1:2]),n=n,p=p, limadj = 0.08)
bvNormalContour(mu=x.bar[1:2,1],eig=eigen(S[1:2,1:2]))
bvNormalContour(mu=x.bar[1:2,1],eig=eigen(S[1:2,1:2]),n=n,p=p)

bvNormalContour(mu=x.bar[1:2,1],eig=eigen(S[1:2,1:2]), limadj = 0.7)



e1 <- matrix(c(1/sqrt(2),1/sqrt(2)),ncol=1)
e2 <- matrix(c(1/sqrt(2),-1/sqrt(2)),ncol=1)

mu <- c(0,0)

s11 <- 1
s12 <- 0.75
eig42 <- list(values=c(s11+s12,s11-s12),vectors=cbind(e1,e2))
bvNormalContour(mu=mu,eig=eig42)
