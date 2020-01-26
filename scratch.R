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

