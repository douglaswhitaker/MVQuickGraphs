# Testing hotfix for c value

bvNormalContour(Sigma=matrix(c(1,0,0,1),nrow=2))
bvNormalContour(Sigma=matrix(c(1,0,0,1),nrow=2),alpha=0.1)
bvNormalContour(Sigma=matrix(c(1,0,0,1),nrow=2),alpha=0.5)

bvNormalContour(mu=c(0,2),Sigma=matrix(c(2,1/sqrt(2),1/sqrt(2),1),nrow=2),alpha=0.5)
bvNormalContour(mu=c(5,10),Sigma=matrix(c(9,16,16,64),nrow=2),alpha=0.05)
points(x=10.273,y=29.551)
