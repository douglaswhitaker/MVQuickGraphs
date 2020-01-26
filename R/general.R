# CLear Screen
cls <- function(){
  cat("\014")
}

############
# plot4in1 #
############
# MATH 3303, Douglas Whitaker
#
# This function creates a plot similar to the "Four in one" plot produced by Minitab
# One notable difference is a normal quantile plot is produced rather than a normal probability plot
# Default graphical parameters are chosen for their similarity to the Minitab plot

plot4in1 <- function(out, type="Regular", pch=19, col="steelblue3", cex=1.2, ...){
  par(mfrow = c(2,2)) # set up a 2x2 grid for plots

  if (type=="Regular") {
    res <- out$residuals
  }

  if (type=="Standardized"){
    res <- rstandard(out)
  }

  if (type=="Studentized"){
    res <- rstudent(out)
  }

  # Q-Q Plot
  qqnorm(res, pch=pch, col=col, cex=cex, ...)
  qqline(res, col="red", ...)

  # Versus Fits
  plot(x=out$fitted.values,y=res,
       pch=pch, col=col, cex=cex,
       main="Versus Fits",
       xlab="Fitted Value",
       ylab=paste(type,"Residuals"), ...)

  abline(h=0, lty=2, ...)

  # Histogram
  hist(res,
       col=col,
       main="Histogram",
       xlab=paste(type,"Residuals"), ...)

  # Versus Order
  n <- length(res)

  plot(y=res,
       x=1:n,
       col=col,
       type="o",
       pch=pch,
       xlab="Observation Order",
       ylab=paste(type,"Residuals"),
       main="Versus Order", ...)
  abline(h=0, lty=2, ...)

  par(mfrow = c(1,1)) # return to default
}

##############
# eigellipse #
##############

eigellipse <- function(X.mean=c(0,0),eig,n,p,xl=NULL,yl=NULL,axes=TRUE,center=TRUE,limadj=0.02,alpha=0.05){
  require(plotrix)
  angle <- atan(eig$vectors[2,1]/eig$vectors[1,1])

  axis1 <- sqrt(eig$values[1])*sqrt((p*(n-1))/(n*(n-p))*qf(1-alpha,p,n-p))
  axis2 <- sqrt(eig$values[2])*sqrt((p*(n-1))/(n*(n-p))*qf(1-alpha,p,n-p))

  axis1.x <- cos(angle)*axis1
  axis1.y <- sin(angle)*axis1
  axis2.x <- cos(angle+pi/2)*axis2
  axis2.y <- sin(angle+pi/2)*axis2

  if (is.null(xl)){
    xl1 <- min(X.mean[1,1]+axis1.x,
               X.mean[1,1]-axis1.x,
               X.mean[1,1]+axis2.x,
               X.mean[1,1]-axis2.x)
    xl1 <- xl1*(1-limadj)
    xl2 <- max(X.mean[1,1]+axis1.x,
               X.mean[1,1]-axis1.x,
               X.mean[1,1]+axis2.x,
               X.mean[1,1]-axis2.x)
    xl2 <- xl2*(1+limadj)
    xl <- c(xl1,xl2)
  }

  if (is.null(yl)){
    yl1 <- min(X.mean[2,1]+axis1.y,
               X.mean[2,1]-axis1.y,
               X.mean[2,1]+axis2.y,
               X.mean[2,1]-axis2.y)
    yl1 <- yl1*(1-limadj)
    yl2 <- max(X.mean[2,1]+axis1.y,
               X.mean[2,1]-axis1.y,
               X.mean[2,1]+axis2.y,
               X.mean[2,1]-axis2.y)
    yl2 <- yl2*(1+limadj)
    yl <- c(yl1,yl2)
  }

  plot(0,pch='',ylab='',xlab='',xlim=xl,ylim=yl)

  lengths <- c(axis1,axis2)
  draw.ellipse(x=X.mean[1,1],y=X.mean[2,1],a=lengths[1],b=lengths[2],angle=angle,deg=FALSE)

  if (axes){
    segments(x1=X.mean[1,1],y1=X.mean[2,1],x0=X.mean[1,1]+axis1.x,y0=X.mean[2,1]+axis1.y)
    segments(x1=X.mean[1,1],y1=X.mean[2,1],x0=X.mean[1,1]-axis1.x,y0=X.mean[2,1]-axis1.y)
    segments(x1=X.mean[1,1],y1=X.mean[2,1],x0=X.mean[1,1]+axis2.x,y0=X.mean[2,1]+axis2.y)
    segments(x1=X.mean[1,1],y1=X.mean[2,1],x0=X.mean[1,1]-axis2.x,y0=X.mean[2,1]-axis2.y)
  }

  if (center){
    segments(x1=X.mean[1,1],y1=X.mean[2,1],y0=X.mean[2,1],x0=0,lty=2)
    segments(x1=X.mean[1,1],y1=X.mean[2,1],x0=X.mean[1,1],y0=0,lty=2)
    points(x=X.mean[1,1],y=X.mean[2,1],pch=19)
  }
}
