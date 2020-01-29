##############
# plot4in1.R #
##############
# MATH 3303, Douglas Whitaker
#
# This function creates a plot similar to the "Four in one" plot produced by Minitab
# One notable difference is a normal quantile plot is produced rather than a normal probability plot
# Default graphical parameters are chosen for their similarity to the Minitab plot
# Note: Minitab colours are actually:
#       For points:                 #0054A6
#       For histogram bins:         #7DA7D9
#       For histogram outline:      #515151
#       For normal reference line:  #931313
#       For zero reference line:    #CCCCCC
#       For grid lines (QQ):        #F6F6F6
#       Gray panel background:      #E5E5E5
#
#

plot4in1 <- function(out, type="Regular", PP=TRUE, pch=19, col="steelblue", cex=1.2, ...){
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
  if (!PP){
    qqnorm(res, pch=pch, col=col, cex=cex, ...)
    qqline(res, col="red", ...)
  }
  # P-P Plot
  if (PP){
    ps <- pnorm(res)
    plot(x = ppoints(length(res)),
         y = sort(ps),
         main = "Normal Probability Plot",
         xlab = ifelse(type=="Regular","Residual",paste(type,"Residual")),
         ylab = "Percent", ,
         pch = pch, col = col, cex = cex, ...)
    abline(0, 1, col = "red", ...)
  }

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
