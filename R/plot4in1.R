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



#' Plot 4-in-1
#'
#' Generates a 2x2 panel graph including four residual diagnostic plots as is
#' popular in some other statistics packages. This was initially written to
#' support students learning R for the first time in a regression modeling
#' course. \code{plot4in1} generates four commonly-used residual diagnostic
#' plots that can be used to assess the linear regression assumptions and
#' ensures a consistent, reasonably-pleasing graphical style across each plot.
#'
#' \code{plot4in1} creates a 2 by 2 panel using \code{par(mfrow = c(2,2))} and
#' then generates four residual diagnostic plots: a Percentile-Percentile (or
#' Quantile-Quantile plot if \code{PP = FALSE}), a scatterplot of the
#' \code{fitted.values} against the residuals, a histogram of the residuals,
#' and scatterplot of the residuals against their order, overplotted.
#'
#' @param out the output of the \code{\link[stats]{lm}} function (an object of
#' class \code{"lm"}). The components of greatest importance from this object
#' are \code{residuals} (perhaps passed to \code{rstandard} of \code{rstudent},
#' depending on \code{type}) and \code{fitted.values}.
#' @param type the type of residuals to be used. There are three possible
#' values: \code{"Regular"}, \code{"Standardized"}, and \code{"Studentized"}.
#' Using \code{type = "Regular"} results in untransformed residuals being used,
#' \code{type = "Standardized"} uses standardized residuals (computed using
#' \code{rstandard}), and \code{type = "Studentized"} uses externally
#' studentized residuals (computed using \code{rstudent}).
#' @param PP logical. If \code{PP = TRUE}, a Normal Percentile Plot (P-P Plot)
#' is displayed in the top-left panel. If \code{PP = FALSE}, a Normal Quantile
#' Plot (Q-Q Plot) is displayed in the top-left panel.
#' @param pch symbol to be used in plotting. \code{pch = 19} is a filled circle
#' (see \code{\link[graphics]{par}}).
#' @param col color of symbol specified in \code{pch} to be used in graphing.
#' The default is \code{"steelblue"} (see \code{\link[graphics]{par}}).
#' @param cex character expansion value, used to adjust the size of the symbol
#' specified in \code{pch}. The default value is \code{cex = 1.2} (see
#' \code{\link[graphics]{par}}).
#' @param ... other arguments to be passed to the graphing functions.
#' @return None
#' @seealso \code{\link[stats]{influence.measures}} for more information about
#' standardized (\code{rstandard}) and studentized (\code{rstudent}) residuals;
#' \code{\link[stats]{qqnorm}} for more information about the Quantile-Quanitle
#' (Q-Q) plot; \code{\link[graphics]{par}} for information about the graphical
#' parameters.
#' @examples
#'
#' out <- lm(Girth ~ Volume, data = trees)
#' plot4in1(out)
#'
#' @export plot4in1
plot4in1 <- function(out, type="Regular", PP=TRUE, pch=19, col="steelblue", cex=1.2, ...){
  # Because we change the user's par:
  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(oldpar))

  graphics::par(mfrow = c(2,2)) # set up a 2x2 grid for plots

  if (type=="Regular") {
    res <- out$residuals
  }

  if (type=="Standardized"){
    res <- stats::rstandard(out)
  }

  if (type=="Studentized"){
    res <- stats::rstudent(out)
  }

  # Q-Q Plot
  if (!PP){
    stats::qqnorm(res, pch=pch, col=col, cex=cex, ...)
    stats::qqline(res, col="red", ...)
  }
  # P-P Plot
  if (PP){
    ps <- stats::pnorm(res)
    graphics::plot(x = stats::ppoints(length(res)),
         y = sort(ps),
         main = "Normal Probability Plot",
         xlab = ifelse(type=="Regular","Residual",paste(type,"Residual")),
         ylab = "Percent",
         pch = pch, col = col, cex = cex, ...)
    graphics::abline(0, 1, col = "red", ...)
  }

  # Versus Fits
  graphics::plot(x=out$fitted.values,y=res,
       pch=pch, col=col, cex=cex,
       main="Versus Fits",
       xlab="Fitted Value",
       ylab=paste(type,"Residuals"), ...)

  graphics::abline(h=0, lty=2, ...)

  # Histogram
  graphics::hist(res,
       col=col,
       main="Histogram",
       xlab=paste(type,"Residuals"), ...)

  # Versus Order
  n <- length(res)

  graphics::plot(y=res,
       x=1:n,
       col=col,
       type="o",
       pch=pch,
       xlab="Observation Order",
       ylab=paste(type,"Residuals"),
       main="Versus Order", ...)
  graphics::abline(h=0, lty=2, ...)
}
