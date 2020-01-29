\name{plot4in1}
\alias{plot4in1}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Plot 4-in-1
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Generates a 2x2 panel graph including four residual diagnostic plots as is popular in some other statistics packages. This was initially written to support students learning R for the first time in a regression modeling course. \code{plot4in1} generates four commonly-used residual diagnostic plots that can be used to assess the linear regression assumptions and ensures a consistent, reasonably-pleasing graphical style across each plot.
}
\usage{
plot4in1(out, type="Regular", PP=TRUE, pch=19, col="steelblue", cex=1.2, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{out}{the output of the \code{\link[stats]{lm}} function (an object of class \code{"lm"}). The components of greatest importance from this object are \code{residuals} (perhaps passed to \code{rstandard} of \code{rstudent}, depending on \code{type}) and \code{fitted.values}.}
  \item{type}{the type of residuals to be used. There are three possible values: \code{"Regular"}, \code{"Standardized"}, and \code{"Studentized"}. Using \code{type = "Regular"} results in untransformed residuals being used, \code{type = "Standardized"} uses standardized residuals (computed using \code{\link[stats]{rstandard}}), and \code{type = "Studentized"} uses externally studentized residuals (computed using \code{\link[stats]{rstudent}}).}
  \item{PP}{logical. If \code{PP = TRUE}, a Normal Percentile Plot (P-P Plot) is displayed in the top-left panel. If \code{PP = FALSE}, a Normal Quantile Plot (Q-Q Plot) is displayed in the top-left panel.}
  \item{pch}{symbol to be used in plotting. \code{pch = 19} is a filled circle (see \code{\link[graphics]{par}}).}
  \item{col}{color of symbol specified in \code{pch} to be used in graphing. The default is \code{"steelblue"} (see \code{\link[graphics]{par}}).}
  \item{cex}{character expansion value, used to adjust the size of the symbol specified in \code{pch}. The default value is \code{cex = 1.2} (see \code{\link[graphics]{par}}).}
  \item{...}{other arguments to be passed to the graphing functions.}
% function(out, type="Regular", PP=TRUE, pch=19, col="steelblue", cex=1.2, ...)
}
\details{
%%  ~~ If necessary, more details than the description above ~~
\code{plot4in1} creates a 2 by 2 panel using \code{par(mfrow = c(2,2))} and then generates four residual diagnostic plots: a Percentile-Percentile (or Quantile-Quantile plot if \code{PP = FALSE}), a scatterplot of the \code{fitted.values} against the residuals, a histogram of the residuals, and scatterplot of the residuals against their order, overplotted.
}
%\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
%}
%\references{
%% ~put references to the literature/web site here ~
%}
%\author{
%%  ~~who you are~~
%}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link[stats]{influence.measures}} for more information about standardized and studentized residuals; \code{\link[stats]{qqnorm}} for more information about the Quantile-Quanitle (Q-Q) plot; \code{\link[graphics]{par}} for information about the graphical parameters.
}
\examples{
out <- lm(Girth ~ Volume, data = trees)
plot4in1(out)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
