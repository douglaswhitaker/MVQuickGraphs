\name{eigenEllipseHelper}
\alias{eigenEllipseHelper}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Helper Function for other Ellipse-from-Eigendecomposition Functions
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Helper function for graphing ellipses from eigendecompositions. This function is used by \code{\link{bvNormalContour}} and \code{\link{confidenceEllipse}}. Essentially this is a wrapper for \code{\link[plotrix]{draw.ellipse}} that also calculates appropriate x-axis and y-axis limits to make graphing an ellipse easier (because the entire ellipse should be visible without any work on the user's part to specify the limits).
}
\usage{
eigenEllipseHelper(mu, lengths, angle, xl, yl, lim.adj, axes, center, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mu}{
%%     ~~Describe \code{mu} here~~
column matrix giving the coordinates for the cener of the ellipse.
}
  \item{lengths}{
%%     ~~Describe \code{lengths} here~~
vector giving the major and minor axis lengths.
}
  \item{angle}{
%%     ~~Describe \code{angle} here~~
angle of rotation (in radians).
}
  \item{xl}{
%%     ~~Describe \code{xl} here~~
x-axis limits. If \code{xl = NULL} then these are computed automatically.
}
  \item{yl}{
%%     ~~Describe \code{yl} here~~
y-axis limits. If \code{yl = NULL} then these are computed automatically.
}
  \item{lim.adj}{
%%     ~~Describe \code{lim.adj} here~~
a value giving an adjustment to the x-axis and y-axis limits computed if either \code{xl = NULL} or \code{yl = NULL}.
}
  \item{axes}{
%%     ~~Describe \code{axes} here~~
logical. If \code{axes = TRUE}, then the major and minor axes are graphed.
}
  \item{center}{
%%     ~~Describe \code{center} here~~
logical. If \code{axes = TRUE} then the center of the ellipse is indicated with a point and dashed lines are drawn to the x-axis and y-axis.
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
other arguments to be passed to the graphing functions.
}
}
\value{None}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
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

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
%\examples{
%##---- Should be DIRECTLY executable !! ----
%##-- ==>  Define data, use random,
%##--	or do  help(data=index)  for the standard data sets.


%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
