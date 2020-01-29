\name{confidenceEllipse}
\alias{confidenceEllipse}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Bivariate Normal Confidence Ellipse
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Draws a (1-\code{alpha})100\% confidence ellipse (two dimensional) for a multivariate normal distribution using the eigendecomposition of the covariance matrix.
}
\usage{
confidenceEllipse(X.mean = c(0,0), eig, n, p,
xl = NULL, yl = NULL,
axes = TRUE, center = FALSE,
lim.adj = 0.02,
alpha = 0.05,
...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X.mean}{a column matrix giving the mean of the two dimensions of the p-dimensional multivariate normal distribution.}
  \item{eig}{the eigenvalues and eigenvectors of the covariance matrix. This should be of the same form as the output of \code{\link{eigen}}, namely a list with two components: \code{values} and \code{vectors}. It is assumed that the largest eigenvalue is given first.}
  \item{n}{the number of observations.}
  \item{p}{the number of dimensions of the multivariate normal distribution. (The resulting graph will always be a two-dimensional confidence region for the two dimensions of a p-dimensional multivaraite normal distribution under consideration.)}
  \item{xl}{a vector giving the lower and upper limits of the x-axis for plotting. If \code{xl = NULL} (default), then reasonable values are computed automatically.}
  \item{yl}{a vector giving the lower and upper limits of the y-axis for plotting. If \code{yl = NULL} (default), then reasonable values are computed automatically.}
  \item{axes}{logical. If \code{axes = TRUE} (default) then the major and minor axes of the ellipse are plotted.}
  \item{center}{logical. If \code{axes = TRUE} then the center of the ellipse is indicated with a point and dashed lines are drawn to the x-axis and y-axis.}
  \item{lim.adj}{a value giving an adjustment to the x-axis and y-axis limits computed if either \code{xl = NULL} or \code{yl = NULL}. Essentially this is a way to have some coarse control over these limits for quick graphing: positive values will increase the distance between the upper and lower limits (making the ellipse appear smaller) while negative values will decrease the distance (and make the ellipse appear larger).}
  \item{alpha}{a value giving the value of alpha to be used when computing the contour. Contours are drawn at the \code{1-alpha} level.}
  \item{...}{other arguments to be passed to the graphing functions.}
}

%X.mean = c(0,0),
%                              eig,
%                              n,
%                              p,
%                              xl = NULL, yl = NULL, # the x and y axis limits; calculated dynamically if not specified
%                              axes = TRUE, # if TRUE, the major and minor axes of the ellipse are graphed
%                              center = FALSE, # if TRUE, a dot at the center of the ellipse and dashed lines to the axes are shown
%                              lim.adj = 0, # an axis adjustment factor used if xl or yl is NULL (to adjust the calculated limits)
%                              alpha = 0.05,

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
\references{
%% ~put references to the literature/web site here ~
Johnson, R. A., & Wichern, D. W. (2007). Applied multivariate statistical analysis (6th ed). Pearson Prentice Hall.
}
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
%
%## The function is currently defined as
%function (x)
%{
%  }
%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ hplot }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
