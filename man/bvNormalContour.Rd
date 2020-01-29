\name{bvNormalContour}
\alias{bvNormalContour}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Bivariate Normal Contour Ellipse
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Draws a contour of constant density at the (1-\code{alpha})100\% level for a bivariate normal distribution using the eigendecomposition of the covariance matrix. This is likely more interesting for learning about the bivariate normal distribution than as a practical tool, for which other functions already exist (e.g. \code{link[graphics]{contour}}).
}
\usage{
bvNormalContour(mu = c(0,0), Sigma=NULL, eig=NULL, xl = NULL, yl = NULL, axes = TRUE, center = FALSE, lim.adj = 0.02, alpha = 0.05, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mu}{a vector giving the mean of the bivariate normal distribution. This is the center of the ellipse.}
  \item{Sigma}{a matrix giving the covariance matrix of the bivariate normal distribution. Either \code{Sigma} or \code{eig} must be specified.}
  \item{eig}{the eigenvalues and eigenvectors of the covariance matrix. This should be of the same form as the output of \code{\link{eigen}}, namely a list with two components: \code{values} and \code{vectors}. It is assumed that the largest eigenvalue is given first. Either \code{Sigma} or \code{eig} must be specified.}
  \item{xl}{a vector giving the lower and upper limits of the x-axis for plotting. If \code{xl = NULL} (default), then reasonable values are computed automatically.}
  \item{yl}{a vector giving the lower and upper limits of the y-axis for plotting. If \code{yl = NULL} (default), then reasonable values are computed automatically.}
  \item{axes}{logical. If \code{axes = TRUE} (default) then the major and minor axes of the ellipse are plotted.}
  \item{center}{logical. If \code{axes = TRUE} then the center of the ellipse is indicated with a point and dashed lines are drawn to the x-axis and y-axis.}
  \item{lim.adj}{a value giving an adjustment to the x-axis and y-axis limits computed if either \code{xl = NULL} or \code{yl = NULL}. Essentially this is a way to have some coarse control over these limits for quick graphing: positive values will increase the distance between the upper and lower limits (making the ellipse appear smaller) while negative values will decrease the distance (and make the ellipse appear larger).}
  \item{alpha}{a value giving the value of alpha to be used when computing the contour. Contours are drawn at the \code{1-alpha} level.}
  \item{...}{other arguments to be passed to the graphing functions.}
%mu = c(0,0), Sigma=NULL, eig=NULL,
%                            xl = NULL, yl = NULL,
%                            axes = TRUE, center = FALSE,
%                            lim.adj = 0.02, alpha = 0.05, ...
}
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
\examples{\dontrun{
mu <- c(2,8)
Sigma <- matrix(c(2,.9,.9,3), ncol = 2)
# Draw a 90\% contour
bvNormalContour(mu = mu, Sigma = Sigma, alpha = 0.10)
}}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ hplot }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
