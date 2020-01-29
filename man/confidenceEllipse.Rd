\name{confidenceEllipse}
\alias{confidenceEllipse}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Bivariate Normal Confidence Ellipse
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Draws a (1-\code{alpha})100\% confidence ellipse for a bivariate normal distribution using the eigendecomposition of the covariance matrix.
}
\usage{
confidenceEllipse(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{eig}{}
  \item{n}{}
  \item{p}{}
  \item{xl}{}
  \item{yl}{}
  \item{axes}{}
  \item{center}{}
  \item{lim.adj}{}
  \item{alpha}{}


%X.mean = c(0,0),
%                              eig,
%                              n,
%                              p,
%                              xl = NULL, yl = NULL, # the x and y axis limits; calculated dynamically if not specified
%                              axes = TRUE, # if TRUE, the major and minor axes of the ellipse are graphed
%                              center = FALSE, # if TRUE, a dot at the center of the ellipse and dashed lines to the axes are shown
%                              lim.adj = 0, # an axis adjustment factor used if xl or yl is NULL (to adjust the calculated limits)
%                              alpha = 0.05,
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x)
{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
