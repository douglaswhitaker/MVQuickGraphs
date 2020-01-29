\name{eigenEllipseHelper}
\alias{eigenEllipseHelper}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
eigenEllipseHelper(mu, lengths, angle, xl, yl, lim.adj, axes, center, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mu}{
%%     ~~Describe \code{mu} here~~
}
  \item{lengths}{
%%     ~~Describe \code{lengths} here~~
}
  \item{angle}{
%%     ~~Describe \code{angle} here~~
}
  \item{xl}{
%%     ~~Describe \code{xl} here~~
}
  \item{yl}{
%%     ~~Describe \code{yl} here~~
}
  \item{lim.adj}{
%%     ~~Describe \code{lim.adj} here~~
}
  \item{axes}{
%%     ~~Describe \code{axes} here~~
}
  \item{center}{
%%     ~~Describe \code{center} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
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
function (mu, lengths, angle, xl, yl, lim.adj, axes, center, 
    ...) 
{
    axis1 <- lengths[1]
    axis2 <- lengths[2]
    axis1.x <- cos(angle) * axis1
    axis1.y <- sin(angle) * axis1
    axis2.x <- cos(angle + pi/2) * axis2
    axis2.y <- sin(angle + pi/2) * axis2
    if (is.null(xl)) {
        xl1 <- mu[1, 1] - abs(axis1.x) - abs(axis2.x)
        xl2 <- mu[1, 1] + abs(axis1.x) + abs(axis2.x)
        xl <- c(xl1 * (1 - lim.adj), xl2 * (1 + lim.adj))
    }
    if (is.null(yl)) {
        yl1 <- mu[2, 1] - abs(axis1.y) - abs(axis2.y)
        yl2 <- mu[2, 1] + abs(axis1.y) + abs(axis2.y)
        yl <- c(yl1 * (1 - lim.adj), yl2 * (1 + lim.adj))
    }
    plot(0, pch = "", ylab = "", xlab = "", xlim = xl, ylim = yl)
    draw.ellipse(x = mu[1, 1], y = mu[2, 1], a = lengths[1], 
        b = lengths[2], angle = angle, deg = FALSE)
    if (axes) {
        segments(x1 = mu[1, 1], y1 = mu[2, 1], x0 = mu[1, 1] + 
            axis1.x, y0 = mu[2, 1] + axis1.y)
        segments(x1 = mu[1, 1], y1 = mu[2, 1], x0 = mu[1, 1] - 
            axis1.x, y0 = mu[2, 1] - axis1.y)
        segments(x1 = mu[1, 1], y1 = mu[2, 1], x0 = mu[1, 1] + 
            axis2.x, y0 = mu[2, 1] + axis2.y)
        segments(x1 = mu[1, 1], y1 = mu[2, 1], x0 = mu[1, 1] - 
            axis2.x, y0 = mu[2, 1] - axis2.y)
    }
    if (center) {
        segments(x1 = mu[1, 1], y1 = mu[2, 1], y0 = mu[2, 1], 
            x0 = 0, lty = 2)
        segments(x1 = mu[1, 1], y1 = mu[2, 1], x0 = mu[1, 1], 
            y0 = 0, lty = 2)
        points(x = mu[1, 1], y = mu[2, 1], pch = 19)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
