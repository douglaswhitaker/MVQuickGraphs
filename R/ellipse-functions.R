# Essentially, this file contains three functions (two exported, one internal).
# confidenceEllipse and bvNormalContour both take the eigen decomposition of a (covariance) matrix to plot
# the appropriate ellipse. These functions calculate the appropriate lengths of the major and minor axes
# and the angle of rotation of the ellipse; these values are then passed to eigenEllipseHelper to actually draw
# the ellipse.

# To do:
# make an add=TRUE parameter (at least for bvNormalContour so that one can make a contour plot with a loop)
# This might be easiest to just do by accepting a vector for alpha. Then, when it comes time to calculate lengths,
# check the length of clevel and if > 1 just loop through the tail end of the function.
#
# change X.mean for confidenceEllipse, allow Sigma = for confidence ellipse
#
# perhaps add options to graph the length of the axes (labels). something like c/lambda_1 = value, near the corresponding axis

# This functions graphs a confidence ellipse for mu based on the eigenvalues and eigenvectors for the covariance matrix S.


#' Bivariate Normal Confidence Ellipse
#'
#' Draws a (1-\code{alpha})100\% confidence ellipse (two dimensional) for a
#' multivariate normal distribution using the eigendecomposition of the
#' covariance matrix.
#'
#'
#' @param X.mean a column matrix giving the mean of the two dimensions of the
#' p-dimensional multivariate normal distribution.
#' @param eig the eigenvalues and eigenvectors of the covariance matrix. This
#' should be of the same form as the output of \code{\link{eigen}}, namely a
#' list with two components: \code{values} and \code{vectors}. It is assumed
#' that the largest eigenvalue is given first.
#' @param n the number of observations.
#' @param p the number of dimensions of the multivariate normal distribution.
#' (The resulting graph will always be a two-dimensional confidence region for
#' the two dimensions of a p-dimensional multivaraite normal distribution under
#' consideration.)
#' @param xl a vector giving the lower and upper limits of the x-axis for
#' plotting. If \code{xl = NULL} (default), then reasonable values are computed
#' automatically.
#' @param yl a vector giving the lower and upper limits of the y-axis for
#' plotting. If \code{yl = NULL} (default), then reasonable values are computed
#' automatically.
#' @param axes logical. If \code{axes = TRUE} (default) then the major and
#' minor axes of the ellipse are plotted.
#' @param center logical. If \code{axes = TRUE} then the center of the ellipse
#' is indicated with a point and dashed lines are drawn to the x-axis and
#' y-axis.
#' @param lim.adj a value giving an adjustment to the x-axis and y-axis limits
#' computed if either \code{xl = NULL} or \code{yl = NULL}. Essentially this is
#' a way to have some coarse control over these limits for quick graphing:
#' positive values will increase the distance between the upper and lower
#' limits (making the ellipse appear smaller) while negative values will
#' decrease the distance (and make the ellipse appear larger).
#' @param alpha a value giving the value of alpha to be used when computing the
#' contour. Contours are drawn at the \code{1-alpha} level.
#' @param ... other arguments to be passed to the graphing functions.
#' @return None
#' @references Johnson, R. A., & Wichern, D. W. (2007). Applied multivariate
#' statistical analysis (6th ed). Pearson Prentice Hall.
#' @examples
#'
#' # 90% Confidence Ellipse for Reading and Vocab from ability.cov
#' x.bar <- ability.cov$center[5:6]
#' Sigma <- ability.cov$cov[5:6,5:6]
#' n <- ability.cov$n.obs
#' p <- length(ability.cov$center)
#'
#' confidenceEllipse(X.mean = x.bar,
#'                   eig = eigen(Sigma),
#'                   n = n, p = p,
#'                   alpha = 0.10)
#'
#' @export confidenceEllipse
confidenceEllipse <- function(X.mean = c(0,0),
                              eig,
                              n,
                              p,
                              xl = NULL, yl = NULL, # the x and y axis limits; calculated dynamically if not specified
                              axes = TRUE, # if TRUE, the major and minor axes of the ellipse are graphed
                              center = FALSE, # if TRUE, a dot at the center of the ellipse and dashed lines to the axes are shown
                              lim.adj = 0.02, # an axis adjustment factor used if xl or yl is NULL (to adjust the calculated limits)
                              alpha = 0.05, # the alpha-level used to determine which ellipse is drawn
                              ...){

  # eigenEllipseHelper is expecting a matrix... might be better to eventually change that
  X.mean <- matrix(X.mean, ncol=1)

  # Calculate the angle of rotation.
  # Because we are taking this as output of eigen(),
  # the eigenvector in column 1 will correspond with the largest eigenvalue,
  # which will thus be the eigenvector associated with the major axis,
  # which is what is used for determining the angle of rotation.
  # Actual calculation is just application of SOHCAHTOA to this vector.
  # The result of this calculation is in radians.
  angle <- atan(eig$vectors[2,1] / eig$vectors[1,1])

  # Calculate the half-length of the major axis
  # This is an application of result (5-19) in Johnson & Wichern (2007)
  axis1 <- sqrt(eig$values[1]) *
    sqrt((p * (n - 1)) /
           (n * (n - p)) *
           stats::qf(1 - alpha, p, n - p))

  # Calculate the half-length of the minor axis
  # This is an application of result (5-19) in Johnson & Wichern (2007)
  axis2 <- sqrt(eig$values[2]) *
    sqrt((p * (n - 1)) /
           (n * (n - p)) *
           stats::qf(1 - alpha, p, n - p))

  # create a vector of axis lengths we can use
  lengths <- c(axis1,axis2)

  eigenEllipseHelper(mu = X.mean, lengths = lengths, angle = angle,
                     xl = xl, yl = yl,
                     axes = axes, center = center,
                     lim.adj = lim.adj, ...)
}



#' Bivariate Normal Contour Ellipse
#'
#' Draws a contour of constant density at the (1-\code{alpha})100\% level for a
#' bivariate normal distribution using the eigendecomposition of the covariance
#' matrix. This is likely more interesting for learning about the bivariate
#' normal distribution than as a practical tool, for which other functions
#' already exist (e.g. \code{link[graphics]{contour}}).
#'
#'
#' @param mu a vector giving the mean of the bivariate normal distribution.
#' This is the center of the ellipse.
#' @param Sigma a matrix giving the covariance matrix of the bivariate normal
#' distribution. Either \code{Sigma} or \code{eig} must be specified.
#' @param eig the eigenvalues and eigenvectors of the covariance matrix. This
#' should be of the same form as the output of \code{\link{eigen}}, namely a
#' list with two components: \code{values} and \code{vectors}. It is assumed
#' that the largest eigenvalue is given first. Either \code{Sigma} or
#' \code{eig} must be specified.
#' @param xl a vector giving the lower and upper limits of the x-axis for
#' plotting. If \code{xl = NULL} (default), then reasonable values are computed
#' automatically.
#' @param yl a vector giving the lower and upper limits of the y-axis for
#' plotting. If \code{yl = NULL} (default), then reasonable values are computed
#' automatically.
#' @param axes logical. If \code{axes = TRUE} (default) then the major and
#' minor axes of the ellipse are plotted.
#' @param center logical. If \code{axes = TRUE} then the center of the ellipse
#' is indicated with a point and dashed lines are drawn to the x-axis and
#' y-axis.
#' @param lim.adj a value giving an adjustment to the x-axis and y-axis limits
#' computed if either \code{xl = NULL} or \code{yl = NULL}. Essentially this is
#' a way to have some coarse control over these limits for quick graphing:
#' positive values will increase the distance between the upper and lower
#' limits (making the ellipse appear smaller) while negative values will
#' decrease the distance (and make the ellipse appear larger).
#' @param alpha a value giving the value of alpha to be used when computing the
#' contour. Contours are drawn at the \code{1-alpha} level.
#' @param ... other arguments to be passed to the graphing functions.
#' @return None
#' @references Johnson, R. A., & Wichern, D. W. (2007). Applied multivariate
#' statistical analysis (6th ed). Pearson Prentice Hall.
#' @examples
#'
#' mu <- c(-1,8)
#' Sigma <- matrix(c(3,2,2,4), ncol = 2)
#' # Draw a 90% contour
#' bvNormalContour(mu = mu, Sigma = Sigma, alpha = 0.10)
#'
#' @export bvNormalContour
bvNormalContour <- function(mu = c(0,0), Sigma=NULL, eig=NULL,
                            xl = NULL, yl = NULL,
                            axes = TRUE, center = FALSE,
                            lim.adj = 0.02, alpha = 0.05, ...){

  # eigenEllipseHelper is expecting a matrix... might be better to eventually change that
  mu <- matrix(mu, ncol=1)

  # Critical value for the constant density contour (always df=2 because this is bivariate normal)
  # Johnson & Wichern (2008) result (4-8)
  # Johnson & Wichern use c^2 as the notation for the critical value; we take the square root immediately
  clevel <- sqrt(stats::qchisq(1 - alpha, df = 2))

  # User needs to supply either eig or Sigma
  if (!is.null(Sigma)){
    eig <- eigen(Sigma)
  }

  # From Johnson & Wichern (2008) result (4-8)
  # The half-lengths are c*sqrt(lambda_i); the eigenvectors determine the angle
  #lengths <- c(clevel * sqrt(s11+s12),clevel*sqrt(s11-s12))
  lengths <- c(clevel * sqrt(eig$values[1]), clevel * sqrt(eig$values[2]))
  angle <- atan(eig$vectors[2,1]/eig$vectors[1,1]) # sohcahtoa

  # Given the center, axis lengths, and angle of rotation, draw the ellipse.
  eigenEllipseHelper(mu = mu, lengths = lengths, angle = angle,
                     xl = xl, yl = yl,
                     axes = axes, center = center,
                     lim.adj = lim.adj, ...)
}

# There seems to be a problem with the lim.adj for the xl yl calculation


#' Helper Function for other
#' Ellipse-from-Eigendecomposition Functions
#'
#' Helper function for graphing ellipses from eigendecompositions. This function
#' is used by \code{\link{bvNormalContour}} and \code{\link{confidenceEllipse}}.
#' Essentially this is a wrapper for \code{\link[plotrix]{draw.ellipse}} that
#' also calculates appropriate x-axis and y-axis limits to make graphing an
#' ellipse easier (because the entire ellipse should be visible without any
#' work on the user's part to specify the limits).
#'
#'
#' @param mu column matrix giving the coordinates for the cener of the ellipse.
#' @param lengths vector giving the major and minor axis lengths.
#' @param angle angle of rotation (in radians).
#' @param xl x-axis limits. If \code{xl = NULL} then these are computed
#' automatically.
#' @param yl y-axis limits. If \code{yl = NULL} then these are computed
#' automatically.
#' @param lim.adj a value giving an adjustment to the x-axis and y-axis limits
#' computed if either \code{xl = NULL} or \code{yl = NULL}.
#' @param axes logical. If \code{axes = TRUE}, then the major and minor axes are
#' graphed.
#' @param center logical. If \code{axes = TRUE} then the center of the ellipse
#' is indicated with a point and dashed lines are drawn to the x-axis and
#' y-axis.
#' @param \dots other arguments to be passed to the graphing functions.
#' @return None
eigenEllipseHelper <- function(mu, lengths, angle, xl, yl, lim.adj, axes, center, ...){
  axis1 <- lengths[1]
  axis2 <- lengths[2]

  # Calculate x and y components for each axis.
  # These are used if xl or y1 is NULL or if center = TRUE

  axis1.x <- cos(angle) * axis1
  axis1.y <- sin(angle) * axis1
  axis2.x <- cos(angle + pi / 2) * axis2
  axis2.y <- sin(angle + pi / 2) * axis2

  # if no x limits are specified, calculate some that will ensure the entirety of the ellipse will fit on the x axis
  if (is.null(xl)){
    xl1 <- mu[1,1] - abs(axis1.x) - abs(axis2.x)
    xl2 <- mu[1,1] + abs(axis1.x) + abs(axis2.x)
    xl <- c(xl1*(1-lim.adj),xl2*(1+lim.adj))
  }

  # if no y limits are specified, calculate some that will ensure the entirety of the ellipse will fit on the y axis
  if (is.null(yl)){
    yl1 <- mu[2,1] - abs(axis1.y) - abs(axis2.y)
    yl2 <- mu[2,1] + abs(axis1.y) + abs(axis2.y)
    yl <- c(yl1*(1-lim.adj),yl2*(1+lim.adj))
  }

  # Call an empty graph that we can add the normal contour ellipseto
  graphics::plot(0,pch='',ylab='',xlab='',xlim=xl,ylim=yl)

  # draw the ellipse
  # draw.ellipse is from package plotrix
  plotrix::draw.ellipse(x = mu[1,1], y = mu[2,1], # center of the ellipse
               a = lengths[1], b = lengths[2], # major and minor axis half-lengths
               angle = angle, deg = FALSE) # calculated angle earlier was in radians

  # if TRUE draw the major and minor axes for the normal contour ellipse
  # These are just line segments using the offsets from the mean calculcated earlier.
  if (axes){
    graphics::segments(x1 = mu[1,1], y1 = mu[2,1], x0 = mu[1,1] + axis1.x, y0 = mu[2,1] + axis1.y)
    graphics::segments(x1 = mu[1,1], y1 = mu[2,1], x0 = mu[1,1] - axis1.x, y0 = mu[2,1] - axis1.y)
    graphics::segments(x1 = mu[1,1], y1 = mu[2,1], x0 = mu[1,1] + axis2.x, y0 = mu[2,1] + axis2.y)
    graphics::segments(x1 = mu[1,1], y1 = mu[2,1], x0 = mu[1,1] - axis2.x, y0 = mu[2,1] - axis2.y)
  }

  # if TRUE show the center as a point and draw dashed lines orthogonal to the x and y axes
  # These are just line vertical and horizontal segments from the origin to the x and y axes, respectively
  if (center){
    graphics::segments(x1 = mu[1,1], y1 = mu[2,1], y0 = mu[2,1], x0 = 0, lty=2)
    graphics::segments(x1 = mu[1,1], y1 = mu[2,1], x0 = mu[1,1], y0 = 0, lty=2)
    graphics::points(x = mu[1,1], y = mu[2,1], pch = 19)
  }
}
