

# Essentially, this file contains three functions (two exported, one internal).
# confidenceEllipse and bvNormalContour both take the eigen decomposition of a (covariance) matrix to plot
# the appropriate ellipse. These functions calculate the appropriate lengths of the major and minor axes
# and the angle of rotation of the ellipse; these values are then passed to eigenEllipseHelper to actually draw
# the ellipse.

# To do: make an add=TRUE parameter (at least for bvNormalContour so that one can make a contour plot with a loop)
# This might be easiest to just do by accepting a vector for alpha. Then, when it comes time to calculate lengths,
# check the length of clevel and if > 1 just loop through the tail end of the function.

# This functions graphs a confidence ellipse for mu based on the eigenvalues and eigenvectors for the covariance matrix S.
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
           qf(1 - alpha, p, n - p))

  # Calculate the half-length of the minor axis
  # This is an application of result (5-19) in Johnson & Wichern (2007)
  axis2 <- sqrt(eig$values[2]) *
    sqrt((p * (n - 1)) /
           (n * (n - p)) *
           qf(1 - alpha, p, n - p))

  # create a vector of axis lengths we can use
  lengths <- c(axis1,axis2)

  eigenEllipseHelper(mu = X.mean, lengths = lengths, angle = angle,
                     xl = xl, yl = yl,
                     axes = axes, center = center,
                     lim.adj = lim.adj, ...)
}

bvNormalContour <- function(mu = c(0,0), Sigma=NULL, eig=NULL,
                            xl = NULL, yl = NULL,
                            axes = TRUE, center = FALSE,
                            lim.adj = 0.02, alpha = 0.05, ...){

  # eigenEllipseHelper is expecting a matrix... might be better to eventually change that
  mu <- matrix(mu, ncol=1)

  # Critical value for the constant density contour (always df=2 because this is bivariate normal)
  # Johnson & Wichern (2008) result (4-8)
  clevel <- qchisq(1 - alpha, df = 2)

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
  plot(0,pch='',ylab='',xlab='',xlim=xl,ylim=yl)

  # draw the ellipse
  # draw.ellipse is from package plotrix
  draw.ellipse(x = mu[1,1], y = mu[2,1], # center of the ellipse
               a = lengths[1], b = lengths[2], # major and minor axis half-lengths
               angle = angle, deg = FALSE) # calculated angle earlier was in radians

  # if TRUE draw the major and minor axes for the normal contour ellipse
  # These are just line segments using the offsets from the mean calculcated earlier.
  if (axes){
    segments(x1 = mu[1,1], y1 = mu[2,1], x0 = mu[1,1] + axis1.x, y0 = mu[2,1] + axis1.y)
    segments(x1 = mu[1,1], y1 = mu[2,1], x0 = mu[1,1] - axis1.x, y0 = mu[2,1] - axis1.y)
    segments(x1 = mu[1,1], y1 = mu[2,1], x0 = mu[1,1] + axis2.x, y0 = mu[2,1] + axis2.y)
    segments(x1 = mu[1,1], y1 = mu[2,1], x0 = mu[1,1] - axis2.x, y0 = mu[2,1] - axis2.y)
  }

  # if TRUE show the center as a point and draw dashed lines orthogonal to the x and y axes
  # These are just line vertical and horizontal segments from the origin to the x and y axes, respectively
  if (center){
    segments(x1 = mu[1,1], y1 = mu[2,1], y0 = mu[2,1], x0 = 0, lty=2)
    segments(x1 = mu[1,1], y1 = mu[2,1], x0 = mu[1,1], y0 = 0, lty=2)
    points(x = mu[1,1], y = mu[2,1], pch = 19)
  }
}
