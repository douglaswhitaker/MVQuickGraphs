



# this will be moved to the DESCRIPTION file
require(plotrix)

# Need to split this into two functions so we can make one for bvNormalContours that recycles the ellipse calculations.
# This functions graphs a confidence ellipse for mu based on the eigenvalues and eigenvectors for the covariance matrix S.
confidenceEllipse <- function(X.mean = c(0,0),
                              eig,
                              n,
                              p,
                              xl = NULL, yl = NULL, # the x and y axis limits; calculated dynamically if not specified
                              axes = TRUE, # if TRUE, the major and minor axes of the ellipse are graphed
                              center = TRUE, # if TRUE, a dot at the center of the ellipse and dashed lines to the axes are shown
                              limadj = 0.02, # an axis adjustment factor used if xl or yl is NULL (to adjust the calculated limits)
                              alpha = 0.05){ # the alpha-level used to determine which ellipse is drawn

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

  # Calculate x and y components for each axis.
  # We will use these to determine position from the origin (mean) later.
  axis1.x <- cos(angle) * axis1
  axis1.y <- sin(angle) * axis1
  axis2.x <- cos(angle + pi / 2) * axis2
  axis2.y <- sin(angle + pi / 2) * axis2

  # if no x limits are specified, calculate some that will ensure the entirety of the ellipse will fit on the x axis
  if (is.null(xl)){
    # calculate the lower x limit by determining the minimum x value needed
    # use the offsets from the mean calculated earlier
    xl1 <- min(X.mean[1,1] + axis1.x,
               X.mean[1,1] - axis1.x,
               X.mean[1,1] + axis2.x,
               X.mean[1,1] - axis2.x)
    # adjust the limit by the specified value
    # larger values of limadj decrease the lower limit
    xl1 <- xl1 * (1 - limadj)

    # calculate the upper x limit
    # using the same set of offsets from the mean, determine the largest value needed
    xl2 <- max(X.mean[1,1] + axis1.x,
               X.mean[1,1] - axis1.x,
               X.mean[1,1] + axis2.x,
               X.mean[1,1] - axis2.x)
    # adjust the limit by the specified value
    # larger values of limadj increase the upper limit
    xl2 <- xl2 * (1 + limadj)
    xl <- c(xl1,xl2)
  }

  # if no y limits are specified, calculate some that will ensure the entirety of the ellipse will fit on the y axis
  if (is.null(yl)){
    # calculate the lower y limit by determining the minimum y value needed
    # use the offsets from the mean calculated earlier
    yl1 <- min(X.mean[2,1] + axis1.y,
               X.mean[2,1] - axis1.y,
               X.mean[2,1] + axis2.y,
               X.mean[2,1] - axis2.y)
    # adjust the limit by the specified value
    # larger values of limadj decrease the lower limit
    yl1 <- yl1 * (1 - limadj)

    # calculate the upper y limit
    # using the same set of offsets from the mean, determine the largest value needed
    yl2 <- max(X.mean[2,1] + axis1.y,
               X.mean[2,1] - axis1.y,
               X.mean[2,1] + axis2.y,
               X.mean[2,1] - axis2.y)
    # adjust the limit by the specified value
    # larger values of limadj increase the upper limit
    yl2 <- yl2 * (1 + limadj)
    yl <- c(yl1,yl2)
  }

  # Call an empty graph that we can add the normal contour ellipseto
  plot(0,pch='',ylab='',xlab='',xlim=xl,ylim=yl)

  # create a vector of axis lengths we can use
  lengths <- c(axis1,axis2)

  # draw the ellipse
  # draw.ellipse is from package plotrix
  draw.ellipse(x = X.mean[1,1], y = X.mean[2,1], # center of the ellipse
               a = lengths[1], b = lengths[2], # major and minor axis half-lengths
               angle = angle, deg = FALSE) # calculated angle earlier was in radians

  # if TRUE draw the major and minor axes for the normal contour ellipse
  # These are just line segments using the offsets from the mean calculcated earlier.
  if (axes){
    segments(x1 = X.mean[1,1], y1 = X.mean[2,1], x0 = X.mean[1,1] + axis1.x, y0 = X.mean[2,1] + axis1.y)
    segments(x1 = X.mean[1,1], y1 = X.mean[2,1], x0 = X.mean[1,1] - axis1.x, y0 = X.mean[2,1] - axis1.y)
    segments(x1 = X.mean[1,1], y1 = X.mean[2,1], x0 = X.mean[1,1] + axis2.x, y0 = X.mean[2,1] + axis2.y)
    segments(x1 = X.mean[1,1], y1 = X.mean[2,1], x0 = X.mean[1,1] - axis2.x, y0 = X.mean[2,1] - axis2.y)
  }

  # if TRUE show the center as a point and draw dashed lines orthogonal to the x and y axes
  # These are just line vertical and horizontal segments from the origin to the x and y axes, respectively
  if (center){
    segments(x1 = X.mean[1,1], y1 = X.mean[2,1], y0 = X.mean[2,1], x0 = 0, lty=2)
    segments(x1 = X.mean[1,1], y1 = X.mean[2,1], x0 = X.mean[1,1], y0 = 0, lty=2)
    points(x = X.mean[1,1], y = X.mean[2,1], pch = 19)
  }
}
