



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
                     limadj = limadj, ...)
}

# This is a work in progress! Need to convert this example to a function.
# Resume working here.
# 2020-01-26 There seems to be a problem with the way the xl and yl are being determined:
# Using the ex5.7 values, run these lines:

# bvNormalContour(mu=x.bar[1:2,1],eig=eigen(S[1:2,1:2]), limadj =0.2)
# bvNormalContour(mu=x.bar[1:2,1],eig=eigen(S[1:2,1:2]))
# bvNormalContour(mu=x.bar[2:3,1],eig=eigen(S[1:2,1:2]),n=n,p=p)

# There also seems to be some strange behaviour with the limadj values
# Center = TRUE does not seem to be working right

bvNormalContour <- function(mu = c(0,0), Sigma=NULL, eig=NULL,
                            xl = NULL, yl = NULL,
                            axes = TRUE, center = TRUE,
                            limadj = 0.02, alpha = 0.05, ...){

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

  # We should call an ellipse function here, passing the calculated lengths to it and have it dynamically generate xl and yl
  angle <- atan(eig$vectors[2,1]/eig$vectors[1,1]) # sohcahtoa

  eigenEllipseHelper(mu = mu, lengths = lengths, angle = angle,
                     xl = xl, yl = yl,
                     axes = axes, center = center,
                     limadj = limadj, ...)

  # plot(0,pch='',ylab='',xlab='',xlim=c(-5,5),ylim=c(-5,5))
  # draw.ellipse(x=mu[1],y=mu[2],
  #              a=lengths[1],b=lengths[2],
  #              angle=angle,deg=FALSE)

}

# There seems to be a problem with the limadj for the xl yl calculation
eigenEllipseHelper <- function(mu, lengths, angle, xl, yl, limadj, axes, center, ...){
  axis1 <- lengths[1]
  axis2 <- lengths[2]

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
    xl1 <- min(mu[1,1] + axis1.x,
               mu[1,1] - axis1.x,
               mu[1,1] + axis2.x,
               mu[1,1] - axis2.x)
    # adjust the limit by the specified value
    # larger values of limadj decrease the lower limit
    xl1 <- xl1 * (1 - limadj)

    # calculate the upper x limit
    # using the same set of offsets from the mean, determine the largest value needed
    xl2 <- max(mu[1,1] + axis1.x,
               mu[1,1] - axis1.x,
               mu[1,1] + axis2.x,
               mu[1,1] - axis2.x)
    # adjust the limit by the specified value
    # larger values of limadj increase the upper limit
    xl2 <- xl2 * (1 + limadj)
    xl <- c(xl1,xl2)
  }

  # if no y limits are specified, calculate some that will ensure the entirety of the ellipse will fit on the y axis
  if (is.null(yl)){
    # calculate the lower y limit by determining the minimum y value needed
    # use the offsets from the mean calculated earlier
    yl1 <- min(mu[2,1] + axis1.y,
               mu[2,1] - axis1.y,
               mu[2,1] + axis2.y,
               mu[2,1] - axis2.y)
    # adjust the limit by the specified value
    # larger values of limadj decrease the lower limit
    yl1 <- yl1 * (1 - limadj)

    # calculate the upper y limit
    # using the same set of offsets from the mean, determine the largest value needed
    yl2 <- max(mu[2,1] + axis1.y,
               mu[2,1] - axis1.y,
               mu[2,1] + axis2.y,
               mu[2,1] - axis2.y)
    # adjust the limit by the specified value
    # larger values of limadj increase the upper limit
    yl2 <- yl2 * (1 + limadj)
    yl <- c(yl1,yl2)
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
