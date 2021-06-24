#' Add a rectangle to a correlation plot

#' @param rstart The name of the variable contained in the correlation plot that will serve as the vertical starting point of the rectangle.
#' @param rend The name of the variable contained in the correlation plot that will serve as the vertical ending point of the rectangle.
#' @param cstart (optional) The name of the variable contained in the correlation plot that will serve as the horizontal starting point of the rectangle.
#' If no cstart or cend provided, creates an on-diagonal rectangle automatically. cstart and cend allows for off-diagonal rectangles.
#' @param cend (optional) The name of the variable contained in the correlation plot that will serve as the horizontal ending point of the rectangle.
#' If no cstart or cend provided, creates an on-diagonal rectangle automatically. cstart and cend allows for off-diagonal rectangles.
#' @param correlation_matrix The correlation matrix the correlation plot is based on.
#' @param mirror If TRUE, also adds the equivalent rectangle from the other side of the diagonal (i.e. the "mirror" of the original).
#' The function ignores this entirely if the rectangle is on the diagonal, as there is no mirror.
#' @param lwd Determines the width of the rectangle lines.
#' @param ... Arguments to modify graphical parameters, etc.
#' @examples
#' #Adding an on-diagonal rectangle to a correlation plot with mirroring, using the Bechtoldt sample correlation matrix provided by the psych package.
#' library(corrplot)
#' library(psych)
#' corrplot(Bechtoldt)
#' make_rect(rstart="First_Names", rend="Flags", correlation_matrix=Bechtoldt, mirror=TRUE)
#'
#' #Adding an off-diagonal rectangle
#' make_rect(rstart="First_Names", rend="Flags", cstart="First_Names", cend="Sentences", correlation_matrix=Bechtoldt)
#'
#' @export

make_rect <- function(rstart, rend, cstart=NULL, cend=NULL, correlation_matrix, mirror=FALSE, lwd=3,...)
{
  #if the rectangle is not off-diagonal
  if (is.null(cstart)==TRUE&&is.null(cend)==TRUE){
    cstart = rstart
    cend = rend
  }

  #uses corrRect function to construct the rectangle using above values as the parameters
  corrplot::corrRect(namesMat = c(cend, rend, cstart, rstart), corrRes = corrplot(correlation_matrix), lwd = lwd, ...)

  #If mirror is set to be TRUE, draws the same rectangle on the other side of the diagonal. Function ignores mirror if diagonal as there is no mirror.
  if (mirror==TRUE){
    if (is.null(cstart)==FALSE&&is.null(cend)==FALSE){
      corrplot::corrRect(namesMat = rbind(c(rend, cend, rstart, cstart), c(cend, rend, cstart, rstart)), corrRes = corrplot(correlation_matrix), lwd = lwd, ...)
    }
  }
}
