#' Median for an ordinal variable
#' 
#' This function simply computes median of ordinal variable, with one modification:
#' if the median is between two values, the larger or smaller value is returned, depending 
#' if the mean is larger or smaller.
#' 
#' @param v A vector of numeric values
#' @return The median as an average of the mean and default median of \code{v}.
#' %% @note %% ~~further notes~~
#' @author Philip T. Reiss and Prince P. Osei
#' %% @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
#' %% @references %% ~put references to the literature/web site here ~
#' @importFrom stats median
#' @importFrom utils data
#' @examples
#' data(cars)           
#' meadian(cars$dist)     
#' @export
meadian <-
function(v) {
	med <- as.double(median(v, na.rm=TRUE))	
	if (!is.na(med) & med != round(med)) med <- med + (mean(v,na.rm=TRUE)>med) - 0.5
	med
}
