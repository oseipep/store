#' Continous state-trait ordinal regression
#' 
#' This function performs regression of a continuous ordinal response on an ordinal predictor compared 
#' with a general model in \code{\link{store}} state-trait ordinal regression.
#' 
#' @param formula An object of class \code{\link{formula}}.
#' @param data A data frame containing the variables in the model.
#' @param id A variable name for the subjects of the \code{data}.
#' @return A fitted three-parameter model object with a continous response and predictor ordinal 
#' variables extracted from \code{formula}.
#' @author Prince P. Osei and Philip T. Reiss
#' @seealso \code{\link{store}}
#' %% @references %% ~put references to the literature/web site here ~
#' @importFrom stats as.formula
#' @importFrom  mgcv gam ocat
#' @examples
#' ## maybe something with simulated data...
#' @export
m3p <- 
  function(formula, data, id) {
  y <- all.vars(formula)[1]  
  x <- all.vars(formula)[2]
  nlev <- length(setdiff(unique(data[,y]),NA))
  data <- addmed(data, x, id=id)
  data$dev <- data[,x] - data[,"pmed"]
  fmla <- as.formula(paste0(y, " ~ pmed + dev + s(", id, ", bs='re')"))
  mgcv::gam(fmla, data = data, method = "REML", family = ocat(R = nlev))
  }
