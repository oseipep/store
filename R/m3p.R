#' Three-parameter state-trait ordinal regression
#' 
#' This function fits a "vanilla" regression model for a repeated ordinal response and an
#' ordinal predictor. On the latent scale this is a linear model with a random person effect
#' and three fixed effects:
#' the intercept, the effect of the trait (person-level median of the predictor), 
#' and the effect of the deviation of state (current predictor value) from trait. This model
#' model serves as a baseline with which the more flexible model implemented by \code{\link{store}} may be compared.
#' See Osei and Reiss (2022) for details.
#' @param formula An object of class \code{\link{formula}}.
#' @param data A data frame containing the variables in the model.
#' @param id A variable name for the subjects of the \code{data}.
#' @return A fitted three-parameter model object with a continous response and predictor ordinal 
#' variables extracted from \code{formula}.
#' @author Prince P. Osei and Philip T. Reiss
#' @seealso \code{\link{store}}
#' @references Osei, P. P. and Reiss, P. T. (2022). Ordinal state-trait regression for intensive longitudinal data. Under revision.
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
