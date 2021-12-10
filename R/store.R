#' State-trait ordinal regression
#' 
#' This function performs regression of an ordinal response on an ordinal predictor with
#' intensive longitudinal data. The methodology allows for separating state effects from 
#' trait effects.
#' 
#' @param formula An expression of the form \code{y ~ x} representing the model.
#' @param data A data frame containing the variables in the model.
#' @param id A variable name for the subjects of the \code{data}.
#' @param penalize A logical variable: if TRUE (the default), a non-linearity penalty is applied.
#' @param method Package implementing ordinal mixed-effect regression: either "\code{mgcv}" (the default) or "\code{ordinal}".
#' @param model The model to fit: "TS" for trait-state model with random effects (the default), "trait" for trait-only model with random effects, "RE" for model with random effects only.
#' @param covt The covariates of the model; default is NULL.
#' @param save.data A logical variable if TRUE (the default) store the \code{data}.
#' @param \ldots Other arguments, passed to \code{\link[mgcv]{gam}}.
#' @return A fitted model object with response and predictor ordinal variables extracted from \code{formula}. 
#' This is a \code{\link[mgcv]{gam}} object with some additional components.
#' @author Philip T. Reiss and Prince P. Osei
#' %% @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
#' %% @references %% ~put references to the literature/web site here ~
#' @importFrom stats as.formula
#' @importFrom ordinal clmm       
#' @importFrom  mgcv gam ocat
#' @examples
#' \dontrun{
#' mod <- store(absorbed.~stressed., data=thoughts, id="Subject")  # takes several minutes
#' summary(mod)
#' plot(mod)
#' }
#' @export
store <-
function(formula, data, id, penalize=TRUE, method="mgcv", model="TS", covt=NULL,
         save.data=TRUE, ...) {
  y <- all.vars(formula)[1]  
  x <- all.vars(formula)[2]
  modmats <- get.modmats(x,data,id)
	medmat <- modmats[[1]]    # dummy variables for median x
	devmat <- modmats[[2]]    # dummy variables for combinations of median and current x
	tsmat <- cbind(medmat,devmat) # dummy variables for combination of trait and state
	nlevels <- max(as.numeric(substr(colnames(medmat),4,4)), na.rm = TRUE)
	if (method=="ordinal") { 
	    if (model=="TS")  rs <- paste0("tsmat +  (1|", id, ")") else
	    if (model=="trait") rs <- paste0("medmat + (1|", id, ")") else
	    if (model=="RE") rs <- paste0("(1|", id, ")") else
	    stop("'model' must be 'TS', 'trait' or 'RE'")
	    
        if (!is.null(covt)) rs <- paste(covt, "+", rs)
        fmla <- as.formula(paste("factor(data[,y]) ~", rs))
	    mdl <- ordinal::clmm(fmla, data=data)
	} 
    else {
        nlev <- length(setdiff(unique(data[,y]),NA))  
        			                                  
        if (model=="TS") rs <- paste0("tsmat +  s(", id, ", bs='re')") else
        if (model=="trait") rs <- paste0("medmat + s(", id, ", bs='re')") else
        if (model=="RE") rs <- paste0("s(", id, ", bs='re')") else
        stop("'model' must be 'TS', 'trait' or 'RE'")
        
        if (!is.null(covt)) rs <- paste(covt, "+", rs)
        fmla <- as.formula(paste("data[,y] ~", rs))
        if (!penalize)  mdl <- mgcv::gam(fmla, data=data, method="REML", family=ocat(R=nlev), ...)
        else if (penalize) {
            # construction of halfP_rr; halfP_ss; halfP_rs
            hp <- make.halfpens(tsmat, nlev)
            P <- crossprod(hp$halfP_rr) + 2*crossprod(hp$halfP_rs) + crossprod(hp$halfP_ss)
            mdl <- mgcv::gam(fmla, paraPen=list(tsmat=list(P)), data=data, method="REML", family=ocat(R=nlev), ...)
        }  
    }	
	mdl$medmat <- medmat
	mdl$devmat <- devmat
	mdl$store.formula <- formula
	mdl$store.method <- method
	mdl$store.model <- model
	if (save.data) mdl$data <- data
	mdl$id <- id
	mdl$nlevels <- nlevels
	mdl$penalize <- penalize
	if (penalize) mdl$halfP <- hp
	mdl$fixed.df <- sum(mdl$edf[substr(names(mdl$edf),1,2) != "s("])
	class(mdl) <- c("store","gam","glm","lm")
	mdl
}
