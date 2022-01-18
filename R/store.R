#' State-trait ordinal regression
#' 
#' This function performs regression of an ordinal response on an ordinal predictor with
#' intensive longitudinal data. The methodology allows for separating state effects from 
#' trait effects.
#' 
#' @param formula An expression of the form \code{y ~ x} representing the model.
#' @param data Data frame containing the variables in the model.
#' @param id Variable name for the subjects of the \code{data}.
#' @param penalize A logical variable: if TRUE (the default), a non-linearity penalty is applied.
#' @param method Package implementing ordinal mixed-effect regression: either "\code{mgcv}" (the default) or "\code{ordinal}".
#' @param model The model to fit: "TS" for trait-state model with random effects (the default), "trait" for trait-only model with random effects, "RE" for model with random effects only.
#' @param covt The covariates of the model; default is NULL.
#' @param save.data A logical variable: if TRUE (the default) store the \code{data}.
#' @param \ldots Other arguments, passed to \code{\link[mgcv]{gam}}.
#' @return A fitted model object with response and predictor ordinal variables extracted from \code{formula}. 
#' This is a \code{\link[mgcv]{gam}} object with some additional components.
#' 
#' When \code{model} is \code{"TS"} or \code{"trait"}, the model matrix is called \code{tsmat}. 
#' Coefficient \code{tsmatmed1}, for example, is the "trait effect" associated with median predictor value 1.
#' Coefficient \code{tsmat1.2} is the "state effect" of current predictor value 2 for an individual whose median is 1.
#' 
#' @author Prince P. Osei and Philip T. Reiss  
#' %% @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
#' %% @references Osei, P. P. and Reiss, P. T. Ordinal state-trait regression for intensive longitudinal data. Under revision.
#' @importFrom stats as.formula predict var
#' @importFrom ordinal clmm       
#' @importFrom  mgcv gam ocat
#' @examples
#' \dontrun{
#' mod <- store(pleasant.~stressed., data=thoughts, id="Subject")  # takes several minutes
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
	if (model=="TS") tsmat <- cbind(medmat,devmat) else
	    if (model=="trait") tsmat <- medmat
	nlevels <- max(as.numeric(substr(colnames(medmat),4,4)), na.rm = TRUE)
	if (method=="ordinal") { 
	    if (model %in% c("TS","trait"))  rs <- paste0("tsmat +  (1|", id, ")") else
	    if (model=="RE") rs <- paste0("(1|", id, ")") else
	    stop("'model' must be 'TS', 'trait' or 'RE'")
	    
        if (!is.null(covt)) rs <- paste(covt, "+", rs)
        fmla <- as.formula(paste("factor(data[,y]) ~", rs))
	    mdl <- ordinal::clmm(fmla, data=data)
	} 
    else {
        nlev <- length(setdiff(unique(data[,y]),NA))  
        			                                  
        if (model %in% c("TS","trait")) rs <- paste0("tsmat +  s(", id, ", bs='re')") else
        if (model=="RE") rs <- paste0("s(", id, ", bs='re')") else
        stop("'model' must be 'TS', 'trait' or 'RE'")
        
        if (!is.null(covt)) rs <- paste(covt, "+", rs)
        fmla <- as.formula(paste("data[,y] ~", rs))
        if (!penalize)  mdl <- mgcv::gam(fmla, data=data, method="REML", family=ocat(R=nlev), ...)
        else if (penalize) {
            # construction of halfP_rr; halfP_ss; halfP_rs
            if (model == "TS") {
              hp <- make.halfpens(tsmat, nlev)
              P <- crossprod(hp$halfP_rr) + 2*crossprod(hp$halfP_rs) + crossprod(hp$halfP_ss)
            }
          else if (model == "trait") {
             hp <- make.tauhalfpen(tsmat,nlev)
             P <- crossprod(hp)
           }
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
	mdl$R2 <- var(predict(mdl, exclude="s(Subject)")) / (var(predict(mdl))+1)
	class(mdl) <- c("store","gam","glm","lm")
	mdl
}
