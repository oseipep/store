#' State-trait ordinal regression
#' 
#' This function performs regression of an ordinal response on an ordinal predictor with
#' intensive longitudinal data. The methodology allows for separating state effects from 
#' trait effects.
#' 
#' @param formula An object of class \code{\link{formula}}.
#' @param data A data frame containing the variables in the model.
#' @param id A variable name for the subjects of the \code{data}.
#' @param pen The method of penalization to be used. If it is "none", no penalization.
#' @param method Package implementing ordinal mixed-effect regression: either "mgcv" (the default) or "ordinal".
#' @param model The model to fit: "TS" for trait-state model with random effects (the default), "trait" for trait-only model with random effects, "RE" for model with random effects only.
#' @param covt The covariance structure of the model; default is NULL.
#' @param save.data A logical variable if TRUE store the \code{data}; default is FALSE.
#' @return A fitted model object with response and predictor ordinal variables extracted from \code{formula}.
#' %% @note %% ~~further notes~~
#' @author Philip T. Reiss and Prince P. Osei
#' %% @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
#' %% @references %% ~put references to the literature/web site here ~
#' @importFrom stats as.formula
#' @importFrom ordinal clmm       
#' @importFrom  mgcv gam ocat
#' @examples
#' ## maybe something with simulated data...
#' @export
store <-
function(formula, data, id, pen="none", method="mgcv", model="TS", covt=NULL,
         save.data=FALSE) {
  y <- all.vars(formula)[1] # extract the variable names of response and predictor
  x <- all.vars(formula)[2]
  modmats <- get.modmats(x,data,id)
	medmat <- modmats[[1]]    # dummy variables for median x
	devmat <- modmats[[2]]    # dummy variables for combinations of median and current x
	nlevels <- max(as.numeric(substr(colnames(medmat),4,4)),na.rm = TRUE)
	if (method=="ordinal") { 
	    if (model=="TS")  rs <- paste0("medmat + devmat + ", "(1|",id,")") else
	    if (model=="trait") rs <- paste0("medmat + ", "(1|",id,")") else
	    if (model=="RE") rs <- paste0("(1|",id,")") else
	    stop("'model' must be 'TS', 'trait' or 'RE'")
	    
      if (!is.null(covt)) rs <- paste(covt, "+", rs)
      fmla <- as.formula(paste("factor(data[,y]) ~", rs))
	    mdl <- ordinal::clmm(fmla, data=data)
	} 
    else {
        nlev <- length(setdiff(unique(data[,y]),NA))  
        			                                  
        if (model=="TS") rs <- paste0("medmat + devmat + ", "s(",id,", bs='re')") else
        if (model=="trait") rs <- paste0("medmat + ", "s(",id,", bs='re')") else
        if (model=="RE") rs <- paste0("s(",id,", bs='re')") else
        stop("'model' must be 'TS', 'trait' or 'RE'")
        
        if (!is.null(covt)) rs <- paste(covt, "+", rs)
        fmla <- as.formula(paste("data[,y] ~", rs))
        if (pen == "none") mdl <- mgcv::gam(fmla, data=data, method="REML", family=ocat(R=nlev))
        else {
        	if (pen=="ridge") P <- diag(ncol(devmat)) 
        	else if (pen=="diag") {
            	cnames1 <- as.numeric(substr(colnames(devmat),1,1))
            	cnames2 <- as.numeric(substr(colnames(devmat),3,3))
            	halfP <- NULL
            	for (j in min(cnames1-cnames2):max(cnames1-cnames2)) {
                	wj <- which(cnames1-cnames2==j)
                	lwj <- length(wj)
                	if (lwj>1) {
                    	for (ii in 1:(lwj-1)) for (jj in (ii+1):lwj) {
                        	nurow <- rep(0,ncol(devmat))
                        	nurow[wj[ii]] <- 1
                        	nurow[wj[jj]] <- -1
                        	halfP <- rbind(halfP, nurow)
                    	}
                	}
            	}
            	P <- crossprod(halfP)
            }
            mdl <- mgcv::gam(fmla, paraPen=list(devmat=list(P)), data=data, method="REML", family=ocat(R=nlev))
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
	mdl$pen <- pen
	class(mdl) <- c("store","gam","glm","lm")
	mdl
}
