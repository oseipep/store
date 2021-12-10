#' @importFrom stats coef
#' @keywords internal
coefTauGamma<-
  function(obj){
    nlevels <- obj$nlevels
    matt <- matrix(NA, nlevels, nlevels)
    for (i in 1:nlevels) for (j in 1:nlevels) {
      naym1 <- paste0("tsmat",i,".",j)
      naym2 <- paste0("tsmatmed",i)
      if (naym1 %in% names(coef(obj))) matt[i,j] <- coef(obj)[naym1]
      if(i==j){
        if (naym2 %in% names(coef(obj))) matt[i,j] <- coef(obj)[naym2]
      }
    }
    matt
  }
