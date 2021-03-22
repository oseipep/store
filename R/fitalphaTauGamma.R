#' @importFrom stats coef
#' @export
fitalphaTauGamma <-
  function(obj){
    nlevels <- obj$nlevels
    method <- obj$store.method
    matt <- matrix(NA, nlevels, nlevels)
    meds <- as.numeric(substr(names(coef(obj))[substr(names(coef(obj)),7,9)=="med"],10,10))
    gmed <- setdiff(1:nlevels,meds)
    if (length(gmed)>1) gmed <- gmed[gmed>1]
    if (method == "ordinal"){
      for (i in 1:nlevels) for (j in 1:nlevels) {
        naym <- c(paste0("medmatmed",i),paste0("devmat",i,".",j))
        if (naym[1] %in% names(coef(obj)) && naym[2] %in% names(coef(obj))){
          matt[i,j] <- sum(coef(obj)[naym])}
        else {
          if (i==gmed) matt[i,j] <- 0
          else matt[i,j] <- coef(obj)[naym[1]]
        }
      }
    }
    else {
      alpha <- paste0("(Intercept)")
      for (i in 1:nlevels) for (j in 1:nlevels) {
        naym <- c(paste0("medmatmed",i),paste0("devmat",i,".",j))
        if (naym[1] %in% names(coef(obj)) && naym[2] %in% names(coef(obj))){
          matt[i,j] <- sum(coef(obj)[alpha],coef(obj)[naym])}
        else {
          if (i==gmed) matt[i,j] <- coef(obj)[alpha]
          else matt[i,j] <- sum(coef(obj)[alpha],coef(obj)[naym[1]],na.rm = TRUE)
        }
      }
    }
    matt
  }


