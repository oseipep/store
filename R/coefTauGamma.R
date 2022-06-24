#' @importFrom stats coef
#' @keywords internal
coefTauGamma<-
  function(obj){
    nlevels <- obj$nlevels
    #vgm <- setdiff(1:nlevels,as.numeric(substr(names(coef(obj))[2:nlevels],9,9)))
    meds <- as.numeric(substr(names(coef(obj))[substr(names(coef(obj)),6,8)=="med"],9,9))
    vgm <- setdiff(1:nlevels,meds)
    if (length(vgm)>1) vgm <- vgm[vgm>1]
    matt <- matrix(NA, nlevels, nlevels)
    for (i in 1:nlevels) for (j in 1:nlevels) {
      naym1 <- paste0("tsmat",i,".",j)
      naym2 <- paste0("tsmatmed",i)
      if (naym1 %in% names(coef(obj))) matt[i,j] <- coef(obj)[naym1]
      if(i==j){
        if ((i == vgm)   && (j == vgm)) matt[i, j] <- 0
        if (naym2 %in% names(coef(obj))) matt[i,j] <- coef(obj)[naym2]
      }
    }
    matt
  }
