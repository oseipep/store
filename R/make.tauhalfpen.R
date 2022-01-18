#' @keywords internal
#' 
make.tauhalfpen <- 
  function(medmat, nlev) {
  halfP_r <- NULL
  for (r in 2:(nlev-1)) {
    tp <- c(r-1, r, r+1) # Tau's 
    cnames <- paste0("med",tp) # the taus
    # a vector of length 3 to be inserted at taus r-1,r,r+1, 
    rcoefs <- c(1, -2, 1) 
    cindex <- NULL # check which r's available and at which positions
    tpentries <- rep(0,ncol(medmat)) 
    for (ii in 1:length(cnames)) {
      cindex <- c(cindex,which(colnames(medmat) == cnames[ii]))
    }
    hh <- colnames(medmat)[cindex] # available in the data
    ll <- setdiff(cnames,hh) # not available in the data
    if( length(ll) == 0) tpentries[cindex] <- rcoefs # all the 3 different positions available
    else {
      iidex <- NULL
      for (ii in 1:length(ll)) iidex <- c(iidex,which(cnames==ll[ii]))
      tpentries[cindex] <- rcoefs[-iidex] # remove the absent ones 
    }
    halfP_r <- rbind(halfP_r,tpentries) 
  }
  colnames(halfP_r) <- colnames(medmat)
  halfP_r
}