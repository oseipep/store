#' @keywords internal
make.halfpens <- function(tsmat, nlev) {
    halfP_rr <- halfP_ss <- halfP_rs <- NULL
    
    #halfP_rr
    for (r in 1:(nlev-2)) {
      for (s in 1:nlev) {
        tpdevs <- c(r,r+1,r+2, r,s,  r+1,s, r+2,s) # Tau's and gamma's combination
        cnames1 <- paste0("med",tpdevs[1:3]) # the taus
        cnames2 <- paste0(tpdevs[c(4,6,8)],".",tpdevs[c(5,7,9)]) # the gammas
        cnames <- c(cnames1,cnames2) # combine the colnames together
        # a vector of length 6 to be inserted at taus r,r+1,r+2, gammas (r,s),(r+1,s),(r+2,s)
        rscoefs <- c(1,-2,1,1,-2,1) 
        cindex <- NULL # check which combination available and at which positions
        tpentries <- rep(0,ncol(tsmat)) 
        for (ii in 1:length(cnames)) {
          cindex <- c(cindex,which(colnames(tsmat) == cnames[ii]))
        }
        hh <- colnames(tsmat)[cindex] # available in the data
        ll <- setdiff(cnames,hh) # not available in the data
        if( length(ll) == 0) tpentries[cindex] <- rscoefs # all the 6 different positions available
        else {
          iidex <- NULL
          for (ii in 1:length(ll)) iidex <- c(iidex,which(cnames==ll[ii]))
          tpentries[cindex] <- rscoefs[-iidex] # remove the absent ones 
        }
        # Check for (x,y) combinations with x=y (--> gamma_x,y=0)
        gd <- which(tpdevs[c(4,6,8)] == tpdevs[c(5,7,9)])
        if (length(gd) > 0) gcnames <- cnames2[-gd]
        else gcnames <- cnames2
        C2 <- setdiff(gcnames,colnames(tsmat))
        if(length(C2 > 0)) next     # no row added
        else halfP_rr <- rbind(halfP_rr,tpentries) 
      }
    }
    
    # halfP_ss
    for (r in 1:nlev) {
      for (s in 1:(nlev-2)) {
        tpdevs <- c(r,s, r,s+1, r,s+2) # r,s combinations
        cnames <- paste0(tpdevs[c(1,3,5)],".",tpdevs[c(2,4,6)]) # the gammas
        # a vector of length 3 to be inserted at (r,s),(r,s+1),(r,s+2)
        rscoefs <- c(1,-2,1) 
        cindex <- NULL # check which combination available and at which positions
        tpentries <- rep(0,ncol(tsmat))  
        for (ii in 1:length(cnames)) {
          cindex <- c(cindex,which(colnames(tsmat) == cnames[ii]))
        }
        hh <- colnames(tsmat)[cindex] # available in the data
        ll <- setdiff(cnames,hh) # not available in the data
        if( length(ll) == 0) tpentries[cindex] <- rscoefs # all the 6 different positions available
        else {
          iidex <- NULL
          for (ii in 1:length(ll)) iidex <- c(iidex,which(cnames==ll[ii]))
          tpentries[cindex] <- rscoefs[-iidex] # remove the absent ones 
        }
        # Check for (x,y) combinations with x=y (--> gamma_x,y=0)
        gd <- which(tpdevs[c(1,3,5)] == tpdevs[c(2,4,6)])
        if (length(gd) > 0) gcnames <- cnames[-gd]
        else gcnames <- cnames
        C2 <- setdiff(gcnames,colnames(tsmat))
        if(length(C2 > 0)) next     # no row added
        else halfP_ss <- rbind(halfP_ss,tpentries) 
      }
    }
    
    # halfP_rs
    for (r in 1:(nlev-1)) {
      for (s in 1:(nlev-1)) {
        tpdevs <- c(r,s, r+1,s, r,s+1, r+1,s+1) # r,s combinations
        cnames <- paste0(tpdevs[c(1,3,5,7)],".",tpdevs[c(2,4,6,8)]) # the gammas
        # a vector of length 4 to be inserted at (r,s),(r+1,s),(r,s+1),(r+1,s+1)
        rscoefs <- c(1,-1,-1,1) 
        cindex <- NULL # check which combination available and at which positions
        tpentries <- rep(0,ncol(tsmat))  
        for (ii in 1:length(cnames)) {
          cindex <- c(cindex,which(colnames(tsmat) == cnames[ii]))
        }
        hh <- colnames(tsmat)[cindex] # available in the data
        ll <- setdiff(cnames,hh) # not available in the data
        if( length(ll) == 0) tpentries[cindex] <- rscoefs # all the 6 different positions available
        else {
          iidex <- NULL
          for (ii in 1:length(ll)) iidex <- c(iidex,which(cnames==ll[ii]))
          tpentries[cindex] <- rscoefs[-iidex] # remove the absent ones 
        }
        # Check for (x,y) combinations with x=y (--> gamma_x,y=0)
        gd <- which(tpdevs[c(1,3,5,7)] == tpdevs[c(2,4,6,8)])
        if (length(gd) > 0) gcnames <- cnames[-gd]
        else gcnames <- cnames
        C2 <- setdiff(gcnames,colnames(tsmat))
        if(length(C2 > 0)) next # no row added
        else halfP_rs <- rbind(halfP_rs,tpentries) 
      }
    }
    colnames(halfP_rr) <- colnames(halfP_ss) <- colnames(halfP_rs) <- colnames(tsmat)
    halfP <- list(halfP_rr=halfP_rr, halfP_ss=halfP_ss, halfP_rs=halfP_rs)
    
    halfP
  }
