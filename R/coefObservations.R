#' @export
coefObservations <-
  function(formula,data,id){
    y <- all.vars(formula)[1]
    x <- all.vars(formula)[2]
    nlevels <- max(unique(data[,x]),na.rm = TRUE)
    tmp <- addmed(data,x,id)
    cyx <- list()
    indx <- 1
    for (k in setdiff(sort(unique(tmp$pmed)),NA)) for (l in setdiff(sort(unique(tmp[,x])),NA)){
      if (any(tmp$pmed==k & tmp[,x]==l, na.rm=TRUE)) {
        cyx[[indx]] <- list(xtstate=c(k,l),ytabs=table(tmp[tmp$pmed==k & tmp[,x]==l,y]))
        indx <- indx +1
      }
    }
    
    matt <- NULL
    for (i in 1:nlevels){
      xtrait <- i
      for(j in 1:nlevels){
        xstate <- j
        count <- 0
        for(k in 1:length(cyx)){
          if (cyx[[k]]$xtstate[1]==xtrait && cyx[[k]]$xtstate[2]==xstate){
            matt <- rbind(matt,c(xtrait,xstate,sum(cyx[[k]]$ytabs)))
            count <- count + 1
          }
        }
        if (count==0){
          matt <- rbind(matt,c(xtrait,xstate,0))
        }
      }
    }
    matt
  }