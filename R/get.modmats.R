#' @importFrom stats model.matrix
#' @keywords internal
get.modmats <-
function(x, data,id) {
  data <- addmed(data,x,id)
	modmat.1 <- model.matrix(~factor(pmed)-1, data)  
	modmat.2 <- model.matrix(~factor(data$pmed):factor(data[,x])-1) 
	#-----------
	modmat1 <- matrix(NA,nrow=nrow(data),ncol=ncol(modmat.1))
	modmat2 <- matrix(NA,nrow=nrow(data),ncol=ncol(modmat.2))
	#----------
	nm1 <- which(!is.na(data$pmed))
	nm2 <- which(!is.na(data[,x]))
	#------------
	modmat1[nm1,] <- modmat.1
	modmat2[nm2,] <- modmat.2
	#---------------
	colnames(modmat1) <- colnames(modmat.1)
	colnames(modmat2) <- colnames(modmat.2)
	# remove column of modmat1 for overall median
	# remove columns of modmat2 with state=trait
	# rename remaining columns
	#-- the medians
	rc <- which(as.numeric(substr(colnames(modmat1),13,13))==median(data[,x],na.rm=T))
	modmat1 <- modmat1[,-rc]
	colnames(modmat1) <- paste0("med",substr(colnames(modmat1),13,13))
	#----------------
	# deviance matrix
	colnames(modmat2) <- paste0(substr(colnames(modmat2),18,18),".",
	                            substr(colnames(modmat2),37,37))
	
	rcs0 <- NULL
	for (i in 1:7){
	  tmp <- which(colnames(modmat2)== paste0(i,".",i))
	  rcs0 <- c(rcs0,tmp)
	}
	cs <- apply(modmat2, 2, function(x) sum(x,na.rm=T))
	rcs1 <- which(cs==0)
	#------------------
	modmat2 <- modmat2[,-c(rcs0,rcs1)]
	modmat2 <- modmat2[,sort(colnames(modmat2))]
	#-----------------
	list(modmat1, modmat2)
}
