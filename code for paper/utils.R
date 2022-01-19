# Functions utilities outside store package:

# A function to produce Figure 1 row 1 and row 2 in manuscript:
plottstore <-
  function(x, type=c("coef","fitted"), bubble=FALSE, col=NULL, xlab="State",
           ylab="Person-specific median", title=NULL, range=c(0,25),
           hjust=0.5,lineheight=1.2,cex.lab=1.5,cex.main=1.5,legend.width=2.4,
           legend.mar=6.1,lwd=1.5,...) {
    require(ggplot2)
    require(fields)
    require(viridisLite)
    require(stringr)
    require(latex2exp)
    formula <- x$store.formula
    method <- x$store.method
    data <- x$data
    id <- x$id
    nlevels <- x$nlevels
    y <- all.vars(formula)[1]
    xx <- all.vars(formula)[2]
    if (is.null(title)) {
      z <- ifelse(x$penalize==FALSE,"Unpenalized","Penalized")
      uAIC <- round(AIC(x),2)
      title <- paste0(str_to_title(y), " on ", str_to_title(xx),", ", z, ", AIC = ", uAIC)
    }
    obsize <- store:::coefObservations(formula,data,id)
    if (type == "coef") {
      matt <- store:::coefTauGamma(x)
      if (is.null(col)) col <- designer.colors(col=c("red","white","blue"), x=c(0,-min(matt, na.rm=T)/(max(matt, na.rm=T)-min(matt, na.rm=T)),1))
      if (bubble) {
        trait0 <- rep(0:(nlevels-1),rep(nlevels,nlevels))
        state0 <- rep(0:(nlevels-1),nlevels)
        trait <- factor(trait0,levels=unique(trait0))
        state <- factor(state0,levels=unique(state0))
        TauGamavals <- as.vector(t(matt))
        ggdat <- data.frame(trait=trait,state=state,TauGamavals=TauGamavals,obsize=obsize[,3])
        mv <- median(ggdat[,"TauGamavals"],na.rm = T)
        qq <- ggplot(ggdat,aes(x = trait, y = state, size = obsize, col = TauGamavals))
        qq <- qq + geom_point() + theme_bw(base_size = range[2],base_family="Times")
        qq <- qq + scale_colour_gradientn(colours=col,na.value="transparent")
        qq <- qq + scale_size(range = range)
        xx <- seq(0,6,by=1)
        for (i in 1:nlevels) {
          qq <- qq + geom_text(x=i,y=i,label=paste0(xx[i]),colour="black",aes(fontface=2),
                               nudge_x = 0.25,nudge_y = 0.25,size=8)
        }
        qq <- qq + theme(text = element_text(size=range[2])) 
        qq <- qq + labs(title=title)
        qq <- qq + theme(plot.title=element_text(hjust = hjust,lineheight=lineheight,size=range[2]))
        qq <- qq + xlab(TeX(xlab)) + ylab(TeX(ylab))
        qq <- qq + labs(size=TeX("# Observations"),color=TeX("Coefficients"))
        print(qq)
      } else {
        image.plot(1:nlevels,1:nlevels, matt[ , 1:nlevels], col=col, 
                   xlab="", ylab="", main="",axes=FALSE,
                   legend.width=legend.width,legend.mar=legend.mar,...)	
        axis(1, at=1:nlevels, labels=seq(0,6,by=1),cex=1.5)
        axis(2, at=1:nlevels, labels=seq(0,6,by=1),cex=1.5,las=1)
        xx <- seq(0,6,by=1)
        for(i in 1:nlevels) text(i,i,paste0(xx[i]),cex=2.0,col="black",font=2)
        box(lwd=lwd)
        title(xlab=TeX(xlab),ylab=TeX(ylab),line = 2*lineheight,cex.lab=cex.lab)
        title(main=TeX(title), line=lineheight,cex.main=cex.main)
      }
    }
    if (type == "fitted") { # Fitted value: alpha + Tau + Gamma
      matt <- store:::fitalphaTauGamma(x)
      if (is.null(col)) col <- designer.colors(col=c("red","white","blue"), x=c(0,-min(matt, na.rm=T)/(max(matt, na.rm=T)-min(matt, na.rm=T)),1))
      if (bubble){
        trait0 <- rep(1:nlevels,rep(nlevels,nlevels))
        state0 <- rep(1:nlevels,nlevels)
        trait <- factor(trait0,levels=unique(trait0))
        state <- factor(state0,levels=unique(state0))
        fitvals <- as.vector(t(matt))
        ggdat <- data.frame(trait=trait,state=state,fitvals=fitvals,obsize=obsize[,3])
        qq <- ggplot(ggdat,aes(x = state, y = trait, size = obsize, col = fitvals))
        qq <- qq + geom_point() + theme_bw(base_size = range[2],base_family = "Times")
        qq <- qq + scale_colour_gradientn(colours=cividis(7),na.value="transparent")
        qq <- qq + scale_size(range = range)
        qq <- qq + theme(text = element_text(size=range[2])) 
        qq <- qq + labs(title=title)
        qq <- qq + theme(plot.title=element_text(hjust = hjust,lineheight=lineheight,size=range[2]))
        qq <- qq + xlab(TeX(xlab)) + ylab(TeX(ylab))
        qq <- qq + labs(color=TeX("Fitted value"),size=TeX("# Observations"))
        qq
      } else {
        image.plot(1:nlevels,1:nlevels, t(matt)[ , 1:nlevels], col=col, 
                   xlab="", ylab="", main="",axes=FALSE,
                   legend.width=legend.width,legend.mar=legend.mar,...)	
        axis(1, at=1:nlevels, labels=1:nlevels)
        axis(2, at=1:nlevels, labels=1:nlevels, las=1)
        box(lwd=lwd)
        title(xlab=TeX(xlab),ylab=TeX(ylab),line = 2*lineheight,cex.lab=cex.lab)
        title(main=TeX(title), line=lineheight,cex.main=cex.main)
      }
    }
  }
#--------------------


# A function to produce the 1x5 grid of store objects
# organize the data into ggplot data format:
taugamaGendat <- function(objList){
  # objList is the store objects for a response variable y
  # e.g., pleasant on all others
  lv <- length(objList)
  y <- all.vars(objList[[1]]$store.formula)[1]
  x <- all.vars(objList[[1]]$store.formula)[2]
  nlevels <- objList[[1]]$nlevels
  data <- objList[[1]]$data
  id <- objList[[1]]$id
  #--------------
  trait0 <- rep(1:nlevels,rep(nlevels,nlevels))
  state0 <- rep(1:nlevels,nlevels)
  trait <- factor(trait0,levels=unique(trait0))
  state <- factor(state0,levels=unique(state0))
  #- create NA's for same variable on itself:
  sv <- data.frame(trait=trait,state=state,taugamavals=rep(NA,length(trait)),
                   obsize=rep(NA,length(trait)),mdpointval=rep(NA,length(trait)))
  sv$predictor <- rep(paste0("x = ",str_to_title(y)),nrow(sv))
  #-------------
  bigDF <- sv
  for (l in 1:lv) {
    formula <- objList[[l]]$store.formula
    xx <- all.vars(formula)[2]
    x <- ifelse((xx=="satisfied.")||(xx=="exhausted."),substr(xx,1,9),substr(xx,1,8))
    matt <- store:::coefTauGamma(objList[[l]])
    # standardized the Tau and Gamma values:
    smatt <- matt/sqrt(var(predict(objList[[l]]))+1) # standardized
    obsize <- store:::coefObservations(formula,data,id)
    taugamavals <- as.vector(t(smatt)) 
    mv <- rep(median(taugamavals,na.rm=T),length(taugamavals))
    tmp <- data.frame(trait=trait,state=state,taugamavals=taugamavals,
                      obsize=obsize[,3],mdpointval=mv)
    tmp$predictor <- rep(paste0("x = ",str_to_title(x)),nrow(tmp))
    bigDF <- rbind(bigDF,tmp)
  }
  bigDF$response <- gl(1,nrow(bigDF),labels=paste0("y = ",str_to_title(y)))
  bigDF
}
#-----

# produce the 1x5 grid of plots:
taugamaGrid<- function(objList,vbls){
  nlevels <- objList[[1]]$nlevels
  y <- all.vars(objList[[1]]$store.formula)[1]
  require(stringr)
  DF <- taugamaGendat(objList)
  DF$predictor <- factor(DF$predictor,levels=paste0("x = ", str_to_title(vbls)))
  require(ggplot2)
  require(latex2exp)
  qq <- ggplot(DF,aes(x=trait,y=state,col=taugamavals,size=obsize))
  qq <- qq + geom_point() + facet_grid(response ~ predictor)
  qq <- qq + theme_bw(base_size=25,base_family="serif")
  qq <- qq + scale_colour_gradient2(low="red",mid="white",high="blue",
                                    na.value="transparent")
  qq <- qq + scale_size(range = c(0,30))
  xx1 <- seq(0,6,by=1)
  xx2 <- seq(-3,3,by=1)
  gg <- which(DF[,"predictor"]== "x = Pleasant")
  for (i in 1:nlevels){
    qq <- qq + geom_text(data=DF[-c(1:49,gg),],x=i,y=i,label=paste0(xx1[i]),
                         nudge_y = 0.25,size=8,colour="black")}
  if (y!="pleasant"){
    for (i in 1:nlevels){
      qq <- qq + geom_text(data=DF[gg,],x=i,y=i,label=paste0(xx2[i]),
                           nudge_y = 0.25,size=8,colour="black")}
  }
  qq <- qq + theme(strip.text.x = element_text(size=40),
                   strip.text.y = element_text(size=40),
                   axis.title.x = element_text(size=40),
                   axis.title.y = element_text(size=40),
                   legend.spacing = unit(0.01,"lines"))
  qq <- qq + xlab(TeX("Trait")) + ylab(TeX("State")) 
  qq <- qq + labs(color=TeX("Coefficients"),size=TeX("# Observations"))
  if(y=="pleasant"){
    qq <- qq + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())
    qq <- qq + ylab(TeX("State")) + xlab("")
    qq <- qq + theme(legend.position = "none")
  }
  if((y!="pleasant")&&(y!="stressed")){
    qq <- qq + theme(strip.text.x=element_blank())
    qq <- qq + ylab(TeX("State")) + xlab("")
    qq <- qq + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())
    qq <- qq + theme(legend.position = "none")
  }
  if(y=="stressed"){
    qq <- qq + theme(strip.text.x=element_blank())
    qq <- qq + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())
    qq <- qq + theme(legend.position = "bottom")
    qq <- qq + theme(legend.key.width = unit(1.0,"cm"),legend.key.size = unit(1.8, "cm"))
    qq <- qq + theme(legend.title = element_text(size = 30))
  }
  qq <- qq + theme(axis.text.y=element_blank(), axis.ticks.y=element_blank())
  qq
}
#----------------------









