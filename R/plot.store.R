#' Plotting for state-trait ordinal regression coefficients and fitted values
#' 
#' Plots coefficients for the various trait-state combinations of a \code{\link{store}} fit.
#' 
#' @param x A model object of class \code{\link{store}}.
#' @param type the type of model output to plot: either \code{"coef"} (the default) to plot coefficients, or \code{"fitted"} to plot fitted values.
#' @param bubble Logical: a bubble plot if \code{TRUE}, otherwise an \code{\link{image}} plot.
#' @param col The colors for the range of the fitted values.
#' @param xlab,ylab x- and y-axis labels.
#' @param title Plot title.
#' @param range Numeric vector of length 2 specifying the minimum and maximum size of the bubbles, if \code{bubble==TRUE}.
#' @param hjust Horizontal justification (in [0,1]).
#' @param lineheight Line height.
#' @param cex.lab Magnification for x and y labels.
#' @param cex.main Magnification for main title.
#' @param legend.width Width in characters of the legend. Default is 2.4.
#' @param legend.mar Width in characters of legend margin that hasthe axis. Default is 6.1.
#' @param lwd Line width of bordering lines around pixels. Default is 1.5
#' @param \dots Other graphical parameters.
#' @return None; a plot is created.
#' @note This function assumes the \code{\link{store}} object includes the underlying data 
#' (\code{save.data==TRUE}, the default in \code{\link{store}}).
#' @author Prince P. Osei and Philip T. Reiss
#' @seealso \code{\link{store}}
#' @references Osei, P. P. and Reiss, P. T. (2022). Ordinal state-trait regression for intensive longitudinal data. Under revision.
#' @importFrom fields image.plot designer.colors
#' @importFrom graphics axis box par title text
#' @importFrom viridisLite cividis
#' @importFrom stats coef median AIC
#' @importFrom ggplot2 ggplot geom_point scale_colour_gradient2 scale_colour_gradientn scale_size 
#' @importFrom ggplot2 theme labs theme_bw element_text aes geom_text
#' @importFrom latex2exp TeX
#' @importFrom stringr str_to_title
#' @export
plot.store <-
  function(x, type=c("coef","fitted"), bubble=FALSE, col=NULL, xlab="State",
           ylab="Person-specific median", title=NULL, range=c(0,25),
           hjust=0.5,lineheight=1.2,cex.lab=1.5,cex.main=1.5,legend.width=2.4,
           legend.mar=6.1,lwd=1.5,...) {
    formula <- x$store.formula
    method <- x$store.method
    data <- x$data
    id <- x$id
    nlevels <- x$nlevels
    y <- all.vars(formula)[1]
    xx <- all.vars(formula)[2]
    if (is.null(title)) {
      z <- ifelse(x$penalize== FALSE,"Unpenalized","Penalized")
      uAIC <- round(AIC(x),2)
      title <- paste0(str_to_title(y), " on ", str_to_title(xx),", ", z, ", AIC = ", uAIC)
    }
    obsize <- coefObservations(formula,data,id)
    if (substr(xx,1,8) == "pleasant") cts <- seq(-3,3,1) else
      cts <- 0:(nlevels-1)
    if (type == "coef") {
      matt <- coefTauGamma(x)
      if (is.null(col)) col <- fields::designer.colors(col=c("red","grey","blue"), x=c(0,-min(matt, na.rm=T)/(max(matt, na.rm=T)-min(matt, na.rm=T)),1))
      if (bubble) {
        trait0 <- rep(cts,rep(nlevels,nlevels))
        state0 <- rep(cts,nlevels)
        trait <- factor(trait0,levels=unique(trait0))
        state <- factor(state0,levels=unique(state0))
        TauGamavals <- as.vector(t(matt))
        ggdat <- data.frame(trait=trait,state=state,TauGamavals=TauGamavals,obsize=obsize[,3])
        mv <- median(ggdat[,"TauGamavals"],na.rm = T)
        qq <- ggplot(ggdat,aes(x = state, y = trait, size = obsize, col = TauGamavals))
        qq <- qq + geom_point() + theme_bw(base_size = range[2],base_family="Times")
        qq <- qq + scale_colour_gradientn(colours=col, na.value="transparent")
        qq <- qq + scale_size(range = range)
        for (i in 1:nlevels) {
          qq <- qq + geom_text(x=i, y=i, label=paste0(cts[i]), colour="black", aes(fontface=2),
                               nudge_x=0.25, nudge_y=0.25, size=8)
        }
        qq <- qq + theme(text = element_text(size=range[2])) 
        qq <- qq + labs(title=title)
        qq <- qq + theme(plot.title=element_text(hjust=hjust, lineheight=lineheight, size=range[2]))
        qq <- qq + xlab(TeX(xlab)) + ylab(TeX(ylab))
        qq <- qq + labs(size=TeX("# Observations"), color=TeX("Coefficients"))
        print(qq)
      } else {
        fields::image.plot(1:nlevels, 1:nlevels, matt[ , 1:nlevels], col=col, 
                   xlab="", ylab="", main="", axes=FALSE,
                   legend.width=legend.width, legend.mar=legend.mar,...)
        graphics::axis(1, at=1:nlevels, labels=cts, cex=1.5)
        graphics::axis(2, at=1:nlevels, labels=cts, cex=1.5, las=1)
        for(i in 1:nlevels) text(i, i, paste0(cts[i]), cex=2.0, col="black", font=2)
        graphics::box(lwd=lwd)
        title(xlab=TeX(xlab), ylab=TeX(ylab), line= 2*lineheight, cex.lab=cex.lab)
        title(main=TeX(title), line=lineheight, cex.main=cex.main)
      }
    }
    if (type == "fitted") { # Fitted value plot: alpha + Tau + Gamma
      matt <- fitalphaTauGamma(x)
      if (is.null(col)) col <- designer.colors(col=c("red","grey","blue"), x=c(0,-min(matt, na.rm=T)/(max(matt, na.rm=T)-min(matt, na.rm=T)),1))
      if (bubble){
        trait0 <- rep(cts,rep(nlevels,nlevels))
        state0 <- rep(cts,nlevels)
        trait <- factor(trait0,levels=unique(trait0))
        state <- factor(state0,levels=unique(state0))
        fitvals <- as.vector(t(matt))
        ggdat <- data.frame(trait=trait,state=state,fitvals=fitvals,obsize=obsize[,3])
        qq <- ggplot(ggdat,aes(x = state, y = trait, size = obsize, col = fitvals))
        qq <- qq + geom_point() + theme_bw(base_size = range[2],base_family = "Times")
        qq <- qq + scale_colour_gradientn(colours=cividis(7), na.value="transparent")
        qq <- qq + scale_size(range = range)
        qq <- qq + theme(text = element_text(size=range[2])) 
        qq <- qq + labs(title=title)
        qq <- qq + theme(plot.title=element_text(hjust = hjust, lineheight=lineheight, size=range[2]))
        qq <- qq + xlab(TeX(xlab)) + ylab(TeX(ylab))
        qq <- qq + labs(color=TeX("Fitted value"), size=TeX("# Observations"))
        print(qq)
      } else {
        fields::image.plot(1:nlevels, 1:nlevels, matt[ , 1:nlevels], col=col, 
                           xlab="", ylab="", main="", axes=FALSE,
                           legend.width=legend.width, legend.mar=legend.mar,...)
        graphics::axis(1, at=1:nlevels, labels=cts, cex=1.5)
        graphics::axis(2, at=1:nlevels, labels=cts, cex=1.5, las=1)
        graphics::box(lwd=lwd)
        title(xlab=TeX(xlab),ylab=TeX(ylab),line = 2*lineheight, cex.lab=cex.lab)
        title(main=TeX(title), line=lineheight, cex.main=cex.main)
      }
    }
  }


