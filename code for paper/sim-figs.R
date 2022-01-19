# Define simfolder (where simulation results stored)
# and figfolder (where figures will be stored)

require(ggplot2)
require(latex2exp)

simfolder = "../data/"
figfolder = "../output/sim-plots/"

# Figure 2 in manuscript:
# comparing the AICs
sims = readRDS(simfolder, "AICSet1vsSet2.rds")
hh1 <- sims$sett1; hh2 <- sims$sett2
AIC = c(hh1[,1],hh2[,1],hh1[,2],hh2[,2],hh1[,3],hh2[,3])
method = rep(c("linear","penalized","unpenalized"),each=200)
setting = rep(c("Setting 1","Setting 2","Setting 1", 
                "Setting 2","Setting 1","Setting 2"), 
              each=100)
sim = rep(1:100,6)
df = data.frame(method=method,sim=sim,AIC=AIC,setting=setting)
df$method = factor(df$method,levels = c("linear","penalized","unpenalized"))
df$setting = factor(df$setting,levels= c("Setting 1","Setting 2"))

# Figure 2:
pdf(paste0(figfolder,"ggplot-sett1vssett2.pdf"),10)
ggplot(df) + geom_line(aes(method,AIC,group=sim)) + facet_wrap(df$setting) +
  theme_grey(base_size = 18)
dev.off()


# Making Figures 3 and 4 of the manuscript:
# Boxplots to compare unpenalized and penalized models
# under settings 1 and 2 (i.e. p=1, p=1/3)
Set1 <- readRDS(paste0(simfolder, "CoefSet1.rds"))
Set2 <- readRDS(paste0(simfolder, "CoefSet2.rds"))

# Compute MSE for setting 1:
ucoef1 <- Set1$ucoef; pcoef1  <- Set1$pcoef
nams <- c(paste0("i",substr(colnames(ucoef1)[1],7,10)),
          substr(colnames(ucoef1)[2:7],6,9),
          paste0("dev",substr(colnames(ucoef1)[8:31],6,8)))
colnames(ucoef1) <- colnames(pcoef1) <- nams
frs <- function(r,s,p=1) 
  0.17-0.15*sign(r-4)*abs(r-4)^p-0.72*sign(s-r)*abs(s-r)^p
upmse <- NULL
for (i in 1:length(nams)){
  if(substr(nams[i],1,3)=="ice") {
    tv <- frs(4,4)
    umse <- mean((ucoef1[,nams[i]]-tv)^2)
    pmse <- mean((pcoef1[,nams[i]]-tv)^2)
  }
  if (substr(nams[i],1,3)=="med"){
    m <- as.numeric(substr(nams[i],4,4))
    tv <- frs(r=m,s=m)-frs(4,4)
    umse <- mean((ucoef1[,nams[i]]-tv)^2)
    pmse <- mean((pcoef1[,nams[i]]-tv)^2)
  }
  if (substr(nams[i],1,3)=="dev"){
    m1 <- as.numeric(substr(nams[i],4,4))
    m2 <- as.numeric(substr(nams[i],6,6))
    tv <- frs(r=m1,s=m2)-frs(r=m1,s=m1)
    umse <- mean((ucoef1[,nams[i]]-tv)^2)
    pmse <- mean((pcoef1[,nams[i]]-tv)^2)
  }
  #-------------
  upmse <- rbind(upmse,c(umse,pmse))
  #--------------
}
colnames(upmse) <- c("mse-unpen","mse-pen")
mseSet1 <- cbind(upmse,apply(upmse, 1, function(v) 100*(v[2]-v[1])/v[1]))

# Compute MSE for setting 2:
ucoef2 <- Set2$ucoef; pcoef2 <- Set2$pcoef
colnames(ucoef2) <- colnames(pcoef2) <- nams
frs <- function(r,s,p=1/3) 
  0.17-0.15*sign(r-4)*abs(r-4)^p-0.72*sign(s-r)*abs(s-r)^p
upmse2 <- NULL
for (i in 1:length(nams)){
  if(substr(nams[i],1,3)=="ice") {
    tv <- frs(4,4)
    umse <- mean((ucoef2[,nams[i]]-tv)^2)
    pmse <- mean((pcoef2[,nams[i]]-tv)^2)
  }
  if (substr(nams[i],1,3)=="med"){
    m <- as.numeric(substr(nams[i],4,4))
    tv <- frs(r=m,s=m)-frs(4,4)
    umse <- mean((ucoef2[,nams[i]]-tv)^2)
    pmse <- mean((pcoef2[,nams[i]]-tv)^2)
  }
  if (substr(nams[i],1,3)=="dev"){
    m1 <- as.numeric(substr(nams[i],4,4))
    m2 <- as.numeric(substr(nams[i],6,6))
    tv <- frs(r=m1,s=m2)-frs(r=m1,s=m1)
    umse <- mean((ucoef2[,nams[i]]-tv)^2)
    pmse <- mean((pcoef2[,nams[i]]-tv)^2)
  }
  #-------------
  upmse2 <- rbind(upmse2,c(umse,pmse))
  #--------------
}
colnames(upmse2) <- c("mse-unpen","mse-pen")
mseSet2 <- cbind(upmse2,apply(upmse2, 1, function(v) 100*(v[2]-v[1])/v[1]))
# the boxplots:
as.numeric(substr(nams[2:7],4,4))->c1
as.numeric(substr(nams[8:31],4,4))->c2
as.numeric(substr(nams[8:31],6,6))->c3
paste0("$\\tau_",c1,"$")->cc1
paste0("$\\gamma_{",c2,", ",c3,"}")->cc2
paste0("$\\alpha$")->cc
nn <- c(cc,cc1,cc2)
#-----------------

# Figure 3 in manuscript:
pdf(paste0(figfolder, "sim-Set1.pdf"), 14)
frs <- function(r,s,p=1) 
  0.17-0.15*sign(r-4)*abs(r-4)^p-0.72*sign(s-r)*abs(s-r)^p
par(mar=c(0.5,4,1.0,1.0),mfrow=c(2,1))
boxplot(ucoef1,yaxt="n",xaxt="n",outpch=20,whisklty=1,staplelty=0)
abline(h=seq(-3,3,by=0.25),v=seq(-0.5,32.5,by=.5),col="lightgray",lty=1,lwd=0.4)
abline(v=c(1.5,7.5,8.5,11.5,16.5,22.5,27.5,30.5),lty=1,lwd=1.5,col="steelblue")
boxplot(ucoef1,add=T,yaxt="n",xaxt="n",outpch=20,whisklty=1,staplelty=0)
axis(2,at=seq(-3.0,3.0,by=1),las=1)
aa <-0.5
for (i in 1:length(nams)){
  if(substr(nams[i],1,3)=="ice") tv <- frs(4,4)
  if (substr(nams[i],1,3)=="med"){
    m <- as.numeric(substr(nams[i],4,4))
    tv <- frs(r=m,s=m)-frs(4,4)
  }
  if (substr(nams[i],1,3)=="dev"){
    m1 <- as.numeric(substr(nams[i],4,4))
    m2 <- as.numeric(substr(nams[i],6,6))
    tv <- frs(r=m1,s=m2)-frs(r=m1,s=m1)
  }
  xx <- seq(aa,i+1/2,by=0.5)
  yy <- rep(tv,length(xx))
  lines(xx,yy,col="gray46",lty=2,lwd=3.5)
  #----------
  aa <- i+1/2
}
title(ylab=TeX("Unpenalized"),line=2.2)
#-- penalized
par(mar=c(4,4,0.5,1.0))
mset1 <- as.character(round(mseSet1[,3]))
#---------------
boxplot(pcoef1,yaxt="n",xaxt="n",outpch=20,whisklty=1,staplelty=0)
abline(h=seq(-2.5,2.5,by=0.25),v=seq(-0.5,32.5,by=.5),col="lightgray",lty=1,lwd=0.4)
abline(v=c(1.5,7.5,8.5,11.5,16.5,22.5,27.5,30.5),lty=1,lwd=1.5,col="steelblue")
boxplot(pcoef1,add=T,yaxt="n",xaxt="n",outpch=20,whisklty=1,staplelty=0)
axis(1,at=seq(1,31,by=1),labels = TeX(nn),las=2)
axis(2,at=seq(-2.0,2.0,by=1),las=1)
aa <-0.5
for (i in 1:length(nams)){
  if(substr(nams[i],1,3)=="ice") tv <- frs(4,4)
  if (substr(nams[i],1,3)=="med"){
    m <- as.numeric(substr(nams[i],4,4))
    tv <- frs(r=m,s=m)-frs(4,4)
  }
  if (substr(nams[i],1,3)=="dev"){
    m1 <- as.numeric(substr(nams[i],4,4))
    m2 <- as.numeric(substr(nams[i],6,6))
    tv <- frs(r=m1,s=m2)-frs(r=m1,s=m1)
  }
  xx <- seq(aa,i+1/2,by=0.5)
  yy <- rep(tv,length(xx))
  lines(xx,yy,col="gray46",lty=2,lwd=3.5)
  #----------
  aa <- i+1/2
}
mtext(TeX(mset1),side=1,line=2.4,at=seq(1,31,by=1),cex=1.0)
title(ylab=TeX("Penalized"),line=2.2)
dev.off()
#-----

# Figure 4 in manuscript:
pdf(paste0(figfolder, "sim-Set2.pdf"), 14)
frs <- function(r,s,p=1/3) 
  0.17-0.15*sign(r-4)*abs(r-4)^p-0.72*sign(s-r)*abs(s-r)^p
par(mar=c(0.5,4,1.0,1.0),mfrow=c(2,1))
boxplot(ucoef2,yaxt="n",xaxt="n",outpch=20,whisklty=1,staplelty=0)
abline(h=seq(-1.75,1.625,by=0.125),v=seq(-0.5,32.5,by=.5),col="lightgray",lty=1,lwd=0.4)
abline(v=c(1.5,7.5,8.5,11.5,16.5,22.5,27.5,30.5),lty=1,lwd=1.5,col="steelblue")
boxplot(ucoef2,add=T,yaxt="n",xaxt="n",outpch=20,whisklty=1,staplelty=0)
axis(2,at=seq(-1.5,1.5,by=1),las=1)
aa <-0.5
for (i in 1:length(nams)){
  if(substr(nams[i],1,3)=="ice") tv <- frs(4,4)
  if (substr(nams[i],1,3)=="med"){
    m <- as.numeric(substr(nams[i],4,4))
    tv <- frs(r=m,s=m)-frs(4,4)
  }
  if (substr(nams[i],1,3)=="dev"){
    m1 <- as.numeric(substr(nams[i],4,4))
    m2 <- as.numeric(substr(nams[i],6,6))
    tv <- frs(r=m1,s=m2)-frs(r=m1,s=m1)
  }
  xx <- seq(aa,i+1/2,by=0.5)
  yy <- rep(tv,length(xx))
  lines(xx,yy,col="gray46",lty=2,lwd=3.5)
  #----------
  aa <- i+1/2
}
title(ylab=TeX("Unpenalized"),line=2.2)
#-- penalized
par(mar=c(4,4,0.5,1.0))
mset2 <- as.character(round(mseSet2[,3]))
#---------------
boxplot(pcoef2,yaxt="n",xaxt="n",outpch=20,whisklty=1,staplelty=0)
abline(h=seq(-1.75,1.625,by=0.125),v=seq(-0.5,32.5,by=.5),col="lightgray",lty=1,lwd=0.4)
abline(v=c(1.5,7.5,8.5,11.5,16.5,22.5,27.5,30.5),lty=1,lwd=1.5,col="steelblue")
boxplot(pcoef2,add=T,yaxt="n",xaxt="n",outpch=20,whisklty=1,staplelty=0)
axis(1,at=seq(1,31,by=1),labels = TeX(nn),las=2)
axis(2,at=seq(-1.5,1.5,by=1),las=1)
aa <-0.5
for (i in 1:length(nams)){
  if(substr(nams[i],1,3)=="ice") tv <- frs(4,4)
  if (substr(nams[i],1,3)=="med"){
    m <- as.numeric(substr(nams[i],4,4))
    tv <- frs(r=m,s=m)-frs(4,4)
  }
  if (substr(nams[i],1,3)=="dev"){
    m1 <- as.numeric(substr(nams[i],4,4))
    m2 <- as.numeric(substr(nams[i],6,6))
    tv <- frs(r=m1,s=m2)-frs(r=m1,s=m1)
  }
  xx <- seq(aa,i+1/2,by=0.5)
  yy <- rep(tv,length(xx))
  lines(xx,yy,col="gray46",lty=2,lwd=3.5)
  #----------
  aa <- i+1/2
}
mtext(TeX(mset2),side=1,line=2.4,at=seq(1,31,by=1),cex=1.0)
title(ylab=TeX("Penalized"),line=2.2)
dev.off()
#-----

# Making Figure 5 of the manuscript:
# comparing the degrees of freedom of the two settings:
dfsims <- readRDS(paste0(simfolder,"DfreedomSet1vsSet2.rds"))
df1 <- dfsims$sett1
df2 <- dfsims$sett2

# Figure 5 in manuscript:
pdf(paste0(figfolder, "edf-histo.pdf"), 8)
maxrange <- range(df1[,2],df2[,2])
breaks <- seq(maxrange[1], maxrange[2], length.out=20)
h1 <- hist(df1[,2],breaks=breaks,plot=F)
h2 <- hist(df2[,2],breaks=breaks,plot=F)
plot( h1, col="lightgray", xlim=c(3,25),ylab="",xlab="",main="",axes=F)  
plot( h2, col="steelblue", xlim=c(3,25), add=T,axes=F) 
abline(v=3,lty=2,lwd=2.5)
axis(1, at=c(0,3,seq(5,25,by=5)), labels=c(0,3,seq(5,25,by=5)))
axis(2, at=seq(0,70,by=10), labels=seq(0,70,by=10),las=1)
box()
title(xlab=TeX("Effective degrees of freedom"), ylab=TeX("Counts"), line=2.4)
dev.off()
#-------------
##################




