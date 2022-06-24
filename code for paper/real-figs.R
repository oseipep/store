# Producing the tables and figures in the manuscript
require(store)
require(latex2exp)
source("utils.R")
modelfolder <- "../data/real-store/"  # where model fit objects were previously stored
figfolder <- "../output/store-plots/" # where to put figures

# Table 1 in manuscript: 
# Chi square test of the cross Tabulation
# H0: Pleasant is independent of stressed; H1: They are dependent
chisq.test(table(thoughts$stressed,thoughts$pleasant))
# X2(36,N=6516) --> p < 2.2e-16
# But this ignores repeated measures structure of the data

#########################################

# Figure 1: Pleasant thoughts on stress 
# unpenalized and penalized of pleasant on stress store objects
pleas <- readRDS(paste0(modelfolder, "pleas.rds"))

pstu <- pleas$stres$unpen
pstp <- pleas$stres$pen

# Figure 1: row 1
pdf(paste0(figfolder,"store-pd1.pdf"),8)
par(family="Times")
plot(pstu,type="coef",xlab="",ylab="State",title="Unpenalized",
           cex.lab=2,cex.main=2,lineheight=1.1,
           legend.width=4.2,legend.mar=9.1,lwd=1.8)
dev.off()

pdf(paste0(figfolder,"store-pd2.pdf"),8)
par(family="Times")
plot(pstp,type="coef",xlab="",ylab="State",title="Penalized",
           cex.lab=2,cex.main=2,lineheight=1.1,legend.width=4.2,
           legend.mar=9.1,lwd=1.8)
dev.off()

# Figure 1: row 2
pdf(paste0(figfolder,"store-pd3.pdf"),10)
par(family="Times")
plot(pstu,type="coef",bubble=T,title="",xlab="Trait",ylab="State")
dev.off()

pdf(paste0(figfolder,"store-pd4.pdf"),10)
par(family="Times")
plot(pstp,type="coef",bubble=T,title="",xlab="Trait",ylab="State")
dev.off()

#########################################

# Figure 6 in manuscript
# Read store fits objects for the 5 variables from source:
vbls <- names(thoughts)[4:8]
for (ll in 1:5) {
  tmp <- readRDS(paste0(modelfolder,substr(vbls[ll],1,5),".rds"))
  res <- list()
  rvbls <- vbls[-ll]
  for (i in 1:4) res[[i]] <- tmp[[i]]$pen
  names(res) <- substr(rvbls,1,5)
  assign(paste0(substr(vbls[ll],1,5)),res)
}

objL <- list(pleas,absor,satis,exhau,stres)
# Plot 1x5 grid for each ordinal response variable:
for (ll in 1:5){
  xx <- substr(vbls[ll],1,5)
  if ( (xx=="stres") || (xx=="pleasant") ){
    pdf(paste0(figfolder,"taugamagrid-",xx,".pdf"),
        width = 36,height = 9.45)
  } else{
    pdf(paste0(figfolder,"taugamagrid-",xx,".pdf"),
        width = 36,height = 7.35)}
  print(taugamaGrid(objL[[ll]],vbls))
  dev.off()
}

#########################################

# Figure 7 in manuscript
hh <- NULL
for (ll in 1:5) {
  tmp1 <- readRDS(paste0(modelfolder,"med", substr(vbls[ll],1,5), ".rds"))
  tmp2 <- readRDS(paste0(modelfolder,substr(vbls[ll],1,5), ".rds"))
  xx <- cbind(sapply(tmp1, function(v) v$pen$R2),
              sapply(tmp2, function(v) v$pen$R2))
  hh <- rbind(hh, xx)
}
colnames(hh) <- c("trait","trait+state")

# plotting the deviance explained using R^2
pdf(paste0(figfolder,"deviance-pd1.pdf"),8)
plot(hh[,2],hh[,1],ylim=range(hh),xlim=range(hh),type="n",ylab="",xlab="",
     las=1)
abline(h=seq(0,.7,by=.02),v=seq(0,.7,by=.02),col="lightgray",lty=1,lwd=0.3)
abline(a=0,b=1,col="red",lty=1,lwd=1.5)
abline(a=0,b=.9,col="gray46",lty=2,lwd=1.5) # y = 0.9x
abline(a=0,b=.7,col="gray46",lty=2,lwd=1.5) # y = 0.7x
abline(a=0,b=.5,col="gray46",lty=2,lwd=1.5) # y = 0.5x
for (i in 1:nrow(hh)) {
  points(hh[i,2],hh[i,1],pch=16,lwd=0.8)
}
#-- add labels
# Pleasant on all others
text(0.528,0.275,labels="P~Sa",cex=.5)
text(0.383,0.169,labels="P~St",cex=.5)
text(0.258,0.135,labels="P~E",cex=.5)
text(0.162,0.1095,labels="P~A",cex=.5)

# Absorbed on all others
text(0.1518,0.058,labels="A~Sa",cex=.5)
text(0.182,0.132,labels="A~P",cex=.5)
text(0.062,0.064,labels="A~E",cex=.5)
text(0.045,0.046,labels="A~St",cex=.5)

# Exhausted on all others
text(0.695,0.56,labels="E~St",cex=.5)
text(0.215,0.144,labels="E~P",cex=.5)
text(0.2095,0.109,labels="E~Sa",cex=.5)
text(0.0275,0.012,labels="E~A",cex=.5)

# Satisfied on all others
text(0.3922,0.325,labels="Sa~P",cex=.5)
text(0.2522,0.210,labels="Sa~St",cex=.5)
text(0.095,0.074,labels="Sa~A",cex=.5)
text(0.146,0.135,labels="Sa~E",cex=.5)

# stressed on all others
text(0.6685,0.524,labels="St~E",cex=.5)
text(0.272,0.154,labels="St~Sa",cex=.5)
text(0.288,0.195,labels="St~P",cex=.5)
text(0.012,0.042,labels="St~A",cex=.5)
text(0.62,0.32,labels = "0.5",cex=0.8)
text(0.62,0.446,labels = "0.7",cex=0.8)
text(0.62,0.570,labels = "0.9",cex=0.8)

title(xlab=TeX("Trait + state model"),line=2.2)
title(ylab=TeX("Trait-only model"),line=2.4)
dev.off()





