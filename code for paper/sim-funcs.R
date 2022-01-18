# simulation study
# Based on varying powers: p = 1; 1/3
onesim <- function(nsub, nrep=15, betas=c(.17, -.15, -.72), p=1, method="mgcv"){
  mm <- round(c(16,46,85,106,85,46,16) * nsub / 400)
  mm[4] <- mm[4] + nsub - sum(mm)
  cmm <- cumsum(mm)
  nobs <- nsub*nrep
  dat <- data.frame(Subject=factor(rep(1:nsub,each=nrep)), 
                    sim.x=rep(NA,nobs), sim.y=rep(NA,nobs))
  # generate x-values for 15 observations per person
  for (k in 1:cmm[1]) dat$sim.x[dat$Subject==k] <- rep(1:2, c(9,6)) 
  for (k in (cmm[1]+1):cmm[2]) dat$sim.x[dat$Subject==k] <- rep(1:4, c(5,5,3,2))
  for (k in (cmm[2]+1):cmm[3]) dat$sim.x[dat$Subject==k] <- rep(1:6, c(3,3,3,3,2,1))
  for (k in (cmm[3]+1):cmm[4]) dat$sim.x[dat$Subject==k] <- rep(1:7, c(1,2,3,3,3,2,1))
  for (k in (cmm[4]+1):cmm[5]) dat$sim.x[dat$Subject==k] <- rep(2:7, c(1,2,3,3,3,3))
  for (k in (cmm[5]+1):cmm[6]) dat$sim.x[dat$Subject==k] <- rep(4:7, c(2,3,5,5))
  for (k in (cmm[6]+1):cmm[7]) dat$sim.x[dat$Subject==k] <- rep(6:7, c(6,9))
  dat <- store:::addmed(dat,"sim.x",id="Subject")
  dat <- dat
  gmed <- median(dat$sim.x)    # check that it's 4!
  # random effects
  ranef <- rnorm(nsub)
  ran <- ranef
  # general linear predictor function
  linpred <- betas[1] + betas[2]*sign(dat$pmed-gmed)*(abs(dat$pmed-gmed)^p) + 
    betas[3]*sign(dat$sim.x-dat$pmed)*(abs(dat$sim.x-dat$pmed)^p) + 
    ranef[dat$Subject] + rlogis(nobs)
  # cut points
  cutpts <- seq(1,9,length.out = 5)
  # observed y
  dat$sim.y <- 1 + (linpred > -1) + 
    (linpred > cutpts[1]) + 
    (linpred > cutpts[2]) + 
    (linpred > cutpts[3]) + 
    (linpred > cutpts[4]) + (linpred > cutpts[5]) 
  # simple, unpenalize and penalize models
  simpfit <- store::m3p(sim.y~sim.x,dat,id="Subject")
  usimfit <- store::store(sim.y~sim.x, dat, id="Subject", penalize = FALSE ,method=method)
  psimfit <- store::store(sim.y~sim.x, dat, id="Subject", method=method)
  list(simple=simpfit, unpen=usimfit, pen=psimfit)
}

multisim <- function(nsim, nsubs, betas=c(.17, -.15, -.72), p=1, method="mgcv") {
  simatt <- list()
  for (k in 1:nsim){
    tmp <- onesim(nsubs, betas=betas, method=method, p=p)
    simatt[[k]] <- tmp
    cat("run", k,"done\n")
  }
  simatt
}
