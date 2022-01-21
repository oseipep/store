require(store)
# Loop through the 20
# Run the model
# Store in modelfolder
modelfolder <- "../data/"

# Trait + state model fits:
vbls = names(thoughts)[4:8]
vbls. = names(thoughts)[10:14]
n <- length(vbls)
for (ll in 1:n) {
  tvbls <- vbls.[-ll]
  res <- list()
  for (k in 1:(n-1)) {
    fmla <- as.formula(paste0(vbls[ll], "~", tvbls[k]))
    unpen <- store(fmla, data=thoughts, id="Subject", penalize=FALSE)
    pen <- store(fmla, data=thoughts, id="Subject")
    res[[k]] <- list(unpen=unpen, pen=pen)
  }
  names(res) <- substr(tvbls,1,5)
  saveRDS(assign(paste0(substr(vbls[ll],1,5)),res),
          paste0(modelfolder,substr(vbls[ll],1,5),".rds"))
}

# Trait only model fits:
for (ll in 1:n) {
  tvbls <- vbls.[-ll]
  res <- list()
  for (k in 1:(n-1)) {
    fmla <- as.formula(paste0(vbls[ll], "~", tvbls[k]))
    unpen <- store(fmla, data=thoughts, id="Subject", penalize=FALSE, model="trait")
    pen <- store(fmla, data=thoughts, id="Subject", model="trait")
    res[[k]] <- list(unpen=unpen, pen=pen)
  }
  names(res) <- substr(tvbls,1,5)
  saveRDS(assign(paste0(substr(vbls[ll],1,5)),res),
          paste0(modelfolder, "med", substr(vbls[ll],1,5),".rds"))
}





