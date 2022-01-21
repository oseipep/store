# Run the simulations and store the results:
require(store)
source("sim-funcs.R")

simfolder = "../data/"

# Setting 1: p=1, the default
# Caution: it takes several hours
Set1 <- multisim(nsim=100, nsubs=400)

saveRDS(Set1, paste0(simfolder, "Set1.rds"))

# Setting 2: p=1/3
# Caution: it takes several hours
Set2 <- multisim(nsim=100, nsubs=400, p=1/3)

saveRDS(Set2, paste0(simfolder,"Set2.rds"))
