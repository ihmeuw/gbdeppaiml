## ---------------------------
## Script name: 
## Purpose of script:
##
## Author: Maggie Walters
## Date Created: 2018-04-11
## Email: mwalte10@uw.edu
## ---------------------------
##
## Notes:
##   
##
## ---------------------------

## Used in basically every script
Sys.umask(mode = "0002")
windows <- Sys.info()[1][["sysname"]]=="Windows"
root <- ifelse(windows,"J:/","/home/j/")
user <- ifelse(windows, Sys.getenv("USERNAME"), Sys.getenv("USER"))
eppasm_dir <- paste0(ifelse(windows, "H:", paste0("/ihme/homes/", user)), "/eppasm/")
setwd(eppasm_dir)
devtools::load_all()
gbdeppaiml_dir <- paste0(ifelse(windows, "H:", paste0("/ihme/homes/", user)), "/gbdeppaiml/")
setwd(gbdeppaiml_dir)
devtools::load_all()

run.name = '201012_ancrt'


params.rt <- list()
for(loc in c('AGO', 'BDI', 'BFA', 'BWA', 'CAF', 'CMR')){
  params.rt[[loc]] <- readRDS(paste0('/ihme/hiv/epp_output/gbd20/201012_ancrt/fit/',loc, '.RDS'))
}


params.og <- list()
for(loc in c('AGO', 'BDI', 'BFA', 'BWA', 'CAF', 'CMR')){
  params.og[[loc]] <- readRDS(paste0('/ihme/hiv/epp_output/gbd20/200713_yuka/fit/',loc, '.RDS'))
}

for(loc in c('AGO', 'BDI', 'BFA', 'BWA', 'CAF', 'CMR') ){
  dt <- rbind(data.table(melt(params.og[[loc]]$`loc`$logincrr))[,run := 'yuka'],
              data.table(melt(params.rt[[loc]]$logincrr))[,run := 'ancrt'])
}


####
#we want to know the one's on ancbias, where we insert the prior on (0.15, 1)
##at the bottom of gbd data sub