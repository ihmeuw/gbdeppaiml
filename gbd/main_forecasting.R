##Set up --------------------------- 
## Script name: main.R
## Purpose of script: Run the EPPASM model
##
## Author: Tahvi Frank
## Date Created: 2022-10-31
## Email: tahvif@uw.edu
## 
rm(list = ls())
Sys.umask(mode = "0002")
windows <- Sys.info()[1][["sysname"]]=="Windows"
root <- ifelse(windows,"J:/","/home/j/")
user <- ifelse(windows, Sys.getenv("USERNAME"), Sys.getenv("USER"))


# Arguments ---------------------------------------
args <- commandArgs(trailingOnly = TRUE)
print(args)
if(length(args) == 0){
  run.name = '221223_bittern'
  loc <- 'BWA'
  stop.year <- 2050
  j <- 1
  paediatric <- TRUE
}else{
  run.name <- args[1]
  j <- as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))
  # j = as.integer(args[5])
  loc <- args[2]
  stop.year <- as.integer(args[3])
  paediatric <- as.logical(args[4])
  
}

print(paste0('J is ', j))


h_root = paste0('/homes/', user, '/')
lib.loc <- paste0(h_root,"R/",R.Version(),"/",R.Version(),".",R.Version())
.libPaths(c(lib.loc,.libPaths()))
packages <- c('fastmatch', 'pkgbuild')
for(p in packages){
  if(p %in% rownames(installed.packages())==FALSE){
    install.packages(p)
  }
  library(p, character.only = T)
}

# library(anclik, lib.loc = "/snfs1/Project/GBD_HIV/packages_r")
library(epp, lib.loc = "/snfs1/Project/GBD_HIV/packages_r")
# anclik_dir <- paste0(ifelse(windows, 'H:', paste0("/ihme/homes/", user)), "/anclik/")
# setwd(anclik_dir)
# devtools::load_all()
# epp_dir <- paste0(ifelse(windows, "H:", paste0("/ihme/homes/", user)), "/epp/")
# setwd(epp_dir)
# devtools::load_all()
eppasm_dir <- paste0(ifelse(windows, "H:", paste0("/ihme/homes/", user)), "/eppasm/")
setwd(eppasm_dir)
pkgbuild::compile_dll(eppasm_dir, debug = FALSE)
devtools::load_all()
gbdeppaiml_dir <- paste0(ifelse(windows, "H:", paste0("/ihme/homes/", user)), "/gbdeppaiml/")
setwd(gbdeppaiml_dir)
devtools::load_all()

gbdyear <- 'gbd20'

run.table <- fread(paste0('/share/hiv/epp_input/gbd20//eppasm_run_table.csv'))
#use the GBD20 final run toggles
c.args <- run.table[run_name=='200713_yuka']

## Many of these toggles aren't used for forecasting since we aren't fitting to data
## However, keeping in the code for now in order to use existing EPP-ASM data prep fxns
geoadjust <- c.args[['geoadjust']]
anc.sub <- c.args[['anc_sub']]
anc.backcast <- c.args[['anc_backcast']]
age.prev <- c.args[['age_prev']]
popadjust <- c.args[['popadjust']]
anc.rt <- c.args[['anc_rt']]
epp.mod <- c.args[['epp_mod']]
geoadjust <- c.args[['anc_sub']]
no_anc <- c.args[['no_anc']]
start.year <- c.args[['start.year']]
trans.params.sub <- c.args[['trans.params.sub']]
pop.sub <- c.args[['pop.sub']]
art.sub <- c.args[['art.sub']]
prev_sub <- c.args[['prev_sub']]
sexincrr.sub <- c.args[['sexincrr.sub']]
plot.draw <- c.args[['plot.draw']]
anc.prior.sub <- c.args[['anc.prior.sub']]
test <- c.args[['test']]
anc.prior.sub <- c.args[['anc.prior.sub']]
prev_sub <- c.args[['prev_sub']]
sexincrr.sub <- c.args[['sexincrr.sub']]

lbd.anc <- T
ped_toggle = TRUE
paediatric = TRUE

file_name = loc
out.dir <- paste0('/ihme/hiv/epp_output/',gbdyear,'/', run.name, "/", file_name)

source(paste0('/ihme/homes/', user, '/gbdeppaiml/gbd/data_prep.R'))
# Location specific toggles ---------------------------------------
# ANC data bias adjustment
##### These locations do not have information from LBD team estimates
##### ZAF ANC data are considered nationally representative so no GeoADjust - this could be challenged in the future
no_geo_adj <-  c(loc.table[epp ==1 & grepl("IND",ihme_loc_id),ihme_loc_id],
                 "PNG","HTI","DOM", 'CPV', loc.table[epp ==1 & grepl("ZAF",ihme_loc_id),ihme_loc_id], 
                 'STP', 'KEN_35626', 'MRT', 'COM')
if(geoadjust & !loc %in% no_geo_adj | loc %in% c('ZWE', 'MWI')){
  geoadjust  <- TRUE
} else {
  geoadjust  <- FALSE
}
print(paste0(loc, ' geoadjust set to ', geoadjust))

# LBD Adjustments
##### Location that don't undergo LBD adjustment, set to TRUE as a default above
if(!loc %in% unlist(strsplit(list.files('/share/hiv/data/PJNZ_EPPASM_prepped_subpop/lbd_anc/2019/'), '.rds')) | loc %in% c('ZAF', 'PNG') | grepl('IND', loc)){
  lbd.anc <- FALSE
}
if(grepl('ancrt', run.name)){
  lbd.anc = F
}

print(paste0(loc, ' lbd.anc set to ', lbd.anc))


# No Sex incrr substitution
if(loc %in% c("MAR","MRT","COM")){
  sexincrr.sub <- FALSE
}
## forecasting temp duct tape


## run multiple draws 
# Prepare the dt object ---------------------------------------
if(stop.year > 2022){
  art = paste0('/share/hiv/spectrum_input/20220621_reference/childARTcoverage/', loc, '.csv')
  pmtct = paste0('/share/hiv/spectrum_input/20220621_reference/PMTCT/', loc, '.csv')
  
}
dt <- read_spec_object(loc, j, start.year, stop.year, run.name = run.name, trans.params.sub,
                       pop.sub, anc.sub,  prev.sub = prev_sub, art.sub = TRUE,
                       sexincrr.sub = sexincrr.sub,  age.prev = age.prev, paediatric = TRUE,
                       anc.prior.sub = FALSE, lbd.anc = lbd.anc,
                       geoadjust = geoadjust, use_2019 = TRUE,
                       test.sub_prev_granular = test,
                       anc.rt = FALSE
                       # anc.backcast,
)
###Extends inputs to the projection year as well as does some site specific changes. This should probably be examined by cycle
dt <- modify_dt(dt, run_name = run.name)
## temp forecasting duct tape
## child ART was projected incorrectly in BWA
if(stop.year > 2022){
  dt <- forecast.sub(loc, start.year, stop.year, j, dt, sub.art.forecast = T)
  new.artrr <- matrix(1, 3, dim(attr(dt,'specfp')$artmx_timerr)[2])
  attr(dt, 'specfp')$artmx_timerr <- new.artrr
}
attr(dt, 'specfp')$art15plus_num <- attr(dt, 'specfp')$art15plus_num[,1:52]
attr(dt, 'specfp')$art15plus_isperc <- attr(dt, 'specfp')$art15plus_isperc[,1:52]

pred.result = simmod.specfp((attr(dt, 'specfp')), VERSION = 'R')
output.dt <- get_gbd_outputs(pred.result, attr(dt, 'specfp'), paediatric = paediatric)
output.dt[,run_num := j]
out.dir <- paste0('/ihme/hiv/epp_output/', gbdyear, '/', run.name, '/', file_name, '/')
dir.create(out.dir, showWarnings = FALSE)
write.csv(output.dt, paste0(out.dir, '/', j, '.csv'), row.names = F)
dir.create(paste0('/ihme/hiv/epp_output/', gbdyear, '/', run.name, '/fit/'), showWarnings = FALSE)
dir.create(paste0('/ihme/hiv/epp_output/', gbdyear, '/', run.name, '/fit/', file_name), showWarnings = FALSE)
saveRDS(pred.result, paste0('/ihme/hiv/epp_output/', gbdyear, '/', run.name, '/fit/', file_name, '/', j, '.RDS'))

artinit = get_artinit(pred.result)
artinit[, draw := j]
dir.create(paste0('/ihme/hiv/epp_output/', gbdyear, '/', run.name, '/artinit/', loc, '/'), showWarnings = FALSE)
write.csv(artinit, paste0('/ihme/hiv/epp_output/', gbdyear, '/', run.name, '/artinit/', loc,'/', j, '.csv'))
#
# ## under-1 splits
if(paediatric){
  split.dt <- get_under1_splits(pred.result, attr(dt, 'specfp'))
  split.dt[,run_num := j]
  write.csv(split.dt, paste0(out.dir, '/under_1_splits_', j, '.csv' ), row.names = F)
}



