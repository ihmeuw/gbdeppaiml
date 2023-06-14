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
  run.name = '230222_dove'
  loc <- "BWA"
  stop.year <- 2050
  j <- 1
  paediatric <- TRUE
  c.scenario = 'reference'
  transition.year = 2021
  gbd.run.name = '200713_yuka'
}else{
  run.name <- args[1]
  j <- as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))
  loc <- args[2]
  stop.year <- as.integer(args[3])
  paediatric <- as.logical(args[4])
  c.scenario <- args[5]
  transition.year <- as.integer(args[6])
  gbd.run.name <- args[7]
  
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
c.args <- run.table[run_name==gbd.run.name]

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

geoadjust  <- FALSE
lbd.anc <- FALSE

# Prepare the dt object ---------------------------------------
dt <- read_spec_object(loc, j, start.year, stop.year, run.name = run.name, trans.params.sub,
                       pop.sub, anc.sub,  prev.sub = prev_sub, art.sub = TRUE,
                       sexincrr.sub = sexincrr.sub,  age.prev = age.prev, paediatric = TRUE,
                       anc.prior.sub = FALSE, lbd.anc = lbd.anc,
                       geoadjust = geoadjust, use_2019 = TRUE,
                       test.sub_prev_granular = test,
                       anc.rt = FALSE, fit.model = FALSE
                       # anc.backcast,
)
###Extends inputs to the projection year as well as does some site specific changes. This should probably be examined by cycle
dt <- modify_dt(dt, run_name = run.name)
attr(dt, 'specfp')$group <- loc.table[ihme_loc_id == loc, group]
## temp fix to get MMR and KHM to run
if(loc %in% c('KHM', 'MMR')){
  attr(dt, 'specfp')$group <- '2'
}

## Sub ART forecast - Set to TRUE for previous ART forecasting methods (age/sex/CD4-specific coverage)
## Set to FALSE for new art initiation method
sub.art.forecast = ifelse(grepl('2', attr(dt, 'specfp')$group), T, F)
attr(dt, 'specfp')$art_pred = sub.art.forecast
## Transmission rate projection - has been tested for group 1A
## Set to FALSE for direct incidence input (same method as Spectrum)
trans.rate.pred <- ifelse(attr(dt, 'specfp')$group == '1A', T, F)

## Substitute forecasting inputs (incidence/transmission rate, adult and child ART, PMTCT)
dt <- forecast.sub(loc, start.year, stop.year, j, dt, run.name, c.scenario, gbd.run.name, sub.art.forecast = sub.art.forecast, trans.rate.pred = trans.rate.pred)

# Set on-ART mortality adjustment to 1 (no adjustment)
new.artrr <- matrix(1, 3, dim(attr(dt,'specfp')$artmx_timerr)[2])
attr(dt, 'specfp')$artmx_timerr <- new.artrr

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
dir.create(paste0('/ihme/hiv/epp_output/', gbdyear, '/', run.name, '/artinit_onart/'), showWarnings = FALSE)
dir.create(paste0('/ihme/hiv/epp_output/', gbdyear, '/', run.name, '/artinit_onart/', loc, '/'), showWarnings = FALSE)
write.csv(artinit, paste0('/ihme/hiv/epp_output/', gbdyear, '/', run.name, '/artinit_onart/', loc,'/', j, '.csv'))
#
# ## under-1 splits
if(paediatric){
  split.dt <- get_under1_splits(pred.result, attr(dt, 'specfp'))
  split.dt[,run_num := j]
  write.csv(split.dt, paste0(out.dir, '/under_1_splits_', j, '.csv' ), row.names = F)
}



