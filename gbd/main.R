## Tahvi Frank
## tahvif@uw.edu/tahvif@gmail.com
### Setup
rm(list=ls())
windows <- Sys.info()[1][["sysname"]]=="Windows"
root <- ifelse(windows,"J:/","/home/j/")
user <- ifelse(windows, Sys.getenv("USERNAME"), Sys.getenv("USER"))
code.dir <- paste0(ifelse(windows, "H:", paste0("/ihme/homes/", user)), "/gbdeppaiml/")
gbdyear <- 'gbd20'
## Packages
library(data.table); library(mvtnorm); library(survey); library(ggplot2); library(plyr); library(dplyr)

## Arguments
args <- commandArgs(trailingOnly = TRUE)
print(args)
if(length(args) > 0) {
  run.name <- args[1]
  loc <- args[2]
  stop.year <- as.integer(args[3])
  j <- as.integer(Sys.getenv("SGE_TASK_ID"))
  paediatric <- as.logical(args[4])
} else {
	run.name <- '191224_trumpet'
	loc <- 'AGO'
	stop.year <- 2022
	j <- 1
	paediatric <- TRUE
}

run.table <- fread(paste0('/share/hiv/epp_input/gbd20/', run.name, '/eppasm_run_table.csv'))
# c.args <- run.table[run_name==run.name]
c.args <- run.table[run_name== run.name]

### Arguments
## Some arguments are likely to stay constant across runs, others we're more likely to test different options.
## The arguments that are more likely to vary are pulled from the eppasm run table
start.year <- 1970
trans.params.sub <- TRUE
pop.sub <- TRUE
art.sub <- TRUE
prev.sub <- TRUE
sexincrr.sub <- TRUE
plot.draw <- FALSE
anc.prior.sub <- TRUE
lbd.anc <- T
geoadjust <- c.args[['anc_sub']]
anc.sub <- c.args[['anc_sub']]
anc.backcast <- c.args[['anc_backcast']]
age.prev <- c.args[['age_prev']]
popadjust <- c.args[['popadjust']]
anc.rt <- c.args[['anc_rt']]
epp.mod <- c.args[['epp_mod']]
### Paths
out.dir <- paste0('/ihme/hiv/epp_output/gbd20/', run.name, "/", loc)

### Functions
library(mortdb, lib = "/home/j/WORK/02_mortality/shared/r")
setwd(paste0(ifelse(windows, "H:", paste0("/ihme/homes/", user)), "/eppasm/"))
devtools::load_all()
setwd(code.dir)
devtools::load_all()

### Tables
loc.table <- fread(paste0('/share/hiv/epp_input/gbd20/', run.name, '/location_table.csv'))



# These locations do not have information from LBD team estimates
# ZAF ANC data are considered nationally representative so no GeoADjust - this could be challenged in the future
no_geo_adj <-  c(loc.table[epp ==1 & grepl("IND",ihme_loc_id),ihme_loc_id],"PNG","HTI","DOM", 'CPV', loc.table[epp ==1 & grepl("ZAF",ihme_loc_id),ihme_loc_id])
# ANC data bias adjustment
if(geoadjust & !loc %in% no_geo_adj){
  geoadjust  <- TRUE
} else {
  geoadjust  <- FALSE
}

if(!loc %in% unlist(strsplit(list.files('/share/hiv/data/PJNZ_EPPASM_prepped_subpop/lbd_anc/'), '.rds'))){
  lbd.anc <- FALSE
}
#debug(read_spec_object)
### Code
## Read in spectrum object, sub in GBD parameters
dt <- read_spec_object(loc, j, start.year, stop.year, trans.params.sub, 
                       pop.sub, anc.sub, anc.backcast, prev.sub = TRUE, art.sub = TRUE, 
                       sexincrr.sub = TRUE,  age.prev = age.prev, paediatric = TRUE, 
                       anc.prior.sub = TRUE, lbd.anc, geoadjust = geoadjust)
#check_inputs(dt)
if(geoadjust){
  attr(dt, 'eppd')$ancsitedat$offset <- attr(dt, 'eppd')$ancsitedat$offset %>% as.numeric()
  
}
dir.create(paste0('/ihme/hiv/epp_output/', gbdyear, "/", run.name, '/dt_objects/'), recursive = T)
saveRDS(dt, file = paste0('/ihme/hiv/epp_output/', gbdyear, "/", run.name, '/dt_objects/', loc, '_dt.RDS'))

if(epp.mod == 'rspline'){attr(dt, 'specfp')$equil.rprior <- TRUE}
#Some substitutions to get things running
if(grepl('NGA', loc)){
  temp <- attr(dt, 'specfp')$paedsurv_artcd4dist
  temp[temp < 0] <- 0
  attr(dt, 'specfp')$paedsurv_artcd4dist <- temp
}
## Replace on-ART mortality RR for TZA and UGA
if(loc %in% c('UGA', 'TZA')){
  temp <- readRDS(paste0('/share/hiv/data/PJNZ_EPPASM_prepped_subpop/MWI.rds'))
  temp.artmxrr <- attr(temp, 'specfp')$artmx_timerr
  attr(dt, 'specfp')$artmx_timerr <- temp.artmxrr
}
if(run.name %in% c("190630_fixonARTIND","190630_fixonARTIND_tightprior")){
  temp <- readRDS(paste0('/share/hiv/data/PJNZ_EPPASM_prepped_subpop/MWI.rds'))
  temp.artmxrr <- attr(temp, 'specfp')$artmx_timerr
  attr(dt, 'specfp')$artmx_timerr <- temp.artmxrr
}
attr(dt, 'eppd')$ancsitedat = unique(attr(dt, 'eppd')$ancsitedat)
## TODO - fix se = 0 data points in ZAF
attr(dt, 'eppd')$hhs <- attr(dt, 'eppd')$hhs[!attr(dt, 'eppd')$hhs$se == 0,]
attr(dt, 'specfp')$relinfectART <- 0.3
if(grepl("IND",loc)){
  if(no_anc){
    attr(dt,"eppd")$ancsitedat <- NULL
  }
  attr(dt, 'specfp')$art_alloc_mxweight <- 0.5
}
## Fit model
fit <- eppasm::fitmod(dt, eppmod = epp.mod, B0 = 1e1, B = 1e3, number_k = 2)
data.path <- paste0('/share/hiv/epp_input/', gbdyear, '/', run.name, '/fit_data/', loc, '.csv')
if(!file.exists(data.path)){save_data(loc, attr(dt, 'eppd'), run.name)}
if(file.exists(data.path)){save_data(loc, attr(dt, 'eppd'), run.name)}

## When fitting, the random-walk based models only simulate through the end of the
## data period. The `extend_projection()` function extends the random walk for r(t)
## through the end of the projection period.
if(epp.mod == 'rhybrid'){
  fit <- extend_projection(fit, proj_years = stop.year - start.year + 1)
}

######extend pmtct_dropout to 2022
if(max(fit$fp$pmtct_dropout$year) < stop.year){
  add_on.year <- seq(max(fit$fp$pmtct_dropout$year) + 1 , stop.year)
  add_on.dropouts <- fit$fp$pmtct_dropout[fit$fp$pmtct_dropout$year == max(fit$fp$pmtct_dropout$year), 2:ncol(fit$fp$pmtct_dropout)]
  fit$fp$pmtct_dropout <- rbind(fit$fp$pmtct_dropout, c(year = unlist(add_on.year), add_on.dropouts))
}
if(dim(fit$fp$artmx_timerr)[2] < fit$fp$SIM_YEARS){
  diff <- dim(fit$fp$artmx_timerr)[2] - fit$fp$SIM_YEARS
  while(diff != 0){
    fit$fp$artmx_timerr <-  abind::abind(fit$fp$artmx_timerr, fit$fp$artmx_timerr[,ncol(fit$fp$artmx_timerr) ])
    diff <- dim(fit$fp$artmx_timerr)[2] - fit$fp$SIM_YEARS

  }
}
if(dim( fit$fp$art15plus_isperc)[2] < fit$fp$SIM_YEARS){
  diff <- dim( fit$fp$art15plus_isperc)[2] - fit$fp$SIM_YEARS
  while(diff != 0){
    fit$fp$art15plus_isperc <-  abind::abind( fit$fp$art15plus_isperc,  fit$fp$art15plus_isperc[,ncol( fit$fp$art15plus_isperc)])
    diff <- dim( fit$fp$art15plus_isperc)[2] - fit$fp$SIM_YEARS

  }
}
if(length( fit$fp$specpop_percelig) < fit$fp$SIM_YEARS){
  diff <- length( fit$fp$specpop_percelig)- fit$fp$SIM_YEARS
  while(diff != 0){
 fit$fp$specpop_percelig <-  abind::abind(  fit$fp$specpop_percelig,  (fit$fp$specpop_percelig)[length( fit$fp$specpop_percelig)])
    diff <- length( fit$fp$specpop_percelig) - fit$fp$SIM_YEARS

  }
}
if(length( fit$fp$pw_artelig) < fit$fp$SIM_YEARS){
  diff <- length( fit$fp$pw_artelig)- fit$fp$SIM_YEARS
  while(diff != 0){
    fit$fp$pw_artelig<-  abind::abind(  fit$fp$pw_artelig,  (fit$fp$pw_artelig)[length( fit$fp$pw_artelig)])
    diff <- length( fit$fp$pw_artelig) - fit$fp$SIM_YEARS

  }
}
if(length( fit$fp$art_dropout) < fit$fp$SIM_YEARS){
  diff <- length( fit$fp$art_dropout)- fit$fp$SIM_YEARS
  while(diff != 0){
    fit$fp$art_dropout<-  abind::abind(  fit$fp$art_dropout,  (fit$fp$art_dropout)[length( fit$fp$art_dropout)])
    diff <- length( fit$fp$art_dropout) - fit$fp$SIM_YEARS

  }
}
if(dim( fit$fp$paedsurv_cd4dist)[3] < fit$fp$SIM_YEARS){
  diff <- dim( fit$fp$paedsurv_cd4dist)[3] - fit$fp$SIM_YEARS
  while(diff != 0){
    fit$fp$paedsurv_cd4dist <-  abind::abind( fit$fp$paedsurv_cd4dist,  fit$fp$paedsurv_cd4dist[,,dim( fit$fp$paedsurv_cd4dist)[3]])
    diff <- dim( fit$fp$paedsurv_cd4dist)[3] - fit$fp$SIM_YEARS

  }
}
if(dim( fit$fp$paedsurv_artcd4dist)[4] < fit$fp$SIM_YEARS){
  diff <- dim( fit$fp$paedsurv_artcd4dist)[4] - fit$fp$SIM_YEARS
  while(diff != 0){
    fit$fp$paedsurv_artcd4dist <-  abind::abind( fit$fp$paedsurv_artcd4dist,  fit$fp$paedsurv_artcd4dist[,,,dim( fit$fp$paedsurv_artcd4dist)[4]])
    diff <- dim( fit$fp$paedsurv_artcd4dist)[4] - fit$fp$SIM_YEARS

  }
}

if(dim( fit$fp$art15plus_num)[2] < fit$fp$SIM_YEARS){
  diff <- dim( fit$fp$art15plus_num)[2] - fit$fp$SIM_YEARS
  while(diff != 0){
    fit$fp$art15plus_num <-  abind::abind( fit$fp$art15plus_num,  fit$fp$art15plus_num[,ncol( fit$fp$art15plus_num) - 1])
    diff <- dim( fit$fp$art15plus_num)[2] - fit$fp$SIM_YEARS

  }
}
if(length( fit$fp$median_cd4init) < fit$fp$SIM_YEARS){
  diff <- length( fit$fp$median_cd4init)- fit$fp$SIM_YEARS
  while(diff != 0){
    fit$fp$median_cd4init<-  abind::abind(  fit$fp$median_cd4init,  (fit$fp$median_cd4init)[length( fit$fp$median_cd4init)])
    diff <- length( fit$fp$median_cd4init) - fit$fp$SIM_YEARS

  }
}
if(length( fit$fp$med_cd4init_input) < fit$fp$SIM_YEARS){
  diff <- length( fit$fp$med_cd4init_input)- fit$fp$SIM_YEARS
  while(diff != 0){
    fit$fp$med_cd4init_input<-  abind::abind(  fit$fp$med_cd4init_input,  (fit$fp$med_cd4init_input)[length( fit$fp$med_cd4init_input)])
    diff <- length( fit$fp$med_cd4init_input) - fit$fp$SIM_YEARS

  }
}
if(length( fit$fp$med_cd4init_cat) < fit$fp$SIM_YEARS){
  diff <- length( fit$fp$med_cd4init_cat)- fit$fp$SIM_YEARS
  while(diff != 0){
    fit$fp$med_cd4init_cat<-  abind::abind(  fit$fp$med_cd4init_cat,  (fit$fp$med_cd4init_cat)[length( fit$fp$med_cd4init_cat)])
    diff <- length( fit$fp$med_cd4init_cat) - fit$fp$SIM_YEARS

  }
}
if(length( fit$fp$verttrans_lag) < fit$fp$SIM_YEARS){
  diff <- length( fit$fp$verttrans_lag)- fit$fp$SIM_YEARS
  while(diff != 0){
    fit$fp$verttrans_lag<-  abind::abind(  fit$fp$verttrans_lag,  (fit$fp$verttrans_lag)[length( fit$fp$verttrans_lag)])
    diff <- length( fit$fp$verttrans_lag) - fit$fp$SIM_YEARS

  }
}

if(length( fit$fp$paedsurv_lag) < fit$fp$SIM_YEARS){
  diff <- length( fit$fp$paedsurv_lag)- fit$fp$SIM_YEARS
  while(diff != 0){
    fit$fp$paedsurv_lag<-  abind::abind(  fit$fp$paedsurv_lag,  (fit$fp$paedsurv_lag)[length( fit$fp$paedsurv_lag)])
    diff <- length( fit$fp$paedsurv_lag) - fit$fp$SIM_YEARS

  }
}

if(length( fit$fp$artcd4elig_idx) < fit$fp$SIM_YEARS){
  diff <- length( fit$fp$artcd4elig_idx)- fit$fp$SIM_YEARS
  while(diff != 0){
    fit$fp$artcd4elig_idx<-  abind::abind(  fit$fp$artcd4elig_idx,  (fit$fp$artcd4elig_idx)[length( fit$fp$artcd4elig_idx)])
    diff <- length( fit$fp$artcd4elig_idx) - fit$fp$SIM_YEARS

  }
}

if(dim( fit$fp$entrantprev)[2] < fit$fp$SIM_YEARS){
  diff <- dim( fit$fp$entrantprev)[2] - fit$fp$SIM_YEARS
  while(diff != 0){
    fit$fp$entrantprev <-  abind::abind( fit$fp$entrantprev,  fit$fp$entrantprev[,ncol( fit$fp$entrantprev) - 1])
    diff <- dim( fit$fp$entrantprev)[2] - fit$fp$SIM_YEARS

  }
}

if(dim( fit$fp$entrantartcov)[2] < fit$fp$SIM_YEARS){
  diff <- dim( fit$fp$entrantartcov)[2] - fit$fp$SIM_YEARS
  while(diff != 0){
    fit$fp$entrantartcov <-  abind::abind( fit$fp$entrantartcov,  fit$fp$entrantartcov[,ncol( fit$fp$entrantartcov) - 1])
    diff <- dim( fit$fp$entrantartcov)[2] - fit$fp$SIM_YEARS

  }
}
## Simulate model for all resamples, choose a random draw, get gbd outputs
#########ensure that all attributes go until the target year
# out.list <- list(
#  fit$fp$artcd4elid_idx,
#
#
#  fit$fp$entrantartcov)
#
# save(out.list, file = paste0('/ihme/homes/mwalte10/out_list_', loc, '.RData'))

result <- gbd_sim_mod(fit, VERSION = 'R')
output.dt <- get_gbd_outputs(result, attr(dt, 'specfp'), paediatric = paediatric)
output.dt[,run_num := j]
## Write output to csv
dir.create(out.dir, showWarnings = FALSE)
write.csv(output.dt, paste0(out.dir, '/', j, '.csv'), row.names = F)
# ## under-1 splits
if(paediatric){
  split.dt <- get_under1_splits(result, attr(dt, 'specfp'))
  split.dt[,run_num := j]
  write.csv(split.dt, paste0(out.dir, '/under_1_splits_', j, '.csv' ), row.names = F)
}
## Write out theta for plotting posterior
param <- data.table(theta = attr(result, 'theta'))
write.csv(param, paste0(out.dir,'/theta_', j, '.csv'), row.names = F)
if(plot.draw){
  plot_15to49_draw(loc, output.dt, attr(dt, 'eppd'), run.name)
}
##END
