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
	run.name <- '200119_ukelele'
	loc <- 'STP'
	stop.year <- 2022
	j <- 1
	paediatric <- TRUE
}

run.table <- fread(paste0('/share/hiv/epp_input/gbd20//eppasm_run_table.csv'))
c.args <- run.table[run_name==run.name]


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
geoadjust <- c.args[['geoadjust']]
anc.sub <- c.args[['anc_sub']]
anc.backcast <- c.args[['anc_backcast']]
age.prev <- c.args[['age_prev']]
popadjust <- c.args[['popadjust']]
anc.rt <- c.args[['anc_rt']]
epp.mod <- c.args[['epp_mod']]
geoadjust <- c.args[['anc_sub']]
no_anc <- c.args[['no_anc']]
anc.prior.sub <- TRUE

### Paths
out.dir <- paste0('/ihme/hiv/epp_output/',gbdyear,'/', run.name, "/", loc)

### Functions
library(mortdb, lib = "/home/j/WORK/02_mortality/shared/r")
setwd(paste0(ifelse(windows, "H:", paste0("/ihme/homes/", user)), "/eppasm/"))
devtools::load_all()
setwd(code.dir)
devtools::load_all()

### Tables
loc.table <- get_locations(hiv_metadata = TRUE)



# These locations do not have information from LBD team estimates
# ZAF ANC data are considered nationally representative so no GeoADjust - this could be challenged in the future
no_geo_adj <-  c(loc.table[epp ==1 & grepl("IND",ihme_loc_id),ihme_loc_id],
                 "PNG","HTI","DOM", 'CPV', loc.table[epp ==1 & grepl("ZAF",ihme_loc_id),ihme_loc_id],"STP","MRT","COM")



# ANC data bias adjustment
if(geoadjust & !loc %in% no_geo_adj){
  geoadjust  <- TRUE
} else {
  geoadjust  <- FALSE
}

if(!loc %in% unlist(strsplit(list.files('/share/hiv/data/PJNZ_EPPASM_prepped_subpop/lbd_anc/2019/'), '.rds'))){
  lbd.anc <- FALSE
}

if(grepl('ZAF', loc)){
  lbd.anc <- FALSE
}
if(grepl('PNG', loc)){
  lbd.anc <- FALSE
}

prev_sub <- TRUE

sexincrr.sub <- TRUE

##Need to figure out where to get these
if(loc %in% c("MAR","MRT","COM")){
  sexincrr.sub <- FALSE
}
if(loc == "STP"){
  prev_sub <- FALSE
}
### Code
## Read in spectrum object, sub in GBD parameters

dt <- read_spec_object(loc, j, start.year, stop.year, trans.params.sub, 
                       pop.sub, anc.sub, anc.backcast, prev.sub = prev_sub, art.sub = TRUE, 
                       sexincrr.sub = sexincrr.sub,  age.prev = age.prev, paediatric = TRUE, 
                       anc.prior.sub = TRUE, lbd.anc = lbd.anc, 
                       geoadjust = geoadjust, use_2019 = TRUE)



##this is a quick fix, will need to correct later
if(lbd.anc){
  attr(dt, 'eppd')$ancsitedat$prev <- attr(dt, 'eppd')$ancsitedat$prev / 0.01
}

if(grepl('ETH', loc)){
  attr(dt, 'eppd')$hhs <-  subset(attr(dt, 'eppd')$hhs, year != '2018')
}
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

if(loc %in% "STP"){
  attr(dt, 'eppd')$hhs <- attr(dt, 'eppd')$hhs[attr(dt, 'eppd')$hhs$subpop=="Pop Fem_restante",] 
  attr(dt, 'eppd')$ancsitedat <- attr(dt, 'eppd')$ancsitedat[attr(dt, 'eppd')$ancsitedat$subpop=="Pop Fem_restante",] 
  attr(dt, 'eppd')$ancsitedat = unique(attr(dt, 'eppd')$ancsitedat)
  attr(dt, 'specfp')$art_alloc_mxweight <- 0.5

}

if(loc %in% "COM"){
  attr(dt, 'eppd')$ancsitedat <- attr(dt, 'eppd')$ancsitedat[attr(dt, 'eppd')$ancsitedat$subpop=="Female Population",] 
  attr(dt, 'eppd')$ancsitedat = unique(attr(dt, 'eppd')$ancsitedat)
  attr(dt, 'specfp')$art_alloc_mxweight <- 0.5
  
}

if(loc %in% "MRT"){
  attr(dt, 'eppd')$ancsitedat <- attr(dt, 'eppd')$ancsitedat[attr(dt, 'eppd')$ancsitedat$subpop=="Pop féminine restante",] 
  attr(dt, 'eppd')$ancsitedat = unique(attr(dt, 'eppd')$ancsitedat)
  attr(dt, 'specfp')$art_alloc_mxweight <- 0.5
  
}

## Fit model
fit <- eppasm::fitmod(dt, eppmod = epp.mod, B0 = 1e4, B = 1e3, number_k = 500)
data.path <- paste0('/share/hiv/epp_input/', gbdyear, '/', run.name, '/fit_data/', loc, '.csv')
if(!file.exists(data.path)){save_data(loc, attr(dt, 'eppd'), run.name)}
if(file.exists(data.path)){save_data(loc, attr(dt, 'eppd'), run.name)}



## When fitting, the random-walk based models only simulate through the end of the
## data period. The `extend_projection()` function extends the random walk for r(t)
## through the end of the projection period.
if(epp.mod == 'rhybrid'){
  fit <- extend_projection(fit, proj_years = stop.year - start.year + 1)
}

if(max(fit$fp$pmtct_dropout$year) < stop.year){
  add_on.year <- seq(max(fit$fp$pmtct_dropout$year) + 1 , stop.year)
  add_on.dropouts <- fit$fp$pmtct_dropout[fit$fp$pmtct_dropout$year == max(fit$fp$pmtct_dropout$year), 2:ncol(fit$fp$pmtct_dropout)]
  fit$fp$pmtct_dropout <- rbind(fit$fp$pmtct_dropout, c(year = unlist(add_on.year), add_on.dropouts))
}

##NOTE: need to get GBD simmod working again - error on BF transmissions - otherwise PAEDIATRIC must be false
#debugonce(gbd_sim_mod)
result <- gbd_sim_mod(fit, VERSION = "R")

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

