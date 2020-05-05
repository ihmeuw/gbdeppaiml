## Tahvi Frank
## tahvif@uw.edu/tahvif@gmail.com
### Setup
rm(list=ls())
Sys.umask(mode = "0002")
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
	run.name <- '200316_windchime_testing1'
	loc <- 'LSO'
	#loc <- 'ERI'
	stop.year <- 2022
	j <- 1
	paediatric <- TRUE
}

run.table <- fread(paste0('/share/hiv/epp_input/gbd20//eppasm_run_table.csv'))
c.args <- run.table[run_name=='200316_windchime']


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
library(mortdb, lib = "/share/mortality/shared/r/")
setwd(paste0(ifelse(windows, "H:", paste0("/ihme/homes/", user)), "/eppasm/"))
devtools::load_all()
setwd(code.dir)
devtools::load_all()
loc.table <- get_locations(hiv_metadata = TRUE)


if(loc == 'ETH_44862'){
  births <-    paste0('/ihme/hiv/epp_input/gbd19/190630_rhino2/births/', loc, '.csv')

}


### Tables



# These locations do not have information from LBD team estimates
# ZAF ANC data are considered nationally representative so no GeoADjust - this could be challenged in the future
no_geo_adj <-  c(loc.table[epp ==1 & grepl("IND",ihme_loc_id),ihme_loc_id],
                 "PNG","HTI","DOM", 'CPV', loc.table[epp ==1 & grepl("ZAF",ihme_loc_id),ihme_loc_id], 'STP', 'KEN_35626', 'MRT', 'COM')


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
# if(loc == "STP"){
#   prev_sub <- FALSE
# }
### Code
## Read in spectrum object, sub in GBD parameters
if(loc %in% c('ZWE', 'MWI')){
  geoadj_test <- TRUE
  
}else{
  geoadj_test <- FALSE
}
dt <- read_spec_object(loc, j, start.year, stop.year, trans.params.sub, 
                       pop.sub, anc.sub, anc.backcast, prev.sub = prev_sub, art.sub = TRUE, 
                       sexincrr.sub = sexincrr.sub,  age.prev = age.prev, paediatric = TRUE, 
                       anc.prior.sub = TRUE, lbd.anc = lbd.anc, 
                       geoadjust = geoadjust, use_2019 = TRUE)
if(loc == 'NGA_25322'){
  attr(dt, 'eppd')$ancsitedat <- as.data.frame(as.data.table(attr(dt, 'eppd')$ancsitedat)[prev < 0.2,])
  
}

#source(paste0('/ihme/homes/mwalte10/data_post_processing.R'))
if(loc == 'IND_4842'){
  sub_in <- readRDS('/ihme/hiv/epp_output/gbd20/200213_violin/dt_objects/IND_4862_dt.RDS')
  sub_in <- attr(sub_in, 'specfp')$paedsurv_artcd4dist
  attr(dt, 'specfp')$paedsurv_artcd4dist <- sub_in
}

if(any(attr(dt, 'eppd')$ancsitedat$prev > 1)){
  print('A prevalence above 1 was removed')
  attr(dt, 'eppd')$ancsitedat <- as.data.frame(as.data.table(attr(dt, 'eppd')$ancsitedat)[prev < 1,])
}

##Remove NA rows on ANCRT cens that are casuing issues
if(loc=="NGA_25343"){
  attr(dt,"eppd")$ancrtcens <- attr(dt,"eppd")$ancrtcens[1:2,] 
  
}

if(grepl('ETH', loc)){
  attr(dt, 'eppd')$hhs <-  subset(attr(dt, 'eppd')$hhs, year != '2018')
}
#check_inputs(dt)
if(geoadjust){
  attr(dt, 'eppd')$ancsitedat$offset <- attr(dt, 'eppd')$ancsitedat$offset %>% as.numeric()
  
}
if(!geoadjust & any(colnames(data.table(attr(dt, 'eppd')$ancsitedat)) == 'year_id')){
  temp <- data.table(attr(dt, 'eppd')$ancsitedat)
  setnames(temp, 'year_id', 'year')
  temp <- temp[,ihme_loc_id := NULL]
  temp <- temp[,high_risk := NULL]
  attr(dt, 'eppd')$ancsitedat <- data.frame(temp)
}

dir.create(paste0('/ihme/hiv/epp_output/', gbdyear, "/", run.name, '/dt_objects/'), recursive = T)
saveRDS(dt, file = paste0('/ihme/hiv/epp_output/', gbdyear, "/", run.name, '/dt_objects/', loc, '_dt.RDS' ))
if(epp.mod == 'rspline'){attr(dt, 'specfp')$equil.rprior <- TRUE}
# 
# #Some substitutions to get things running
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
if(loc %in% c('CPV')){
  temp <- readRDS(paste0('/share/hiv/data/PJNZ_EPPASM_prepped_subpop/CPV.rds'))
  temp.artmxrr <- attr(temp, 'specfp')$artmx_timerr
  attr(dt, 'specfp')$artmx_timerr <- temp.artmxrr
}
if(loc %in% c('LSO')){
  temp <- readRDS(paste0('/share/hiv/data/PJNZ_EPPASM_prepped_subpop/LSO.rds'))
  temp.artmxrr <- attr(temp, 'specfp')$artmx_timerr
  attr(dt, 'specfp')$artmx_timerr <- temp.artmxrr
}
if(loc %in% c('MLI')){
  temp <- readRDS(paste0('/share/hiv/data/PJNZ_EPPASM_prepped_subpop/MLI.rds'))
  temp.artmxrr <- attr(temp, 'specfp')$artmx_timerr
  attr(dt, 'specfp')$artmx_timerr <- temp.artmxrr
}
if(loc %in% c('TZA')){
  temp <- readRDS(paste0('/share/hiv/data/PJNZ_EPPASM_prepped_subpop/TZA.rds'))
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
# if(loc == 'GNQ'){
#   attr(dt, 'eppd')$hhs <- subset(attr(dt, 'eppd')$hhs, sex == 'both')
#   
# }

attr(dt, 'specfp')$relinfectART <- 0.3

if(grepl("IND",loc)){
  if(no_anc){
    attr(dt,"eppd")$ancsitedat <- NULL
  }
  attr(dt, 'specfp')$art_alloc_mxweight <- 0.5
}

if(loc %in% "STP"){
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


# ######extend pmtct_dropout to 2022, and fill any missing years
if(max(attr(dt, 'specfp')$pmtct_dropout$year) < stop.year){
  add_on.year <- seq(max(attr(dt, 'specfp')$pmtct_dropout$year) + 1 , stop.year)
  add_on.dropouts <- attr(dt, 'specfp')$pmtct_dropout[attr(dt, 'specfp')$pmtct_dropout$year == max(attr(dt, 'specfp')$pmtct_dropout$year), 2:ncol(attr(dt, 'specfp')$pmtct_dropout)]
  attr(dt, 'specfp')$pmtct_dropout <- rbind(attr(dt, 'specfp')$pmtct_dropout, c(year = unlist(add_on.year), add_on.dropouts))
}
if(length(unique(attr(dt, 'specfp')$pmtct_dropout$year)) < attr(dt, 'specfp')$SIM_YEARS){
  missing_years <- setdiff(seq(start.year, stop.year), attr(dt, 'specfp')$pmtct_dropout$year)
  temp.dt <- data.table( attr(dt, 'specfp')$pmtct_dropout)
  extend_back <- temp.dt[year == min(year),]
  list <- list()
  for(years in missing_years){
    x <- data.table(extend_back)
    list[[years - min(missing_years) + 1]] <- x[,year := years]
  }
  extend_back <- do.call(rbind, list)
  extend_back <- data.table(extend_back)
  extend_back[,year := missing_years]
  new <- rbind(attr(dt, 'specfp')$pmtct_dropout, extend_back)
  new <- new[order(year),]
  new <- as.data.frame(new)
  attr(dt, 'specfp')$pmtct_dropout <- new
}

if(dim(attr(dt, 'specfp')$artmx_timerr)[2] < attr(dt, 'specfp')$SIM_YEARS){
  diff <- dim(attr(dt, 'specfp')$artmx_timerr)[2] - attr(dt, 'specfp')$SIM_YEARS
  while(diff != 0){
    attr(dt, 'specfp')$artmx_timerr <-  abind::abind(attr(dt, 'specfp')$artmx_timerr, attr(dt, 'specfp')$artmx_timerr[,ncol(attr(dt, 'specfp')$artmx_timerr) ])
    diff <- dim(attr(dt, 'specfp')$artmx_timerr)[2] - attr(dt, 'specfp')$SIM_YEARS
    
  }
}
if(dim( attr(dt, 'specfp')$art15plus_isperc)[2] < attr(dt, 'specfp')$SIM_YEARS){
  diff <- dim( attr(dt, 'specfp')$art15plus_isperc)[2] - attr(dt, 'specfp')$SIM_YEARS
  while(diff != 0){
    attr(dt, 'specfp')$art15plus_isperc <-  abind::abind( attr(dt, 'specfp')$art15plus_isperc,  attr(dt, 'specfp')$art15plus_isperc[,ncol( attr(dt, 'specfp')$art15plus_isperc)])
    diff <- dim( attr(dt, 'specfp')$art15plus_isperc)[2] - attr(dt, 'specfp')$SIM_YEARS
    
  }
}
if(length( attr(dt, 'specfp')$specpop_percelig) < attr(dt, 'specfp')$SIM_YEARS){
  diff <- length( attr(dt, 'specfp')$specpop_percelig)- attr(dt, 'specfp')$SIM_YEARS
  while(diff != 0){
    attr(dt, 'specfp')$specpop_percelig <-  abind::abind(  attr(dt, 'specfp')$specpop_percelig,  (attr(dt, 'specfp')$specpop_percelig)[length( attr(dt, 'specfp')$specpop_percelig)])
    diff <- length( attr(dt, 'specfp')$specpop_percelig) - attr(dt, 'specfp')$SIM_YEARS
    
  }
}
if(length( attr(dt, 'specfp')$pw_artelig) < attr(dt, 'specfp')$SIM_YEARS){
  diff <- length( attr(dt, 'specfp')$pw_artelig)- attr(dt, 'specfp')$SIM_YEARS
  while(diff != 0){
    attr(dt, 'specfp')$pw_artelig<-  abind::abind(  attr(dt, 'specfp')$pw_artelig,  (attr(dt, 'specfp')$pw_artelig)[length( attr(dt, 'specfp')$pw_artelig)])
    diff <- length( attr(dt, 'specfp')$pw_artelig) - attr(dt, 'specfp')$SIM_YEARS
    
  }
}
if(length( attr(dt, 'specfp')$art_dropout) < attr(dt, 'specfp')$SIM_YEARS){
  diff <- length( attr(dt, 'specfp')$art_dropout)- attr(dt, 'specfp')$SIM_YEARS
  while(diff != 0){
    attr(dt, 'specfp')$art_dropout<-  abind::abind(  attr(dt, 'specfp')$art_dropout,  (attr(dt, 'specfp')$art_dropout)[length( attr(dt, 'specfp')$art_dropout)])
    diff <- length( attr(dt, 'specfp')$art_dropout) - attr(dt, 'specfp')$SIM_YEARS
    
  }
}
if(dim( attr(dt, 'specfp')$paedsurv_cd4dist)[3] < attr(dt, 'specfp')$SIM_YEARS){
  diff <- dim( attr(dt, 'specfp')$paedsurv_cd4dist)[3] - attr(dt, 'specfp')$SIM_YEARS
  while(diff != 0){
    attr(dt, 'specfp')$paedsurv_cd4dist <-  abind::abind( attr(dt, 'specfp')$paedsurv_cd4dist,  attr(dt, 'specfp')$paedsurv_cd4dist[,,dim( attr(dt, 'specfp')$paedsurv_cd4dist)[3]])
    diff <- dim( attr(dt, 'specfp')$paedsurv_cd4dist)[3] - attr(dt, 'specfp')$SIM_YEARS
    
  }
}
if(dim( attr(dt, 'specfp')$incrr_age)[3] < attr(dt, 'specfp')$SIM_YEARS){
  diff <- dim( attr(dt, 'specfp')$incrr_age)[3] - attr(dt, 'specfp')$SIM_YEARS
  while(diff != 0){
    attr(dt, 'specfp')$incrr_age <-  abind::abind( attr(dt, 'specfp')$incrr_age,  attr(dt, 'specfp')$incrr_age[,,dim( attr(dt, 'specfp')$incrr_age)[3]])
    diff <- dim( attr(dt, 'specfp')$incrr_age)[3] - attr(dt, 'specfp')$SIM_YEARS
    
  }
}
if(dim( attr(dt, 'specfp')$paedsurv_artcd4dist)[4] < attr(dt, 'specfp')$SIM_YEARS){
  diff <- dim( attr(dt, 'specfp')$paedsurv_artcd4dist)[4] - attr(dt, 'specfp')$SIM_YEARS
  while(diff != 0){
    attr(dt, 'specfp')$paedsurv_artcd4dist <-  abind::abind( attr(dt, 'specfp')$paedsurv_artcd4dist,  attr(dt, 'specfp')$paedsurv_artcd4dist[,,,dim( attr(dt, 'specfp')$paedsurv_artcd4dist)[4]])
    diff <- dim( attr(dt, 'specfp')$paedsurv_artcd4dist)[4] - attr(dt, 'specfp')$SIM_YEARS
    
  }
}

if(dim( attr(dt, 'specfp')$art15plus_num)[2] < attr(dt, 'specfp')$SIM_YEARS){
  diff <- dim( attr(dt, 'specfp')$art15plus_num)[2] - attr(dt, 'specfp')$SIM_YEARS
  while(diff != 0){
    attr(dt, 'specfp')$art15plus_num <-  abind::abind( attr(dt, 'specfp')$art15plus_num,  attr(dt, 'specfp')$art15plus_num[,ncol( attr(dt, 'specfp')$art15plus_num) - 1])
    diff <- dim( attr(dt, 'specfp')$art15plus_num)[2] - attr(dt, 'specfp')$SIM_YEARS
    
  }
}
if(length( attr(dt, 'specfp')$median_cd4init) < attr(dt, 'specfp')$SIM_YEARS){
  diff <- length( attr(dt, 'specfp')$median_cd4init)- attr(dt, 'specfp')$SIM_YEARS
  while(diff != 0){
    attr(dt, 'specfp')$median_cd4init<-  abind::abind(  attr(dt, 'specfp')$median_cd4init,  (attr(dt, 'specfp')$median_cd4init)[length( attr(dt, 'specfp')$median_cd4init)])
    diff <- length( attr(dt, 'specfp')$median_cd4init) - attr(dt, 'specfp')$SIM_YEARS
    
  }
}
if(length( attr(dt, 'specfp')$med_cd4init_input) < attr(dt, 'specfp')$SIM_YEARS){
  diff <- length( attr(dt, 'specfp')$med_cd4init_input)- attr(dt, 'specfp')$SIM_YEARS
  while(diff != 0){
    attr(dt, 'specfp')$med_cd4init_input<-  abind::abind(  attr(dt, 'specfp')$med_cd4init_input,  (attr(dt, 'specfp')$med_cd4init_input)[length( attr(dt, 'specfp')$med_cd4init_input)])
    diff <- length( attr(dt, 'specfp')$med_cd4init_input) - attr(dt, 'specfp')$SIM_YEARS
    
  }
}
if(length( attr(dt, 'specfp')$med_cd4init_cat) < attr(dt, 'specfp')$SIM_YEARS){
  diff <- length( attr(dt, 'specfp')$med_cd4init_cat)- attr(dt, 'specfp')$SIM_YEARS
  while(diff != 0){
    attr(dt, 'specfp')$med_cd4init_cat<-  abind::abind(  attr(dt, 'specfp')$med_cd4init_cat,  (attr(dt, 'specfp')$med_cd4init_cat)[length( attr(dt, 'specfp')$med_cd4init_cat)])
    diff <- length( attr(dt, 'specfp')$med_cd4init_cat) - attr(dt, 'specfp')$SIM_YEARS
    
  }
}
if(length( attr(dt, 'specfp')$verttrans_lag) < attr(dt, 'specfp')$SIM_YEARS){
  diff <- length( attr(dt, 'specfp')$verttrans_lag)- attr(dt, 'specfp')$SIM_YEARS
  while(diff != 0){
    attr(dt, 'specfp')$verttrans_lag<-  abind::abind(  attr(dt, 'specfp')$verttrans_lag,  (attr(dt, 'specfp')$verttrans_lag)[length( attr(dt, 'specfp')$verttrans_lag)])
    diff <- length( attr(dt, 'specfp')$verttrans_lag) - attr(dt, 'specfp')$SIM_YEARS
    
  }
}

if(length( attr(dt, 'specfp')$paedsurv_lag) < attr(dt, 'specfp')$SIM_YEARS){
  diff <- length( attr(dt, 'specfp')$paedsurv_lag)- attr(dt, 'specfp')$SIM_YEARS
  while(diff != 0){
    attr(dt, 'specfp')$paedsurv_lag<-  abind::abind(  attr(dt, 'specfp')$paedsurv_lag,  (attr(dt, 'specfp')$paedsurv_lag)[length( attr(dt, 'specfp')$paedsurv_lag)])
    diff <- length( attr(dt, 'specfp')$paedsurv_lag) - attr(dt, 'specfp')$SIM_YEARS
    
  }
}

if(length( attr(dt, 'specfp')$artcd4elig_idx) < attr(dt, 'specfp')$SIM_YEARS){
  diff <- length( attr(dt, 'specfp')$artcd4elig_idx)- attr(dt, 'specfp')$SIM_YEARS
  while(diff != 0){
    attr(dt, 'specfp')$artcd4elig_idx<-  abind::abind(  attr(dt, 'specfp')$artcd4elig_idx,  (attr(dt, 'specfp')$artcd4elig_idx)[length( attr(dt, 'specfp')$artcd4elig_idx)])
    diff <- length( attr(dt, 'specfp')$artcd4elig_idx) - attr(dt, 'specfp')$SIM_YEARS
    
  }
}

if(dim( attr(dt, 'specfp')$entrantprev)[2] < attr(dt, 'specfp')$SIM_YEARS){
  diff <- dim( attr(dt, 'specfp')$entrantprev)[2] - attr(dt, 'specfp')$SIM_YEARS
  while(diff != 0){
    attr(dt, 'specfp')$entrantprev <-  abind::abind( attr(dt, 'specfp')$entrantprev,  attr(dt, 'specfp')$entrantprev[,ncol( attr(dt, 'specfp')$entrantprev) - 1])
    diff <- dim( attr(dt, 'specfp')$entrantprev)[2] - attr(dt, 'specfp')$SIM_YEARS
    
  }
}

if(dim( attr(dt, 'specfp')$entrantartcov)[2] < attr(dt, 'specfp')$SIM_YEARS){
  diff <- dim( attr(dt, 'specfp')$entrantartcov)[2] - attr(dt, 'specfp')$SIM_YEARS
  while(diff != 0){
    attr(dt, 'specfp')$entrantartcov <-  abind::abind( attr(dt, 'specfp')$entrantartcov,  attr(dt, 'specfp')$entrantartcov[,ncol( attr(dt, 'specfp')$entrantartcov) - 1])
    diff <- dim( attr(dt, 'specfp')$entrantartcov)[2] - attr(dt, 'specfp')$SIM_YEARS
    
  }
}

if(length(attr(dt, 'specfp')$artpaed_isperc) < attr(dt, 'specfp')$SIM_YEARS){
  diff <- attr(dt, 'specfp')$SIM_YEARS - length(attr(dt, 'specfp')$artpaed_isperc)
  add_names <- setdiff(seq(start.year, stop.year), as.numeric(names(attr(dt, 'specfp')$artpaed_isperc)))
  add <- rep(FALSE, length(add_names))
  names(add) <- add_names
  new <- c(attr(dt, 'specfp')$artpaed_isperc, add)
  new <- new[order(names(new))]
  attr(dt, 'specfp')$artpaed_isperc <-  new
  
  
}

if(length(attr(dt, 'specfp')$artpaed_num) < attr(dt, 'specfp')$SIM_YEARS){
  diff <- attr(dt, 'specfp')$SIM_YEARS - length(attr(dt, 'specfp')$artpaed_num)
  add_names <- setdiff(seq(start.year, stop.year), as.numeric(names(attr(dt, 'specfp')$artpaed_num)))
  add <- rep(0, length(add_names))
  names(add) <- add_names
  new <- c(attr(dt, 'specfp')$artpaed_num, add)
  new <- new[order(names(new))]
  attr(dt, 'specfp')$artpaed_num <-  new
  
  
}

if(length(attr(dt, 'specfp')$cotrim_isperc) < attr(dt, 'specfp')$SIM_YEARS){
  diff <- attr(dt, 'specfp')$SIM_YEARS - length(attr(dt, 'specfp')$cotrim_isperc)
  add_names <- setdiff(seq(start.year, stop.year), as.numeric(names(attr(dt, 'specfp')$cotrim_isperc)))
  add <- rep(FALSE, length(add_names))
  names(add) <- add_names
  new <- c(attr(dt, 'specfp')$cotrim_isperc, add)
  new <- new[order(names(new))]
  attr(dt, 'specfp')$cotrim_isperc <-  new
  
  
}

if(length(attr(dt, 'specfp')$cotrim_num) < attr(dt, 'specfp')$SIM_YEARS){
  diff <- attr(dt, 'specfp')$SIM_YEARS - length(attr(dt, 'specfp')$cotrim_num)
  add_names <- setdiff(seq(start.year, stop.year), as.numeric(names(attr(dt, 'specfp')$cotrim_num)))
  add <- rep(0, length(add_names))
  names(add) <- add_names
  new <- c(attr(dt, 'specfp')$cotrim_num, add)
  new <- new[order(names(new))]
  attr(dt, 'specfp')$cotrim_num <-  new
  
  
}

if(any(colnames(attr(dt, 'eppd')) == 'year_id')){
  x <- as.data.table(attr(dt, 'eppd')$ancsitedat)
  x <- setnames(x, 'year_id', 'year')
  attr(dt, 'eppd')$ancsitedat <-  as.data.frame(x)
}

## Fit model

fit <- eppasm::fitmod(dt, eppmod = epp.mod, B0 = 1e5, B = 1e3, number_k = 500)
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

