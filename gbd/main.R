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
  run.name <- "201001_socialdets"
  loc <- 'AGO'
  stop.year <- 2022
  j <- 1
  paediatric <- FALSE
}
run.table <- fread(paste0('/share/hiv/epp_input/gbd20//eppasm_run_table.csv'))
if(grepl('IND',loc)){
  temp.run.name = '2020_ind_test_agg9'
}else{
  temp.run.name = run.name
}
c.args <- run.table[run_name=='200921_socialdets']
#c.args <- run.table[run_name==run.name]

### Arguments
## Some arguments are likely to stay constant across runs, others we're more likely to test different options.
## The arguments that are more likely to vary are pulled from the eppasm run table
start.year <- 1970
trans.params.sub <- TRUE
pop.sub <- TRUE
art.sub <- TRUE
prev_sub <- TRUE
sexincrr.sub <- TRUE
plot.draw <- FALSE
anc.prior.sub <- TRUE
lbd.anc <- T
test <- NULL
ped_toggle = FALSE
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
prev_sub <- TRUE
sexincrr.sub <- TRUE
### Paths
if(!is.null(test)){
  out.dir <- paste0('/ihme/hiv/epp_output/',gbdyear,'/', run.name, '_', test,"/", loc)
}else{
  out.dir <- paste0('/ihme/hiv/epp_output/',gbdyear,'/', run.name, "/", loc)
}
### Functions
library(mortdb, lib = "/share/mortality/shared/r/")
setwd(paste0(ifelse(windows, "H:", paste0("/ihme/homes/", user)), "/eppasm/"))
devtools::load_all()
setwd(code.dir)
devtools::load_all()
loc.table <- get_locations(hiv_metadata = TRUE)
##set toggles
if(loc == 'ETH_44862'){  ##I imagine this was for a test and can be taken off? Note that its in the set_toggles function too
  births <-    paste0('/ihme/hiv/epp_input/gbd19/190630_rhino2/births/', loc, '.csv')
  print(paste0(loc, ' births from rhino2 subbed in'))
}
# These locations do not have information from LBD team estimates
# ZAF ANC data are considered nationally representative so no GeoADjust - this could be challenged in the future
no_geo_adj <-  c(loc.table[epp ==1 & grepl("IND",ihme_loc_id),ihme_loc_id],
                 "PNG","HTI","DOM", 'CPV', loc.table[epp ==1 & grepl("ZAF",ihme_loc_id),ihme_loc_id], 'STP', 'KEN_35626', 'MRT', 'COM')
if(loc %in% c('ZWE', 'MWI')){
  geoadj_test <- TRUE
}else{
  geoadj_test <- FALSE
}
# ANC data bias adjustment
if(geoadjust & !loc %in% no_geo_adj){
  geoadjust  <- TRUE
} else {
  geoadjust  <- FALSE
}
print(paste0(loc, ' geoadjust set to ', geoadjust))
if(!loc %in% unlist(strsplit(list.files('/share/hiv/data/PJNZ_EPPASM_prepped_subpop/lbd_anc/2019/'), '.rds'))){
  lbd.anc <- FALSE
}
if(grepl('IND',loc)){
  lbd.anc <- FALSE
}
if(loc %in% c("ZAF","PNG")){
  lbd.anc <- FALSE
}
print(paste0(loc, ' lbd.anc set to ', lbd.anc))
##Need to figure out where to get these
if(loc %in% c("MAR","MRT","COM")){
  sexincrr.sub <- FALSE
}
dt <- read_spec_object(loc, j, start.year, stop.year, trans.params.sub,
                       pop.sub, anc.sub, anc.backcast, prev.sub = prev_sub, art.sub = TRUE,
                       sexincrr.sub = sexincrr.sub,  age.prev = age.prev, paediatric = ped_toggle,
                       anc.prior.sub = TRUE, lbd.anc = lbd.anc,
                       geoadjust = geoadjust, use_2019 = TRUE,
                       test.sub_prev_granular = test)
#dt <- readRDS('/ihme/hiv/epp_output/gbd20/2020_ind_test_agg9/dt_objects/IND_4841_dt.RDS')
mod <- data.table(attr(dt, 'eppd')$hhs)[prev == 0.0005,se := 0]
mod[prev == 0.0005, prev := 0]
attr(dt, 'eppd')$hhs <- data.frame(mod)
dt <- modify_dt(dt)
attr(dt, 'specfp')$art_alloc_mxweight <- 0.5
sub.anc.prior <- function(dt,loc){
  if(loc %in%  c("SDN","SSD","SOM","GNB","MDG","PNG", "COM")){
    #   if(loc %in%  c("SDN","SSD","SOM","MDG","PNG", "COM")){
    ancbias.pr.mean <<- 0.15
    ancbias.pr.sd <<- 0.001
  }else if(loc %in% "MRT"){
    ancbias.pr.mean <<- 0.15
    ancbias.pr.sd <<- 0.001
  } else {
    ancbias.pr.mean <<- 0.15
    ancbias.pr.sd <<- 1
  }
  return(dt)
}
dt <- sub.anc.prior(dt, loc)
zero_prev_locs <- fread(prev_surveys)
zero_prev_locs <- unique(zero_prev_locs[prev == 0.0005,iso3])
if(loc == 'TZA'){
  mod <- data.table(attr(dt, 'eppd')$hhs)
  mod[year == 2017, se := 0.015]
  attr(dt, 'eppd')$hhs <- data.frame(mod)
}
## Fit model

if(loc %in% zero_prev_locs){
  if(grepl('IND',loc)){
    epp.mod = 'rlogistic'
    fit <- eppasm::fitmod(dt, eppmod = epp.mod, B0 = 1e4, B = 1e3, number_k = 100, ageprev = 'binom')
    
  }else{
    fit <- eppasm::fitmod(dt, eppmod = epp.mod, B0 = 1e4, B = 1e3, number_k = 100, ageprev = 'binom')
    
  }
  
}else{
  # 
  # fit <- eppasm::fitmod(dt, eppmod = epp.mod, B0 = 1e5, B = 1e3, number_k = 500, fitincrr = 'regincrr')
  fit <- eppasm::fitmod(obj = dt, eppmod = epp.mod, B0 = 1e5, B = 1e3, number_k = 300)
  

}

data.path <- paste0('/share/hiv/epp_input/', gbdyear, '/', run.name, '/fit_data/', loc,'.csv')
save_data(loc, attr(dt, 'eppd'), run.name)

## When fitting, the random-walk based models only simulate through the end of the
## data period. The `extend_projection()` function extends the random walk for r(t)
## through the end of the projection period.

if(epp.mod == 'rhybrid'){
  fit <- extend_projection(fit, proj_years = stop.year - start.year + 1)
}

if(max(fit$fp$pmtct_dropout$year) < stop.year & ped_toggle){
  add_on.year <- seq(max(fit$fp$pmtct_dropout$year) + 1 , stop.year)
  add_on.dropouts <- fit$fp$pmtct_dropout[fit$fp$pmtct_dropout$year == max(fit$fp$pmtct_dropout$year), 2:ncol(fit$fp$pmtct_dropout)]
  fit$fp$pmtct_dropout <- rbind(fit$fp$pmtct_dropout, c(year = unlist(add_on.year), add_on.dropouts))
}

##NOTE: need to get GBD simmod working again - error on BF transmissions - otherwise PAEDIATRIC must be false


draw <- j

  result <- gbd_sim_mod(fit, VERSION = "R")
  # rvec <- fnCreateParam(theta = attr(result, 'theta'), fp =fit$fp)
  # saveRDS(rvec, file = paste0("/ihme/homes/mwalte10/hiv_gbd2020/", run.name, '/', loc, ".RDS"))
  # 
  dir.create(paste0('/ihme/hiv/epp_output/', gbdyear, '/', run.name, '/fit/', loc, '/'), recursive = T)
  saveRDS(result, paste0('/ihme/hiv/epp_output/', gbdyear, '/', run.name, '/fit/', loc, '/', draw, '.RDS'))
  
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

