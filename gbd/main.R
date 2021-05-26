##Set up --------------------------- 
## Script name: main.R
## Purpose of script: Run the EPPASM model
##
## Author: Maggie Walters
## Date Created: 2018-04-11
## Email: mwalte10@uw.edu
## 
##
## Notes: Created by Tahvi Frank and modified for GBD20 by Maggie Walters
## Some arguments are likely to stay constant across runs, others we're more likely to test different options.
## The arguments that are more likely to vary are pulled from the eppasm run table
##

## Used in basically every script
Sys.umask(mode = "0002")
windows <- Sys.info()[1][["sysname"]]=="Windows"
root <- ifelse(windows,"J:/","/home/j/")
user <- ifelse(windows, Sys.getenv("USERNAME"), Sys.getenv("USER"))


# Arguments ---------------------------------------
args <- commandArgs(trailingOnly = TRUE)
print(args)
if(length(args) == 0){
  array.job = FALSE
  run.name <- "200713_yuka"
  loc <- 'AGO'
  stop.year <- 2022
  j <- 1
  paediatric <- FALSE
}else{
  run.name <- args[1]
  array.job <- as.logical(args[2])
}

if(!array.job & length(args) > 0){
  loc <- args[3]
  stop.year <- as.integer(args[4])
  j <- as.integer(Sys.getenv("SGE_TASK_ID"))
  paediatric <- as.logical(args[5])
}

eppasm_dir <- paste0(ifelse(windows, "H:", paste0("/ihme/homes/", user)), "/eppasm/")
setwd(eppasm_dir)
devtools::load_all()
gbdeppaiml_dir <- paste0(ifelse(windows, "H:", paste0("/ihme/homes/", user)), "/gbdeppaiml/")
setwd(gbdeppaiml_dir)
devtools::load_all()

gbdyear <- 'gbd20'
stop.year = 2022

run.table <- fread(paste0('/share/hiv/epp_input/gbd20//eppasm_run_table.csv'))
in_run_table = F
if(in_run_table){
  c.args <- run.table[run_name==run.name]
}else{
  #use the GBD20 final run toggles if the run isn't in the run table
  c.args <- run.table[run_name=='200713_yuka']
}

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

# Array job ---------------------------------------
if(array.job){
  array.dt <- fread(paste0('/ihme/hiv/epp_input/gbd20/',run.name,'/array_table.csv'))
  task_id <- as.integer(Sys.getenv("SGE_TASK_ID"))
  j <- array.dt[task_id,draws]
  file_name <- array.dt[task_id,loc_scalar]
  combo_num <- array.dt[task_id,combo]
  loc <- array.dt[task_id,ihme_loc_id]
  pred.mat <- readRDS('/ihme/homes/mwalte10/hiv_gbd2019/requests/haidong_proj/maggie/pref_mat.RDS')
  foi_scalar <- unique(pred.mat[ihme_loc_id == loc & combo == combo_num])[,.(year_id, scalar)]
}else{
  file_name <- loc
  foi_scalar = 1
}
out.dir <- paste0('/ihme/hiv/epp_output/',gbdyear,'/', run.name, "/", file_name)

source('/ihme/homes/mwalte10/gbdeppaiml/gbd/data_prep.R')
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


# Prepare the dt object ---------------------------------------
dt <- read_spec_object(loc, j, start.year, stop.year, trans.params.sub,
                       pop.sub, anc.sub,  prev.sub = prev_sub, art.sub = TRUE,
                       sexincrr.sub = sexincrr.sub,  age.prev = age.prev, paediatric = TRUE,
                       anc.prior.sub = TRUE, lbd.anc = lbd.anc,
                       geoadjust = geoadjust, use_2019 = TRUE,
                       test.sub_prev_granular = test,
                       anc.rt = TRUE
                       # anc.backcast,
                       )
###Switched to a binomial model, so we can now handle observations of zero
mod <- data.table(attr(dt, 'eppd')$hhs)[prev == 0.0005,se := 0]
mod[prev == 0.0005, prev := 0]
attr(dt, 'eppd')$hhs <- data.frame(mod)


###Extends inputs to the projection year as well as does some site specific changes. This should probably be examined by cycle
dt <- modify_dt(dt, run_name = run.name)
# if(loc == 'CAF'){
#   print('outliering some ancrt points')
#   dat <- data.table(attr(dt, 'eppd')$ancsitedat)
#   dat <- dat[!(site %in% c('Mambélé', "N'Gaoundaye", "Gobongo") & type == 'ancrt')]
#   attr(dt, 'eppd')$ancsitedat <- as.data.frame(dat)
# }
# if(loc == 'CMR'){
#   print('outliering some ancrt points')
#   dat <- data.table(attr(dt, 'eppd')$ancsitedat)
#   dat <- dat[!(site %in% c("Fondation Chantal Biya") & type == 'ancrt' & prev > 0.2)]
#   attr(dt, 'eppd')$ancsitedat <- as.data.frame(dat)
# }

###Replacement of a few priors
attr(dt, 'specfp')$art_alloc_mxweight <- 0.5
sub.anc.prior <- function(dt,loc){
  if(loc %in%  c("SDN","SSD","SOM","GNB","MDG","PNG", "COM")){
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

###Get the locations that should be run with the binomial likelihood
# zero_prev_locs <- fread(prev_surveys)
zero_prev_locs <- fread("/ihme/hiv/epp_input/gbd20/prev_surveys.csv")
zero_prev_locs <- unique(zero_prev_locs[prev == 0.0005 & use == TRUE,iso3])

# Fit model ---------------------------------------
# dt <- readRDS(paste0('/ihme/hiv/epp_output/gbd20/200713_yuka/dt_objects/',loc,'_dt.RDS'))
# attr(dt,"eppd")$ancsitedat <- as.data.frame(attr(dt,"eppd")$ancsitedat)

fit <- eppasm::fitmod(dt, eppmod = ifelse(grepl('IND', loc),'rlogistic',epp.mod), 
                      B0 = 1e5, B = 1e3, number_k = 500, 
                      ageprev = ifelse(loc %in% zero_prev_locs,'binom','probit'))

dir.create(paste0('/ihme/hiv/epp_output/gbd20/', run.name, '/fitmod/'))
saveRDS(fit, file = paste0('/ihme/hiv/epp_output/gbd20/', run.name, '/fitmod/', loc, '_', j, '.RDS'))


dir.create(paste0('/ihme/hiv/epp_output/gbd20/', run.name, '/inc_rate/'))
dir.create(paste0('/ihme/hiv/epp_output/gbd20/', run.name, '/prev_rate/'))
data.path <- paste0('/share/hiv/epp_input/', gbdyear, '/', run.name, '/fit_data/', loc,'.csv')


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



draw <- j
result <- gbd_sim_mod(fit, VERSION = "R")
# dir.create(paste0('/ihme/hiv/epp_output/', gbdyear, '/', run.name, '/fit/', loc, '/'), recursive = T)
# saveRDS(result, paste0('/ihme/hiv/epp_output/', gbdyear, '/', run.name, '/fit/', loc, '/', draw, '.RDS'))
dir.create(paste0('/ihme/hiv/epp_output/', gbdyear, '/', run.name, '/fit/', file_name, '/'), recursive = T)
saveRDS(result, paste0('/ihme/hiv/epp_output/', gbdyear, '/', run.name, '/fit/', file_name, '/', draw, '.RDS'))
#
# #results
# ##track the output of the prev and inc through get_gbd_outputs
output.dt <- get_gbd_outputs(result, attr(dt, 'specfp'), paediatric = paediatric)
output.dt[,run_num := j]
out.dir <- paste0('/ihme/hiv/epp_output/gbd20/', run.name, '/', file_name, '/')
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
params <- fnCreateParam(theta = unlist(param), fp = fit$fp)
saveRDS(params, paste0('/ihme/hiv/epp_output/', gbdyear, '/', run.name, '/fit/', file_name, '.RDS'))


data.path <- paste0('/share/hiv/epp_input/', gbdyear, '/', run.name, '/fit_data/', loc,'.csv')
save_data(loc, attr(dt, 'eppd'), run.name)


