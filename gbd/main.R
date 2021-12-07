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
rm(list = ls())
Sys.umask(mode = "0002")
windows <- Sys.info()[1][["sysname"]]=="Windows"
root <- ifelse(windows,"J:/","/home/j/")
user <- ifelse(windows, Sys.getenv("USERNAME"), Sys.getenv("USER"))


# Arguments ---------------------------------------
args <- commandArgs(trailingOnly = TRUE)
print(args)
if(length(args) == 0){
  array.job = TRUE
  run.name <- "dev_step4a"
  loc <- 'FRA'
  stop.year <- 2022
  j <- 1
  paediatric <- TRUE
}else{
  run.name <- args[1]
  array.job <- as.logical(args[2])
  j <- as.integer(Sys.getenv("SGE_TASK_ID"))
  
}

if(!array.job & length(args) > 0){
  loc <- args[3]
  stop.year <- as.integer(args[4])
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

lbd.anc <- F
ped_toggle = TRUE
paediatric = TRUE

file_name = loc
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
##attr(dt, 'eppd')
##attr(dt, 'specfp')
dt <- read_spec_object(loc, j, start.year, stop.year, trans.params.sub,
                       pop.sub, anc.sub,  prev.sub = prev_sub, art.sub = TRUE,
                       sexincrr.sub = sexincrr.sub,  age.prev = age.prev, paediatric = TRUE,
                       anc.prior.sub = TRUE, lbd.anc = lbd.anc,
                       geoadjust = geoadjust, use_2019 = TRUE,
                       test.sub_prev_granular = test,
                       anc.rt = FALSE, run_name = run.name
                       # anc.backcast,
                       )

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
attr(dt, 'eppd')$ancsitedat <- data.frame(attr(dt, 'eppd')$ancsitedat)


draw <- j
##need to make this fp$incidpopage == 0L the incidence rate
incid <- fread('/ihme/hiv/spectrum_input/200713_yuka/incidence/FRA.csv')
attr(dt, 'specfp')$incidpopage <- as.integer(0)
attr(dt, 'specfp')$incidinput <- incid[,3]

##not sure what this is doing
inputIncSexRatio = fread('/home/j/WORK/04_epi/01_database/02_data/hiv/04_models/gbd2015/02_inputs/AIM_assumptions//sex_age_pattern/FtoM_inc_ratio_epidemic_specific.csv')
# We need to adjust the sex ratio of incidence in order to get the sex ratio of
# deaths to line up with the ratio in the VR data.
inputSexRatioAdj = fread("/home/j/WORK/04_epi/01_database/02_data/hiv/04_models/gbd2015/02_inputs/AIM_assumptions//sex_age_pattern/post_1995_sex_ratios.csv")
# isoIndex = inputSexRatioAdj[iso3 == loc,]
# sexRatioVR = float([row for row in inputSexRatioAdj[1:] if row[isoIndex] == parent][0][1])
# sexRatioAdj = sexRatioVR / .42
inputIncSexRatio <- inputIncSexRatio[epidemic_class == 'CON',]
inputIncSexRatio[,year := 1990:2020]
inputIncSexRatio <- inputIncSexRatio$FtoM_inc_ratio
attr(dt, 'specfp')$incrr_sex <- c(rep(inputIncSexRatio[1], length(seq(1970,1989))), inputIncSexRatio, rep(inputIncSexRatio[length(inputIncSexRatio)], length(seq(2020,2025))))

age_ratio <- fread("/home/j/WORK/04_epi/01_database/02_data/hiv/04_models/gbd2015/02_inputs/AIM_assumptions/sex_age_pattern/age_IRRs/Feb17/GEN_IRR.csv")
age_ratio[,mean := (upper - lower )/ 2]
age_ratio <- age_ratio[,.(age, sex, mean)]
age_ratio_year <- list()
for(i in 1970:2025){
  age_ratio_year <- rbind(age_ratio_year, age_ratio[,year:= i])
}
age_ratio <- age_ratio_year
age_map_dt <- data.table(age_year = c(15:80))
age_map_dt[,age := unlist(lapply(seq(15,75, by = 5), rep, times = 5))]
age_map_dt[age_year == 80, age := 75]
age_ratio <- merge(age_map_dt, age_ratio, by = 'age', allow.cartesian = T)
#age_ratio <- dcast(age_ratio, age + age_year + sex ~ year, value.var = 'mean')
keeper <- array(0, dim = c(66,2,56))
for(i in c(1970:2025)){
target <- age_ratio[year == i,.(sex, mean)]
target <- cbind(target[sex == 1,mean], target[sex == 2,mean])
 keeper[,,i - 1969] <- target
}
target <- readRDS('/ihme/hiv/epp_output/gbd20/200713_yuka/dt_objects/AGO_dt.RDS')
target <- attr(target, 'specfp')
attributes(keeper) <- attributes(target$incrr_age)

attr(dt, 'specfp')$incrr_age <- keeper

simmod.specfp(attr(dt, 'specfp'), VERSION = 'R')




















result <- gbd_sim_mod(dt, VERSION = "R")
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


