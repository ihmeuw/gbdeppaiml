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
  loc <- 'CYP'
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
#start.year = 2003
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
if(!file.exists(paste0('/ihme/hiv/epp_output/gbd20/',run.name, '/dt_objects/', loc, '_dt.RDS'))){
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
  zero_prev_locs <- fread("/ihme/hiv/epp_input/gbd20/prev_surveys.csv")
  zero_prev_locs <- unique(zero_prev_locs[prev == 0.0005 & use == TRUE,iso3])
  attr(dt, 'eppd')$ancsitedat <- data.frame(attr(dt, 'eppd')$ancsitedat)
  
  
  dt <- group_2_dt_mods(loc, dt)
}else{
 dt <- readRDS(paste0('/ihme/hiv/epp_output/gbd20/',run.name, '/dt_objects/', loc, '_dt.RDS'))
}
draw <- j
#need to make this fp$incidpopage == 0L the incidence rate
compare.dt.run <- fread(paste0('/ihme/hiv/spectrum_draws/200713_yuka/compiled/stage_1/summary/', loc, '_all_age.csv'))
incid_new <- compare.dt.run[variable == 'inc_rate',value]# * 100

incid <- fread(paste0('/ihme/hiv/spectrum_input/200713_yuka/incidence/', loc, '.csv'))
attr(dt, 'specfp')$incidpopage <- as.integer(0)
col <- draw + 2
incid <- data.frame(incid)
if(loc == 'FRA'){
  incid <-  incid[,col] / 1000
  
}else{
  incid <-  incid[,col] / 100
  
}
# incid[1:which(seq(1970,2025) == 1978)] <- 0
attr(dt, 'specfp')$incidinput <- incid_new

###try converting ART to number, causing simmod to break
{
  if(all(attr(dt, 'specfp')$art15plus_isperc)){
    art = attr(dt, 'specfp')$art15plus_num
    prev <- fread(paste0('/ihme/hiv/spectrum_prepped/aggregates/200713_yuka/',loc ,'.csv'))
    prev <- prev[age_group_id %in% c(8:20,30,31,32,235), .(year_id, sex_id, run_num, pop_hiv)]
    prev[,pop_hiv := sum(pop_hiv), by = c('year_id', 'run_num', 'sex_id')]
    prev <- unique(prev)
    prev[,pop_hiv := mean(pop_hiv), by = c('year_id',  'sex_id')]
    prev[,run_num := NULL]
    last <- prev[year_id == 2022,]
    replace <- list()
    for(i in c(2023:2025)){
      x <- copy(last)
      x[,year_id := i]
      replace <- rbind(replace, x)
    }
    prev <- rbind(prev, replace)
    prev <- unique(prev)
    art[1,] <- (art[1,] / 100) * prev[sex_id == 1,pop_hiv]
    art[2,] <- (art[2,] / 100) * prev[sex_id == 2,pop_hiv]
    attr(dt, 'specfp')$art15plus_num <- art
    attr(dt, 'specfp')$art15plus_isperc <- !attr(dt, 'specfp')$art15plus_isperc
  }

}

###########################################
#Results
##########################################
result = simmod.specfp(attr(dt, 'specfp'), VERSION = 'R')
dir.create(paste0('/ihme/hiv/epp_output/', gbdyear, '/', run.name, '/fit/', file_name, '/'), recursive = T)
saveRDS(result, paste0('/ihme/hiv/epp_output/', gbdyear, '/', run.name, '/fit/', file_name, '/', draw, '.RDS'))

output.dt <- get_gbd_outputs(result, attr(dt, 'specfp'), paediatric = paediatric)
output.dt[,run_num := j]
out.dir <- paste0('/ihme/hiv/epp_output/gbd20/', run.name, '/', file_name, '/')
dir.create(out.dir, showWarnings = FALSE)
write.csv(output.dt, paste0(out.dir, '/', j, '.csv'), row.names = F)

if(paediatric){
  split.dt <- get_under1_splits(result, attr(dt, 'specfp'))
  split.dt[,run_num := j]
  write.csv(split.dt, paste0(out.dir, '/under_1_splits_', j, '.csv' ), row.names = F)
}


