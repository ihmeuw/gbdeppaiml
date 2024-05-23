################################################################################
## Purpose: Produce national-level results by aggregating subnationals (i.e.
#Aggregating up from the lower levels at which spectrum is run)
## Date created: 
## Date modified: Feb 19, 2019 for Decomp 2019
## Author: Austin Carter, aucarter@uw.edu
## Run instructions: 
## Notes:
################################################################################

### Setup
rm(list=ls())
windows <- Sys.info()[1]=="Windows"
root <- ifelse(windows,"J:/","/home/j/")
user <- ifelse(windows, Sys.getenv("USERNAME"), Sys.getenv("USER"))
code.dir <- paste0(ifelse(windows, "H:/", "/homes/"), user, "/hiv_gbd/")

## Packages
library(data.table); library(parallel); library(assertable); library(dplyr)

## Arguments
args <- commandArgs(trailingOnly = TRUE)
if(length(args) > 0) {
  parent <- args[1]
  spec_name <- args[2]
  ncores <- args[3]
} else {
  parent <- "KEN_44793"
  spec_name <- "240304_platypus"
  ncores <- 20
}

n.draws <- 1000
stages <- c("stage_1")
suffix <- "_ART_data.csv"
id.vars <- c("run_num", "year", "sex_id", "age_group_id")


### Paths
# in.dir <- paste0("/ihme/hiv/spectrum_draws/", run.name, "/compiled/")
# single.age.dir <- paste0('/ihme/hiv/spectrum_draws/', run.name,'/detailed_deaths/')
eppasm_dir <- paste0('/share/hiv/epp_output/gbd23/', spec_name, '/')
out_dir <- paste0("/ihme/hiv/spectrum_prepped/art_draws/",spec_name)
dir.create(out_dir, recursive = T, showWarnings = F)
out_dir_death <- paste0("/ihme/hiv/spectrum_prepped/death_draws/",spec_name)
dir.create(out_dir_death, recursive = T, showWarnings = F)
out_dir_birth <- paste0("/ihme/hiv/spectrum_prepped/birth_prev/",spec_name, '/')
dir.create(out_dir_birth, recursive = T, showWarnings = F)


### Functions
library(mortdb, lib = "/mnt/team/mortality/pub/shared/r/4")
source("/share/cc_resources/libraries/current/r/get_population.R")


### Tables
age.table <- data.table(get_age_map(type="all", gbd_year = 2020))
loc.table <- as.data.table(get_locations(hiv_metadata = T))


## We want to pull UTLA stage 2 results, so set location table logic to not pull state results
loc.table[grepl('GBR', ihme_loc_id) & level == 5, spectrum := 0]
loc.table[grepl('GBR', ihme_loc_id) & level == 4,]

### Code
## Find Spectrum children
child.locs <- c()
loc.id <- loc.table[ihme_loc_id == parent, location_id]
children <- loc.table[parent_id == loc.id, location_id]
child.locs <- c(child.locs, loc.table[location_id %in% children & spectrum == 1, ihme_loc_id])
new.parents <- loc.table[location_id %in% children & spectrum != 1, location_id]

while(length(new.parents) > 0) {
  parents <- new.parents
  new.parents <- c()
  for(cparent in parents) {
    children <- loc.table[parent_id == cparent, location_id]
    child.locs <- c(child.locs, loc.table[location_id %in% children & spectrum == 1, ihme_loc_id])
    new.parents <- c(new.parents, loc.table[location_id %in% children & spectrum != 1, location_id])
  }
}	


# Read in child data
spec_combined <- rbindlist(
  mclapply(child.locs,
           function(loc) {
             for(stage in stages) {
               in.path <- paste0(eppasm_dir, "compiled/", loc, suffix)
               if(file.exists(in.path)) {
                 out.stage <- stage
                 break
               }
             }
             dt <- fread(in.path)
           }
           , mc.cores = ncores)
)

spec_combined[, non_hiv_deaths := as.numeric(non_hiv_deaths)][, pop := as.numeric(pop)][, pop_neg := as.numeric(pop_neg)][,hiv_deaths:= as.numeric(hiv_deaths)]
spec_combined[,new_hiv:= as.numeric(new_hiv)][,total_births:= as.numeric(total_births)][,hiv_births := as.numeric(hiv_births)][,suscept_pop:=as.numeric(suscept_pop)]
spec_combined[, pop_art:= as.numeric(pop_art)][,pop_gt350:= as.numeric(pop_gt350)][,pop_200to350:= as.numeric(pop_200to350)][, pop_lt200:= as.numeric(pop_lt200)]

spec_combined <- spec_combined[, lapply(.SD, sum), by = id.vars]

##################################################################################################################
## Convert from Spectrum to rate space
##################################################################################################################
convert_to_rate <- function(x) { 
  rate <- ifelse(spec_combined[['spec_pop']] != 0,
                 x/spec_combined[['spec_pop']], 0)
  return(rate)
} 

convert_to_rate_2 <- function(x) {
  rate <- ifelse(spec_combined[['spec_pop']] != 0,
                 x/((spec_combined[['spec_pop']]+spec_combined[['spec_pop_last']])/2), 0)  
  return(rate)
} 

shift_to_midyear <- function(x) {
  ## TODO we need to revisit repeating the last year
  x_last <- x[length(x)]
  x_lag <- c(shift(x, type = "lead")[-length(x)], x_last)
  out_x <- (x + x_lag) / 2
  return(out_x)
}

spec_combined[, non_hiv_deaths_prop := non_hiv_deaths / (non_hiv_deaths + hiv_deaths)]
spec_combined[is.na(non_hiv_deaths_prop), non_hiv_deaths_prop := 1]

### get the mid year spec_pop for incidence and death
setnames(spec_combined, 'pop', 'spec_pop')
spec_combined_last <- spec_combined[,.(year, sex_id, run_num, age_group_id, spec_pop)]
spec_combined_last[, year:= year+1]
spec_combined_0 <- spec_combined[year==min(spec_combined$year),.(year, sex_id, run_num, age_group_id, suscept_pop)]
setnames(spec_combined_0, "suscept_pop", "spec_pop")
spec_combined_last <- rbind(spec_combined_last, spec_combined_0)
setnames(spec_combined_last, "spec_pop", "spec_pop_last")

spec_combined <- merge(spec_combined, spec_combined_last, by=c("year", "sex_id", "run_num", "age_group_id"))

## Shift HIV incidence and deaths
## EPPASM output infections and deaths are midyear-to-midyear
## We want incidence and deaths to be calendar year, and populations to be midyear
convert_vars <- c('non_hiv_deaths', 'hiv_deaths', 'new_hiv', 'hiv_births', 'total_births')
spec_combined[,(convert_vars) := lapply(.SD,shift_to_midyear),.SDcols=convert_vars, by=c("sex_id", "run_num", "age_group_id")] 
spec_combined[,(convert_vars) := lapply(.SD,convert_to_rate),.SDcols=convert_vars] 
print(head(spec_combined))

convert_vars2 <- c("suscept_pop","pop_neg","pop_lt200","pop_200to350","pop_gt350","pop_art")
spec_combined[,(convert_vars2) := lapply(.SD,convert_to_rate),.SDcols=convert_vars2] 
print(spec_combined)

##################################################################################################################
## Format and Output
setnames(spec_combined,"year","year_id")



print(head(spec_combined))
assert_values(spec_combined, names(spec_combined), "gte", 0)
assert_values(spec_combined, colnames(spec_combined), "not_na")

## fix larger than 1 values
spec_mean <- spec_combined[,.(non_hiv_deaths_mean = quantile(non_hiv_deaths,probs = 0.5),
                              hiv_deaths_mean = quantile(hiv_deaths,probs = 0.5),
                              new_hiv_mean = quantile(new_hiv,probs = 0.5),
                              pop_art_mean = quantile(pop_art,probs = 0.5),
                              pop_gt350_mean = quantile(pop_gt350,probs = 0.5),
                              pop_200to350_mean = quantile(pop_200to350,probs = 0.5),
                              pop_lt200_mean = quantile(pop_lt200,probs = 0.5)),
                           by = c("year_id", "sex_id", "age_group_id")]
spec_combined <- merge(spec_combined, spec_mean, by = c("year_id", "sex_id", "age_group_id"))

spec_combined[suscept_pop>1, suscept_pop := 1][pop_neg>1, pop_neg :=1]
spec_combined[non_hiv_deaths>1, non_hiv_deaths := non_hiv_deaths_mean][hiv_deaths>1, hiv_deaths := hiv_deaths_mean]
spec_combined[new_hiv>1, new_hiv := new_hiv_mean][pop_art >1, pop_art := pop_art_mean][pop_gt350>1, pop_gt350 := pop_gt350_mean]
spec_combined[pop_200to350>1, pop_200to350 := pop_200to350_mean][pop_lt200>1, pop_lt200 :=pop_lt200_mean]


## test missing data
id.vars = list(year_id = c(1970:2024), sex_id = c(1,2), age_group_id = c(2,3,6:20, 30:32, 34, 235, 238, 388, 389), run_num = c(1:1000))
missing = assertable::assert_ids(spec_combined, id_vars = id.vars, warn_only = F)

## save deaths
write.csv(spec_combined[,list(sex_id,year_id,age_group_id,run_num,hiv_deaths,non_hiv_deaths, non_hiv_deaths_prop)],paste0(out_dir_death,"/",parent,"_ART_deaths.csv"),row.names=F)

# # Rescramble draws to match Reckoning output before outputting non-fatal results (can't do it to the fatal that feeds into the Reckoning because we want to preserve within-draw correlation between them)
# spec_combined <- merge(spec_combined,draw_map,by="run_num")

## save all
write.csv(spec_combined[,list(sex_id,year_id,run_num,hiv_deaths, non_hiv_deaths, new_hiv,hiv_births,suscept_pop,total_births,pop_neg,pop_lt200,pop_200to350,pop_gt350,pop_art,age_group_id)],paste0(out_dir,"/",parent,"_ART_data.csv"),row.names=F)


# Read in child birth prevalence data
suffix <- ".csv"
birth_dt <- rbindlist(
  mclapply(child.locs,
           function(loc) {
             for(stage in stages) {
               in.path <- paste0(out_dir_birth, "/", loc, suffix)
               if(file.exists(in.path)) {
                 out.stage <- stage
                 break
               }
             }
             dt <- fread(in.path)
           }
           , mc.cores = ncores)
)

id.vars <- c("run_num", "year", "sex_id", "age_group_id")
birth_dt <- birth_dt[, lapply(.SD, sum), by = id.vars]
birth_dt[, birth_prev_rate := birth_prev_count / gbd_pop]
id.vars = list(year = c(1970:2024), sex_id = c(1,2), age_group_id = 164, run_num = c(1:1000))
missing = assertable::assert_ids(birth_dt, id_vars = id.vars, warn_only = F)

## save birth prevalence
write.csv(birth_dt[,.(year, sex_id, run_num, age_group_id, birth_prev_rate, birth_prev_count, gbd_pop)], paste0(out_dir_birth, parent, '.csv'), row.names = F)
