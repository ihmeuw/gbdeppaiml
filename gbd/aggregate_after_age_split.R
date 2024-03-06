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
  spec_name <- "231129_bandicoot"
  ncores <- 20
}

n.draws <- 1000
stages <- c("stage_1")
suffix <- "_ART_data.csv"
id.vars <- c("run_num", "year_id", "sex_id", "age_group_id")


### Paths
# in.dir <- paste0("/ihme/hiv/spectrum_draws/", run.name, "/compiled/")
# single.age.dir <- paste0('/ihme/hiv/spectrum_draws/', run.name,'/detailed_deaths/')
out_dir <- paste0("/ihme/hiv/spectrum_prepped/art_draws/",spec_name)
dir.create(out_dir, recursive = T, showWarnings = F)
out_dir_death <- paste0("/ihme/hiv/spectrum_prepped/death_draws/",spec_name)
dir.create(out_dir_death, recursive = T, showWarnings = F)
out_dir_birth <- paste0("/ihme/hiv/spectrum_prepped/birth_prev/",spec_name, '/')
dir.create(out_dir_birth, recursive = T, showWarnings = F)


### Functions
library(mortdb, lib = "/mnt/team/mortality/pub/shared/r/4")
source("/share/cc_resources/libraries/current/r/get_population.R")

add.age.groups <- function(pop.dt) {
  pop <- pop.dt[, c("year_id", "sex_id", "age_group_id", "population", "location_id"), with = F]
  if(!(1 %in% unique(pop$age_group_id))) {
    u5.gbd.pop <- pop[age_group_id %in% c(2,3,238,34,388,389)]
    u5.gbd.pop[, age_group_id := 1]
    u5.gbd.pop <- u5.gbd.pop[,.(population = sum(population)), by = c("year_id", "sex_id", "age_group_id", "location_id")]
    pop <- rbind(pop, u5.gbd.pop)	
  }
  if(!(21 %in% unique(pop$age_group_id))) {
    o80.gbd.pop <- pop[age_group_id %in% c(30:32, 235) ]
    o80.gbd.pop[, age_group_id := 21]
    o80.gbd.pop <- o80.gbd.pop[,.(population = sum(population)), by = c("year_id", "sex_id", "age_group_id", "location_id")]
    pop <- rbind(pop, o80.gbd.pop)	
  }
  return(pop)
}


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
combined.dt <- rbindlist(
  mclapply(child.locs,
           function(loc) {
             for(stage in stages) {
               in.path <- paste0(out_dir, "/", loc, suffix)
               if(file.exists(in.path)) {
                 out.stage <- stage
                 break
               }
             }
             dt <- fread(in.path)
           }
           , mc.cores = ncores)
)


# Add up children and write parent
combined.dt <- combined.dt[, lapply(.SD, sum), by = id.vars]
combined.dt[, non_hiv_deaths_prop := non_hiv_deaths / (non_hiv_deaths + hiv_deaths)]

print(head(combined.dt))
assert_values(combined.dt, names(combined.dt), "gte", 0)
assert_values(combined.dt, colnames(combined.dt), "not_na")

## test missing data
id.vars = list(year_id = c(1970:2024), sex_id = c(1,2), age_group_id = c(2,3,6:20, 30:32, 34, 235, 238, 388, 389), run_num = c(1:1000))
missing = assertable::assert_ids(combined.dt, id_vars = id.vars, warn_only = F)


write.csv(combined.dt[,list(sex_id,year_id,age_group_id,run_num,hiv_deaths,non_hiv_deaths, non_hiv_deaths_prop)],paste0(out_dir_death,"/",parent,"_ART_deaths.csv"),row.names=F)
write.csv(combined.dt[,list(sex_id,year_id,run_num,hiv_deaths, non_hiv_deaths, new_hiv,hiv_births,suscept_pop,total_births,pop_neg, pop_lt200,pop_200to350,pop_gt350,pop_art,age_group_id)],paste0(out_dir,"/",parent,"_ART_data.csv"),row.names=F)

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
