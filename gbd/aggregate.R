################################################################################
## Purpose: Produce national-level results by aggregating subnationals (i.e.
#Aggregating up from the lower levels at which spectrum is run)
## Date created: 
## Date modified: July 2019 for GBD19
## Author: Austin Carter, aucarter@uw.edu
## Run instructions: 
## Notes:
################################################################################

### Setup
rm(list=ls())
windows <- Sys.info()[1]=="Windows"
root <- ifelse(windows,"J:/","/home/j/")
user <- ifelse(windows, Sys.getenv("USERNAME"), Sys.getenv("USER"))
code.dir <- paste0(ifelse(windows, "H:/", "/homes/"), user, "/hiv_gbd2019/")

## Packages
library(data.table); library(parallel); library(assertable)

## Arguments
args <- commandArgs(trailingOnly = TRUE)
if(length(args) > 0) {
  parent <- args[1]
  run.name <- args[2]
  spec.run.name <- args[3]
  ncores <- args[4]
  
} else {
  parent <- "IND"
  run.name <- "190630_rhino2"
  spec.run.name <- "190630_rhino"
  ncores <- 2
}



id.vars <- c("run_num", "year", "sex", "age")

### Paths
in.dir <- paste0("/ihme/hiv/epp_output/gbd19/", run.name, "/compiled/")
single.age.dir <- paste0('/ihme/hiv/spectrum_draws/', spec.run.name,'/detailed_deaths/')

### Functions
library(mortdb, lib = "/home/j/WORK/02_mortality/shared/r")
source(paste0(root, "Project/Mortality/shared/functions/get_age_map.r"))
source(paste0(root, "Project/Mortality/shared/functions/get_locations.r"))
source(paste0(root, "temp/central_comp/libraries/current/r/get_population.R"))

### Tables
age.table <- data.table(get_age_map(type="all"))
loc.table <- as.data.table(get_locations(hiv_metadata = T))

### Code
## Find  children
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

##Read in the first child file, then append the sum of other files to reduce memory requirements  
suffix <- ".csv"
loc_i <- child.locs[1]
if(length(child.locs) > 1){

  combined.dt <- fread(paste0(in.dir, "/", loc_i, suffix))
    for(loc in child.locs[2:length(child.locs)]){
    print(loc)
    in.path <- paste0(in.dir, "/", loc, suffix)
    dt <- fread(in.path,blank.lines.skip = T)
    dt[,pop_gt350 := as.numeric(pop_gt350)]
    combined.dt <- rbind(combined.dt,dt)[, lapply(.SD, sum), by = id.vars]
    rm(dt)
  }
  out.dt <- combined.dt
} else {
  out.dt <- fread(paste0(in.dir, "/", loc_i, suffix))
}


#Add this column to prevent future issues 
if(!("suscept_pop" %in% colnames(out.dt))){
  out.dt[,suscept_pop := pop_neg]
}

out.path <- paste0(in.dir, "/", parent, suffix)
write.csv(out.dt, out.path, row.names=F)

##Under 1 splits
suffix <- "_under1_splits.csv"
id.vars <- c("year","run_num")
  loc_i <- child.locs[1]
  combined.dt <- fread(paste0(in.dir, "/", loc_i, suffix))
  for(loc in child.locs[2:length(child.locs)]){
    print(loc)
    in.path <- paste0(in.dir, "/", loc, suffix)
    dt <- fread(in.path,blank.lines.skip = T)
    combined.dt <- rbind(combined.dt,dt)[, lapply(.SD, sum), by = id.vars]
    rm(dt)
  }
  out.dt <- combined.dt

out.path <- paste0(in.dir, "/", parent, suffix)
write.csv(out.dt, out.path, row.names=F)



# Multiply summed up India locations by ratio of Minor Territories pop to non-Minor Territories India
# Only needed if we end up putting India through EPP-ASM
if(parent == "IND_44538") {
  age.map <- fread(paste0(root, "temp/aucarter/maps/age_map.csv"))[age %in% unique(out.dt$age), .(age_group_id, age)]
  pop.locs <- loc.table[parent_id == 163, location_id]
  pop.table <- add.age.groups(get_population(age_group_id = -1,
                                             location_id = pop.locs, year_id = -1, 
                                             sex_id = 1:2, location_set_id = 79, 
                                             gbd_round_id = 6, decomp_step="step4"))
  merged.pop <- merge(pop.table, age.map, by = "age_group_id")
  merged.pop[, sex := ifelse(sex_id == 1, "male", "female")]
  merged.pop[, c("age_group_id", "sex_id") := NULL]
  setnames(merged.pop, "year_id", "year")
  
  other.dt <- copy(merged.pop[location_id != 44538])
  other.dt <- other.dt[, .(other_pop = sum(population)), by = .(year, age, sex)]
  minor.dt <- copy(merged.pop[location_id == 44538])
  merged.dt <- merge(minor.dt, other.dt, by = c("year", "age", "sex"))
  merged.dt[, ratio := population / other_pop]
  merged.dt[, age := as.integer(age)]
  
  merged.out <- merge(out.dt, merged.dt[, .(year, age, sex, ratio)], by = c("year", "sex", "age"), all.x = T)
  val.vars <- setdiff(names(merged.out), c("year", "sex", "age", "run_num", "ratio"))
  matrix <- as.matrix(merged.out[, val.vars, with = F])
  ratio <- merged.out$ratio
  ratio.matrix <- sweep(matrix, MARGIN = 1, ratio, `*`)
  ratio.dt <- as.data.table(ratio.matrix)
  bound.dt <- cbind(merged.out[, .(year, age, sex, run_num)], ratio.dt)
  out.dt <- copy(bound.dt)
  out.path <- paste0(in.dir, "/", parent, suffix)
  write.csv(out.dt, out.path, row.names=F)
}


##Unsure when this is needed 
##Compile single-age deaths for forecasting
# pop <- get_population(age_group_id = c(49:127, 161, 21), 
#                       location_id = loc.id, year_id = 1970:2019, 
#                       sex_id = 1:2, single_year_age = TRUE, gbd_round_id = 6,
#                       decomp_step = "step1")
# pop_2 <- get_population(age_group_id = c(2:4, 21), 
#                         location_id = loc.id, year_id = 1970:2019,
#                         sex_id = 1:2, gbd_round_id = 6, decomp_step = "step1")
# pop_2[age_group_id < 5, age_group_id := 161]
# pop_2 <- pop_2[, .(population = sum(population)), by = c('age_group_id', 'location_id', 'year_id', 'sex_id', 'run_id')]
# pop <- rbind(pop, pop_2, use.names = T)
# pop <- merge(pop, age.table[,.(age_group_id, age_group_name_short)], by = 'age_group_id')
# setnames(pop, c('age_group_name_short', 'year_id'), c('age', 'year'))
# pop[sex_id == 1, sex := 'male']
# pop[sex_id == 2, sex := 'female']
# pop[, age := as.integer(age)]
# 
# deaths.dt <- rbindlist(
# 	mclapply(child.locs,
# 		function(loc) {
# 			print(loc)
# 			draws <- list.files(paste0(single.age.dir, '/', loc))
# 			dt <- rbindlist(lapply(draws, function(draw){
# 				in.path <- paste0(single.age.dir, "/", loc, '/', draw)
# 				loc.dt <- fread(in.path)
# 			}))	
# 		}
# 	, mc.cores = ncores)
# )
# 
# deaths.dt <- deaths.dt[, .(deaths = sum(deaths)), by = c('age', 'year', 'sex', 'run_num')]
# deaths.dt[, age_5 := age - age%%5]
# deaths.dt[, deaths_5 := sum(deaths), by = c('age_5', 'run_num', 'sex', 'year')]
# deaths.dt[, prop := ifelse(deaths_5 == 0, 0, deaths/deaths_5)]
# 
# ##Calcualte of mort rate for single age compared to 5 year mort rate
# deaths.dt <- merge(deaths.dt, pop[,.(age, sex, year, population)], by = c('age', 'year', 'sex'))
# deaths.dt[, mort_rate := ifelse(population == 0, 0, deaths/population)]
# deaths.dt[, pop_5 := sum(population), by = c('age_5', 'run_num', 'sex', 'year')]
# deaths.dt[, mort_rate_5 := ifelse(pop_5 == 0, 0, deaths_5/pop_5)]
# deaths.dt[, single_to_5_ratio := ifelse(mort_rate_5 == 0, 0, mort_rate/mort_rate_5)]
# deaths.dt[sex == 'female', sex_id := 2]
# deaths.dt[sex == 'male', sex_id := 1]
# deaths.dt[, c('sex', 'iso3', 'deaths_5', 'population', 'mort_rate', 'mort_rate_5', 'age_5', 'pop_5') := NULL]
# assert_ids(deaths.dt, id_vars = list(sex_id = 1:2, run_num = 1:n.draws, year = unique(deaths.dt$year), age = 0:80))
# assert_values(deaths.dt, names(deaths.dt), 'not_na')
# assert_values(deaths.dt, 'prop', 'lte', 1)
# cast.dt <- dcast.data.table(deaths.dt, age + sex_id + year ~ run_num, value.var = c('prop', 'single_to_5_ratio'))
# 
# out.path <- paste0(single.age.dir, '/compiled/')
# dir.create(out.path, recursive = T, showWarnings = F)
# write.csv(cast.dt, paste0(out.path, parent, '.csv'), row.names = F)
### End