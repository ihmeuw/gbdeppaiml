### Setup
rm(list=ls())
windows <- Sys.info()[1][["sysname"]]=="Windows"
root <- ifelse(windows,"J:/","/home/j/")
user <- ifelse(windows, Sys.getenv("USERNAME"), Sys.getenv("USER"))

args <- commandArgs(trailingOnly = TRUE)
if(length(args) > 0) {
	run.name <- args[1]
	proj.end <- args[2]
	run.group2 <- args[3]
	decomp.step <- args[4]
	gbdyear <- args[5]
} else {
	run.name <- "240304_platypus"
	proj.end <- 2024
	run.group2 <- FALSE
	decomp.step <- "iterative"
	gbdyear <- 'gbd23'
}

## Functions
library(data.table)
library(mortdb, lib = "/mnt/team/mortality/pub/shared/r/4")
source( "/ihme/cc_resources/libraries/current/r/get_population.R")
source('/ihme/cc_resources/libraries/current/r/get_covariate_estimates.R')
source('/ihme/cc_resources/libraries/current/r/get_cod_data.R')
source("/ihme/cc_resources/libraries/current/r/get_ids.R")
source(paste0("/homes/",user,"/hiv_gbd/shared_functions/extrapolate_years.R"))

## Create input firectory
out.dir <- paste0('/ihme/hiv/epp_input/', gbdyear, '/', run.name, "/")
dir.create(out.dir, recursive = TRUE, showWarnings = TRUE)

## Locations
loc.table <- get_locations(hiv_metadata = TRUE)
write.csv(loc.table, paste0(out.dir, 'location_table.csv'), row.names = F)
age.map <- get_age_map(type = 'envelope')
write.csv(age.map, paste0(out.dir, 'age_map.csv'), row.names = F)


input.table <- fread(paste0('/share/hiv/epp_input/gbd23/input_ids.csv'))
# if(!run.name %in% input.table$run_name){
#   c.args <- input.table[run_name=='200713_yuka']
#   
# }else{
#   c.args <- input.table[run_name==run.name]
# }
ASFR <- 635
population <- 355
population_sa <- 299
# migration <- c.args[['migration']]
# mlt <- c.args[['mlt']]
# birth <- c.args[['births']]

## Determine locations
if(run.group2){
  ## Prep inputs for all estimation locations
  epp.locs <- loc.table[spectrum == 1, location_id]
  
}else{
  ## Prep inputs for standard group 1 epp locations
  epp.locs <- c(loc.table[epp == 1, location_id],loc.table[ihme_loc_id %in% c("STP","MAR","MRT","COM", 'IND_44583'), location_id])

}

id.parents <- unique(loc.table[location_id %in% epp.locs,parent_id])
id.parents.level.up <- unique(loc.table[location_id %in% id.parents,parent_id])
parent.locs.epp <- loc.table[spec_agg==1 & location_id %in% id.parents.level.up, location_id]
parent.locs <- unique(c(id.parents,parent.locs.epp))
parent.locs <- loc.table[location_id %in% parent.locs,location_id]

###########################
## Population single age
###########################
pop.all <-  get_mort_outputs(
  "population single year", "estimate",
  gbd_year = 2023,
  run_id = population_sa,
  age_group_id = c(28, 238,21 ,50:127),
  location_id = c(epp.locs, parent.locs), year_id = seq(1970, proj.end), sex_id = 1:2)
pop.all <- pop.all[,.(age_group_id, location_id, year_id, sex_id, population, run_id)]
unique(pop.all$age_group_id)

pop.o80 <-  get_mort_outputs(
  "population", "estimate",
  gbd_year = 2023,
  run_id = population,
  age_group_id = 21,
  location_id = c(epp.locs, parent.locs), year_id = seq(1970, proj.end), sex_id = 1:2)
pop.o80 <- pop.o80[,.(age_group_id, location_id, year_id, sex_id, mean, run_id)]
## fill in missing locations
setdiff(c(epp.locs, parent.locs), unique(pop.o80$location_id))
pop.44858 <-  get_mort_outputs(
  "population", "estimate",
  gbd_year = 2023,
  run_id = population,
  age_group_id =21,
  location_id = c(60908, 95069, 94364), year_id = seq(1970, proj.end), sex_id = 1:2)
pop.44858 <- pop.44858[, .(mean = sum(mean), location_id = 44858), by = c("age_group_id", "year_id", "sex_id", "run_id")]
pop.o80 <- rbind(pop.o80, pop.44858)
setnames(pop.o80, "mean", "population")

pop.all <- rbind(pop.all,  pop.o80, use.names = T)
dir.create(paste0(out.dir, '/population_single_age'), showWarnings = F)
invisible(lapply(c(epp.locs, parent.locs), function(c.location_id) {
  out.pop <- copy(pop.all[location_id == c.location_id])
  c.iso <- loc.table[location_id == c.location_id, ihme_loc_id]
  write.csv(out.pop, paste0(out.dir, '/population_single_age/', c.iso, ".csv"), row.names = F)
}))

############################################
###For India Rural-urban Splitting locations
############################################
india.locs <- loc.table[level>4 & grepl("IND", ihme_loc_id) ,location_id]
pop.all <-  get_mort_outputs(
  "population single year", "estimate",
  gbd_year = 2023,
  run_id = population_sa,
  age_group_id = c(28, 238 ,50:127),
  location_id = india.locs, year_id = seq(1970, proj.end), sex_id = 1:2)
pop.all <- pop.all[,.(age_group_id, location_id, year_id, sex_id, population, run_id)]
pop.o80 <-  get_mort_outputs(
  "population", "estimate",
  gbd_year = 2023,
  run_id = population,
  age_group_id = 21,
  location_id = india.locs, year_id = seq(1970, proj.end), sex_id = 1:2)
pop.o80 <- pop.o80[,.(age_group_id, location_id, year_id, sex_id, mean, run_id)]
setnames(pop.o80, "mean", "population")

pop.all <- rbind(pop.all,  pop.o80, use.names = T)

dir.create(paste0(out.dir, '/population_single_age/india_splitting_locs/'), showWarnings = F)
invisible(lapply(india.locs, function(c.location_id) {
  out.pop <- copy(pop.all[location_id == c.location_id])
  c.iso <- loc.table[location_id == c.location_id, ihme_loc_id]
  write.csv(out.pop, paste0(out.dir, '/population_single_age/india_splitting_locs/', c.iso, ".csv"), row.names = F)
}))

############################################
### Population
############################################
pop <-  get_mort_outputs(
  "population", "estimate",
  gbd_year = 2023,
  run_id = population,
  age_group_id =c(8:20),
  location_id = c(epp.locs, parent.locs), year_id = seq(1970, proj.end), sex_id = 1:2)
## add missing location
pop.44858 <-  get_mort_outputs(
  "population", "estimate",
  gbd_year = 2023,
  run_id = population,
  age_group_id =c(8:20),
  location_id = c(60908, 95069, 94364), year_id = seq(1970, proj.end), sex_id = 1:2)
pop.44858 <- pop.44858[, .(mean = sum(mean), location_id = 44858), by = c("age_group_id", "year_id", "sex_id", "run_id")]
pop <- rbind(pop, pop.44858, fill = T)
setnames(pop, 'mean', 'population')
pop <- pop[,.(age_group_id, location_id, year_id, sex_id, population, run_id)]

dir.create(paste0(out.dir, '/population'), showWarnings = F)
invisible(lapply(c(epp.locs, parent.locs), function(c.location_id) {
  out.pop <- copy(pop[location_id == c.location_id])
  c.iso <- loc.table[location_id == c.location_id, ihme_loc_id]
  write.csv(out.pop, paste0(out.dir, '/population/', c.iso, ".csv"), row.names = F)
}))

############################################
### Population Split
############################################
pop.splits <-  get_mort_outputs(
  "population", "estimate",
  gbd_year = 2023,
  run_id = population,
  age_group_id = c(2,3,30,31,32,34,235,238,388,389),
  location_id = c(epp.locs, parent.locs), year_id = seq(1970, proj.end), sex_id = 1:2)
## add missing location
pop.44858 <-  get_mort_outputs(
  "population", "estimate",
  gbd_year = 2023,
  run_id = population,
  age_group_id = c(2,3,30,31,32,34,235,238,388,389),
  location_id = c(60908, 95069, 94364), year_id = seq(1970, proj.end), sex_id = 1:2)
pop.44858 <- pop.44858[, .(mean = sum(mean), location_id = 44858), by = c("age_group_id", "year_id", "sex_id", "run_id")]
pop.splits <- rbind(pop.splits, pop.44858, fill = T)
setnames(pop.splits, "mean", "population")
pop.splits <- pop.splits[,.(age_group_id, location_id, year_id, sex_id, population, run_id)]

dir.create(paste0(out.dir, '/population_splits'), showWarnings = F)
invisible(lapply(c(epp.locs,id.parents), function(c.location_id) {
  c.iso <- loc.table[location_id == c.location_id, ihme_loc_id]
  write.csv(pop.splits[location_id == c.location_id], paste0(out.dir, '/population_splits/', c.iso, ".csv"), row.names = F)
}))

###For India Rural-urban Splitting locations
india.locs <- loc.table[level>4 & grepl("IND", ihme_loc_id) ,location_id]
pop.splits <-  get_mort_outputs(
  "population", "estimate",
  gbd_year = 2023,
  run_id = population,
  age_group_id = c(2,3,30,31,32,34,235,238,388,389),
  location_id = india.locs, year_id = seq(1970, proj.end), sex_id = 1:2)
setnames(pop.splits, "mean", "population")
dir.create(paste0(out.dir, '/population_splits/'), showWarnings = F)
invisible(lapply(india.locs, function(c.location_id) {
  c.iso <- loc.table[location_id == c.location_id, ihme_loc_id]
  write.csv(pop.splits[location_id == c.location_id], paste0(out.dir, '/population_splits/', c.iso, ".csv"), row.names = F)
}))

############################################
#### Migration
############################################
mig <- fread(paste0('/mnt/team/fertilitypop/pub/population/popReconstruct/359/upload/net_migration_single_year.csv'))[measure_id==55]
if(max(mig$year_id) < proj.end){
  trans_vars = c("mean")
  id_vars = c("age_group_id", 'location_id', 'sex_id', 'measure_id')
  mig = extrapolate_years(mig,years_to_average = 5, end_year = proj.end, id_vars = id_vars, trans_vars=trans_vars)
}
mig <- mig[,.(location_id, year_id, sex_id, age_group_id, measure_id, mean)]
age_groups <- get_ids("age_group")
age_groups[age_group_name=="<1 year",age_group_name := "0"]
age_groups <- age_groups[age_group_id != 238,]
age_groups[age_group_id == 49,age_group_id := 238]

mig = merge(mig,age_groups, by='age_group_id')
mig <- mig[age_group_name %in% c(0:95)]
mig$age_group_name <- as.integer(mig$age_group_name)
mig = merge(mig,loc.table[,.(location_id,ihme_loc_id)],by="location_id")
mig = mig[,c('age_group_id','measure_id'):=NULL]
setnames(mig, c('year_id', 'sex_id','mean','age_group_name'), c('year', 'sex','value','age'))

mig[age > 80, age := 80]
mig <- mig[year >= 1970, .(value = sum(value)), by = c('age', 'sex', 'year', 'ihme_loc_id')]
dir.create(paste0(out.dir, '/migration'), showWarnings = F)
invisible(lapply(epp.locs, function(c.location_id){
  c.iso <- loc.table[location_id == c.location_id, ihme_loc_id]
  mig.loc <- mig[ihme_loc_id == c.iso]
  if(nrow(mig.loc) == 0){
    if(loc.table[ihme_loc_id == c.iso, level] > 3){
      ## Crude population split of migration for subnationals -- this is temporary until pop model outputs subnationals
      parent.iso <- substr(c.iso, 1, 3)
      parent.pop <- fread(paste0(out.dir, '/population_single_age/', parent.iso, '.csv'))
      child.pop <- fread(paste0(out.dir, '/population_single_age/', c.iso, '.csv'))
      setnames(parent.pop, "population", "parent")
      setnames(child.pop, "population", "child")
      merged.pop <- merge(parent.pop, child.pop, by = c("age_group_id", "year_id", "sex_id"))
      collapsed.pop <- merged.pop[, lapply(.SD, sum), by = .(year_id, sex_id), .SDcols = c("parent", "child")]
      collapsed.pop[, prop := child / parent]
      setnames(collapsed.pop, c('year_id', 'sex_id'), c('year', 'sex'))
      mig.loc <- mig[ihme_loc_id == parent.iso]
      mig.loc <- merge(mig.loc, collapsed.pop, by = c('year', 'sex'))
      mig.loc[, value := value * prop]
      mig.loc[, c('parent', 'child', 'prop', 'ihme_loc_id') := NULL]
    } else{
      mig.loc <- data.table(expand.grid(age = 0:80, sex = 1:2, year = 1970:proj.end, ihme_loc_id = c.iso, value = 0))
    }
  }
  mig.loc <- mig.loc[,.(year,sex,age,value)]
  write.csv(mig.loc, paste0(out.dir, '/migration/', c.iso, '.csv'), row.names = F)
}))

#############################################################
## ASFR
#############################################################
asfr <- get_mort_outputs(model_name = 'ASFR', 
                         model_type = 'estimate', 
                         run_id = ASFR, 
                         location_id = epp.locs)
## add missing location
asfr.44858 <- get_mort_outputs(model_name = 'ASFR', 
                         model_type = 'estimate', 
                         run_id = ASFR, 
                         location_id = c(60908, 95069, 94364))
asfr.44858 <- asfr.44858[,.(location_id=44858, ihme_loc_id = "ETH_44858", 
                            mean= mean(mean), lower= mean(lower), upper=mean(upper)),
                         by = c("year_id", "age_group_id")]
asfr <- rbind(asfr, asfr.44858, fill=T)
setnames(asfr, c('mean', 'lower', 'upper'), c('mean_value','lower_value', 'upper_value'))
asfr[,sex_id := 2]
asfr <- data.table(asfr)

asfr <- asfr[age_group_id %in% c(8:14) & sex_id == 2, list(year_id, age_group_id, mean_value, location_id)]
asfr[, age := (age_group_id - 5) * 5]
setnames(asfr, c('mean_value', 'year_id'), c('value', 'year'))
dir.create(paste0(out.dir, '/ASFR'))
invisible(lapply(epp.locs, function(c.location_id){
  c.iso <- loc.table[location_id == c.location_id, ihme_loc_id]
  write.csv(asfr[location_id == c.location_id, list(value, age, year)], paste0(out.dir, '/ASFR/', c.iso, '.csv'), row.names = F)
}))

#############################################################
## Births and SRB
#############################################################
births <-  get_mort_outputs(
  "birth", "estimate",
  gbd_year = 2021,
  run_id = "best",
  ##all ages 10-54
  age_group_id = 169,
  location_id = c(epp.locs, parent.locs), year_id = seq(1970, proj.end), sex_id = 1:2)
setdiff(epp.locs, unique(births$location_id))
# # add missing locs
# births.44858 <-  get_mort_outputs(
#   "birth", "estimate",
#   gbd_year = 2023,
#   run_id = "best",
#   ##all ages 10-54
#   age_group_id = 169,
#   location_id = c(60908, 95069, 94364), 
#   year_id = seq(1970, proj.end), 
#   sex_id = 1:2)
# births.44858 = births.44858[,.(mean=sum(mean), location_id=44858, ihme_loc_id="ETH_44858"), 
#                             by = c("run_id", "year_id", "sex_id", "age_group_id")]
# births <- rbind(births, births.44858, fill =T)
setnames(births, 'mean', 'population')
births = extrapolate_years(births, years_to_average = 2, end_year = proj.end, trans_vars = "population", id_vars = c("age_group_id","location_id","sex_id","run_id"))
births[,population := sum(population), by = c('location_id', 'year_id', 'sex_id')]
births[,age_group_id := 164]
births <- unique(births[,.(age_group_id, location_id, year_id, sex_id, population, run_id)])

dir.create(paste0(out.dir, '/births'), showWarnings = F)
dir.create(paste0(out.dir, '/SRB'), showWarnings = F)
invisible(lapply(epp.locs, function(c.location_id) {
  print(c.location_id)
  out.births <- copy(births[location_id == c.location_id])
  c.iso <- loc.table[location_id == c.location_id, ihme_loc_id]
  births.dt <- out.births[,.(population = sum(population)), by = c('age_group_id', 'location_id', 'year_id', 'run_id')]
  write.csv(births.dt, paste0(out.dir, '/births/', c.iso, ".csv"), row.names = F)
  out.births[,sex := ifelse(sex_id == 1, 'male', 'female')]
  out.births[,sex_id := NULL]
  srb.dt <- dcast.data.table(out.births, year_id + location_id + run_id ~ sex, value.var = 'population')
  srb.dt[, male_srb := male/(female + male)]
  srb.dt[, female_srb := female/(female + male)]
  srb.dt[,c('female', 'male') := NULL]
  write.csv(srb.dt, paste0(out.dir, '/SRB/', c.iso, ".csv"), row.names = F)
}))


#############################################################
## Save inputs versions
#############################################################
input.table <- rbind(input.table,
                     data.table(run_name = run.name,
                                asfr = unique(ASFR),
                                population = population,
                                population_sa = population_sa,
                                births = unique(births$run_id),
                                migration = 264,
                                mlt = NA, 
                                stgpr = NA, 
                                crosswalk_id = NA, 
                                bundle_id = NA))
write.csv(input.table, '/share/hiv/epp_input/gbd23/input_ids.csv', row.names = F)



## Prep CoD data and case notifications
if(run.group2 == TRUE){
  cod.dt <- get_cod_data(cause_id = 298, decomp_step = decomp.step)
  cod.dt <- cod.dt[data_type == 'Vital Registration']
  cod.dt <- cod.dt[, list(model = 'VR', type = 'point', deaths = sum(deaths), rate = weighted.mean(x = rate, w = pop)), by = .(location_id, year, age_group_id, sex)]
  cod.dt <- cod.dt[age_group_id != 27]
  cod.dt <- melt(cod.dt, id.vars = c('location_id', 'age_group_id', 'type', 'year', 'sex', 'model'))
  setnames(cod.dt, c('variable', 'value'), c('metric', 'mean'))
  cod.dt[, c('lower', 'upper') := .(NA, NA)]
  cod.dt <- merge(cod.dt, loc.table[, list(ihme_loc_id, location_id)], by = c('location_id'))
  cod.dt[, location_id := NULL]
  cod.dt[metric == 'deaths', metric := 'Count']
  cod.dt[metric == 'rate', metric:= 'Rate']
  cod.dt[, indicator := 'Deaths']
  setnames(cod.dt, 'sex', 'sex_id')
  cod.dt[, sex := ifelse(sex_id == 1, 'male', 'female')]
  cod.dt[,sex_id := NULL]
  cod.dt <- merge(cod.dt, age.map[,.(age_group_id, age = age_group_name_short)], by = 'age_group_id')
  
  
  ########################
  ##RIGHT NOW RUN.GROUP2 == FALSE BUT WOULD THIS NEED TO BE UPDATED IF IT WEREN'T?
  diagn.dt <- fread(paste0('/ihme/hiv/results_comparison/comparison_data/high_income_incidence_extractions/combined_high_income_cases_outlier.csv'))
  diagn.dt <- diagn.dt[outlier == 0, .(sex_id, ihme_loc_id, mean = cases, year, age = 'All Ages', age_group_id = 22, model = 'Case Report', type = 'point', metric = 'Count', indicator = 'Incidence', lower = NA, upper = NA)]
  diagn.dt[sex_id == 3, sex := 'both']
  diagn.dt[sex_id == 2, sex := 'female']
  diagn.dt[sex_id == 1, sex := 'male']
  diagn.dt[, sex_id := NULL]
  
  group2.fitdata <- rbind(cod.dt, diagn.dt, use.names = T)
  dir.create(paste0('/share/hiv/epp_input/gbd19/', run.name, '/fit_data/'), showWarnings = F, recursive = T)
  invisible(lapply(unique(group2.fitdata$ihme_loc_id), function(loc){
    write.csv(group2.fitdata[ihme_loc_id == loc], paste0('/share/hiv/epp_input/', gbdyear, '/', run.name, '/fit_data/', loc, '.csv'), row.names = F)
  }))

}


  
  
  
  
  
  
