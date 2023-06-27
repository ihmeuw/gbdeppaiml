##Prep inputs
rm(list = ls())
print('started script')
Sys.umask(mode = "0002")
user <- Sys.info()[["user"]]
jpath <- ifelse(Sys.info()[1]=="Windows", "J:/", "/home/j/")
hpath <- ifelse(Sys.info()[1]=="Windows", "H:/", paste0("/homes/", user, "/"))
print('paths set')


### Functions
library(mortdb, lib = "/mnt/team/mortality/pub/shared/r/4")
library(assertable)
print('read in mortdb')
source(paste0(hpath,"/hiv_forecasting_inputs/find_child_locs.R"))
print('find child locs read in')
invisible(sapply(list.files("/share/cc_resources/libraries/current/r/", full.names = T), source))
print('ihme funcs read')
library(data.table); library(ggplot2); library(parallel); library(assertable)
print('libs read in')
### Tables
loc.table <- data.table(get_locations(hiv_metadata = T, level = "all", gbd_year = 2020))
run.list <- loc.table[spectrum == 1 & !grepl('NOR_6', ihme_loc_id) & !(grepl('GBR', ihme_loc_id) & most_detailed == 0), ihme_loc_id]
# forecast_loc_table <- get_location_metadata(location_set_id = 39, gbd_round_id = 7, decomp_step = 'iterative')

args <- commandArgs(trailingOnly = TRUE)
run.name <- args[1]
max.year <- as.integer(args[2])
last_gbd_run <- args[3]
transition.year <- as.integer(args[4])
cores <- as.integer(args[5])

print(cores)

### Arguments
if (is.na(args[1])) {
  run.name <- "230620_falcon"
  max.year <- 2050
  last_gbd_run <- "200713_yuka"
  transition.year <- 2021
  cores <- 10
}
population <- T
migration <- T
asfr <- T
births <- T
static_inputs <- T
gbdyear = 'gbd20'

invisible(sapply(list.files("/share/cc_resources/libraries/current/r/", full.names = T), source))
out.dir <- paste0('/ihme/hiv/epp_input/', gbdyear, '/', run.name, "/")
plot.dir <- paste0("/share/hiv/spectrum_plots/",run.name,"_forecasting/inputs/")
dir.create(plot.dir,recursive = TRUE)
age.map <- get_age_map(gbd_year = 2019, type = "all")

###This will get updated based on what demographics provides us
fp_map <- fread(paste0('/ihme/hiv/spectrum_input/20220418_forecasting/fp_map.csv'))

##need to aggregate all of the locations that we don't run individually
# agg.locs <- c(loc.table[spec_agg == 1 & grepl('KEN', ihme_loc_id), ihme_loc_id], 'IDN', 'IND_44538',  'ZAF', 'NGA', 'ETH', 'IND', 'USA', 'BRA', 'CHN')




####################
### POPULATIONS  ##
####################
# 5-year-age-group population
# Make sure run_id for GBD population corresponds with forecasted population
pop <- fread(fp_map[run.name == run.name & var == 'population',fp])
pop <- pop[year_id >= transition.year]
pop[age_group_id %in% c(2,3,4,5), age_group_id := 1]
pop[age_group_id %in% c(30, 31,32,235), age_group_id := 21]
pop <- pop[,.(population = sum(population)), by = c('year_id', 'age_group_id', 'location_id', 'sex_id', 'scenario')]

setnames(pop, 'population', 'mean')
gbd.pop.dt <-  get_mort_outputs(
  "population", "estimate",
  run_id = 271,
  age_group_id = c(1,6:21),
  location_id = unique(c(loc.table[level==3,location_id], 
                         loc.table[ihme_loc_id %in% run.list, location_id])), sex_id = 1:2)
gbd.pop.dt = gbd.pop.dt[year_id >= 1970 & year_id < transition.year]
gbd.pop.dt[,upload_population_estimate_id := NULL]
gbd.pop <- gbd.pop.dt[,.(age_group_id, location_id, year_id, sex_id,  mean, run_id)]
setnames(gbd.pop, 'mean', 'population')
gbd.pop.save <- copy(gbd.pop)
gbd.pop[,age := ifelse(age_group_id == 1, 0, (age_group_id - 5) * 5)]
setnames(gbd.pop, c('year_id', 'sex_id', 'population'), c('year', 'sex', 'value'))
gbd.pop[,c("run_id","age_group_id"):= NULL]
nat.pop = gbd.pop[location_id %in% unique(loc.table[level==3,location_id])]
gbd.pop = gbd.pop[location_id %in% loc.table[ihme_loc_id %in% run.list, location_id]]

pop <- pop[year_id <= max.year,]
pop <- pop[,.(location_id,year_id,sex_id,age_group_id,mean)]
pop <- pop[age_group_id %in% c(1,6:21) & sex_id != 3]
pop[,age := ifelse(age_group_id == 1, 0, (age_group_id - 5) * 5)]
pop[,age_group_id := NULL]
setnames(pop, c('year_id', 'sex_id', 'mean'), c('year', 'sex', 'value'))
nat.pop.future = pop[location_id %in% unique(loc.table[level==3,location_id]) & year > max(nat.pop$year)]
nat.pop = rbind(nat.pop, nat.pop.future)

## Get single year age pops to calculate age splits
## Forecasting doesn't currently generate single year age population in the future
## So we take the age splits of the final GBD year of population and apply them to 5-year age groups in the future
pop.all <-  get_mort_outputs(
  "population single year", "estimate",
  run_id = 271,
  age_group_id = c(28, 238,21 ,50:127),
  location_id = loc.table[ihme_loc_id %in% run.list, location_id], sex_id = 1:2)
pop.all = pop.all[year_id >= 1970 & year_id < transition.year]
pop.all[,upload_population_single_year_estimate_id := NULL]
setnames(gbd.pop.dt, 'mean', 'population')
pop.all = rbind(pop.all, gbd.pop.dt[age_group_id == 21], use.names = T)
age.prop = pop.all
age.prop[age_group_id == 28, age := 0]
age.prop[age_group_id == 238, age := 1]
age.prop[age_group_id == 21, age := 80]
age.prop[is.na(age), age := age_group_id - 48]
age.prop[,age_5 := age - age%%5]
age.five = age.prop[,.(population_5 = sum(population)), by= c('sex_id', 'age_5', 'year_id', 'location_id')]
age.prop = merge(age.prop, age.five, by = c('sex_id', 'age_5', 'year_id', 'location_id'))
age.prop[,pop_prop := population/population_5]
age.prop = age.prop[,.(year_id, sex_id, age_group_id, location_id, age_5, pop_prop)]
max.prop = age.prop[year_id == max(age.prop$year_id)]
for(c.year in (max(age.prop$year)+1):max.year){
  max.prop[,year_id := c.year]
  age.prop = rbind(age.prop, max.prop)
}


if(population){

  print("pop")
  dir.create(paste0(out.dir, "/population/"), showWarnings = FALSE, recursive = TRUE)
  dir.create(paste0(out.dir, "/population_single_age/"), showWarnings = FALSE, recursive = TRUE)
  for(loc in run.list){
    ### Bind past and future populations  ##
    ########################################
    
      print(loc)
      
      loc_id <- loc.table[ihme_loc_id==loc, location_id]
      parent_id <- loc.table[ihme_loc_id==loc, parent_id]
      pop_loc_future <- pop[location_id==loc_id]
      
      ##Bind 1970:2019
      pop_loc_current <- gbd.pop[location_id == loc_id]
      pop_loc <- rbind(pop_loc_current[,.(year,sex,value,age,location_id)],pop_loc_future[year > max(pop_loc_current$year)])
      pop_loc = pop_loc[order(year),.(year,sex,age,value)]

      assert_ids(pop_loc , list(year = 1970:max.year, sex = 1:2, age = seq(0,80,by=5)))
      assert_values(pop_loc , names(pop_loc), test = "not_inf")
      assert_values(pop_loc , names(pop_loc), test = "not_nan")
      assert_values(pop_loc , names(pop_loc), test = "not_na")

      write.csv(pop_loc,paste0(out.dir,'/population/', loc, '.csv'),row.names = FALSE)
    
      #single year age population
      setnames(pop_loc, c('year', 'sex','age', 'value'), c('year_id', 'sex_id', 'age_5', 'population'))
      loc.all = age.prop[location_id == loc_id]
      loc.all = merge(loc.all, pop_loc, by = c('year_id', 'sex_id', 'age_5'))
      loc.all[, population := pop_prop*population]
      loc.all[, c('age_5', 'pop_prop') := NULL]
      assert_ids(loc.all , list(year_id = 1970:max.year, sex_id = 1:2, age_group_id = c(28, 21, 238, 50:127)))
      assert_values(loc.all , names(loc.all), test = "not_inf")
      assert_values(loc.all , names(loc.all), test = "not_nan")
      assert_values(loc.all , names(loc.all), test = "not_na")
      write.csv(loc.all, paste0(out.dir, "/population_single_age/", loc, ".csv"), row.names = F)
  }
}

####################
###  MIGRATION  ####
####################

if(migration){
  ##Migration gets updated as per GBD
  ##from Tahiya via email, 119 is the best migration estimates, which I think follows from 35 of the terminator run, which is 271 of population estimates
  # Missing subnationals - population splitting nationals
  # "No_mig" list is same as prep_inputs_forecasting.R in the hiv_forecasting_inputs repo, where we set migration to 0 in the future
  gbd_mig <- fread(paste0("/mnt/team/fertilitypop/pub/population/popReconstruct/271/upload/net_migration_single_year.csv"))[year_id >= 1970]
  gbd_mig <- gbd_mig[year_id < transition.year]
  gbd_mig_copy = merge(gbd_mig,loc.table[,.(location_id,ihme_loc_id,parent_id)],by="location_id")
  gbd_mig_parents = copy(gbd_mig_copy)[,list(mean = sum(mean)), by = c("year_id","age_group_id","parent_id","measure_id","sex_id")]
  gbd_mig_parents[,location_id := parent_id]
  gbd_mig_parents[,parent_id := NULL]
  gbd_mig <- rbind(gbd_mig,gbd_mig_parents[!(location_id %in% unique(gbd_mig$location_id))])
  gbd_mig = merge(gbd_mig,loc.table[,.(location_id,ihme_loc_id)],by="location_id")
  
  dir.create(paste0(out.dir,'/migration/'), showWarnings = F, recursive=TRUE)
  migration <- fread(fp_map[run.name == run.name & var == 'migration',fp])[year_id <= max.year & year_id >= transition.year]
 # mclapply(run.list, function(loc){
  for(loc in run.list){
    print(loc)
    loc_id <- loc.table[ihme_loc_id == loc, location_id]
    no_mig <- c("MHL", "AND", "DMA", "ASM","BMU", "GRL", "MNP","COK", "MCO" ,"NRU" ,"NIU" ,
                "PLW", "KNA" ,"SMR", "TKL" ,"TUV")
    
    mig <- copy(gbd_mig[measure_id==19 & sex_id %in% c(1,2) & location_id == loc_id])
    age_groups <- age.map[,.(age_group_id, age_group_name)]
    age_groups[age_group_name=="<1 year",age_group_name := "0"]
    age_groups[age_group_id == 238,age_group_name := "1"]
    age_groups <- age_groups[age_group_id != 5,]
    mig = merge(mig,age_groups, by='age_group_id')
    mig.o80 = mig[age_group_id %in% 128:237]
    mig.o80 = mig.o80[,.(age_group_id = 21, location_id = loc_id, mean = sum(mean), ihme_loc_id = loc, age_group_name = '80'), by = c('year_id', 'sex_id', 'measure_id')]
    mig = mig[!age_group_id %in% 128:237]
    mig = rbind(mig, mig.o80, use.names = T)
    mig = mig[,c('age_group_id','measure_id'):=NULL]
    setnames(mig, c('year_id', 'sex_id','mean','age_group_name'), c('year', 'sex','value','age'))
    
   if(!loc %in% no_mig){
     if(loc_id %in% unique(migration$location_id)){
      future.mig = migration[location_id == loc_id]
      future.mig = merge(future.mig,age_groups, by='age_group_id')
      mig.o80 = future.mig[age_group_id %in% 128:237]
      mig.o80 = mig.o80[,.(age_group_id = 21, location_id = loc_id, value = sum(value), age_group_name = '80'), by = c('year_id', 'sex_id')]
      future.mig = future.mig[!age_group_id %in% 128:237]
      future.mig = rbind(future.mig, mig.o80, use.names = T)
      future.mig =  future.mig[,c('age_group_id'):=NULL]
      setnames(future.mig, c('year_id', 'sex_id','age_group_name'), c('year', 'sex','age'))
      mig[,c('ihme_loc_id') := NULL]
      mig = rbind(mig, future.mig, use.names = T)
     }else{
       #For subnationals not in the migration file, split national
       parent.id <- loc.table[ihme_loc_id == substr(loc, 1, 3), location_id]
       child.id <- loc.table[ihme_loc_id == loc, location_id]
       future.mig = migration[location_id == parent.id]
       future.mig = merge(future.mig,age_groups, by='age_group_id')
       mig.o80 = future.mig[age_group_id %in% 128:237]
       mig.o80 = mig.o80[,.(age_group_id = 21, location_id = loc_id, value = sum(value), age_group_name = '80'), by = c('year_id', 'sex_id')]
       future.mig = future.mig[!age_group_id %in% 128:237]
       future.mig = rbind(future.mig, mig.o80, use.names = T)
       future.mig =  future.mig[,c('age_group_id'):=NULL]
       setnames(future.mig, c('year_id', 'sex_id','age_group_name'), c('year', 'sex','age'))
       
       parent.pop <- nat.pop[location_id == parent.id]
       setnames(parent.pop, "value", "parent")
       
       child.pop <- fread(paste0(out.dir,'/population/', loc, '.csv'))
       setnames(child.pop, "value", "child")
       parent.pop[,location_id := NULL]
       merged.pop <- merge(parent.pop, child.pop, by = c("age", "year", "sex"))
       
       collapsed.pop <- merged.pop[, lapply(.SD, sum), by = .(year, sex, age), .SDcols = c("parent", "child")]
       collapsed.pop[,prop := ifelse(parent == 0, 0, child / parent)]
       setnames(collapsed.pop, 'age','age_5')
       future.mig[,age := as.numeric(age)]
       future.mig[,age_5 := age - age%%5]
       future.mig = merge(future.mig, collapsed.pop[,.(year, sex, age_5, prop)], by = c('year', 'sex', 'age_5'))
       future.mig[, value := value * prop]
       future.mig[,c('prop', 'age_5') := NULL]
       mig[,c('ihme_loc_id') := NULL]
       mig = rbind(mig, future.mig, use.names = T)
       mig[,location_id := loc_id]
     }
   }
    
    if(loc %in% no_mig){
      sub.dt <- mig
      ##Extend using 0 value in places with no parent or child file
      max.obs <- max(sub.dt$year)
      ext.years <- setdiff(max(unique(sub.dt$year)):max.year,unique(sub.dt$year))
      
      for(y in ext.years){
        extend.dt <- sub.dt[year==max.obs]
        extend.dt <- extend.dt[,year:=y]
        extend.dt[,value := 0]
        sub.dt <- rbind(sub.dt,extend.dt, fill = T)
      }
      sub.dt[is.na(value), value := 0]
      mig <- sub.dt[,.(age,sex,year,value)]
      mig[,age := as.integer(age)]
    } 
    mig <- mig[order(year,age,sex)]
    mig[,age := as.integer(age)]
    id.vars = list(age = c(0:80), sex = 1:2, year = 1970:max.year)
    assertable::assert_ids(mig, id.vars)
    write.csv(mig,paste0(out.dir,'/migration/', loc, '.csv'), row.names = FALSE)
  }
}

####################
###    ASFR     ####
####################  
# For missing subnats - using national ASFR forecasts multiplied by ratio of subnat:national in max year of subnat estimates
# Same as what was done in prep_inputs_forecasting.R in hiv_forecasting_inputs repo
if(asfr){
  dir.create(paste0(out.dir,'/ASFR/'), showWarnings = T, recursive=TRUE)
  asfr_fp <- fp_map[run.name == run.name & var == 'asfr', fp]
  asfr_forecasts <- fread(asfr_fp)[scenario == 0 & statistic == 'mean',]
  asfr_forecasts <- asfr_forecasts[year_id <= max.year]

  # asfr_gbd <- get_mort_outputs(model_name = 'ASFR', model_type = 'estimate', gbd_round_id = 7, location_id = loc.table[ihme_loc_id %in% run.list, location_id])
  asfr_gbd <- get_covariate_estimates(covariate_id = 13, 
                                     location_id = loc.table[ihme_loc_id %in% run.list | level == 3, location_id],
                                     gbd_round_id=7, 
                                     decomp_step = "iterative")
  asfr_forecasts = asfr_forecasts[year_id >= transition.year]
  asfr_gbd = asfr_gbd[year_id <= transition.year]
  asfr_gbd <- asfr_gbd[age_group_id %in% c(8:14) & sex_id == 2, 
                       list(year_id, age_group_id, value=mean_value, location_id)]
  
  asfr_gbd[, age := (age_group_id - 5) * 5]
  asfr_gbd[,age_group_id := NULL]
  
  for(loc in run.list){
# mclapply(run.list, function(loc){
    print(loc)
    
    parent_id <- loc.table[ihme_loc_id==substr(loc,1,3),location_id]
    loc_id <- loc.table[ihme_loc_id==loc,location_id]
    
    if(loc_id %in% unique(asfr_forecasts$location_id) | parent_id %in% unique(asfr_forecasts$location_id)){
      
        if(nchar(loc) > 3 & !(loc_id %in% unique(asfr_forecasts$location_id))){
        # Subnationals
        asfr <- asfr_forecasts[location_id==parent_id]
        asfr <- asfr[age_group_id %in% c(8:14)]
        asfr <- asfr[, age := (age_group_id - 5) * 5]
        asfr <- asfr[age %in% 15:49]
        asfr[,c('age_group_id','location_id','scenario', 'statistic') := NULL]
        
        sub.dt <- asfr_gbd[location_id==loc_id]
        setnames(sub.dt,"value","sub_value")

        merge.dt <- merge(asfr[,.(age,year_id,value)],sub.dt[,.(age,year_id,sub_value)],by=c("age","year_id"),all.x=TRUE)
        merge.dt[,prop := sub_value/value]
        
        prop_max <- merge.dt[year_id == max(sub.dt$year_id)]
        ext_dt <- merge.dt[year_id > max(sub.dt$year_id)]
        ext_dt <- merge(ext_dt[,prop := NULL],prop_max[,.(age,prop)],by=c("age"))
        ext_dt[,sub_value := value * prop]
        asfr <- rbind(sub.dt[,c('location_id') := NULL],ext_dt[,c('value','prop') := NULL])
        setnames(asfr,'sub_value','value')
        
      } else {
        sub.dt <- asfr_gbd[location_id==loc_id]
        sub.dt[,c('location_id') := NULL]
        
        asfr = asfr_forecasts[location_id==loc_id & year_id > max(sub.dt$year_id)]
        asfr <- asfr[age_group_id %in% c(8:14)]
        asfr <- asfr[, age := (age_group_id - 5) * 5]
        asfr <- asfr[age %in% 15:49]
        asfr[,c('age_group_id','location_id','scenario', 'statistic') := NULL]
        asfr <- rbind(asfr,sub.dt,use.names=TRUE)
      }
      
    } else {
      
      asfr = asfr_gbd[location_id == loc_id] 
      max.obs <- max(asfr$year_id)
      ext.years <- setdiff(max(unique(asfr$year_id)):max.year,unique(asfr$year_id))
      
      if(max.obs < max.year){
        for(y in ext.years){
          extend.dt <- asfr[year_id==max.obs]
          extend.dt <- extend.dt[,year_id:=y]
          asfr <- rbind(asfr,extend.dt)
        }
      }
      
    }
    if(any(colnames(asfr) == 'year_id')){
      setnames(asfr,"year_id","year")
      
    }
    asfr <- asfr[order(year,age)]
    
    id.vars = list(year = 1950:max.year, age = seq(15,45,5))
    assert_ids(asfr,id.vars)
    assert_values(asfr,names(asfr), test = "not_na")
    assert_values(asfr, names(asfr), test = "not_inf")
    assert_values(asfr, names(asfr), test = "not_nan")
    write.csv(asfr, paste0(out.dir,'/ASFR/', loc, '.csv'), row.names = F)
  }
}
####################
### BIRTHS & SRB####
####################
## Forecasted births - currently using age 0 population intercept shifted to 2022 births
if(births){
  dir.create(paste0(out.dir, '/births'), showWarnings = F)
  dir.create(paste0(out.dir, '/SRB'), showWarnings = F)
  births <-  get_mort_outputs(
    "birth", "estimate",
    gbd_year = 2020,
    run_id = 'best',
    ##all ages 10-54
    age_group_id = 169,
    location_id = loc.table[ihme_loc_id %in% run.list, location_id], year_id = seq(1970, max.year), sex_id = 1:2)
  setnames(births, 'mean', 'population')
  births = births[,population := sum(population), by = c('location_id', 'year_id', 'sex_id')]
  births[,age_group_id := 164]
  births <- unique(births[,.(age_group_id, location_id, year_id, sex_id, population, run_id)])
  ext.births = births[year_id == max(pop.all$year_id)]
  ext.births = merge(ext.births, pop.all[age==0 & year_id == max(year_id), .(location_id, sex_id, age_0 = population)], by = c('location_id', 'sex_id'))
  ext.births[,prop := population/age_0]
  for(loc in run.list){
    print(loc)
    loc_id = loc.table[ihme_loc_id == loc, location_id]
    # bring in future pop
    age0.pop = fread(paste0(out.dir, '/population_single_age/', loc, '.csv'))[age_group_id == 28 & year_id > max(births$year_id)]
    age0.pop = merge(age0.pop, ext.births[location_id == loc_id, .(prop, sex_id)], by = c('sex_id'))
    age0.pop[,population := population * prop]
    age0.pop[,prop := NULL]
    age0.pop[,run_id := unique(births$run_id)]
    loc.births = rbind(births[location_id == loc_id], age0.pop, use.names = T)
    out.births <- loc.births[,.(population = sum(population)), by = c('age_group_id', 'location_id', 'year_id', 'run_id')]
    write.csv(out.births, paste0(out.dir, '/births/', loc, ".csv"), row.names = F)
    loc.births[,sex := ifelse(sex_id == 1, 'male', 'female')]
    loc.births[,sex_id := NULL]
    srb.dt <- dcast.data.table(loc.births, year_id + location_id + run_id ~ sex, value.var = 'population')
    srb.dt[, male_srb := male/(female + male)]
    srb.dt[, female_srb := female/(female + male)]
    srb.dt[,c('female', 'male') := NULL]
    write.csv(srb.dt, paste0(out.dir, '/SRB/', loc, ".csv"), row.names = F)
  }
}



#HIV Inputs - these get held constant for extension years
# if(static_inputs){
# 
#   lapply(run.list, function(loc){
#     for(c.input in 
#         c(
#       'onARTmortality',
#                      'noARTmortality',
#                      'averageCD4duration',
#                      'TFRreduction',
#                      'percentBF',
#                      'childProgParam',
#                      'childMortNoART',
#                      'childMortOnART',
#                      'childARTDist',
#                      'childDistNewInf',
#                      'averageCD4duration',
#                      'prevalence', 
#                      'childARTDist',
#                      'childARTeligibility',
#                      'adultARTeligibility',
#                      'SRB',
#                      'PMTCTdropoutRates'
#                      )
#       )
#     {
#       print(loc)
#       print(c.input)
#       
#       if(!loc %in% run.locs){
# 
#         sub_loc <- first(find.children(loc))
#         
#       } else {
#         sub_loc <- loc
#       }
#       
#       if(c.input == 'prevalence'){
#         dir.list <- list(paste0('/share/hiv/spectrum_input/', last_gbd_run,"/",c.input, '/', sub_loc, '.csv'),
#                          paste0('/share/hiv/epp_output/gbd20/', last_gbd_run,"/aggregated/",c.input, '/', sub_loc, '.csv'))
#         for(x in dir.list){
#           if(file.exists(x)){
#             input.dt <- fread(x)
#             break
#           }
#         }
#       }else{
#         if(file.exists(paste0('/share/hiv/spectrum_input/', last_gbd_run,"/",c.input, '/', sub_loc, '.csv'))){
#           input.dt <- fread(paste0('/share/hiv/spectrum_input/', last_gbd_run,"/",c.input, '/', sub_loc, '.csv'))
#         }else{
#           next
#         }
#       }
# 
#       if("year" %in% colnames(input.dt)){
#           max.obs <- max(input.dt$year)
#           ext.years <- setdiff(max(unique(input.dt$year)):max.year,unique(input.dt$year))
#           
#           if(max.obs < max.year){
#             for(y in ext.years){
#               extend.dt <- input.dt[year==max.obs]
#               extend.dt <- extend.dt[,year:=y]
#               input.dt <- rbind(input.dt,extend.dt)
#             }
#           }
#           
#       }
#         #Exception for adult ART eligibility
#         if(c.input == "adultARTeligibility"){
#           input.dt <- unique(input.dt[year > max.obs,cd4_threshold := 999])
#         }
#         
#         dir.create(paste0(out_dir,'/', c.input, '_a/'),recursive = T)
#         write.csv(input.dt, paste0(out_dir,"/", c.input, '_a/', loc, '.csv'), row.names = F)
#       
#     }
#   })
#   dir.check <- paste0(out_dir, c('onARTmortality',
#                                    'noARTmortality',
#                                    'averageCD4duration',
#                                    'TFRreduction',
#                                    'percentBF',
#                                    'childProgParam',
#                                    'childMortNoART',
#                                    'childMortOnART',
#                                    'childARTDist',
#                                    'childDistNewInf',
#                                    'averageCD4duration',
#                                    'prevalence', 
#                                    'childARTDist',
#                                    'childARTeligibility',
#                                    'adultARTeligibility',
#                                    'SRB',
#                                    'PMTCTdropoutRates'), '_a')
#   
#   # for(dir in dir.check){
#   #   print(dir)
#   #   lapply(lower.list, check_loc_results, check_dir = dir,prefix="",postfix=".csv")
#   #         
#   # }
# 
# 
#   mclapply(run.list, function(loc){
#     
#       for(c.input in c('adultARTcoverage')){
#         print(loc)
#         
#         epp = loc.table[ihme_loc_id==loc,epp]
# 
#         model_version <- "200713_yuka"
#       
#         
#         if(loc %in% gsub(".csv","",list.files("/share/hiv/spectrum_prepped/aggregates/191206_inputs_testing"))){
#           model_version <- "191206_inputs_testing"
#         }
#         
#         if(epp==1 & 
#            loc %in% gsub("_Adult_ART_cov.csv","",
#            list.files(paste0("/home/j//WORK/04_epi/01_database/02_data/hiv/04_models/gbd2015/02_inputs/extrapolate_ART/PV_testing/UNAIDS_2019")))){
#           
#            input.dt <- fread(paste0("/home/j//WORK/04_epi/01_database/02_data/hiv/04_models/gbd2015/02_inputs/extrapolate_ART/PV_testing/UNAIDS_2019/",loc,"_Adult_ART_cov.csv"))
#            input.dt <- convert.art(out.dt = input.dt, loc = loc, c.fbd_version = NULL) ##Adds on cov_pct total column required by Spectrum
#            input.dt[,pop_start := NULL]
#              
#           } else if(file.exists(paste0('/share/hiv/spectrum_input/',model_version,'/',c.input, '/', loc, '.csv'))){
#             
#            input.dt <- fread(paste0('/share/hiv/spectrum_input/',model_version,'/',c.input, '/', loc, '.csv'))
#             
#           } else {
#             
#           input.dt <- 1
#         }
#         
#         if(!is.data.table(input.dt)){
# 
#           input.dt <- fread(paste0('/share/hiv/spectrum_input/20200702_forecasting/',c.input,"/", loc, '.csv'))  ##These are aggregate inputs generated only for forecasting
#         }
# 
#         print(model_version)
#         
#         max.obs <- max(input.dt$year)
#         ext.years <- setdiff(max(unique(input.dt$year)):max.year,unique(input.dt$year))
#         
#         if(max.obs < max.year){
#           for(y in ext.years){
#             extend.dt <- input.dt[year==max.obs]
#             extend.dt <- extend.dt[,year:=y]
#             input.dt <- rbind(input.dt,extend.dt)
#           }
#         }
#         
#         if("ART_cov_pct_total" %in% colnames(input.dt)){
#           
#           input.dt[ART_cov_pct_total >= 0, ART_cov_num := 0]
#           
#         }
#         
#           input.dt[ART_cov_pct >= 0, ART_cov_num := 0]
#         
#         dir.create(paste0(out_dir,'/', c.input, '_a/'),recursive = T)
#         write.csv(input.dt, paste0(out_dir,"/", c.input, '_a/', loc, '.csv'), row.names = F)
#         
#       }
#   },mc.cores = cores)
#    check_loc_results(run.list,paste0(out_dir, '/adultARTcoverage_a/'),prefix="",postfix=".csv")
#   
#   
#   
# }


