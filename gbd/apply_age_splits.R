### Setup
rm(list=ls())
windows <- Sys.info()[1][["sysname"]]=="Windows"
root <- ifelse(windows,"J:/","/home/j/")
user <- ifelse(windows, Sys.getenv("USERNAME"), Sys.getenv("USER"))
code.dir <- paste0(ifelse(windows, "H:", paste0("/ihme/homes/", user)), "/gbdeppaiml/")

### Arguments
args <- commandArgs(trailingOnly = TRUE)
if(length(args) > 0) {
  loc <- args[1]
  run.name <- args[2]
  spec.name <- args[3]
} else {

  loc <- "AGO"
  run.name = "230809_meixin"
  spec.name = "230809_meixin"

}
fill.draw <- T
fill.na <- T
if(run.name == "220407_Meixin"){
  gbdyear <- 'gbdTEST'
} else{
  gbdyear <- 'gbd22'
}

### Paths
#library(vctrs, lib.loc="/ihme/singularity-images/rstudio/lib/4.1.3.4")
eppasm_dir <- paste0('/share/hiv/epp_output/', gbdyear, '/', run.name, '/')
out_dir <- paste0("/ihme/hiv/spectrum_prepped/art_draws/",spec.name)
dir.create(out_dir, recursive = T, showWarnings = F)
out_dir_death <- paste0("/ihme/hiv/spectrum_prepped/death_draws/",spec.name)
dir.create(out_dir_death, recursive = T, showWarnings = F)
out_dir_birth <- paste0("/ihme/hiv/spectrum_prepped/birth_prev/",spec.name, '/')
dir.create(out_dir_birth, recursive = T, showWarnings = F)


### Functions
library(mortdb, lib ="/mnt/team/mortality/pub/shared/r/4")
source( "/ihme/cc_resources/libraries/current/r/get_population.R")
setwd(paste0(ifelse(windows, "H:", paste0("/ihme/homes/", user)), "/eppasm/"))
devtools::load_all()
setwd(code.dir)
devtools::load_all()


## Libraries etc.
library(data.table); library(foreign); library(assertable)
ids <- fread('/ihme/hiv/epp_input/gbd20/input_ids.csv')
if(!run.name %in% unique(ids$run_name)){
  id_copy <- copy(ids[run_name == '200713_yuka',])
  id_copy[,run_name := run.name]
  ids <- rbind(ids, id_copy)
}


# if(run.name == '200505_xylo' | run.name == '200713_yuka' | run.name == '200713_yuka_ETH_test' | run.name == '200713_yuka_ETH_test_rlog' |  run.name == '200713_yuka_ETH_test_rspline' | run.name == 'zaf_full_run_0.15'){
  age_map <- data.table(fread(paste0('/ihme/hiv/epp_input/gbd20/', '200316_windchime', "/age_map.csv")))
  
# }else{
#   age_map <- data.table(fread(paste0('/ihme/hiv/epp_input/', gbdyear, '/', run.name, "/age_map.csv")))
#   
# }
age_map <- age_map[(age_group_id %in% c(2, 34, 49, 388, 238, 389,3, seq(6,21))) ,list(age_group_id,age=age_group_name_short)]
age_map[age == "12-23 mo.",age_group_id := 238]


## Create a map to scramble draws from Spectrum
locations <- data.table(get_locations(hiv_metadata = T))
# locations <- get_location_metadata(gbd_round_id = 7, decomp_step = 'iterative', location_set_id = 35)
loc_id <- locations[ihme_loc_id==loc,location_id]

## Use draw maps to scramble draws so that they are not correlated over time
## This is because Spectrum output is semi-ranked due to Ranked Draws into EPP
## Then, it propogates into here which would screw up downstream processes
## This is done here as opposed to raw Spectrum output because we want to preserve the draw-to-draw matching of Spectrum and lifetables, then scramble them after they've been merged together
# draw_map <- fread("/ihme/hiv/spectrum_prepped/draw_map.csv")
# draw_map <- draw_map[location_id==loc_id,list(old_draw,new_draw)]
# setnames(draw_map,"old_draw","run_num")
# draw_map[,run_num:=run_num+1]
# draw_map[,new_draw:=new_draw+1]


##################################################################################################################
## Import and Format Data


## Bring in EPPASM Draws
#dir.list <- c('/ihme/hiv/epp_output/gbd20/200713_yuka_ETH_test/', '/ihme/hiv/epp_output/gbd20/200713_yuka/', '/ihme/hiv/epp_output/gbd20/200505_xylo/')
dir.list <- paste0('/ihme/hiv/epp_output/',gbdyear,'/',run.name, '/')

for(dir in dir.list){
  if(file.exists(paste0(dir, '/compiled/', loc, '.csv'))){
    eppasm_dir <- dir
    break
    
  }else{
    next
  }
  
}

spec_draw <- data.table(fread(paste0(eppasm_dir,"/compiled/",loc,".csv"), blank.lines.skip = T))
spec_draw[age >= 5,age_gbd :=  as.character(age - age%%5)]
spec_draw[age %in% 1, age_gbd := "12-23 mo."]
spec_draw[age %in% 2:4, age_gbd := "2-4"]
spec_draw[age == 0, age_gbd := 0 ]
spec_draw[, non_hiv_deaths := as.numeric(non_hiv_deaths)][, pop := as.numeric(pop)][, pop_neg := as.numeric(pop_neg)][,hiv_deaths:= as.numeric(hiv_deaths)]
spec_draw[,new_hiv:= as.numeric(new_hiv)][,total_births:= as.numeric(total_births)][,hiv_births := as.numeric(hiv_births)][,birth_prev:=as.numeric(birth_prev)]
spec_draw[, pop_art:= as.numeric(pop_art)][,pop_gt350:= as.numeric(pop_gt350)][,pop_200to350:= as.numeric(pop_200to350)][, pop_lt200:= as.numeric(pop_lt200)]
spec_draw <- spec_draw[,.(pop = sum(pop), hiv_deaths = sum(hiv_deaths), non_hiv_deaths = sum(non_hiv_deaths), new_hiv = sum(new_hiv), pop_neg = sum(pop_neg),
                    total_births = sum(total_births), hiv_births = sum(hiv_births), birth_prev = sum(birth_prev),
                    pop_art = sum(pop_art), pop_gt350 = sum(pop_gt350), pop_200to350 = sum(pop_200to350), pop_lt200 = sum(pop_lt200)), 
                    by = c('age_gbd', 'sex', 'year', 'run_num')]
setnames(spec_draw, 'age_gbd', 'age')

## identify unreasonable draws in NGA
if(loc %like% "NGA"){
  draws.list <- unique(spec_draw[pop > 100*quantile(pop, probs = 0.75), run_num])
  draws.list <- unique(c(unique(spec_draw[hiv_deaths > 100*quantile(hiv_deaths, probs = 0.75), run_num]), draws.list))
  draws.list <- unique(c(unique(spec_draw[non_hiv_deaths > 100*quantile(non_hiv_deaths, probs = 0.75), run_num]), draws.list))
  draws.list <- unique(c(unique(spec_draw[new_hiv > 100*quantile(new_hiv, probs = 0.75), run_num]), draws.list))
  draws.list <- unique(c(unique(spec_draw[pop_neg > 100*quantile(pop_neg, probs = 0.75), run_num]), draws.list))
  spec_draw <- spec_draw[!(run_num %in% draws.list)]
  }

# Fill in missing draws
if(fill.draw) {
  have.draws <- unique(spec_draw$run_num)
  need.draws <- setdiff(1:1000, have.draws)
  for(draw in need.draws) {
    replace.draw <- sample(have.draws, 1)
    replace.dt <- spec_draw[run_num == replace.draw]
    replace.dt[, run_num := draw]
    spec_draw <- rbind(spec_draw, replace.dt)
  }
}
# fill in missing data
id.vars = list(year = c(1970:2024), sex = c("female","male"), age = c("0","12-23 mo.","2-4", "5", "10", "15","20","25", "30", "35", "40", "45", "50", "55", "60", "65", "70", "75", "80"), run_num = c(1:1000))
missing = assertable::assert_ids(spec_draw, id_vars = id.vars, warn_only = T)
if(class(missing)[1] != "character"){
  if(fill.draw) {
    need.draws <- unique(missing$run_num)
    have.draws <- setdiff(1:1000, need.draws)
    for(draw in need.draws) {
      replace.draw <- sample(have.draws, 1)
      replace.dt <- spec_draw[run_num == replace.draw]
      spec_draw <- spec_draw[run_num!=draw]
      replace.dt[, run_num := draw]
      spec_draw <- rbind(spec_draw, replace.dt)
    }
  }
}

id.vars = list(year = c(1970:2024), sex = c("female","male"), age = c("0","12-23 mo.","2-4", "5", "10", "15","20","25", "30", "35", "40", "45", "50", "55", "60", "65", "70", "75", "80"), run_num = c(1:1000))
missing = assertable::assert_ids(spec_draw, id_vars = id.vars, warn_only = F)


# Fill in NA's
na.present <- any(is.na(spec_draw))
if(fill.na & na.present) {
  na.draws <- unique(spec_draw[rowSums(is.na(spec_draw)) > 0, run_num])
  remain.draws <- setdiff(1:1000, na.draws)
  for(draw in na.draws) {
    replace.draw <- sample(remain.draws, 1)
    replace.dt <- copy(spec_draw[run_num == replace.draw])
    replace.dt[, run_num := draw]
    spec_draw <- rbind(spec_draw[run_num != draw], replace.dt)
  }
}
## Calculate birth prevalence rate
birth_pop <-  get_mort_outputs(
  "birth", "estimate",
  gbd_year = 2021,
  run_id = ids[run_name == run.name, births],
  ##all ages 10-54
  age_group_id = 169,
  location_id = loc_id, year_id = seq(1970, 2022), sex_id = 1:2)
birth_pop.2023 <- birth_pop[year_id==2022]
birth_pop.2023[, year_id:= 2023]
birth_pop <- rbind(birth_pop, birth_pop.2023)
birth_pop.2023[, year_id:= 2024]
birth_pop <- rbind(birth_pop, birth_pop.2023)

setnames(birth_pop, 'mean', 'population')
birth_pop[,population := sum(population), by = c('location_id', 'year_id', 'sex_id')]
birth_pop[,age_group_id := 164]
setnames(birth_pop, c("year_id", "population"), c("year", "gbd_pop"))
birth_dt <- copy(spec_draw)
birth_dt[sex=="male",sex_id:=1]
birth_dt[sex=="female",sex_id:=2] 
birth_dt <- birth_dt[,.(age_group_id = 164, birth_prev = sum(birth_prev), total_births = sum(total_births)), by = c('year', 'run_num')]
# birth_dt[, total_births := sum(total_births)/2, by = c('year', 'run_num')]
birth_dt[, birth_prev_rate := ifelse(total_births == 0, 0, birth_prev/total_births)]
birth_dt[, sex_id:=1]
birth_dt.female <- copy(birth_dt)
birth_dt.female[, sex_id:=2]
birth_dt <- rbind(birth_dt, birth_dt.female)
## Scale to GBD pop
birth_dt <- merge(birth_dt, birth_pop[,.(year, gbd_pop, sex_id)], by = c('year', 'sex_id'))
birth_dt[, birth_prev_count := birth_prev_rate * gbd_pop]

## test missing data
id.vars = list(year = c(1970:2024), sex_id = c(1,2), age_group_id = 164, run_num = c(1:1000))
missing = assertable::assert_ids(birth_dt, id_vars = id.vars, warn_only = F)

## save birth prevalence
write.csv(birth_dt[,.(year, sex_id, run_num, age_group_id, birth_prev_rate, birth_prev_count)], paste0(out_dir_birth, loc, '.csv'), row.names = F)
spec_draw[,birth_prev := NULL]

## split under 1 age group
output.u1 <- split_u1.new_ages(spec_draw[age == 0], loc, run.name = run.name, gbdyear = gbdyear)
output.u1[age=="x_388", age := "1-5 mo."]
output.u1[age=="x_389", age := "6-11 mo."]

if(fill.draw) {
    have.draws <- unique(output.u1$run_num)
    need.draws <- setdiff(1:1000, have.draws)
    for(draw in need.draws) {
      replace.draw <- sample(have.draws, 1)
      replace.dt <- output.u1[run_num == replace.draw]
      output.u1 <- output.u1[run_num!=draw]
      replace.dt[, run_num := draw]
      output.u1 <- rbind(output.u1, replace.dt)
    }
}
id.vars = list(year = c(1970:2024), sex = c("female","male"), age = c("enn","6-11 mo.","lnn","1-5 mo."), run_num = c(1:1000))
missing = assertable::assert_ids(output.u1, id_vars = id.vars, warn_only = T)


spec_draw <- spec_draw[age != 0]
spec_draw <- rbind(spec_draw, output.u1[,pop_death_pop := NULL], use.names = T)    

spec_draw[sex=="male",sex_id:=1]
spec_draw[sex=="female",sex_id:=2] 
spec_draw[,age:=as.character(age)]
# spec_draw[age == "1-5 mo.", age := "1m"][age == "6-11 mo.", age := "6m"]
spec_draw <- merge(spec_draw,age_map,by="age")
spec_draw[,age:=NULL]
## vestigial column
spec_draw[, suscept_pop := pop_neg]



## Save over-80 Spectrum pops for splitting
spec_o80 <- data.table(spec_draw[age_group_id==21,])
spec_o80[, age_group_id := NULL]

# Get raw proportions for splitting populations and other general ones
# if(run.name == "zaf_full_run_0.15"){ ## update it once we have new demographic input
  pop <- fread(paste0('/share/hiv/epp_input/gbd20/200713_yuka/population_splits/', loc, '.csv'))
  pop.2023 <- pop[year_id==2022]
  pop.2023[, year_id := 2023]
  pop <- rbind(pop, pop.2023)
  pop.2023[, year_id := 2024]
  pop <- rbind(pop, pop.2023)
# }else{
#   pop <- fread(paste0('/share/hiv/epp_input/', gbdyear, '/', run.name, '/population_splits/', loc, '.csv'))
# }
o80_pop <- pop[age_group_id %in% c(30:32, 235),]
o80_pop[,pop_total:=sum(population), by=list(sex_id,year_id)]
o80_pop[,pop_prop:=population/pop_total, by=list(sex_id,year_id)]
# Create proportions for incidence 
o80_pop[,pop_prop_inc:=pop_prop]
# Create proportions for death 
o80_pop[,pop_prop_death:=pop_prop]
o80_pop <- o80_pop[,list(location_id,sex_id,year = year_id,age_group_id,pop_total,pop_prop,pop_prop_inc,pop_prop_death)]

## Split over-80 proportionally by age-group populations
spec_o80 <- merge(spec_o80,o80_pop,by=c("year","sex_id"),allow.cartesian=T) # m:m merge -- expand pops by draw, spec by age groups

# Split all variables that can be split by population
pop_weight_all <- function(x) return(x*spec_o80[['pop_prop']])
all_age_vars <- c("suscept_pop","pop_neg","non_hiv_deaths","pop_lt200","pop_200to350","pop_gt350","pop_art","pop")
spec_o80[,(all_age_vars) := lapply(.SD,pop_weight_all),.SDcols=all_age_vars] 

# Split incidence
pop_weight_inc <- function(x) return(x*spec_o80[['pop_prop_inc']])
inc_vars <- c("new_hiv")
spec_o80[,(inc_vars) := lapply(.SD,pop_weight_inc),.SDcols=inc_vars] 

# Split deaths 
pop_weight_death <- function(x) return(x*spec_o80[['pop_prop_death']])
death_vars <- c("hiv_deaths")
spec_o80[,(death_vars) := lapply(.SD,pop_weight_death),.SDcols=death_vars] 

spec_o80[,c("pop_total","pop_prop","pop_prop_inc","pop_prop_death","location_id") :=NULL]


##################################################################################################################
## Convert from Spectrum to rate space
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

# Recombine with spec_draw
spec_combined <- rbindlist(list(spec_draw[!age_group_id == 21,],spec_o80),use.names=T) # Drop 0-5 and 80+, replace with spec_u5 and spec_o80 
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

## test missing data
id.vars = list(year_id = c(1970:2024), sex_id = c(1,2), age_group_id = c(2,3,6:20, 30:32, 34, 235, 238, 388, 389), run_num = c(1:1000))
missing = assertable::assert_ids(spec_combined, id_vars = id.vars, warn_only = F)

## save birth prevalence
write.csv(spec_combined[,list(sex_id,year_id,age_group_id,run_num,hiv_deaths,non_hiv_deaths, non_hiv_deaths_prop)],paste0(out_dir_death,"/",loc,"_ART_deaths.csv"),row.names=F)

# # Rescramble draws to match Reckoning output before outputting non-fatal results (can't do it to the fatal that feeds into the Reckoning because we want to preserve within-draw correlation between them)
# spec_combined <- merge(spec_combined,draw_map,by="run_num")

## save birth prevalence
write.csv(spec_combined[,list(sex_id,year_id,run_num,hiv_deaths, non_hiv_deaths, new_hiv,hiv_births,suscept_pop,total_births,pop_neg,pop_lt200,pop_200to350,pop_gt350,pop_art,age_group_id)],paste0(out_dir,"/",loc,"_ART_data.csv"),row.names=F)

