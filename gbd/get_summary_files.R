## ---------------------------
## Script name: 
## Purpose of script:
##
## Author: Maggie Walters
## Date Created: 2018-04-11
## Email: mwalte10@uw.edu
## ---------------------------
##
## Notes:
##   
##
## ---------------------------

## Used in basically every script
Sys.umask(mode = "0002")
windows <- Sys.info()[1][["sysname"]]=="Windows"
root <- ifelse(windows,"J:/","/home/j/")
user <- ifelse(windows, Sys.getenv("USERNAME"), Sys.getenv("USER"))
eppasm_dir <- paste0(ifelse(windows, "H:", paste0("/ihme/homes/", user)), "/eppasm/")
setwd(eppasm_dir)
devtools::load_all()
gbdeppaiml_dir <- paste0(ifelse(windows, "H:", paste0("/ihme/homes/", user)), "/gbdeppaiml/")
setwd(gbdeppaiml_dir)
devtools::load_all()
library(parallel)

args <- commandArgs(trailingOnly = TRUE)
if(length(args) > 0) {
  run.name = args[1]
  loc = args[2]
} else {
  run.name = '200713_yuka'
  loc = 'AGO'
}

loc.table <- get_locations(gbd_year = 2020, hiv_metadata = T)

if(file.exists(paste0('/ihme/hiv/epp_input/gbd20/',run.name,'/array_table.csv'))){
  array.dt <- fread(paste0('/ihme/hiv/epp_input/gbd20/',run.name,'/array_table.csv'))
  locs <- unique(array.dt[,loc_scalar])
}else{
  locs <- loc.table[epp == 1, ihme_loc_id]
}

# get_summary(loc,  run.name = run.name, gbdyear = 'gbd20', 
#             paediatric = T, old.splits = F)

run.name.old = '200714_yuka'; run.name.new = run.name; paediatric = T; old.splits = FALSE; test_run = NULL; loc_name = loc


  ## create gbd age groups
  output <- fread(paste0('/share/hiv/epp_output/', gbdyear, '/',run.name, '/compiled/', loc, '.csv'))
  if(grepl('socialdets', run.name) | grepl('tvfoi', run.name)){
    loc_name = unlist(strsplit(loc, '_'))[[1]]
  }else{
    loc_name = loc
  }
  ## create gbd age groups
  output[age >= 5,age_gbd :=  age - age%%5]
  output[age %in% 1:4, age_gbd := 1]
  output[age == 0, age_gbd := 0 ]
  output <- output[,.(pop = sum(pop), hiv_deaths = sum(hiv_deaths), non_hiv_deaths = sum(non_hiv_deaths), new_hiv = sum(new_hiv), pop_neg = sum(pop_neg),
                      total_births = sum(total_births), hiv_births = sum(hiv_births), birth_prev = sum(birth_prev),
                      pop_art = sum(pop_art), pop_gt350 = sum(pop_gt350), pop_200to350 = sum(pop_200to350), pop_lt200 = sum(pop_lt200)), by = c('age_gbd', 'sex', 'year', 'run_num')]
  setnames(output, 'age_gbd', 'age')
  if(paediatric){
    
    if(old.splits){
      output.u1 <- split_u1(output[age == 0], loc, run.name.old, run.name.new)
      
    }else{
      output.u1 <- split_u1.new_ages(output[age == 0], loc, run.name.old, run.name.new, gbdyear, test_run = test_run, loc_name)
      
    }
    output <- output[age != 0]
    output <- rbind(output, output.u1[,pop_death_pop := NULL], use.names = T)    
  }
  output[, hivpop := pop_art + pop_gt350 + pop_200to350 + pop_lt200]
  output[,c('pop_gt350', 'pop_200to350', 'pop_lt200', 'birth_prev', 'pop_neg', 'hiv_births', 'total_births') := NULL]
  output.count <- melt(output, id.vars = c('age', 'sex', 'year', 'pop', 'run_num'))
  
  
  if(old.splits){
    age.map <- fread(paste0('/ihme/hiv/epp_input/gbd19', '/', run.name.old, "/age_map.csv"))
    
  }else{
    age.map <- fread(paste0('/ihme/hiv/epp_input/gbd20', '/', run.name.new, "/age_map.csv"))
    age.map[age_group_id == 388, age_group_name_short := 'x_388']
    age.map[age_group_id == 389, age_group_name_short := 'x_389']
  }
  age.map[age_group_name_short == 'All', age_group_name_short := 'All']
  if(!paediatric){
    age.spec <- age.map[age_group_id %in% 8:21,.(age_group_id, age = age_group_name_short)]
    age.spec[, age := as.integer(age)]
  }else{
    age.spec <- age.map[age_group_id %in% c(2:21, 388, 389),.(age_group_id, age = age_group_name_short)]
  }
  output.count <- merge(output.count, age.spec, by = 'age')
  output.count[, age := NULL]
  
  # Collapse to both sex
  both.sex.dt <- output.count[,.(value = sum(value), pop = sum(pop)), by = c('year', 'variable', 'age_group_id', 'run_num')]
  both.sex.dt[, sex := 'both']
  all.sex.dt <- rbind(output.count, both.sex.dt)
  
  # Collapse to all-ages and adults
  all.age.dt <- all.sex.dt[,.(value = sum(value), pop = sum(pop)), by = c('year', 'variable', 'sex','run_num')]
  all.age.dt[, age_group_id := 22]
  
  adult.dt <- all.sex.dt[age_group_id %in% 8:14, .(value = sum(value), pop = sum(pop)), by = c('year', 'variable', 'sex', 'run_num')]
  adult.dt[, age_group_id := 24]
  
  age.dt <- rbindlist(list(all.sex.dt, all.age.dt, adult.dt), use.names = T)
  
  output.rate <- copy(age.dt)
  ## Denominator for ART pop is HIV+ pop
  art.pop <- output.rate[variable == 'hivpop']
  art.pop[, pop := NULL]
  setnames(art.pop, 'value', 'pop')
  art.pop[, variable := 'pop_art']
  art.merge <- merge(art.pop, output.rate[variable == 'pop_art',.(age_group_id, sex, year, variable, value, run_num)], by = c('age_group_id', 'sex', 'year', 'variable', 'run_num'))
  output.rate <- output.rate[!variable == 'pop_art']
  output.rate <- rbind(output.rate, art.merge, use.names = T)
  output.rate[, rate := ifelse(pop == 0, 0, value/pop)]
  output.rate[, value := NULL]
  
  ## Bind together
  setnames(output.rate, 'rate', 'value')
  output.rate[, metric := 'Rate']
  age.dt[, metric := 'Count']
  out.dt <- rbind(output.rate, age.dt, use.names = T)
  out.dt[variable == 'hiv_deaths', variable := 'Deaths']
  out.dt[variable == 'pop_art', variable := 'ART']
  out.dt[variable == 'non_hiv_deaths', variable := 'Background']
  out.dt[variable == 'new_hiv', variable := 'Incidence']
  out.dt[variable == 'hivpop', variable := 'Prevalence']
  setnames(out.dt, 'variable', 'measure')
  age.map <- rbind(age.map[,.(age_group_id, age_group_name_short)], data.table(age_group_id = 24, age_group_name_short = '15 to 49'))
  ##STOP MAGGIE
  out.dt <- merge(out.dt, age.map[,.(age_group_id, age = age_group_name_short)], by = 'age_group_id', allow.cartesian = TRUE)
  out.dt <- out.dt[,.(mean = mean(value), lower = quantile(value, 0.025), upper = quantile(value, 0.975)), by = c('age_group_id', 'sex', 'year', 'measure', 'metric', 'age')]
  dir.create(paste0('/ihme/hiv/epp_output/', gbdyear, '/', run.name, '/summary_files/'))
  write.csv(out.dt, paste0('/ihme/hiv/epp_output/', gbdyear, '/', run.name, '/summary_files/', loc, '.csv'), row.names = F)
  


# mclapply(locs, get_summary, run.name = run.name, gbdyear = 'gbd20', 
#          paediatric = T, old.splits = F, mc.cores =20)
