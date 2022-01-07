## ---------------------------
## Script name: 
## Purpose of script:
##
## Author: Maggie Walters
## Date Created: 2018-04-11
## Email: mwalte10@uw.edu
## ---------------------------
##
## Notes: Modify DT objects for group 2 locations
##   
##
## ---------------------------

## Used in basically every script
Sys.umask(mode = "0002")
windows <- Sys.info()[1][["sysname"]]=="Windows"
root <- ifelse(windows,"J:/","/home/j/")
user <- ifelse(windows, Sys.getenv("USERNAME"), Sys.getenv("USER"))
library(data.table)

group_2_dt_mods <- function(loc, dt){
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
  dir.create(paste0('/ihme/hiv/epp_output/gbd20/',run.name, '/dt_objects/'), recursive = T)
  saveRDS(dt, paste0('/ihme/hiv/epp_output/gbd20/',run.name, '/dt_objects/', loc, '_dt.RDS'))
  return(dt)
  
}
