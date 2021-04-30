## ---------------------------
## Script name: 
## Purpose of script:
##
## Author: Maggie Walters
## Date Created: 2018-04-11
## Email: mwalte10@uw.edu
## ---------------------------
##
## Notes: Used to modify pmtct, child ART, and adult ART to be covid adjusted
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

locs <- loc.table[epp ==1 | ihme_loc_id %in% c('MRT', 'STP', 'COM'),]
locs <- loc.table[grepl('IND', ihme_loc_id) & level == 4,]


all_art = rbindlist(lapply(locs$location_id,function(iso){
  print(iso)
  loc = locs[location_id == iso, ihme_loc_id]
  if(grepl('IND', loc)){
    in.path <- paste0('/ihme/hiv/data/UNAIDS_extrapolated/GBD20/adultARTcoverage/' ,'140520','/', loc, '_Adult_ART_cov.csv')
  }else{
    for(c.year in c('UNAIDS_2019', 'UNAIDS_2018', 'UNAIDS_2017', 'UNAIDS_2016', 'UNAIDS_2015', '140520')){
      if(file.exists(paste0('/ihme/hiv/data/UNAIDS_extrapolated/GBD20/adultARTcoverage/' ,c.year,'/', loc, '_Adult_ART_cov.csv'))){
        
        in.path <- paste0('/ihme/hiv/data/UNAIDS_extrapolated/GBD20/adultARTcoverage/' ,c.year,'/', loc, '_Adult_ART_cov.csv')
        
        
        break
      }
      
    } 
  }
  if(grepl('ZAF', loc)){
    in.path <- paste0('/share/hiv/data/UNAIDS_extrapolated/GBD20//ZAF_sub/', loc, '_Adult_ART_cov.csv')
  }
  dt = fread(paste0(in.path))
  dt = dt[,.(year, sex, ART_cov_num)]
  setnames(dt, c('year', 'sex', 'ART_cov_num'), c('year_id', 'sex_id', 'value'))
  dt[,location_id := iso]
  return(dt)
}), fill =T)
setnames(all_art, "value", "art")
#Pull in ART scalars and create adjusted files for covid reference scenario 2020 and 2021
all_art <- merge(all_art, locs[,.(location_id, level, parent_id, ihme_loc_id)])
all_art[level == 5, parent_id := 180]
all_art[level != 3, location_id := parent_id]
art_scalar = fread("/home/j/Project/goalkeepers_2020/covid_interruption_covariates/2020-07-30/any_missed_meds_WEIGHTED/mrbrt_any_missed_meds_WEIGHTED_results_annual.csv")[scenario_id == 0]
art_scalar[,ihme_loc_id := NULL] ; art_scalar[,sex_id := NULL]
art_scalar = merge(all_art, art_scalar,all.x=TRUE, by = c('location_id', 'year_id'))
art_scalar[,art_covid := art * value]
art_gg = art_scalar[,.(year_id, ihme_loc_id, art, art_covid, value, sex_id)]
art_gg <- merge(art_gg, loc.table[,.(ihme_loc_id, location_id)])
art_gg[is.na(art_covid), art_covid := art]

dir.create('/ihme/hiv/data/UNAIDS_extrapolated/GBD20/adultARTcoverage/covid/')
setnames(art_gg, 'art_covid', 'ART_cov_num')
art_gg[,ART_cov_pct := 0]
setnames(art_gg, c('sex_id', 'year_id'), c('sex', 'year'))
art_gg <- art_gg[,.(year, sex, ART_cov_pct, ART_cov_num, ihme_loc_id)]
for(loc in unique(art_gg$ihme_loc_id)){
  x <- copy(art_gg)
  x <- x[ihme_loc_id == loc,]
  x[,ihme_loc_id := NULL]
  print(loc)
  write.csv(x, paste0('/ihme/hiv/data/UNAIDS_extrapolated/GBD20/adultARTcoverage/covid/', loc, '_Adult_ART_cov.csv'), row.names = T)
}



#children inputs ---------------------------------------
art_scalar = fread("/home/j/Project/goalkeepers_2020/covid_interruption_covariates/2020-07-30/any_missed_meds_WEIGHTED/mrbrt_any_missed_meds_WEIGHTED_results_annual.csv")[scenario_id == 0]
x = rbindlist(lapply(locs$location_id,function(iso){
  
  print(iso)
  loc = locs[location_id == iso, ihme_loc_id]
  for(c.year in c('UNAIDS_2019', 'UNAIDS_2018', 'UNAIDS_2017', 'UNAIDS_2016', 'UNAIDS_2015', '140520')){
    if(file.exists(paste0('/ihme/hiv/data/UNAIDS_extrapolated/GBD20/childARTcoverage/',c.year, '/', loc, '_Child_ART_cov.csv'))){
      art <- paste0('/ihme/hiv/data/UNAIDS_extrapolated/GBD20/childARTcoverage/',c.year, '/', loc, '_Child_ART_cov.csv')
      break
    }}

if(!any(ls() == 'art')){
  art <- (paste0('/share/hiv/epp_input/gbd19/paeds/childARTcoverage/', loc, '.csv'))
  
}
if(loc %in% c('NGA', 'KEN', 'KEN_44796')){
  art <- paste0('/share/hiv/epp_input/gbd19/paeds/childARTcoverage/', loc, '.csv')
  
}
  if(grepl('IND', loc)){
    temp.loc <- loc.table[parent_id == loc.table[ihme_loc_id == loc, location_id], ihme_loc_id][1]
  }else{
    temp.loc <- loc
  }
  
  for(c.year in c('UNAIDS_2019', 'UNAIDS_2018', 'UNAIDS_2017', 'UNAIDS_2016', 'UNAIDS_2015', '140520')){
    if(file.exists( paste0('/ihme/hiv/data/UNAIDS_extrapolated/GBD20/PMTCT/', c.year,'/', temp.loc, '_PMTCT_ART_cov.csv'))){
      pmtct <- paste0('/ihme/hiv/data/UNAIDS_extrapolated/GBD20/PMTCT/', c.year,'/', temp.loc, '_PMTCT_ART_cov.csv')
      break
    }}
dir.create('/ihme/hiv/data/UNAIDS_extrapolated/GBD20/childARTcoverage/covid/')
dir.create('/ihme/hiv/data/UNAIDS_extrapolated/GBD20/PMTCT/covid/')
scalar <- copy(art_scalar)[location_id == iso,]

art <- fread(art)
art <- merge(art, scalar[,.(year_id, value)], by.x = 'year', by.y = 'year_id', all.x = T)
art[,value := ifelse(is.na(value), 1, value)]
if(any(art$ART_cov_pct) !=  0){
  print('needs converting')
}

art[,ART_cov_num := ART_cov_num * value] ; art[,Cotrim_cov_num := Cotrim_cov_num * value]
art[,value := NULL]
write.csv(art, paste0('/ihme/hiv/data/UNAIDS_extrapolated/GBD20/childARTcoverage/covid/', loc , '_Child_ART_cov.csv'), row.names = F)

pmtct <- fread(pmtct)
pmtct <- melt(pmtct, id.vars = 'year')
pop <- 


setnames(pmtct, 'value', 'no_covid')
pmtct <- merge(pmtct, scalar[,.(year_id, value)], by.x = 'year', by.y = 'year_id', all.x = T)
pmtct[,value := ifelse(is.na(value), 1, value)]


pmtct[,covid := no_covid * value] 
pmtct[,value := NULL] ; pmtct[,no_covid := NULL]
pmtct <- dcast(pmtct, year ~ variable, value.var = 'covid')
write.csv(pmtct, paste0('/ihme/hiv/data/UNAIDS_extrapolated/GBD20/PMTCT/covid/', loc , '_PMTCT_ART_cov.csv'), row.names = F)
print(loc)
  
}
))
