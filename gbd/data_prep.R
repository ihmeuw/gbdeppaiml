######################################
#Written by: Maggie Walters
#mwalte10@uw.edu
#This script should be loaded into the environment that is running eppasm
######################################
# data_prep <- function(loc){
user <- Sys.getenv("USER")
run.table <- fread(paste0('/share/hiv/epp_input/gbd20//eppasm_run_table.csv'))

c.args <- run.table[run_name==run.name,]
dir.table <- fread(paste0('/share/homes/',user,'/dir_table_log_gbd20.csv'))
dir.table <- dir.table[ref == min(ref),] ##Changed from max which had ASFR/births set to F, might have been old
if(grepl('test', loc)){
  loc <- 'IND_4856'
}


dir.table[,'ASFR' := as.logical(ASFR)]
dir.table[,'births' := as.logical(births)]
dir.table[,'SRB' := as.logical(SRB)]
dir.table[,'migration' := as.logical(migration)]
dir.table[,'prev_surveys' := as.logical(prev_surveys)]
dir.table[,'art' := as.logical(art)]
dir.table[,'tem_art' := as.logical(tem_art)]
dir.table[,'population_single_age' := as.logical(population_single_age)]
dir.table[,'fp_root' := as.logical(fp_root)]
dir.table[,'childARTcoverage' := as.logical(childARTcoverage)]
dir.table[,'pmtct' := as.logical(pmtct)]
dir.table[,'on.art' := as.logical(on.art)]
input_root <- paste0('/ihme/hiv/epp_input/', gbdyear, '/',run.name, '/')

loc.table <- get_locations(hiv_metadata = TRUE)
if(grepl('IND', loc)){
  temp.loc <- loc.table[parent_id == loc.table[ihme_loc_id == loc, location_id], ihme_loc_id][1]
}else{
  temp.loc <- loc
}


if(dir.table[ref == max(ref),ASFR]){
  ASFR <- paste0(input_root, '/ASFR/', loc, '.csv')
}else{
  ASFR <- paste0('/ihme/hiv/epp_input/gbd19/190630_rhino2/ASFR/', loc, '.csv')
}
if(dir.table[ref == max(ref),births]){
  births <- paste0(input_root, '/births/', loc, '.csv')
}else{
  births <- paste0('/ihme/hiv/epp_input/gbd19/190630_rhino2/births/', loc, '.csv')
}
if(dir.table[ref == max(ref),SRB]){
  SRB <- paste0(input_root, '/SRB/', loc, '.csv')
}else{
  SRB <- paste0('/ihme/hiv/epp_input/gbd19/190630_rhino2/SRB/', loc, '.csv')
}
if(dir.table[ref == max(ref),migration]){
  migration <- paste0(input_root, '/migration/', loc, '.csv')
}else{
  migration <- paste0('/ihme/hiv/epp_input/gbd19/190630_rhino2/migration/', loc, '.csv')
}
if(dir.table[ref == max(ref),prev_surveys]){
  
  prev_surveys <- paste0('/ihme/hiv/epp_input/gbd20/prev_surveys.csv')
  if(grepl('IND', loc)){
    prev_surveys <- paste0('/ihme/hiv/epp_input/gbd20/prev_surveys_ind.csv')
    
  }
  
 #prev_surveys <- '/ihme/hiv/data/prevalence_surveys/prev_surveys_temp.csv'
}else{
  ##need to look up old FP
  prev_surveys <- paste0('/ihme/hiv/epp_input/gbd19/190630_rhino2/prev_surveys/')
}
if(dir.table[ref == max(ref),art]){
  for(c.year in c('covid','UNAIDS_2019', 'UNAIDS_2018', 'UNAIDS_2017', 'UNAIDS_2016', 'UNAIDS_2015', '140520')){
    if(file.exists(paste0('/ihme/hiv/data/UNAIDS_extrapolated/GBD20/adultARTcoverage/' ,c.year,'/', loc, '_Adult_ART_cov.csv'))){
      
      art.dt <- paste0('/ihme/hiv/data/UNAIDS_extrapolated/GBD20/adultARTcoverage/' ,c.year,'/', loc, '_Adult_ART_cov.csv')
      
      
      break
    }
    
  }
}else{
  for(c.year in c('UNAIDS_2019', 'UNAIDS_2018', 'UNAIDS_2017', 'UNAIDS_2016', 'UNAIDS_2015', '140520')){
    if(file.exists(paste0('/home/j/WORK/04_epi/01_database/02_data/hiv/04_models/gbd2015/02_inputs/extrapolate_ART/PV_testing/' ,c.year,'/', loc, '_Adult_ART_cov.csv'))){
      
      art.dt <- paste0('/home/j/WORK/04_epi/01_database/02_data/hiv/04_models/gbd2015/02_inputs/extrapolate_ART/PV_testing/' ,c.year,'/', loc, '_Adult_ART_cov.csv')
      
      
      break
    }
  }
}
if(loc == 'KEN'){
  art.dt <- 'temp'
  pmtct <- 'temp'
}
# if(grepl('IND', loc)){
#   art.dt <- paste0('/ihme/hiv/data/UNAIDS_extrapolated/GBD20/adultARTcoverage/' ,'140520','/', loc, '_Adult_ART_cov.csv')
#   
# }
tem_art <- paste0('/share/hiv/data/UNAIDS_extrapolated/GBD20//ZAF_sub/', loc, '_Adult_ART_cov.csv')
if(dir.table[ref == max(ref),population_single_age]){
  population_single_age <- paste0(input_root, '/population_single_age/', loc, '.csv')
}else{
  population_single_age <- paste0('/ihme/hiv/epp_input/gbd19/190630_rhino2/population_single_age/', loc, '.csv')
}
if(dir.table[ref == max(ref),fp_root]){
  fp_root <- paste0('/ihme/hiv/spectrum_input/191224_trumpet/')
  artdist <- paste0(fp_root, '/childARTDist/', temp.loc, '.csv')
  if(grepl('IND',temp.loc)){
    artelig <- paste0(fp_root, '/childARTeligibility/AGO.csv')
  }else{
    artelig <- paste0(fp_root, '/childARTeligibility/', temp.loc, '.csv')
  }
  percbf <- paste0(fp_root, '/percentBF/', temp.loc, '.csv')
  mort.art <- paste0(fp_root, "/childMortOnART/",temp.loc, '.csv')
  prog <-  paste0(fp_root, "/childProgParam/" ,temp.loc, '.csv')
  mort.offart <-  paste0(fp_root, '/childMortNoART/', temp.loc, '.csv')
  dropout <- paste0(fp_root, '/PMTCTdropoutRates/', temp.loc, '.csv')
}else{
  fp_root <- '/share/hiv/epp_input/gbd19/paeds/'
  artdist <- paste0(fp_root, '/childARTDist/', temp.loc, '.csv')
  if(grepl('IND',temp.loc)){
    artelig <- paste0(fp_root, '/childARTeligibility/AGO.csv')
  }else{
    artelig <- paste0(fp_root, '/childARTeligibility/', temp.loc, '.csv')
  }
  percbf <- paste0(fp_root, '/percentBF/', temp.loc, '.csv')
  mort.art <- paste0(fp_root, "/childMortOnART/",temp.loc, '.csv')
  prog <-  paste0(fp_root, "/childProgParam/" ,temp.loc, '.csv')
  mort.offart <-  paste0(fp_root, '/childMortNoART/', temp.loc, '.csv')
  dropout <- paste0(fp_root, '/PMTCTdropoutRates/', temp.loc, '.csv')
  
}
if(dir.table[ref == max(ref),childARTcoverage]){
  for(c.year in c('covid','UNAIDS_2019', 'UNAIDS_2018', 'UNAIDS_2017', 'UNAIDS_2016', 'UNAIDS_2015', '140520')){

    if(file.exists(paste0('/ihme/hiv/data/UNAIDS_extrapolated/GBD20/childARTcoverage/',c.year, '/', loc, '_Child_ART_cov.csv'))){
      art <- paste0('/ihme/hiv/data/UNAIDS_extrapolated/GBD20/childARTcoverage/',c.year, '/', loc, '_Child_ART_cov.csv')
      break
    }}
}else{
  art <- (paste0('/share/hiv/epp_input/gbd19/paeds/childARTcoverage/', loc, '.csv'))
}
# if(!any(ls() == 'art')){
#   art <- (paste0('/share/hiv/epp_input/gbd19/paeds/childARTcoverage/', loc, '.csv'))
#   
# }
# if(loc %in% c('NGA', 'KEN', 'KEN_44796')){
#   art <- paste0('/share/hiv/epp_input/gbd19/paeds/childARTcoverage/', loc, '.csv')
#   
# }
if(dir.table[ref == max(ref),pmtct]){
  for(c.year in c('covid','UNAIDS_2019', 'UNAIDS_2018', 'UNAIDS_2017', 'UNAIDS_2016', 'UNAIDS_2015', '140520')){
    print(c.year)
    if(file.exists( paste0('/ihme/hiv/data/UNAIDS_extrapolated/GBD20/PMTCT/', c.year,'/', loc, '_PMTCT_ART_cov.csv'))){
      pmtct <- paste0('/ihme/hiv/data/UNAIDS_extrapolated/GBD20/PMTCT/', c.year,'/', loc, '_PMTCT_ART_cov.csv')
      break
    }else{
      pmtct <- paste0('/ihme/hiv/data/UNAIDS_extrapolated/GBD20/PMTCT/', c.year,'/', temp.loc, '_PMTCT_ART_cov.csv')
      break
    }
    
    }
}else{
  pmtct <- paste0('/share/hiv/epp_input/gbd19/paeds/PMTCT/', temp.loc, '.csv')
}
if(dir.table[ref == max(ref),on.art]){
  mortart <- paste0("/ihme/hiv/mrbrt_output/gbd20/", loc,"_HIVonART.csv")
  print('Using MRBRT')
  
} else {
  mortart <- paste0(root,"/temp/TB/joyma/HIV/model_draw2/gbd20/", loc,"_HIVonART.csv")
  print('Using test model')
}


print(paste('Using', ASFR, 'for ASFR', sep = ' '))
print(paste('Using', births, 'for births', sep = ' '))
print(paste('Using', SRB, 'for SRB', sep = ' '))
print(paste('Using', migration, 'for migration', sep = ' '))
print(paste('Using', prev_surveys, 'for prevalence surveys', sep = ' '))
if(grepl('ZAF', loc)){
  print(paste('Using', tem_art, 'for adult ART', sep = ' '))
}else{
  print(paste('Using', art.dt, 'for adult ART', sep = ' '))
}
print(paste('Using', population_single_age, 'for population', sep = ' '))
print(paste('Using', artdist, 'for child ART distribution', sep = ' '))
print(paste('Using', artelig, 'for child ART eligibility', sep = ' '))
print(paste('Using', percbf, 'for percent BF', sep = ' '))
print(paste('Using', mort.art, 'for child mortality on ART', sep = ' '))
print(paste('Using', prog, 'for child Prog param', sep = ' '))
print(paste('Using', mort.offart, 'for child mortality off art', sep = ' '))
print(paste('Using', dropout, 'for pmtct dropout rates', sep = ' '))
print(paste('Using', art, 'for child ART', sep = ' '))
print(paste('Using', pmtct, 'for PMTCT', sep = ' '))
# return(list( 'asfr' = ASFR,
#              'pop' = population_single_age,
#              'mig' = migration,
#              'birth' = births,
#              
#   
# ))

# }