### Setup
rm(list=ls())
windows <- Sys.info()[1][["sysname"]]=="Windows"
root <- ifelse(windows,"J:/","/home/j/")
user <- ifelse(windows, Sys.getenv("USERNAME"), Sys.getenv("USER"))
code.dir <- paste0(ifelse(windows, "H:", paste0("/ihme/homes/", user)), "/gbdeppaiml/")

## Packages
library(data.table); library(mvtnorm); library(survey);

## Arguments
args <- commandArgs(trailingOnly = TRUE)
print(args)
if(length(args) > 0) {
  loc <- args[1]
  unaids_year <- args[2]
} else {
  loc <- "AGO"
  unaids_year <- 2021


}
### Functions
library(mortdb, lib = "/home/j/WORK/02_mortality/shared/r")
setwd(paste0(ifelse(windows, "H:", paste0("/ihme/homes/", user)), "/eppasm/"))
devtools::load_all()
setwd(code.dir)
devtools::load_all()

### Tables
loc.table <- readRDS('/ihme/hiv/epp_input/gbd20/loc_table.RDS')
loc.list <- loc.table[,ihme_loc_id] %>% unique
loc.table[grepl('ZAF', ihme_loc_id), unaids_2021 := 1]
loc.table[grepl('ZAF', ihme_loc_id), unaids_recent := 2021]

loc.table[grepl('ETH', ihme_loc_id), unaids_2021 := loc.table[ihme_loc_id == 'ETH', unaids_2021]]
loc.table[grepl('ETH', ihme_loc_id), unaids_recent := loc.table[ihme_loc_id == 'ETH', unaids_recent]]

loc.table[grepl('NGA', ihme_loc_id), unaids_2021 := loc.table[ihme_loc_id == 'NGA', unaids_2021]]
loc.table[grepl('NGA', ihme_loc_id), unaids_recent := loc.table[ihme_loc_id == 'NGA', unaids_recent]]

loc.table[grepl('KEN', ihme_loc_id), unaids_2021 := loc.table[ihme_loc_id == 'KEN', unaids_2021]]
loc.table[grepl('KEN', ihme_loc_id), unaids_recent := loc.table[ihme_loc_id == 'KEN', unaids_recent]]
saveRDS(loc.table, '/ihme/hiv/epp_input/gbd20/loc_table.RDS')


loc.list <- setdiff(loc.list, c("MUS", "PRT", "QAT", "SAU", "SGP", "STP", "UKR", "URY", "UZB", "VNM", loc.list[grepl('ZAF',loc.list)], 
                                'MDA', 'BRA', 'KWT', loc.list[grepl('MEX',loc.list)]))
loc.list <- c(loc.list, 'MEX')

##MKW, 11/8/2021: only doing locations that have their pjnz moved easily, not doing india.
for(loc in loc.list){
unaids_year <- loc.table[ihme_loc_id==loc, unaids_recent]
if(is.na(unaids_year)){next}
dir.create(paste0("/share/hiv/data/PJNZ_prepped/",unaids_year,"/"),recursive = TRUE, showWarnings = FALSE)
if(file.exists(paste0("/share/hiv/data/PJNZ_prepped/",unaids_year,"/", loc, '.rds')) | unaids_year == 2013){next}

if(grepl('1', loc.table[ihme_loc_id == loc, group])){
  if(!grepl('IND', loc)){
    #tryCatch({
      val <- prepare_spec_object(loc)
      
    #}, error=function(e){})
    #if('val' %in% ls()){
      print(attr(val,"country"))
      
    #}
  }else{
  
    val <- prepare_spec_object_ind(loc)
  }
}else{
  val <- prepare_spec_object_group2(loc)
}

# if('val' %in% ls()){
#   tryCatch({
     saveRDS(val, paste0("/share/hiv/data/PJNZ_prepped/",unaids_year,"/", loc, '.rds'))
    
  #}, error=function(e){})
  rm(val)
  }

#}

