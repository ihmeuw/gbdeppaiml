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
  loc = 'SSD'

}

loc.table <- get_locations(gbd_year = 2020, hiv_metadata = T)

if(file.exists(paste0('/ihme/hiv/epp_input/gbd20/',run.name,'/array_table.csv'))){
  array.dt <- fread(paste0('/ihme/hiv/epp_input/gbd20/',run.name,'/array_table.csv'))
  locs <- unique(array.dt[,loc_scalar])
}else{
  locs <- loc.table[epp == 1, ihme_loc_id]
}

get_summary(loc,  run.name = run.name, gbdyear = 'gbd20',
            paediatric = T, old.splits = F)
