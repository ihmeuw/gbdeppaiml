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
rm(list = ls())
Sys.umask(mode = "0002")
windows <- Sys.info()[1][["sysname"]]=="Windows"
root <- ifelse(windows,"J:/","/home/j/")
user <- ifelse(windows, Sys.getenv("USERNAME"), Sys.getenv("USER"))
h_root = '/homes/mwalte10/'
lib.loc <- paste0(h_root,"R/",R.Version(),"/",R.Version(),".",R.Version())
.libPaths(c(lib.loc,.libPaths()))
packages <- c('fastmatch')
for(p in packages){
if(p %in% rownames(installed.packages())==FALSE){
install.packages(p)
}
library(p, character.only = T)
}
library(vctrs, lib.loc="/ihme/singularity-images/rstudio/lib/4.1.3.4")
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
  gbdyear = args[3]
} else {
  run.name = '220329_maggie'
  loc <- 'AGO'
  gbdyear = "gbdTEST"
}
library(mortdb, lib ="/mnt/team/mortality/pub/shared/r/4")

loc.table <- get_locations(gbd_year = 2020, hiv_metadata = T)

if(file.exists(paste0('/ihme/hiv/epp_input/gbd20/',run.name,'/array_table.csv'))){
  array.dt <- fread(paste0('/ihme/hiv/epp_input/gbd20/',run.name,'/array_table.csv'))
  locs <- unique(array.dt[,loc_scalar])
}else{
  locs <- loc.table[epp == 1, ihme_loc_id]
}
get_summary(loc,  run.name = run.name, gbdyear = gbdyear,
            paediatric = F, old.splits = F)
