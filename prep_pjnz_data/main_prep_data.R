### Setup
rm(list=ls())
windows <- Sys.info()[1][["sysname"]]=="Windows"
root <- ifelse(windows,"J:/","/home/j/")
user <- ifelse(windows, Sys.getenv("USERNAME"), Sys.getenv("USER"))
code.dir <- paste0(ifelse(windows, "H:", paste0("/homes/", user)), "/gbdeppaiml/")

## Packages
library(data.table); library(mvtnorm); library(survey);

## Arguments
args <- commandArgs(trailingOnly = TRUE)
print(args)
if(length(args) > 0) {
  loc <- args[1]
} else {
  loc <- "PER"
}
### Functions
library(mortdb, lib = "/home/j/WORK/02_mortality/shared/r")
setwd(paste0(ifelse(windows, "H:", paste0("/homes/", user)), "/eppasm/"))
devtools::load_all()
setwd(code.dir)
devtools::load_all()

dir.create("/share/hiv/data/PJNZ_EPPASM_prepped_2a/")
### Tables
loc.table <- data.table(get_locations(hiv_metadata = T))


if(is.na(loc.table[ihme_loc_id==loc,unaids_recent])){
    stop("no UNAIDS year")
  }
  
if(loc.table[ihme_loc_id==loc,unaids_recent] %in% c(2013)){
    stop("old PJNZ")
  }
  
if(grepl('1', loc.table[ihme_loc_id == loc, group])){
  if(!grepl('IND', loc)){
    val <- prepare_spec_object(loc)
  }else{
    val <- prepare_spec_object_ind(loc)
  }
}else{
  val <- prepare_spec_object_group2(loc)
}


saveRDS(val, paste0('/share/hiv/data/PJNZ_EPPASM_prepped_2a/', loc, '.rds'))


