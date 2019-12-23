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
  loc <- "SDN"
  unaids_year <- 2019

}
### Functions
library(mortdb, lib = "/home/j/WORK/02_mortality/shared/r")
setwd(paste0(ifelse(windows, "H:", paste0("/ihme/homes/", user)), "/eppasm/"))
devtools::load_all()
setwd(code.dir)
devtools::load_all()

### Tables
loc.table <- data.table(get_locations(hiv_metadata = T))
dir.create(paste0("/share/hiv/data/PJNZ_prepped/",unaids_year,"/"),recursive = TRUE)

#Alternate metadata until 2019 becomes available
loc.table <- fread("/ihme/mortality/shared/hiv_model_strategy_2020.csv")
loc.list = loc.table[unaids_2019==1 & grepl("1",group) & epp==1,ihme_loc_id]


if(grepl('1', loc.table[ihme_loc_id == loc, group])){
  if(!grepl('IND', loc)){
    val <- prepare_spec_object(loc)
  }else{
    val <- prepare_spec_object_ind(loc)
  }
}else{
  val <- prepare_spec_object_group2(loc)
}

for(loc in l){
print(loc)
val <- prepare_spec_object(loc)
saveRDS(val, paste0("/share/hiv/data/PJNZ_prepped/",unaids_year,"/", loc, '.rds'))

}
