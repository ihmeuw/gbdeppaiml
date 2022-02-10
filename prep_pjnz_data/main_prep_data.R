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
  loc <- "BRA"
  unaids_year <- 2015


}
### Functions
library(mortdb, lib = "/mnt/team/mortality/pub/shared/r")
setwd(paste0(ifelse(windows, "H:", paste0("/ihme/homes/", user)), "/eppasm/"))
devtools::load_all()
setwd(code.dir)
devtools::load_all()

### Tables
loc.table <- data.table(get_locations(hiv_metadata = T))

##get the subnats to have unaids_recent


loc.list.first <- loc.table[unaids_recent != 2013 & spectrum == 1 & epp != 1 & group != '1A' & group != '1B',ihme_loc_id] %>% unique
loc.list <- loc.table[!is.na(unaids_recent) & unaids_recent != 2013 & epp != 1 & group != '1A' & group != '1B',ihme_loc_id] %>% unique
#loc.list <- loc.table[unaids_recent == 2015 & epp != 1 & group != '1A' & group != '1B',ihme_loc_id] %>% unique

unaids_2020 <- strsplit(list.dirs('/snfs1/DATA/UNAIDS_ESTIMATES/2020/'), split = '//')
unaids_2020 <- sapply(unaids_2020[2:length(unaids_2020)], '[[', 2)

notworking <- c('ESP', 'BHS', 'COL', 'CRI', 'NIC', 'VEN', 'KWT', 'OMN', 'SYR', 'BGD', 'BTN', 'STP', 'MEX_4657')
loc.list <- setdiff(loc.list, loc.list.first)

##MAR IS NOT WORKING
# for(loc in loc.list[(which(loc.list == 'MRT')+1):length(loc.list)]){
for(loc in loc.list[2:length(loc.list)]){
unaids_year <- loc.table[ihme_loc_id==loc, unaids_recent]
if(is.na(unaids_year)){next}
dir.create(paste0("/share/hiv/data/PJNZ_prepped/",unaids_year,"/"),recursive = TRUE, showWarnings = FALSE)


if(grepl('1', loc.table[ihme_loc_id == loc, group])){
  if(!grepl('IND', loc)){
    val <- prepare_spec_object(loc)
    print(attr(val,"country"))
  }else{
    val <- prepare_spec_object_ind(loc)
  }
}else{
  ##should be run on the vr_ancbias branch
  ##need to integrate changes made on the dev_step4a branch into vr_ancbias
  val <- prepare_spec_object_group2(loc)
}


saveRDS(val, paste0("/share/hiv/data/PJNZ_prepped/",unaids_year,"/", loc, '.rds'))

}
cal = readRDS(paste0("/share/hiv/data/PJNZ_EPPASM_prepped/", loc, '.rds'))
nrow(unique(attr(cal,"eppd")$ancsitedat))


