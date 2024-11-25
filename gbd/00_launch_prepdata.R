### Setup
rm(list=ls())
windows <- Sys.info()[1][["sysname"]]=="Windows"
root <- ifelse(windows,"J:/","/home/j/")
user <- ifelse(windows, Sys.getenv("USERNAME"), Sys.getenv("USER"))
code.dir <- paste0(ifelse(windows, "H:", paste0("/homes/", user)), "/gbdeppaiml/prep_pjnz_data/")
date <- substr(gsub("-","",Sys.Date()),3,8)

## Packages
library(data.table)

## Arguments
cluster.project <- "proj_hiv"
unaids_year <- 2023


## Launch prepare locations file
### Functions
# file_names <- list.files(path = paste0(ifelse(windows, "H:", paste0("/ihme/homes/", user)), "/eppasm/R/"), pattern = "\\.R$")
# file_names <- paste0(ifelse(windows, "H:", paste0("/ihme/homes/", user)), "/eppasm/R/", file_names)
# lapply(file_names, source)

library(mortdb, lib = "/home/j/WORK/02_mortality/shared/r")
library(eppasm, lib.loc = "/snfs1/Project/GBD_Select_Infectious_Diseases/J TEMP HOLD/HIV/packages_r")


gbdeppaiml_dir <- paste0(ifelse(windows, "H:", paste0("/ihme/homes/", user)), "/gbdeppaiml/")
setwd(gbdeppaiml_dir)
devtools::load_all()
source("/ihme/homes/mzhang25/eppasm/R/read-spectrum-files.R")
source("/ihme/homes/mzhang25/eppasm/R/spectrum.R")
source("/ihme/homes/mzhang25/eppasm_ihme/R/fit-model.R")
# source("/ihme/homes/mzhang25/gbdeppaiml/lbd_anc_align/archived_files/ancsitedat_func.R")
# source("/ihme/homes/mzhang25/gbdeppaiml/R/prep_pjnz_data.R")
# source("/ihme/homes/mzhang25/gbdeppaiml/R/gbd_prep_data.R")


### Functions
library(mortdb, lib = "/home/j/WORK/02_mortality/shared/r")
loc.table <- data.table(get_locations(hiv_metadata = T))

#Alternate metadata until 2019 becomes available
loc.table <- fread("/mnt/team/mortality/pub/shared/hiv_model_strategy_2024.csv")
all_locs = loc.table[unaids_recent==unaids_year & grepl("1",group) & epp==1,ihme_loc_id]

### Code
epp.list <- sort(loc.table[epp == 1, ihme_loc_id])
loc.list <- epp.list
dir.create(paste0('/ihme/hiv/data/PJNZ_prepped/', unaids_year, '/'))

# ### Tables
# loc.table <- data.table(get_locations(hiv_metadata = T))
# loc.list <- loc.table[,ihme_loc_id] %>% unique

unaids_2023 <- strsplit(list.dirs('/snfs1/DATA/UNAIDS_ESTIMATES/2023/'), split = '//')
unaids_2023 <- sapply(unaids_2023[3:length(unaids_2023)], '[[', 2)

unaids_2022 <- strsplit(list.dirs('/snfs1/DATA/UNAIDS_ESTIMATES/2022/'), split = '//')
unaids_2022 <- sapply(unaids_2022[2:length(unaids_2022)], '[[', 2)


for(loc in all_locs){
  unaids_year <- loc.table[ihme_loc_id==loc, unaids_recent]
  dir.create(paste0("/share/hiv/data/PJNZ_prepped/",unaids_year,"/"),recursive = TRUE, showWarnings = FALSE)
  
  
  if(grepl('1', loc.table[ihme_loc_id == loc, group])){
    if(!grepl('IND', loc)){
      val <- prepare_spec_object(loc, gbdyear = gbdyear)
      print(attr(val,"country"))
    }else{
      val <- prepare_spec_object_ind(loc)
    }
  }else{
    val <- prepare_spec_object_group2(loc)
  }
  
  
  saveRDS(val, paste0("/share/hiv/data/PJNZ_prepped/",unaids_year,"/", loc, '.rds'))
  
}
cal = readRDS(paste0("/share/hiv/data/PJNZ_EPPASM_prepped/", loc, '.rds'))
nrow(unique(attr(cal,"eppd")$ancsitedat))


### End



