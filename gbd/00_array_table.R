## ---------------------------
## Script name: array_table.R
## Purpose of script: run to set up an array table if iterating over anything
##
## Author: Maggie Walters
## Date Created: 2020-10-07
## Email: mwalte10@uw.edu
## ---------------------------
##
## Notes: 
##   
##

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

loc.list <- loc.table[epp == 1, ihme_loc_id]
run.name <- "201113_socialdets"

pred.mat <- readRDS('/ihme/homes/mwalte10/hiv_gbd2019/requests/haidong_proj/maggie/pref_mat.RDS')
array.dt <- list(ihme_loc_id = unique(pred.mat$ihme_loc_id)[1:5], year_id = c(2000,2022), draws = 1, combos = c(1:128))
array.dt <- expand.grid(array.dt)
array.dt <- data.table(array.dt)
colnames(array.dt) <- c('ihme_loc_id', 'year_id', 'draws', 'combo')
array.dt <- array.dt[!grepl('_', array.dt[,ihme_loc_id])]
array.dt[,loc_scalar := paste0(ihme_loc_id, '_', combo)]

##run name needs to be changed here
dir.create(paste0('/ihme/hiv/epp_input/gbd20/', run.name))
write.csv(array.dt, paste0('/ihme/hiv/epp_input/gbd20/', run.name,'/array_table.csv'), row.names = F)
