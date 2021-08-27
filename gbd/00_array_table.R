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
run.name <- "soc_dets_run"

parent_child <- loc.table[epp  == 1 & level == 4 | epp == 1 & level == 5,.(ihme_loc_id, parent_id)]
parent_child <- parent_child[!grep('IND', ihme_loc_id)]
parent_child <- parent_child[grep('KEN', ihme_loc_id), parent_id := 180]


pred.mat <- readRDS(paste0('/ihme/hiv/epp_input/gbd20/', run.name, '/pred_mat_country_spec.RDS'))

# copy <- list()
# for(loc in c('NGA', 'KEN', 'ETH', 'ZAF')){
#   x <- copy(pred.mat[ihme_loc_id == loc,])
#   children <- parent_child[loc.table[ihme_loc_id == loc, location_id] == parent_id, ihme_loc_id]
#   child.dt <- list()
#   for(child in children){
#     y <- copy(x)
#     y[,ihme_loc_id := child]
#     y[,loc_year := paste0(ihme_loc_id, '_', year_id)]
#     child.dt <- rbind(child.dt, y)
#   }
#   copy <- rbind(copy, child.dt)
# }
# pred.mat <- rbind(pred.mat[!ihme_loc_id %in% c('NGA', 'KEN', 'ETH', 'ZAF'),], copy)

array.dt <- list(ihme_loc_id = unique(pred.mat$ihme_loc_id), combos = unique(pred.mat$combo))
array.dt <- expand.grid(array.dt)
array.dt <- data.table(array.dt)
if(run.name == '201217_socialdets'){
  array.dt <- rbind(array.dt, data.table(ihme_loc_id = 'AGO', combos = 5))
}
# colnames(array.dt) <- c('ihme_loc_id', 'year_id', 'draws', 'combo')
colnames(array.dt) <- c('ihme_loc_id','combo')
array.dt[,loc_scalar := paste0(ihme_loc_id, '_', combo)]

pred.mat[,year_id := year_var]
scalar <- unique(pred.mat[ihme_loc_id %in% unique(array.dt$ihme_loc_id),.(ihme_loc_id, combo, pred, foi, year_id)])
scalar[,scale_foi := pred/ foi]
if(run.name == '201217_socialdets'){
  scalar_addition <- list(ihme_loc_id = 'AGO', combo = 5, year_id = c(2500),
                          pred = 0, foi = 0, scale_foi = 1)
  scalar_addition <- data.table(expand.grid(scalar_addition))
  scalar <- rbind(scalar, scalar_addition)
}
if(run.name == '201226_socialdets'){
  scalar[,scale_foi := 1]
  scalar[,pred := foi]
}


array.dt <- merge(scalar[,.(ihme_loc_id, combo, pred, scale_foi, year_id)], array.dt, by = c('ihme_loc_id', 'combo'))

##run name needs to be changed here
dir.create(paste0('/ihme/hiv/epp_input/gbd20/', run.name))
write.csv(array.dt, paste0('/ihme/hiv/epp_input/gbd20/', run.name,'/array_table.csv'), row.names = F)
