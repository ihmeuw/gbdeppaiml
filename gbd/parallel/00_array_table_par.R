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

source(paste0(ifelse(windows, "H:", paste0("/ihme/homes/", user)), "/gbdeppaiml/gbd/00_req_packages.R"))

loc.list <- loc.table[epp == 1, ihme_loc_id]
loc.list <- c('AGO', 'CAF', 'COD', 'COG', 'CPV', 'CIV')
run.name <- "201015_socialdets_sens"
cores = 20
desired_draws = 100


draw.colnames <- paste0('draw_', c(1:20))
desired_draws = 100
draw.dt <- matrix(data = c(1:desired_draws), ncol = 20, byrow = T)
draw.dt <- as.data.table(draw.dt)
colnames(draw.dt) <- draw.colnames
array.dt <- list(ihme_loc_id = loc.list, scale_foi = seq(0.1, 1, by = 0.05))
array.dt <- expand.grid(array.dt)
array.dt <- data.table(array.dt)
colnames(array.dt) <- c('ihme_loc_id', 'scale_foi')
array.dt.list <- list()
for(row in 1:nrow(array.dt)){
  array.dt.list <- rbind(array.dt.list, cbind(array.dt[row,],draw.dt))
}
array.dt <- array.dt.list
array.dt <- array.dt[!grepl('_', array.dt[,ihme_loc_id])]
array.dt[,loc_scalar := paste0(ihme_loc_id, '_', scale_foi)]

##run name needs to be changed here
dir.create(paste0('/ihme/hiv/epp_input/gbd20/', run.name))
write.csv(array.dt, paste0('/ihme/hiv/epp_input/gbd20/', run.name,'/array_table.csv'), row.names = F)




