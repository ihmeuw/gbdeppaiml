## ---------------------------
## Script name: vet_inputs.R
## Purpose of script: checks that the version that was used is equal to the version that was anticipated
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

source(paste0(ifelse(windows, "H:", paste0("/ihme/homes/", user)), "/gbdeppaiml/gbd/00_req_packages.R"))
proj.end <- 2022
locs <- loc.table[epp == 1, ihme_loc_id]
id_table <- fread(paste0('/ihme/homes/', user, '/gbdeppaiml/gbd/input_ids.csv'))
run.name = '200713_yuka'
population = id_table[run == run.name, population]
population_sa = id_table[run == run.name, population_sa]
ASFR = id_table[run == run.name, ASFR]
birth = id_table[run == run.name, birth]
migration = id_table[run == run.name, migration]


diff <- list()

diff_func <- function(loc, input){
  
}
for (loc in locs) {
 used <- fread(paste0('/ihme/hiv/epp_input/gbd20/200713_yuka/population/',loc,'.csv'))
 correct_pull <- get_mort_outputs(
   "population", "estimate",
   gbd_year = 2020,
   run_id = population,
   age_group_id =c(8:20),
   location_id = loc.table[ihme_loc_id == loc, location_id], year_id = seq(1970, proj.end), sex_id = 1:2)
 setnames(correct_pull, 'mean', 'population')
 correct_pull <- correct_pull[,.(age_group_id, location_id, year_id, sex_id, population, run_id)]
 setnames(used, 'population', 'used')
 setnames(correct_pull, 'population', 'correct')
 
 dt <- merge(used, correct_pull, by = c('age_group_id', 'location_id',
                                        'year_id', 'sex_id'))
 dt[,diff := used - correct]
 
 diff[[loc]] <- dt[,diff]
 
}
