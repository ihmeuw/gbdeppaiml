proj.end <- 2022
run.group2 <- FALSE
decomp.step <- "iterative"
ASFR <- 439
population <- 232
population_sa <- 186
migration <- 86
mlt <- 333
birth = 73
locs <- loc.table[epp == 1, ihme_loc_id]
diff <- list()
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
