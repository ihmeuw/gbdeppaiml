## ########################################################################################################################################
##
## 
## Author: Maggie Walters (mwalte10@uw.edu)
## 
## Purpose: Retrieves India state populations and stores them at /ihme/hiv/epp_input/gbd*year*/*run_name_ind*/population
##          
## 
## 
##   
##   
##
## 
##   
##
## ########################################################################################################################################
rm(list = ls())
#########################
##general set up
#########################
{windows <- Sys.info()[1][["sysname"]]=="Windows"
root <- ifelse(windows,"J:/","/home/j/")
user <- ifelse(windows, Sys.getenv("USERNAME"), Sys.getenv("USER"))

## Packages
library(data.table)

args <- commandArgs(trailingOnly = TRUE)
if(length(args) > 0) {
  run.name <- args[1]
  proj.end <- args[2]
  run.group2 <- args[3]
} else {
  run.name <- "2020_ind_test_agg9"
  proj.end <- 2022
  run.group2 <- FALSE
}

gbdyear <- 'gbd20'
out.dir <- paste0('/ihme/hiv/epp_input/', gbdyear, '/', run.name, "/populations/")
dir.create(out.dir, recursive = TRUE, showWarnings = TRUE)

## Functions
library(mortdb, lib = "/home/j/WORK/02_mortality/shared/r")
source( "/ihme/cc_resources/libraries/current/r/get_population.R")
source('/ihme/cc_resources/libraries/current/r/get_covariate_estimates.R')
source('/ihme/cc_resources/libraries/current/r/get_cod_data.R')}

#########################
##India location ids
#########################
loc.table <- get_locations(hiv_metadata = TRUE)
loc.table[epp == 1 & most_detailed == 1,]
india.locs <- loc.table[grepl("IND_", loc.table[,ihme_loc_id]), location_id]

pop <- get_population(age_group_id = c(28, 49:127), location_id = india.locs, year_id = 1970:2019, gbd_round_id = 6, sex_id = 1:2, single_year_age = T, decomp_step = 'step4')
#EXTEND TO 2022
pop.all.20 <- pop[year_id == 2019,]
pop.all.20[,year_id := rep(2020, nrow(pop.all.20))] 
pop.all.21 <- pop[year_id == 2019,]
pop.all.21[,year_id := rep(2021, nrow(pop.all.21))] 
pop.all.22 <- pop[year_id == 2019,]
pop.all.22[,year_id := rep(2022, nrow(pop.all.22))] 
pop <- rbind(pop, pop.all.20, pop.all.21, pop.all.22)
## this is a separate call because you can't get 80+ with single_age_pop = TRUE
pop.o80 <- get_population(age_group_id = c(21), location_id = india.locs, year_id = 1970:2019, gbd_round_id = 6, sex_id = 1:2, decomp_step = 'step4')
#EXTEND TO 2022
pop.o80.20 <- pop.o80[year_id == 2019,]
pop.o80.20[,year_id := rep(2020, nrow(pop.o80.20))] 
pop.o80.21 <- pop.o80[year_id == 2019,]
pop.o80.21[,year_id := rep(2021, nrow(pop.o80.21))] 
pop.o80.22 <- pop.o80[year_id == 2019,]
pop.o80.22[,year_id := rep(2022, nrow(pop.o80.22))] 
pop.o80 <- rbind(pop.o80, pop.o80.20, pop.o80.21, pop.o80.22)
pop.all <- rbind(pop, pop.o80, use.names = T)


invisible(lapply(india.locs, function(c.location_id) {
  out.pop <- copy(pop.all[location_id == c.location_id])
  c.iso <- loc.table[location_id == c.location_id, location_id]
  write.csv(out.pop, paste0(out.dir, c.iso, ".csv"), row.names = F)
}))










