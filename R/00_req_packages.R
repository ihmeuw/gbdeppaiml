## ---------------------------
## Script name: 00_req_packages
##
## Purpose of script: Load in all of the required packages into the R workspace, can be modified by users needs
##
## Author: Maggie Walters
##
## Date Created: 2020-10-07
##
## Email: mwalte10@uw.edu
##
## ---------------------------
##
## Notes:
##loading in the hiv functions requires some specification of loc, gbdyear, and run.name
loc = 'AGO'
gbdyear = 'gbd20'
run.name = '200713_yuka'
##
## ---------------------------

## set gbdeppaiml and hiv_gbd2019 directories
windows <- Sys.info()[1][["sysname"]]=="Windows"
user <- ifelse(windows, Sys.getenv("USERNAME"), Sys.getenv("USER"))
root <- ifelse(windows,"J:/","/home/j/")
# gbdeppaiml_dir <- paste0(ifelse(windows, "H:", paste0("/ihme/homes/", user)), "/gbdeppaiml/")
eppasm_dir <- paste0(ifelse(windows, "H:", paste0("/ihme/homes/", user)), "/eppasm/")
hiv_gbd2019_dir <- paste0(ifelse(windows, "H:", paste0("/ihme/homes/", user)), "/hiv_gbd2019/")

## ---------------------------
##Frequently used packages
library(data.table); library(mvtnorm); library(survey); library(ggplot2); library(plyr); library(dplyr); library(assertable); library(parallel)
library(mortdb, lib = "/share/mortality/shared/r/")


## load in ihme functions
invisible(sapply(list.files("/share/cc_resources/libraries/current/r/", full.names = T), source))

## load in hiv functions
# setwd(eppasm_dir)
# devtools::load_all()
# setwd(gbdeppaiml_dir)
# devtools::load_all()
source(paste0(root,"/Project/Mortality/shared/functions/check_loc_results.r"))

##load in loc.table
loc.table <- get_locations(hiv_metadata = TRUE)

##load in age map
age.map <- get_age_map()
