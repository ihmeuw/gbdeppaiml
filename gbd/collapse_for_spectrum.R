################################################################################
## Purpose: Generate Spectrum-ready both sexes, age-aggregate incidence and prevalence
## Date created: May 31,2020
## Date modified:
## Author: Deepa Jahagirdar
## Run instructions: 
## Notes: We need aggregate incidence and prevalence if we want to run Spectrum or Forecasting pipelines
################################################################################

#' @import data.table, assertable
#' @param loc string, ihme_loc_id (example: MWI)
#' @param gbd_year string, GBD year (example: "gbd20")
#' @param run.name string, EPP-ASM run name (example: "190630_rhino2)

#' @return data.table that gets saved out in spectrum sub folder of the EPP-ASM run
#'

## Setup
rm(list=ls())
windows <- Sys.info()[1][["sysname"]]=="Windows"
root <- ifelse(windows,"J:/","/home/j/")
user <- ifelse(windows, Sys.getenv("USERNAME"), Sys.getenv("USER"))
code.dir <- paste0(ifelse(windows, "H:", paste0("/homes/", user)), "/gbdeppaiml/")

## Packages
library(data.table)
library(assertable)

## Arguments
args <- commandArgs(trailingOnly = TRUE)
if(length(args) > 0) {
  run.name <- args[2]
  loc <- args[1]
  gbd_year <- args[3]
} else {
  run.name <- "200505_xylo"
  loc <- 'MWI'
  gbd_year <- "gbd20"
}


##Paths
epp_path = paste0("/share/hiv/epp_output/",gbd_year,"/",run.name,"/compiled/")
out_path = paste0("/share/hiv/epp_output/",gbd_year,"/",run.name,"/aggregated/")
dir.create(out_path, recursive = TRUE, showWarnings = TRUE)

##Read in EPP-ASM compiled file
granular <- fread(paste0(epp_path,loc,".csv"))

##Collapse
spec.dt <- granular[age %in% 15:49, .(age, sex, year, run_num, pop_neg, new_hiv, pop)]
spec.dt <- spec.dt[,.(new_hiv = sum(new_hiv), 
                      pop_neg = sum(pop_neg), 
                      pop = sum(pop)), by = c('year', 'run_num')]
spec.dt[, inc := ifelse(pop_neg == 0, 0, new_hiv/pop_neg)]
spec.dt[, prev := ifelse(pop == 0, 0, (pop - pop_neg)/pop)]


##Incidence
inc.dt <- spec.dt[,.(year, run_num, inc)]
inc.dt[,inc:=inc*100]
inc.dt <- dcast.data.table(inc.dt,year~run_num, value.var='inc')
setnames(inc.dt, names(inc.dt)[!names(inc.dt) == 'year'], paste0('draw', names(inc.dt)[!names(inc.dt) == 'year']))
inc.dt <- inc.dt[order(year)]

##Prevalence
prev.dt <- spec.dt[,.(year, run_num, prev)]
prev.dt[,prev:=prev*100]
prev.dt <- dcast.data.table(prev.dt,year~run_num, value.var='prev')
setnames(prev.dt, names(prev.dt)[!names(prev.dt) == 'year'], paste0('draw', names(prev.dt)[!names(prev.dt) == 'year']))
prev.dt <- prev.dt[order(year)]

##Write out
dir.create(paste0(out_path,"/incidence/"))
write.csv(inc.dt,paste0(paste0(out_path,"/incidence/",loc,".csv")), row.names = FALSE)

dir.create(paste0(out_path,"/prevalence/"))
write.csv(prev.dt,paste0(paste0(out_path,"/prevalence/",loc,".csv")), row.names = FALSE)











