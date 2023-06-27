## Tahvi Frank
## tahvif@uw.edu/tahvif@gmail.com
### Setup
rm(list=ls())
Sys.umask(mode = "0002")
windows <- Sys.info()[1][["sysname"]]=="Windows"
root <- ifelse(windows,"J:/","/home/j/")
user <- ifelse(windows, Sys.getenv("USERNAME"), Sys.getenv("USER"))
code.dir <- paste0(ifelse(windows, "H:", paste0("/ihme/homes/", user)), "/gbdeppaiml/")
date <- substr(gsub("-","",Sys.Date()),3,8)

## Packages
library(data.table)

## Arguments
run.name <- "230620_falcon"
gbd.run.name <- "200713_yuka"
transition.year <- 2021
proj.end <- 2050
n.draws = 10
array.job = T
paediatric <- TRUE
cluster.project <- "proj_hiv"
gbdyear <- "gbd20"
c.scenario <- 'reference'

### Paths
input.dir <- paste0("/ihme/hiv/epp_input/", gbdyear, '/', run.name, "/")
dir.create(input.dir, recursive = TRUE, showWarnings = FALSE)
dir <- paste0("/ihme/hiv/epp_output/", gbdyear, '/', run.name, "/")
dir.create(dir, showWarnings = FALSE)
dir.table <- fread(paste0('/share/hiv/epp_input/gbd20//dir_table_log_gbd20.csv'))

run.table <- fread(paste0('/share/hiv/epp_input/gbd20//eppasm_run_table.csv'))

### Functions
source(paste0('/ihme/homes/', user, '/rt-shared-functions/cluster_functions.R'))
source(paste0(root,"/Project/Mortality/shared/functions/check_loc_results.r"))
library(mortdb, lib = "/ihme/mortality/share/r/4")
library(mortdb, lib = "/mnt/team/mortality/pub/shared/r/4")

### Tables
loc.table <- data.table(get_locations(hiv_metadata = T))

### Code
forecast.list <- readRDS(paste0('/ihme/hiv/spectrum_input/20230202_forecasting/loc_list.RDS'))
loc.list = loc.table[spectrum == 1 & !grepl('NOR_6', ihme_loc_id) & !(grepl('GBR', ihme_loc_id) & most_detailed == 0), ihme_loc_id]

# Prep demographic inputs
if(!file.exists(paste0(input.dir, "population/"))) {
  prep.job <- paste0("qsub -l m_mem_free=10G -l fthread=1 -l h_rt=00:30:00 -q all.q -N forecast_prep_inputs_", run.name," -P ",cluster.project," ",
                      "-e /share/temp/sgeoutput/", user, "/errors ",
                      "-o /share/temp/sgeoutput/", user, "/output ",
                      code.dir, "gbd/singR_shell.sh ",
                      code.dir, "gbd/forecasting_prep_inputs.R"," ",run.name," ",proj.end, " ", gbd.run.name, " ", transition.year, ' ', 10)
  print(prep.job)
  system(prep.job)
}

## Prep scenario-specific inputs projected based on rate of change (child ART, cotrim, PMTCT) 
if(!file.exists(paste0('/share/hiv/spectrum_input/', run.name, '_', c.scenario, '/PMTCT/'))) {
  drivers.job <- paste0("qsub -l m_mem_free=10G -l fthread=1 -l h_rt=00:30:00 -q all.q -N forecast_drivers_", run.name," -P ",cluster.project," ",
                     "-e /share/temp/sgeoutput/", user, "/errors ",
                     "-o /share/temp/sgeoutput/", user, "/output ",
                     code.dir, "gbd/singR_shell.sh ",
                     code.dir, "gbd/forecasting_prep_drivers.R"," ",run.name," ",gbd.run.name, " ",c.scenario, " ", proj.end, " ", 10, " ", n.draws, " ", transition.year)
  print(drivers.job)
  system(drivers.job)
}

## NOTE - need to prep incidence/transmission rate, adult ART coverage
## This code has not been changed from hiv_forecasting_inputs repo

for(loc in loc.list) {  
  submit_array_job(script = paste0(code.dir, 'gbd/main_forecasting.R'), n_jobs = n.draws,
                   queue = 'all.q', memory = '7G', threads = 1, time = "24:00:00",  name = paste0(loc, '_', run.name, '_eppasm'),
                   archive = F, args = c(run.name, loc, proj.end, paediatric, c.scenario, transition.year, gbd.run.name))
}
# #Make sure all locations are done
dirs = paste0('/ihme/hiv/epp_output/', gbdyear, '/', run.name, '/', loc.list)
lapply(dirs, dir.exists)

#Draw compilation

submit_job(script = paste0(code.dir, 'gbd/compile_draws.R'),
           queue = 'all.q', memory = '30G', threads = 1, time = "01:00:00", name = paste0(loc, '_', run.name, '_compile'),
           archive = F, args = c(run.name,  array.job,  loc,  'TRUE', paediatric))

# Make sure all locations are done
check_files(paste0(loc.list, '.csv'),paste0('/share/hiv/epp_output/', gbdyear, '/', run.name, '/compiled/'))


submit_job(script = paste0(code.dir, 'gbd/get_summary_files.R'),
           queue = 'all.q', memory = '5G', threads = 1, time = "01:00:00", name = paste0(loc, '_', run.name, '_summary'),
           archive = F, args = c(run.name,   loc))
#  #Make sure all locations are done
check_loc_results(paste0(loc.list, '.csv'),paste0('/share/hiv/epp_output/', gbdyear, '/', run.name, '/summary_files/'))










  
