## ---------------------------
## Script name: 02_launch_eppasm.R
## Purpose of script: launch the eppasm model
##
## Author: Tahvi Frank
## Date Created: 2019
## Modified by Maggie Walters, mwalte10@uw.edu
## Email: mwalte10@uw.edu
## ---------------------------
##
## Notes:
##   
##
## ---------------------------

## Used in basically every script
rm(list = ls())
Sys.umask(mode = "0002")
windows <- Sys.info()[1][["sysname"]]=="Windows"
root <- ifelse(windows,"J:/","/home/j/")
user <- ifelse(windows, Sys.getenv("USERNAME"), Sys.getenv("USER"))
# gbdeppaiml_dir <- paste0(ifelse(windows, "H:", paste0("/ihme/homes/", user)), "/gbdeppaiml/")
# setwd(gbdeppaiml_dir)
# devtools::load_all()
library(data.table)
date <- substr(gsub("-","",Sys.Date()),3,8)
source(paste0('/ihme/homes/', user, '/rt-shared-functions/cluster_functions.R'))


## Arguments
#run.name = '200713_yuka_newUNAIDS'
run.name = "240529_meixin_test2art"
compare.run <- c("231129_bandicoot")
proj.end <- 2024
if(file.exists(paste0('/ihme/hiv/epp_input/gbd20/',run.name,'/array_table.csv'))){
  n.draws = nrow(fread(paste0('/ihme/hiv/epp_input/gbd20/',run.name,'/array_table.csv')))
  array.job = T
}else{
  n.draws = 1000
  array.job = F
}
run.group2 <- FALSE
paediatric <- TRUE
cluster.project <- "proj_hiv"
plot_ART <- FALSE
reckon_prep <- FALSE
decomp.step <- "iterative"
gbdyear <- "gbd23"
redo_offsets <- F
testing = FALSE
test = NULL
run_eppasm = T
code.dir = paste0('/homes/', user, '/gbdeppaiml/')

### Paths
dir <- paste0("/ihme/hiv/epp_output/", gbdyear, '/', run.name, "/")
dir.create(dir, showWarnings = FALSE)
# dir.table <- fread(paste0('/share/hiv/epp_input/gbd20//dir_table_log_gbd20.csv'))
# 
# run.table <- fread(paste0('/share/hiv/epp_input/gbd23//eppasm_run_table.csv'))

# if(!run.name %in% unique(run.table$run_name)){
#   stop("Add run comment and new run to run tracker")
#   # new_run = copy(run.table)[run_name == "200713_yuka"]
#   # new_run[,run_name := run.name]
#   # new_run[,comments := "Testing swap SWZ pop structure for LSO"]
#   # run.table = rbind(run.table,new_run)
#   # fwrite(run.table,paste0('/share/hiv/epp_input/gbd20//eppasm_run_table.csv'),row.names = FALSE)
# }     


### Functions
source(paste0(root,"/Project/Mortality/shared/functions/check_loc_results.r"))
library(mortdb, lib ="/mnt/team/mortality/pub/shared/r/4")

### Tables
loc.table <- data.table(get_locations(hiv_metadata = T))

### Code
epp.list <- sort(loc.table[epp == 1 & grepl('1', group), ihme_loc_id])
loc.list <- epp.list
# loc.have <- list.dirs("/share/hiv/epp_output/gbd22/230809_meixin/fit")
# loc.have <- gsub("/share/hiv/epp_output/gbd22/230809_meixin/fit/","",loc.have)
# setdiff(loc.list, loc.have)

# EPP-ASM ---------------------------------------
if(run_eppasm & !array.job){
    for(loc in loc.list) {    
      ## Run EPPASM
      submit_array_job(script = paste0(code.dir, 'gbd/main.R'), n_jobs = n.draws,
                       queue = 'all.q', memory = '7G', threads = 1, time = "24:00:00", name = paste0(loc, '_', run.name, '_eppasm'),
                       archive = F, args = c(run.name, loc, proj.end, paediatric, TRUE))
    }
}
# EPP-ASM test for MOZ
loc <- "MOZ"
for(c.scalar in seq(0.5, 1, 0.1)) {    
  ## Run EPPASM
  submit_array_job(script = paste0(code.dir, 'gbd/main_MOZ_test.R'), n_jobs = n.draws,
                   queue = 'all.q', memory = '7G', threads = 1, time = "24:00:00", name = paste0(loc, '_', run.name, '_eppasm'),
                   archive = F, args = c(run.name, loc, proj.end, paediatric, TRUE, c.scalar))
}
for (loc in loc.list) {
  for(c.scalar in c(1)) {    
    ## Run EPPASM
    submit_array_job(script = paste0(code.dir, 'gbd/main_MOZ_test.R'), n_jobs = n.draws,
                     queue = 'all.q', memory = '7G', threads = 1, time = "24:00:00", name = paste0(loc, '_', run.name, '_eppasm'),
                     archive = F, args = c(run.name, loc, proj.end, paediatric, TRUE, c.scalar))
  }
}

    ## Make sure all locations are done
    dirs = paste0('/ihme/hiv/epp_output/', gbdyear, '/', run.name, '/', loc.list)
    lapply(dirs, dir.exists)

    #Draw compilation
    for(loc in loc.list) {
      submit_job(script = paste0(code.dir, 'gbd/compile_draws.R'),
                      queue = 'all.q', memory = '30G', threads = 1, time = "01:00:00", name = paste0(loc, '_', run.name, '_compile'),
                      archive = F, args = c(run.name,  array.job,  loc,  'TRUE', paediatric, gbdyear))
    }
    # Make sure all locations are done
    check_files(paste0(loc.list, '.csv'),paste0('/share/hiv/epp_output/', gbdyear, '/', run.name, '/compiled/'))

    for(loc in loc.list) {
      submit_job(script = paste0(code.dir, 'gbd/get_summary_files.R'),
                 queue = 'all.q', memory = '10G', threads = 1, time = "01:00:00", name = paste0(loc, '_', run.name, '_summary'),
                 archive = F, args = c(run.name,   loc, gbdyear))
    }
      #  #Make sure all locations are done
       check_loc_results(paste0(loc.list, '.csv'),paste0('/share/hiv/epp_output/', gbdyear, '/', run.name, '/summary_files/'))

    for(loc in loc.list) {
      submit_job(script = paste0(code.dir, 'gbd/main_plot_output.R'),
                 queue = 'all.q', memory = '20G', threads = 1, time = "00:15:00", name = paste0(loc, '_', run.name, '_plot'),
                 archive = T, args = c(loc, run.name, compare.run, gbdyear))
    }
    



# Compile plots ---------------------------------------
# plot.holds <- paste(paste0(loc.list, '_plot_eppasm'), collapse = ",")
# plot.string <- paste0("qsub -l m_mem_free=1G -l fthread=1 -l h_rt=00:35:00 -q all.q -P ", cluster.project, " ",
#                       "-e /share/homes/", user, "/errors ",
# 
#                       "-o /share/temp/sgeoutput/", user, "/output ",
#                       "-N ", "compile_plots_eppasm ",
#                       "-hold_jid ", plot.holds, " ",
#                       code.dir, "gbd/singR_shell.sh ",
#                       code.dir, "gbd/compile_plots.R ",
#                       run.name, " ", gbdyear)
# print(plot.string)
# system(plot.string)









  
