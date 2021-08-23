## ---------------------------
## Script name: launch_lbd_process.R
## Purpose of script: Align ANC data with LBD process. Approved by Chris in GBD20
##
## Author: Maggie Walters
## Date Created: 2020-10-12
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
code.dir <- paste0(ifelse(windows, "H:", paste0("/ihme/homes/", user)), "/gbdeppaiml/")
cluster.project <- 'proj.hiv'

source(paste0(ifelse(windows, "H:", paste0("/ihme/homes/", user)), "/gbdeppaiml/gbd/00_req_packages.R"))


##The input table contains all relevant file paths, if any of them are updated then the appropriate scripts are launched to realign the ANC data
input_table <- fread(paste0(ifelse(windows, "H:", paste0("/ihme/homes/", user)), '/gbdeppaiml/lbd_anc_align/inputs.csv'))

# Arguments ---------------------------------------
##This script is called by 01_prep_inputs.R
args <- commandArgs(trailingOnly = TRUE)
print(args)
if(length(args) > 0) {
  run.name <- args[1]
  run.name.old <- args[2]
} else {
  run.name.old <- '200316_windchime'
  run.name <- '210408_antman'
}

##This only needs to be rerun if any of the file paths in the input file have changed. Note that between runs the anc_no_offset WILL change
rerun_conditions <- c(input_table[run_name == run.name.old, lbd_anc_data] != input_table[run_name == run.name, lbd_anc_data],
                      input_table[run_name == run.name.old, geo_codebook] != input_table[run_name == run.name, geo_codebook],
                      input_table[run_name == run.name.old, sf_dir] != input_table[run_name == run.name, sf_dir],
                      input_table[run_name == run.name.old, rd] != input_table[run_name == run.name, rd],
                      input_table[run_name == run.name.old, lbd_anc_mean_est] != input_table[run_name == run.name, lbd_anc_mean_est])

# Run scripts ---------------------------------------
if(any(rerun_conditions == TRUE)){
  #Creates latitude and longitude table
  lat_long.string <- paste0("qsub -l m_mem_free=30G -l fthread=1 -l h_rt=01:00:00 -q all.q -P ", cluster.project, " ",
                            "-e /share/homes/", user,"/errors ",
                            "-o /share/temp/sgeoutput/", user, "/output ",
                            "-N ",  "lat_long ",
                            code.dir, "gbd/singR_shell.sh ",
                            code.dir, "lbd_anc_align/lat_long.R ")
  print(lat_long.string)
  system(lat_long.string)
  
  recreate.string <- paste0("qsub -l m_mem_free=30G -l fthread=1 -l h_rt=01:00:00 -q all.q -P ", cluster.project, " ",
                        "-e /share/homes/", user,"/errors ",
                        "-o /share/temp/sgeoutput/", user, "/output ",
                        "-N ",  "lbd_processing ",
                        "-hold_jid ", "lat_long ",
                        code.dir, "gbd/singR_shell.sh ",
                        code.dir, "lbd_anc_align/lbd_anc_recreate.R ", 
                        run.name)
  print(recreate.string)
  system(recreate.string)
  
  offset.string <- paste0("qsub -l m_mem_free=30G -l fthread=1 -l h_rt=01:00:00 -q all.q -P ", cluster.project, " ",
                        "-e /share/homes/", user,"/errors ",
                        "-o /share/temp/sgeoutput/", user, "/output ",
                        "-N ", "create_offsets ",
                        "-hold_jid ", "lbd_processing ",
                        code.dir, "gbd/singR_shell.sh ",
                        code.dir, "lbd_anc_align/offsets/create_offsets.R ")
  print(offset.string)
  system(offset.string)
}

