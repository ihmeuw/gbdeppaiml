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
date <- substr(gsub("-","",Sys.Date()),3,8)

## Arguments
run.name <- "210408_antman"
spec.name <- "210408_antman"
compare.run <- c("200713_yuka")

proj.end <- 2023
if(file.exists(paste0('/ihme/hiv/epp_input/gbd20/',run.name,'/array_table.csv'))){
  n.draws = nrow(fread(paste0('/ihme/hiv/epp_input/gbd20/',run.name,'/array_table.csv')))
  array.job = T
}else{
  n.draws = 10
  array.job = F
}
run.group2 <- FALSE
paediatric <- TRUE
cluster.project <- "proj_hiv"
plot_ART <- FALSE
reckon_prep <- FALSE
decomp.step <- "iterative"
gbdyear <- "gbd21"
redo_offsets <- F
testing = FALSE
test = NULL
run_eppasm = T
code.dir = '/homes/mwalte10/gbdeppaiml/'

### Paths
input.dir <- paste0("/ihme/hiv/epp_input/", gbdyear, '/', run.name, "/")
dir.create(input.dir, recursive = TRUE, showWarnings = FALSE)
dir <- paste0("/ihme/hiv/epp_output/", gbdyear, '/', run.name, "/")
dir.create(dir, showWarnings = FALSE)
dir.table <- fread(paste0('/share/hiv/epp_input/gbd21//dir_table_log_gbd20.csv'))

run.table <- fread(paste0('/share/hiv/epp_input/gbd21//eppasm_run_table.csv'))

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

### Tables
loc.table <- data.table(get_locations(hiv_metadata = T))

### Code
epp.list <- sort(loc.table[epp == 1 & grepl('1', group), ihme_loc_id])
loc.list <- c(epp.list, 'STP', 'MRT', 'COM')
# loc.list <- c(loc.list[grepl('IND', loc.list)])

# Array job EPP-ASM ---------------------------------------
if(array.job){
  epp.string <- paste0("qsub -l m_mem_free=7G -l fthread=1 -l h_rt=24:00:00 -l archive=True -q all.q -P ", cluster.project, " ",
                     "-e /share/temp/sgeoutput/", user, "/errors ",
                     "-o /share/temp/sgeoutput/", user, "/output ",
                     "-N ", "eppasm_", run.name, ' ',
                     "-tc 1000 ",
                     "-t 1:", n.draws, " ",
                     "-hold_jid eppasm_prep_inputs_", run.name," ",
                     '/ihme/singularity-images/rstudio/shells/execR.sh -i ',
                     '/ihme/singularity-images/hiv/hiv_11.img ',
                     # code.dir, "gbd/singR_shell.sh ",
                     '-s ', 
                     code.dir, "gbd/main.R ",
                     run.name, " ", array.job)
print(epp.string)
system(epp.string)

#Draw compilation
draw.string <- paste0("qsub -l m_mem_free=30G -l fthread=1 -l h_rt=01:00:00 -q long.q -P ", cluster.project, " ",
                      "-e /share/temp/sgeoutput/", user, "/errors ",
                      "-o /share/temp/sgeoutput/", user, "/output ",
                      "-N ", "save_draws_", run.name, ' ',
                      "-hold_jid ",    "eppasm_", run.name, ' ',
                      "-tc 100 ",
                      "-t 1:", n.draws, " ",
                      code.dir, "gbd/singR_shell.sh ",
                      code.dir, "gbd/compile_draws.R ",
                      run.name, " ", array.job, ' TRUE ', paediatric)
print(draw.string)
system(draw.string)
}

# EPP-ASM ---------------------------------------
if(run_eppasm & !array.job){
for(loc in loc.list) {    
  ## Run EPPASM
    epp.string <- paste0("qsub -l m_mem_free=7G -l fthread=1 -l h_rt=24:00:00 -l archive=True -q all.q -P ", cluster.project, " ",
                         "-e /share/temp/sgeoutput/", user, "/errors ",
                         "-o /share/temp/sgeoutput/", user, "/output ",
                         "-N ", loc,"_",run.name, "_eppasm ",
                         "-tc 1000 ",
                         "-t 1:", n.draws, " ",
                         "-hold_jid eppasm_prep_inputs_", run.name," ",
                         '/ihme/singularity-images/rstudio/shells/execR.sh -i ',
                         '/ihme/singularity-images/hiv/hiv_11.img ',
                         # code.dir, "gbd/singR_shell.sh ",
                         '-s ', 
                         code.dir, "gbd/main.R ",
                         run.name, " ", array.job," ", loc, " ", proj.end, " ", paediatric)
  #  print(epp.string)
  # system(epp.string)

  
      #Draw compilation
      draw.string <- paste0("qsub -l m_mem_free=30G -l fthread=1 -l h_rt=01:00:00 -q all.q -P ", cluster.project, " ",
                            "-e /share/temp/sgeoutput/", user, "/errors ",
                            "-o /share/temp/sgeoutput/", user, "/output ",
                            "-N ", loc,"_",run.name, "_save_draws ",
                            "-hold_jid ", loc,"_",run.name, "_eppasm ",
                            '/ihme/singularity-images/rstudio/shells/execR.sh -i ',
                            '/ihme/singularity-images/hiv/hiv_11.img ',
                            # code.dir, "gbd/singR_shell.sh ",
                            '-s ', 
                            code.dir, "gbd/compile_draws.R ",
                            run.name, " ", array.job, ' ', loc, ' TRUE ', paediatric)
      # print(draw.string)
      # system(draw.string)
      
      summary.string <- paste0("qsub -l m_mem_free=10G -l fthread=1 -l h_rt=01:00:00 -q all.q -P ", cluster.project, " ",
                            "-e /share/temp/sgeoutput/", user, "/errors ",
                            "-o /share/temp/sgeoutput/", user, "/output ",
                            "-N ", loc,  '_summary_', run.name, " ",
                            "-hold_jid ", loc,"_",run.name, "_save_draws ",
                            '/ihme/singularity-images/rstudio/shells/execR.sh -i ',
                            '/ihme/singularity-images/hiv/hiv_11.img ',
                            # code.dir, "gbd/singR_shell.sh ",
                            '-s ',                             
                            code.dir, "gbd/get_summary_files.R ",
                            run.name, ' ', loc)
      print(summary.string)
      system(summary.string)

      plot.string <- paste0("qsub -l m_mem_free=20G -l fthread=1 -l h_rt=00:15:00 -l archive -q all.q -P ", cluster.project, " ",
                            "-e /share/temp/sgeoutput/", user, "/errors ",
                            "-o /share/temp/sgeoutput/", user, "/output ",
                            "-N ", loc, "_plot_eppasm ",
                            "-hold_jid ", loc,"_summary_",run.name, " ",
                            '/ihme/singularity-images/rstudio/shells/execR.sh -i ',
                            '/ihme/singularity-images/hiv/hiv_11.img ',
                            # code.dir, "gbd/singR_shell.sh ",
                            '-s ', 
                            code.dir, "gbd/main_plot_output.R ",
                            loc, " ", run.name, ' ', compare.run)
     
      print(plot.string)
      system(plot.string)

}
}



#Make sure all locations are done
check_loc_results(loc.list,paste0('/share/hiv/epp_output/', gbdyear, '/', run.name, '/compiled/'),prefix="",postfix=".csv")
have <- list.files(paste0('/share/hiv/epp_output/', gbdyear, '/', run.name, '/compiled/'))
have <- have[!grepl('under1', have)]
have <- unlist(lapply(have, gsub, pattern = '.csv', replacement = ''))
plotting_dirs <- c(paste0('/ihme/hiv/epp_output/', gbdyear, '/', run.name, '/15to49_plots/'),
                   paste0('/ihme/hiv/epp_output/', gbdyear, '/', run.name, '/age_specific_plots/Deaths/'),
                   paste0('/ihme/hiv/epp_output/', gbdyear, '/', run.name, '/age_specific_plots/Incidence/'),
                   paste0('/ihme/hiv/epp_output/', gbdyear, '/', run.name, '/age_specific_plots/Prevalence/'),
                   paste0('/ihme/hiv/epp_output/', gbdyear, '/', run.name, '/paeds_plots/'))
for(dir in plotting_dirs){
  setwd(dir)
  name <- tail(unlist(strsplit(dir, split = '/')), 1)
  if(file.exists(paste0(dir, name, '.pdf'))){
    unlink(paste0(dir, name, '.pdf'), recursive = T)
  }
    
  system(paste0("/usr/bin/ghostscript -dBATCH -dSAFER -DNOPAUSE -q -sDEVICE=pdfwrite -sOutputFile=", name, ".pdf -f $(ls | sort -n | xargs)"))
  dir.create(paste0('/ihme/hiv/epp_output/', gbdyear, '/', run.name, '/plots/'))
  if(file.exists(paste0('/ihme/hiv/epp_output/', gbdyear, '/', run.name, '/plots/', name, '.pdf'))){
    unlink(paste0('/ihme/hiv/epp_output/', gbdyear, '/', run.name, '/plots/', name, '.pdf'), recursive = T)
  }
  file.copy(paste0(dir, name, '.pdf'), paste0('/ihme/hiv/epp_output/', gbdyear, '/', run.name, '/plots/', name, '.pdf'))
}

# Compile plots ---------------------------------------
plot.holds <- paste(paste0(loc.list, '_plot_eppasm'), collapse = ",")
plot.string <- paste0("qsub -l m_mem_free=1G -l fthread=1 -l h_rt=00:35:00 -q all.q -P ", cluster.project, " ",
                      "-e /share/homes/", user, "/errors ",

                      "-o /share/temp/sgeoutput/", user, "/output ",
                      "-N ", "compile_plots_eppasm ",
                      "-hold_jid ", plot.holds, " ",
                      code.dir, "gbd/singR_shell.sh ",
                      code.dir, "gbd/compile_plots.R ",
                      run.name, " ", gbdyear)
print(plot.string)
system(plot.string)









  
