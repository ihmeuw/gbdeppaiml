## Script description ---------------------------
## Script name: 00_prep_inputs_proper.R
## Purpose of script: Prepare inputs for EPP-ASM locations pulling the most recent populations and such
##
## Author: Maggie Walters
## Date Created: 2018-04-11
## Email: mwalte10@uw.edu
##
## Notes:
##   
##

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


# Arguments ---------------------------------------
gbdyear = 'gbd20'
run.name = '201015_socialdets_sens'
old_run.name = '201007_socialdets_sens'
spec.name = '200713_yuka'
code.dir <- paste0(ifelse(windows, "H:", paste0("/ihme/homes/", user)), "/gbdeppaiml/")
loc.table <- get_locations(hiv_metadata = T)
loc.list <- loc.table[epp == 1, ihme_loc_id]

# Toggles ---------------------------------------
new_inputs = F
copy_inputs = T
plot_ART = F
eppasm_inputs = F
prev_surveys = F
art_proportions = F
redo_offsets = F

# Copy Inputs ---------------------------------------
if(copy_inputs){
  from = old_run.name
  to = run.name
  dir.create(paste0('/ihme/hiv/epp_input/', gbdyear, '/', to, '/'), recursive = T, showWarnings = F)
  dirs <- list.dirs(paste0('/ihme/hiv/epp_input/gbd20/', from, '/'))
  file_paths <- lapply(dirs, list.files, full.names = T)
  file_paths.list <- list()
  for(loc in loc.list){
    file_paths.x <- unlist(file_paths)[grep(loc, unlist(file_paths))]
    file_paths.list <- c(file_paths.list, file_paths.x)
  }
  new_dirs <- unlist(lapply(dirs, gsub, pattern = from, replacement = to))
  copy_dt <- data.table(copy_from = dirs, copy_to = new_dirs)
  mk_copy <- function(copy_dt, loc.list = c('AGO', 'BDI', 'BEN', 'CAF')){
    dir.create(copy_dt[,copy_to])
    for(loc in loc.list){
      file.copy(from = paste0(copy_dt[,copy_from],'/', loc, '.csv'),
                to = paste0(copy_dt[,copy_to], '/',loc, '.csv'), overwrite = T)
    }
    
  }
  for(row in 1:nrow(copy_dt)){
    mk_copy(copy_dt = copy_dt[row,], loc.list = loc.list)
  }
  file.copy(from = paste0('/ihme/hiv/epp_input/', gbdyear, '/', from, '/location_table.csv'), 
            to  = paste0('/ihme/hiv/epp_input/', gbdyear, '/', to, '/location_table.csv'), overwrite = T)
  file.copy(from = paste0('/ihme/hiv/epp_input/', gbdyear, '/', from, '/age_map.csv'), 
            to  = paste0('/ihme/hiv/epp_input/', gbdyear, '/', to, '/age_map.csv'), overwrite = T)
  file.copy(from = paste0('/ihme/hiv/epp_input/', gbdyear, '/', from, '/param_map.csv'), 
            to  = paste0('/ihme/hiv/epp_input/', gbdyear, '/', to, '/param_map.csv'), overwrite = T)
  
}


# Proper Inputs ---------------------------------------
if(new_inputs){
  # Compare ART inputs ---------------------------------------
  if(plot_ART){
    for(loc in loc.list) { 
      art.string <- paste0("qsub -l m_mem_free=1G -l fthread=1 -l h_rt=00:30:00 -l archive -q all.q -P ", cluster.project, " ",
                           "-e /share/temp/sgeoutput/", user, "/errors ",
                           "-o /share/temp/sgeoutput/", user, "/output ",
                           "-N ", loc, "_plot_art ",
                           code.dir, "gbd/singR_shell.sh ",
                           paste0(paste0("/ihme/homes/", user), "/hiv_gbd2019/01_prep/plot_ART.R "),
                           "2019 ", loc, " ", "2017", " ",run.name)
      print(art.string )
      system(art.string )
    }
    
    plot.dir <- paste0("/ihme/hiv/epp_input/", gbdyear, '/', run.name,"/art_plots/")
    setwd(plot.dir)
    system(paste0("/usr/bin/ghostscript -dBATCH -dSAFER -DNOPAUSE -q -sDEVICE=pdfwrite -sOutputFile=art_plots.pdf -f *"))
    # Move to parent directory
    system(paste0("mv ", plot.dir, "/art_plots.pdf ",input.dir,"/"))
    # Delete location specific plots
    system(paste0("rm -r -f ", plot.dir, "/"))
    
  }
  
  # Pull new inputs and prep for EPP-ASM ---------------------------------------
  if(eppasm_inputs) {
    prep.job <- paste0("qsub -l m_mem_free=10G -l fthread=1 -l h_rt=00:30:00 -q all.q -N eppasm_prep_inputs_", run.name," -P ",cluster.project," ",
                       "-e /share/temp/sgeoutput/", user, "/errors ",
                       "-o /share/temp/sgeoutput/", user, "/output ",
                       code.dir, "gbd/singR_shell.sh ",
                       code.dir, "gbd/gbd_prep_inputs.R"," ",run.name," ",proj.end, " ", run.group2, " ", decomp.step)
    print(prep.job)
    system(prep.job)
    
    prep.job <- paste0("qsub -l m_mem_free=10G -l fthread=1 -l h_rt=00:30:00 -q all.q -N eppasm_prep_inputs_", run.name," -P ",cluster.project," ",
                       "-e /share/temp/sgeoutput/", user, "/errors ",
                       "-o /share/temp/sgeoutput/", user, "/output ",
                       code.dir, "gbd/singR_shell.sh ",
                       code.dir, "gbd/gbd_prep_ind_inputs.R"," ",run.name," ",proj.end, " ", run.group2)
    print(prep.job)
    system(prep.job)
  }
  
  # Prepare prevalence surveys ---------------------------------------
  if(prev_surveys){
    prev.job <- paste0("qsub -l m_mem_free=4G -l fthread=1 -l h_rt=00:10:00 -q all.q -N prev_cache_", run.name," -P ",cluster.project," ",
                       "-e /share/temp/sgeoutput/", user, "/errors ",
                       "-o /share/temp/sgeoutput/", user, "/output ",
                       code.dir, "gbd/singR_shell.sh ",
                       code.dir, "gbd/cache_prev_surveys_age_sex.R"," ",run.name)
    print(prev.job)
    system(prev.job)
  }
  
  
  # Prepare ART proportions for India and Kenya ---------------------------------------
  if(art_proportions){
    prop.job <- paste0("qsub -l m_mem_free=2G -l fthread=1 -l h_rt=00:20:00 -q all.q -P ", cluster.project," -N eppasm_art_prop_", run.name," -hold_jid eppasm_prev_cache_", run.name, " ", 
                       "-e /share/temp/sgeoutput/", user, "/errors ",
                       "-o /share/temp/sgeoutput/", user, "/output ",
                       "-hold_jid eppasm_prep_inputs_", run.name,',eppasm_prev_cache_', run.name,' ',
                       code.dir, "gbd/singR_shell.sh ", 
                       code.dir, "gbd/prep_art_props.R ", run.name)
    print(prop.job)
    system(prop.job)
  }
  
  # Prepare offsets for ART data ---------------------------------------
  if(redo_offsets){
    redo_offsets.string <- paste0("qsub -l m_mem_free=7G -l fthread=1 -l h_rt=24:00:00 -l archive=True -q all.q -P ", cluster.project, " ",
                                  "-e /share/homes/", user, "/errors ",
                                  "-o /share/temp/sgeoutput/", user, "/output ",
                                  "-N ",  "redo_offsets ",
                                  code.dir, "gbd/singR_shell.sh ",
                                  code.dir, "lbd_anc_align/00_launch_lbd_process.R ",
                                  ##191224_trumpet is a placeholder for the old run
                                  run.name, '191224_trumpet')
    print(redo_offsets.string)
    system(redo_offsets.string)
  }
  
}


