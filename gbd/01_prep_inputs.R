## Script description ---------------------------
## Script name: 00_prep_inputs.R
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
source(paste0('/ihme/homes/', user, '/rt-shared-functions/cluster_functions.R'))



# Arguments ---------------------------------------
gbdyear = 'gbdTEST'
run.name = '220329_maggie'
old_run.name = '200713_yuka'
spec.name = '200713_yuka'
code.dir <- paste0(ifelse(windows, "H:", paste0("/ihme/homes/", user)), "/gbdeppaiml/")
loc.table <- get_locations(hiv_metadata = T)
loc.list <-  c(loc.table[epp == 1, ihme_loc_id], 'MRT', 'STP', 'COM')
loc.list = 'AGO'

# Toggles ---------------------------------------
##make copy inputs and new inputs opposite
new_inputs = T
copy_inputs = F
plot_ART = F
eppasm_inputs = T
prev_surveys = F
art_proportions = F
redo_offsets = F

# Args for sbatches ---------------------------------------
run.table <- fread(paste0('/share/hiv/epp_input/', gbdyear, '/eppasm_run_table.csv'))
run.table <- run.table[run_name == run.name,]
proj.end = run.table[,proj.end]
decomp.step = run.table[,decomp_step]
run.group2 = F

# Copy Inputs ---------------------------------------
if(copy_inputs){
  from = old_run.name
  to = run.name
  dir.create(paste0('/ihme/hiv/epp_input/', gbdyear, '/', to, '/'), recursive = T, showWarnings = T)
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
      submit_job(script = paste0(paste0("/ihme/homes/", user), "/hiv_gbd/01_prep/plot_ART.R"), queue = 'all.q', memory = '1G', threads = 1, time = '00:30:00',
                 name = paste0(loc, '_plot_art'),
                 archive = F, args = c(2019, loc, 2017, run.name))
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
    submit_job(script = paste0(code.dir, "gbd/gbd_prep_inputs.R"), queue = 'all.q', memory = '10G', threads = 1, time = '00:30:00',
               name = 'prep_gbd_inputs',
               archive = F, args = c(run.name, proj.end, run.group2, decomp.step, gbdyear))
    ##check that the necessary files exists
    dirs = c('SRB', 'births', 'ASFR', 'migration', 'population_splits', 'population', 'population_single_age')
    dirs = paste0('/ihme/hiv/epp_input/', gbdyear, '/', run.name, '/', dirs)
    lapply(dirs, dir.exists)

    submit_job(script = paste0(code.dir, "gbd/gbd_prep_ind_inputs.R"), queue = 'all.q', memory = '10G', threads = 1, time = '00:30:00',
               name = 'prep_gbd_inputs_ind',
               archive = F, args = c(run.name, proj.end, run.group2))
    
  }
  
  # Prepare prevalence surveys ---------------------------------------
  if(prev_surveys){
    submit_job(script = paste0(code.dir, "gbd/cache_prev_surveys_age_sex.R"), queue = 'all.q', memory = '4G', threads = 1, time = '00:10:00',
               name = 'prep_prev_surveys',
               archive = F, args = c(gbdyear))
    file.exists(paste0('/ihme/hiv/epp_input/', gbdyear, '/prev_surveys_ind.csv'))
  }
  
  
  # Prepare ART proportions for India and Kenya ---------------------------------------
  if(art_proportions){
    submit_job(script = paste0(code.dir, "gbd/prep_art_props.R"), queue = 'all.q', memory = '2G', threads = 1, time = '00:20:00',
               name = 'prep_art_props',
               archive = F, args = c(run.name, gbdyear))
    file.exists(paste0('/ihme/hiv/epp_input/', gbdyear, '/', run.name, '/art_prop.csv'))

  }
  
  # Prepare offsets for ART data ---------------------------------------
  if(redo_offsets){
    input_table <- fread(paste0(ifelse(windows, "H:", paste0("/ihme/homes/", user)), '/gbdeppaiml/gbd/inputs.csv'))
    
    ##This only needs to be rerun if any of the file paths in the input file have changed. Note that between runs the anc_no_offset WILL change
    rerun_conditions <- c(input_table[run.name == run.name.old, lbd_anc_data] != input_table[run.name == run.name.old, lbd_anc_data],
                          input_table[run.name == run.name.old, geo_codebook] != input_table[run.name == run.name.old, geo_codebook],
                          input_table[run.name == run.name.old, sf_dir] != input_table[run.name == run.name.old, sf_dir],
                          input_table[run.name == run.name.old, rd] != input_table[run.name == run.name.old, rd],
                          input_table[run.name == run.name.old, lbd_anc_mean_est] != input_table[run.name == run.name.old, lbd_anc_mean_est])
    
    
    # Run scripts ---------------------------------------
    if(any(rerun_conditions == TRUE)){
      submit_job(script = paste0(code.dir, "gbd/lat_long.R"), queue = 'all.q', memory = '30G', threads = 1, time = '01:00:00',
                 name = 'lat_long',
                 archive = F)
      
      
      submit_job(script = paste0(code.dir, "gbd/lbd_anc_recreate.R"), queue = 'all.q', memory = '30G', threads = 1, time = '01:00:00',
                 name = 'lbd_processing',
                 archive = F)
      
      
      submit_job(script = paste0(code.dir, "offsets/create_offsets.R"), queue = 'all.q', memory = '30G', threads = 1, time = '01:00:00',
                 name = 'create_offsets',
                 archive = F)
    }
    
  }
  
}


