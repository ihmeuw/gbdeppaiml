## ---------------------------
## Script name: 01a_india_steps.R
## Purpose of script: Prepare EPP-ASM india output to go into spectrum 
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
rm(list = ls())
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
run.name = '200713_yuka'
spec.name = '200713_yuka'
code.dir <- paste0(ifelse(windows, "H:", paste0("/ihme/homes/", user)), "/gbdeppaiml/")
loc.table <- get_locations(hiv_metadata = T)

# Split India EPP-ASM results ---------------------------------------
### Split India states to Urban Rural and generate values for Territories
system(paste0("qsub -l m_mem_free=200G -l fthread=1 -l h_rt=08:00:00 -l archive -q all.q -P ", cluster.project, " ",
              "-e /share/temp/sgeoutput/", user, "/errors ",
              "-o /share/temp/sgeoutput/", user, "/output ",
              "-N ", "india_split ",
              code.dir, "gbd/singR_shell.sh ",
              code.dir, "gbd/split_ind_states.R ",
              run.name, ' iterative'))


#Make sure all locations that originally went through Spectrum are there
ind_locs <- loc.table[grepl("1",group) & spectrum==1 & grepl('IND', ihme_loc_id),ihme_loc_id]
check_loc_results(ind_locs,paste0('/share/hiv/epp_output/gbd20/', run.name, '/compiled/'),prefix="",postfix=".csv")

# Prepare files for Spectrum ---------------------------------------
#Move over India inputs for Spectrum
ind_locs <- setdiff(ind_locs, c('IND_43880', 'IND_43877', 'IND_43911', 'IND_43910', 'IND_43875', 'IND_43874',
                                'IND_43909', 'IND_43872', 'IND_43873', 'IND_43882', 'IND_43881', 'IND_43883', 'IND_43884'))
inputs <- list(inc="incidence",prev="prevalence")
dir.create(paste0('/ihme/hiv/spectrum_input/', spec.name, '/incidence/'))
dir.create(paste0('/ihme/hiv/spectrum_input/', spec.name, '/prevalence/'))

for(input.x in names(inputs)){
  for(loc_i in ind.locs){
    file.copy(from = paste0('/ihme/hiv/epp_output/', gbdyear, '/', run.name,'/compiled/IND_',input.x,"/",loc_i,".csv"),
              to = paste0('/share/hiv/spectrum_input/', spec.name, '/',inputs[input.x],"/",loc_i,".csv"), overwrite = T)
  }
}
