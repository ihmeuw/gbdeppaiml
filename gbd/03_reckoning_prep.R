## ---------------------------
## Script name: 03_reckoning_prep.R
## Purpose of script: Aggregate up to full locations and apply age splits
##
## Author: Maggie Walters
## Date Created: 2018-04-11
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
source(paste0('/ihme/homes/', user, '/rt-shared-functions/cluster_functions.R'))

# Arguments ---------------------------------------
gbdyear = 'gbd23'
run.name = "240529_meixin_test2art"
spec.name = "240529_meixin_test2art"
code.dir <- paste0(ifelse(windows, "H:", paste0("/ihme/homes/", user)), "/gbdeppaiml/")
loc.table <- get_locations(hiv_metadata = T)
cluster.project = 'proj_hiv'

# Aggregate and apply age splits ---------------------------------------
## Aggregate to higher levels for EPP-ASM child locs - not India because it goes through Spectrum
## Prepare for post-reckoning steps
loc.list <- loc.table[epp == 1, ihme_loc_id]
eppasm_parents <-  c("KEN","ZAF","ETH","KEN_44793" ,"KEN_44794","KEN_44795", "KEN_44796" ,"KEN_44797", "KEN_44798","KEN_44799", "KEN_44800","NGA")
all_loc_list <- c(loc.list,eppasm_parents)
## Aggregation and reckoning prep for higher levels

for(loc in loc.list){
    submit_job(script = paste0(code.dir, 'gbd/apply_age_splits.R'),
               queue = 'all.q', memory = '50G', threads = 1, time = "02:00:00", name = paste0(loc, '_age_splits'),
               archive = T, args = c(loc, run.name,  spec.name, gbdyear))
    
}
  
for(loc in eppasm_parents){
    submit_job(script = paste0(code.dir, 'gbd/aggregate_after_age_split.R'),
               queue = 'all.q', memory = '100G', threads = 1, time = "02:15:00", name = paste0(loc, '_aggregate'),
               archive = T, args = c(loc, spec.name, 10))
}



check_loc_results(all_loc_list,paste0("/ihme/hiv/spectrum_prepped/art_draws/",spec.name,"/"),prefix="",postfix="_ART_data.csv")
