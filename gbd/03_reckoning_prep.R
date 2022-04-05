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

# Arguments ---------------------------------------
gbdyear = 'gbd20'
run.name = '200713_yuka'
spec.name = '200713_yuka'
code.dir <- paste0(ifelse(windows, "H:", paste0("/ihme/homes/", user)), "/gbdeppaiml/")
loc.table <- get_locations(hiv_metadata = T)
cluster.project = 'proj_hiv'

# Aggregate and apply age splits ---------------------------------------
## Aggregate to higher levels for EPP-ASM child locs - not India because it goes through Spectrum
## Prepare for post-reckoning steps
loc.list <- loc.table[epp == 1, ihme_loc_id]
saveRDS(c(loc.list, 'MRT', 'COM', 'STP'), '/ihme/homes/mwalte10/maggie_redo_prep.RDS')
eppasm_parents <-  c("KEN","ZAF","ETH","KEN_44793" ,"KEN_44794","KEN_44795", "KEN_44796" ,"KEN_44797", "KEN_44798","KEN_44799", "KEN_44800","NGA")
all_loc_list <- c(loc.list,eppasm_parents, 'MRT', 'COM', 'STP')
# redo <- readRDS('/ihme/homes/mwalte10/redo.RDS')
# all_loc_list <- intersect(all_loc_list, redo)
## Aggregation and reckoning prep for higher levels
  for(loc in rev(all_loc_list)){
    if(loc %in% eppasm_parents){
      prep.string <- paste0("qsub -l m_mem_free=100G -l fthread=2 -l h_rt=02:00:00 -l archive -q all.q -P ", cluster.project, " ",
                            "-e /share/temp/sgeoutput/", user, "/errors ",
                            "-o /share/temp/sgeoutput/", user, "/output ",
                            "-N ", loc, "_aggregate ",
                            "-hold_jid ", loc,"_save_draws ",
                            '/ihme/singularity-images/rstudio/shells/execR.sh -i ',
                            '/ihme/singularity-images/hiv/hiv_11.img ',
                            # code.dir, "gbd/singR_shell.sh ",
                            '-s ',
                            code.dir, "gbd/aggregate.R ",
                            loc, " ", run.name, " ", spec.name," ",2)
      # print(prep.string)
      # system(prep.string)
    }
    
    
    prep.string <- paste0("qsub -l m_mem_free=50G -l fthread=1 -l h_rt=02:00:00 -l archive -q all.q -P ", cluster.project, " ",
                          "-e /share/temp/sgeoutput/", user, "/errors ",
                          "-o /share/temp/sgeoutput/", user, "/output ",
                          "-N ", loc, "_apply_age_splits ",
                          "-hold_jid ", loc,"_aggregate ",
                          '/ihme/singularity-images/rstudio/shells/execR.sh -i ',
                          '/ihme/singularity-images/hiv/hiv_11.img ',
                          # code.dir, "gbd/singR_shell.sh ",
                          '-s ',
                          code.dir, "gbd/apply_age_splits.R ",
                          loc, " ", run.name, " ", spec.name)
    # print(prep.string)
    # system(prep.string)
    
    early <- fread(paste0('/ihme/hiv/spectrum_prepped/art_draws/200713_yuka/temp/', loc, '_ART_data.csv'))
    if(max(early$year_id) == 2022){
      if(nrow(early[new_hiv > 1,]) > 0){
        stop()
      }
      write.csv(early, paste0('/ihme/hiv/spectrum_prepped/art_draws/200713_yuka/', loc, '_ART_data.csv'), row.names = F)
    }else{
      late <- fread(paste0('/ihme/hiv/spectrum_prepped/art_draws/200713_yuka/', loc, '_ART_data.csv'))
      dt <- rbind(early[year_id %in% c(1970:2019)], late[year_id %in% c(2020:2022)])
      if(nrow(dt[new_hiv > 1,]) > 0){
        stop()
      }
      write.csv(dt, paste0('/ihme/hiv/spectrum_prepped/art_draws/200713_yuka/', loc, '_ART_data.csv'), row.names = F)
    }
    print(which(loc == all_loc_list) / length(all_loc_list))
    
    
  }


check_loc_results(all_loc_list,paste0("/ihme/hiv/spectrum_prepped/art_draws/",spec.name,"/"),prefix="",postfix="_ART_data.csv")
