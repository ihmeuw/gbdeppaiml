### Setup
rm(list=ls())
windows <- Sys.info()[1][["sysname"]]=="Windows"
root <- ifelse(windows,"J:/","/home/j/")
user <- ifelse(windows, Sys.getenv("USERNAME"), Sys.getenv("USER"))
code.dir <- paste0(ifelse(windows, "H:", paste0("/homes/", user)), "/gbdeppaiml/")
date <- substr(gsub("-","",Sys.Date()),3,8)

## Packages
library(data.table)

## Arguments
run.name <- "191218_group2_tests"
compare.run <- NA
proj.end <- 2019
n.draws <- 1
run.group2 <- TRUE
paediatric <- FALSE
cluster.project <- "proj_hiv"

### Paths
input.dir <- paste0("/ihme/hiv/epp_input/gbd19/", run.name, "/")
dir.create(input.dir, recursive = TRUE, showWarnings = FALSE)
dir <- paste0("/ihme/hiv/epp_output/gbd19/", run.name, "/")
dir.create(dir, showWarnings = FALSE)

### Functions
library(mortdb, lib = "/home/j/WORK/02_mortality/shared/r")

### Tables
loc.table <- data.table(get_locations(hiv_metadata = T))

### Code
epp.list <- sort(loc.table[epp == 1, ihme_loc_id])

locs = gsub(".rds","",list.files("/share/hiv/data/PJNZ_EPPASM_prepped_2a/"))
loc.list = locs

# Cache inputs
if(!file.exists(paste0(input.dir, "population/"))) {
  prep.job <- paste0("qsub -l m_mem_free=10G -l fthread=1 -l h_rt=00:30:00 -q all.q -N eppasm_prep_inputs_", run.name," -P ",cluster.project," ",
                      "-e /share/temp/sgeoutput/", user, "/errors ",
                      "-o /share/temp/sgeoutput/", user, "/output ",
                      code.dir, "gbd/singR_shell.sh ",
                      code.dir, "gbd/gbd_prep_inputs.R"," ",run.name," ",proj.end, " ", run.group2)
  print(prep.job)
  system(prep.job)
}

# Cache prevalence surveys
if(!file.exists(paste0(input.dir, 'prev_surveys.csv'))){
  prev.job <- paste0("qsub -l m_mem_free=4G -l fthread=1 -l h_rt=00:10:00 -q all.q -N prev_cache_", run.name," -P ",cluster.project," ",
                     "-e /share/temp/sgeoutput/", user, "/errors ",
                     "-o /share/temp/sgeoutput/", user, "/output ",
                     code.dir, "gbd/singR_shell.sh ", 
                     code.dir, "gbd/cache_prev_surveys.R"," ",run.name)
  print(prev.job)
  system(prev.job)
}

# Prepare ART proportions
if(!file.exists(paste0(input.dir, 'art_prop.csv'))){
  prop.job <- paste0("qsub -l m_mem_free=2G -l fthread=1 -l h_rt=00:20:00 -q all.q -P ", cluster.project," -N eppasm_art_prop_", run.name," -hold_jid eppasm_prev_cache_", run.name, " ", 
                     "-e /share/temp/sgeoutput/", user, "/errors ",
                     "-o /share/temp/sgeoutput/", user, "/output ",
                     "-hold_jid eppasm_prep_inputs_", run.name,',eppasm_prev_cache_', run.name,' ',
                     code.dir, "gbd/singR_shell.sh ", 
                    code.dir, "gbd/prep_art_props.R ", run.name)
  print(prop.job)
  system(prop.job)
}

## Launch EPP
for(loc in loc.list) {
    ## Run EPPASM
    epp.string <- paste0("qsub -l m_mem_free=1G -l fthread=1 -l h_rt=04:00:00 -l archive -q all.q -P ", 
                         cluster.project, " ",
                         "-e /share/temp/sgeoutput/", user, "/errors ",
                         "-o /share/temp/sgeoutput/", user, "/output ",
                         "-N ", loc, "_eppasm ",
                         "-t 1:", n.draws, " ",
                         "-hold_jid eppasm_prep_inputs_", run.name," ",
                         code.dir, "gbd/singR_shell.sh ",
                         code.dir, "gbd/main.R ",
                         run.name, " ", loc, " ", proj.end, " ", paediatric)
    print(epp.string)
    system(epp.string)

    ## Draw compilation
    draw.string <- paste0("qsub -l m_mem_free=2G -l fthread=1 -l h_rt=00:10:00 -q all.q -P ", cluster.project, " ",
                          "-e /share/temp/sgeoutput/", user, "/errors ",
                          "-o /share/temp/sgeoutput/", user, "/output ",
                          "-N ", loc, "211`_save_draws ",
                          "-hold_jid ", loc, "_eppasm ",
                          code.dir, "gbd/singR_shell.sh ",
                          code.dir, "gbd/compile_draws.R ",
                          run.name, " ", loc, ' ', n.draws, ' TRUE ', paediatric)
    print(draw.string)
    system(draw.string)

    ## Create aggregate and age-specific plots
    plot.string <- paste0("qsub -l m_mem_free=1G -l fthread=1 -l h_rt=00:15:00 -q all.q -P ", cluster.project, " ",
                          "-e /share/temp/sgeoutput/", user, "/errors ",
                          "-o /share/temp/sgeoutput/", user, "/output ",
                          "-N ", loc, "_plot_eppasm ",
                          "-hold_jid ", loc, "_save_draws ",
                          code.dir, "gbd/singR_shell.sh ", 
                          code.dir, "gbd/main_plot_output.R ",
                          loc, " ", run.name, ' ', paediatric, ' ', compare.run)
    print(plot.string)
    system(plot.string)
}

## Compile plots
  plot.holds <- paste(paste0(loc.list, '_plot_eppasm'), collapse = ",")
  plot.string <- paste0("qsub -l m_mem_free=1G -l fthread=1 -l h_rt=00:15:00 -q all.q -P ", cluster.project, " ",
                        "-e /share/temp/sgeoutput/", user, "/errors ",
                        "-o /share/temp/sgeoutput/", user, "/output ",
                        "-N ", "compile_plots_eppasm ",
                        # "-hold_jid ", plot.holds, " ",
                        code.dir, "gbd/singR_shell.sh ", 
                        code.dir, "gbd/compile_plots.R ",
                        run.name)
  print(plot.string)
  system(plot.string)

# ## Check for which draws are missing
# dir <- paste0("/ihme/hiv/epp_output/gbd17/", run.name, "/")
# missing.dt <- rbindlist(lapply(loc.list, function(loc) {
#     files <- list.files(paste0(dir, loc), "incid")
#     numbers <- as.integer(gsub("results_incid", "", gsub(".csv", "", files)))
#     missing <- setdiff(1:n.draws, numbers)
#     if(length(missing) > 0) {
#         dt <- data.table(ihme_loc_id = loc, missing = missing)
#     } else {
#         dt <- data.table()
#     }
# }))
# sum.dt <- missing.dt[, .(num = .N), by = "ihme_loc_id"]
# as.data.frame(sum.dt)
# # loc.list <- sum.dt[num == n.draws, ihme_loc_id]
# 
# ## Fill in missing India and Kenya locations
# system(paste0("qsub -P ", cluster.project," -pe multi_slot 1 -N missing_subnats ", code.dir, "shell_R.sh ", code.dir, "epp-feature-reset/gbd/create_missing_subnats.R ", run.name))
# 
# ## Split India states to Urban Rural
# system(paste0("qsub -P ", cluster.project," -pe multi_slot 1 -N ind_split -hold_jid missing_subnats ", code.dir, "shell_R.sh ", code.dir, "epp-feature-reset/gbd/split_ind_states.R ", run.name))
# 
# ## Plot and combine results
# if(plot) {
#     ind.locs <- c("IND_44538", loc.table[grepl("IND", ihme_loc_id) & level == 5, ihme_loc_id])
#     plot.list <- c(epp.list, ind.locs)
#     for(loc in plot.list) {
#         # Plot individual locations
#         plot.string <- paste0("qsub -P ", cluster.project, " -pe multi_slot 1 ", 
#                               "-e /share/temp/sgeoutput/", user, "/errors ",
#                               "-o /share/temp/sgeoutput/", user, "/output ",
#                               "-N ", loc, "_plot_epp ",
#                               "-hold_jid ", loc, "_save_draws ",
#                               code.dir, "shell_R.sh ",
#                               code.dir, "epp-feature-reset/gbd/plot_epp.R ",
#                               loc, " ", run.name)
#         print(plot.string)
#         system(plot.string)
#     }
# 
#     # Compile all locations
#     combine.holds <- paste(paste0(plot.list, "_plot_epp"), collapse = ",")
#     combine.string <- paste0("qsub -P ", cluster.project, " -pe multi_slot 1 ", 
#                              "-e /share/temp/sgeoutput/", user, "/errors ",
#                              "-o /share/temp/sgeoutput/", user, "/output ",
#                              "-N combine_epp_plots ",
#                              "-hold_jid ", combine.holds, " ",
#                              code.dir, "shell_R.sh ",
#                              code.dir, "epp-feature-reset/gbd/combine_epp_plots.R ",
#                              run.name)
#     print(combine.string)
#     system(combine.string)
# }
# 
# ## Compile comparison data for Spectrum plots
# system(paste0("qsub -P ", cluster.project," -N compile_epp -hold_jid combine_epp_plots ", code.dir, "shell_R.sh ", code.dir, "epp-feature-reset/gbd/compile_epp_data.R ", run.name))

### End
