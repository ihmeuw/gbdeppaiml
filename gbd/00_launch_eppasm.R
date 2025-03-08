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
run.name <- "201012_ancrt"
spec.name <- "200713_yuka"
compare.run <- c("200713_yuka")

proj.end <- 2022
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
gbdyear <- "gbd20"
redo_offsets <- F
testing = FALSE
test = NULL
run_eppasm = T
gbdyear = 'gbd20'

### Paths
input.dir <- paste0("/ihme/hiv/epp_input/", gbdyear, '/', run.name, "/")
dir.create(input.dir, recursive = TRUE, showWarnings = FALSE)
dir <- paste0("/ihme/hiv/epp_output/", gbdyear, '/', run.name, "/")
dir.create(dir, showWarnings = FALSE)
dir.table <- fread(paste0('/share/hiv/epp_input/gbd20//dir_table_log_gbd20.csv'))

run.table <- fread(paste0('/share/hiv/epp_input/gbd20//eppasm_run_table.csv'))

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
library(mortdb, lib = "/ihme/mortality/shared/r")

### Tables
loc.table <- data.table(get_locations(hiv_metadata = T))

### Code
epp.list <- sort(loc.table[epp == 1 & grepl('1', group), ihme_loc_id])
loc.list <- epp.list


#Make comparison ART plots
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

plot.dir <- paste0("/ihme/hiv/epp_input/gbd19/",run.name,"/art_plots/")
setwd(plot.dir)
system(paste0("/usr/bin/ghostscript -dBATCH -dSAFER -DNOPAUSE -q -sDEVICE=pdfwrite -sOutputFile=art_plots.pdf -f *"))
# Move to parent directory
system(paste0("mv ", plot.dir, "/art_plots.pdf ",input.dir,"/"))
# Delete location specific plots
system(paste0("rm -r -f ", plot.dir, "/"))

}

# Cache inputs
if(!file.exists(paste0(input.dir, "population/"))) {
  prep.job <- paste0("qsub -l m_mem_free=10G -l fthread=1 -l h_rt=00:30:00 -q all.q -N eppasm_prep_inputs_", run.name," -P ",cluster.project," ",
                      "-e /share/temp/sgeoutput/", user, "/errors ",
                      "-o /share/temp/sgeoutput/", user, "/output ",
                      code.dir, "gbd/singR_shell.sh ",
                      code.dir, "gbd/gbd_prep_inputs.R"," ",run.name," ",proj.end, " ", run.group2, " ", decomp.step)
  print(prep.job)
  system(prep.job)
}

# Cache prevalence surveys  - THIS PART OF THE PIPELINE NEEDS IMPROVING
# if(!file.exists(paste0(input.dir, 'prev_surveys.csv'))){
#   prev.job <- paste0("qsub -l m_mem_free=4G -l fthread=1 -l h_rt=00:10:00 -q all.q -N prev_cache_", run.name," -P ",cluster.project," ",
#                      "-e /share/temp/sgeoutput/", user, "/errors ",
#                      "-o /share/temp/sgeoutput/", user, "/output ",
#                      code.dir, "gbd/singR_shell.sh ", 
#                      code.dir, "gbd/cache_prev_surveys_age_sex.R"," ",run.name)
#   print(prev.job)
#   system(prev.job)
# }


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


if(redo_offsets){
  redo_offsets.string <- paste0("qsub -l m_mem_free=7G -l fthread=1 -l h_rt=24:00:00 -l archive=True -q all.q -P ", cluster.project, " ",
                       "-e /share/homes/", user, "/errors ",
                       "-o /share/temp/sgeoutput/", user, "/output ",
                       "-N ",  "redo_offsets ",
                       code.dir, "gbd/singR_shell.sh ",
                       code.dir, "lbd_anc_align/launch_lbd_process.R ",
                       ##191224_trumpet is a placeholder for the old run
                       run.name, '191224_trumpet')
  print(redo_offsets.string)
  system(redo_offsets.string)
}


if(array.job){
  epp.string <- paste0("qsub -l m_mem_free=7G -l fthread=1 -l h_rt=24:00:00 -l archive=True -q all.q -P ", cluster.project, " ",
                     "-e /share/temp/sgeoutput/", user, "/errors ",
                     "-o /share/temp/sgeoutput/", user, "/output ",
                     "-N ", "eppasm_", run.name, ' ',
                     "-tc 100 ",
                     "-t 1:", n.draws, " ",
                     "-hold_jid eppasm_prep_inputs_", run.name," ",
                     code.dir, "gbd/singR_shell.sh ",
                     code.dir, "gbd/main.R ",
                     run.name, " ", array.job)
print(epp.string)
system(epp.string)
}


if(run_eppasm & !array.job){
for(loc in loc.list) {    ## Run EPPASM



    epp.string <- paste0("qsub -l m_mem_free=7G -l fthread=1 -l h_rt=24:00:00 -l archive=True -q all.q -P ", cluster.project, " ",
                         "-e /share/temp/sgeoutput/", user, "/errors ",
                         "-o /share/temp/sgeoutput/", user, "/output ",
                         "-N ", loc,"_",run.name, "_eppasm ",
                         "-tc 100 ",
                         "-t 1:", n.draws, " ",
                         "-hold_jid eppasm_prep_inputs_", run.name," ",
                         code.dir, "gbd/singR_shell.sh ",
                         code.dir, "gbd/main.R ",
                         run.name, " ", array.job," ", loc, " ", proj.end, " ", paediatric)
                         
  
                      
   #  print(epp.string)
  # system(epp.string)

     
      #
      #
      #Draw compilation
      draw.string <- paste0("qsub -l m_mem_free=30G -l fthread=1 -l h_rt=01:00:00 -q all.q -P ", cluster.project, " ",
                            "-e /share/temp/sgeoutput/", user, "/errors ",
                            "-o /share/temp/sgeoutput/", user, "/output ",
                            "-N ", loc,"_",run.name, "_save_draws ",
                            "-hold_jid ", loc,"_",run.name, "_eppasm ",
                            code.dir, "gbd/singR_shell.sh ",
                            code.dir, "gbd/compile_draws.R ",
                            run.name, " ", loc, ' ', n.draws, ' TRUE ', paediatric)
      # print(draw.string)
      # system(draw.string)

      plot.string <- paste0("qsub -l m_mem_free=20G -l fthread=1 -l h_rt=00:15:00 -l archive -q all.q -P ", cluster.project, " ",
                            "-e /share/temp/sgeoutput/", user, "/errors ",
                            "-o /share/temp/sgeoutput/", user, "/output ",
                            "-N ", loc, "_plot_eppasm ",
                            "-hold_jid ", loc,"_",run.name, "_save_draws ",
                            code.dir, "gbd/singR_shell.sh ",
                            code.dir, "gbd/main_plot_output.R ",
                            loc, " ", run.name, ' ', paediatric, ' ', compare.run, ' ', test)
     
      # print(plot.string)
      system(plot.string)
     # }
    #

}
}


#Make sure all locations are done
check_loc_results(loc.list,paste0('/share/hiv/epp_output/', gbdyear, '/', run.name, '/compiled/'),prefix="",postfix=".csv")
# 

### Split India states to Urban Rural and generate values for Territories
system(paste0("qsub -l m_mem_free=200G -l fthread=1 -l h_rt=08:00:00 -l archive -q all.q -P ", cluster.project, " ",
               "-e /share/temp/sgeoutput/", user, "/errors ",
               "-o /share/temp/sgeoutput/", user, "/output ",
               "-N ", "india_split ",
               code.dir, "gbd/singR_shell.sh ",
               code.dir, "gbd/split_ind_states.R ",
               run.name, ' iterative'))


#Make sure all locations that originally went through Spectrum are there
x <- loc.table[grepl("1",group) & spectrum==1,ihme_loc_id]

check_loc_results(x[grepl('IND',x)],paste0('/share/hiv/epp_output/gbd20/', run.name, '/compiled/'),prefix="",postfix=".csv")
 
## Compile plots
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

## Aggregate to higher levels for EPP-ASM child locs - not India because it goes through Spectrum
## Prepare for post-reckoning steps
eppasm_parents <-  c("KEN","ZAF","ETH","KEN_44793" ,"KEN_44794","KEN_44795", "KEN_44796" ,"KEN_44797", "KEN_44798","KEN_44799", "KEN_44800","NGA")
all_loc_list <- c(loc.list,eppasm_parents, 'MRT', 'COM', 'STP')
## Aggregation and reckoning prep for higher levels
if(reckon_prep){
  for(loc in all_loc_list){
    if(loc %in% eppasm_parents[grepl("KEN", eppasm_parents)]){
    prep.string <- paste0("qsub -l m_mem_free=100G -l fthread=2 -l h_rt=02:00:00 -l archive -q all.q -P ", cluster.project, " ",
                          "-e /share/temp/sgeoutput/", user, "/errors ",
                          "-o /share/temp/sgeoutput/", user, "/output ",
                          "-N ", loc, "_aggregate ",
                          "-hold_jid ", loc,"_save_draws ",
                          code.dir, "gbd/singR_shell.sh ",
                          code.dir, "gbd/aggregate.R ",
                          loc, " ", run.name, " ", spec.name," ",2)
    print(prep.string)
    system(prep.string)
  }


  prep.string <- paste0("qsub -l m_mem_free=50G -l fthread=1 -l h_rt=02:00:00 -l archive -q all.q -P ", cluster.project, " ",
                        "-e /share/temp/sgeoutput/", user, "/errors ",
                        "-o /share/temp/sgeoutput/", user, "/output ",
                        "-N ", loc, "_apply_age_splits ",
                        "-hold_jid ", loc,"_aggregate ",
                        code.dir, "gbd/singR_shell.sh ",
                        code.dir, "gbd/apply_age_splits.R ",
                        loc, " ", run.name, " ", spec.name)
  print(prep.string)
  system(prep.string)

    }
}


check_loc_results(c(loc.list,eppasm_parents),paste0("/ihme/hiv/spectrum_prepped/art_draws/",spec.name,"/"),prefix="",postfix="_ART_data.csv")

#Move over India inputs for Spectrum
ind.locs <- loc.table[grepl("IND",ihme_loc_id) & spectrum==1,ihme_loc_id]
ind.locs <- setdiff(ind.locs, c('IND_43880', 'IND_43877', 'IND_43911', 'IND_43910', 'IND_43875', 'IND_43874',
                               'IND_43909', 'IND_43872', 'IND_43873', 'IND_43882', 'IND_43881', 'IND_43883', 'IND_43884'))
inputs <- list(inc="incidence",prev="prevalence")
dir.create(paste0('/ihme/hiv/spectrum_input/', spec.name, '/incidence/'))
dir.create(paste0('/ihme/hiv/spectrum_input/', spec.name, '/prevalence/'))

for(input.x in names(inputs)){
  for(loc_i in ind.locs){

    file.copy(from = paste0('/ihme/hiv/epp_output/gbd20/',run.name,'/compiled/IND_',input.x,"/",loc_i,".csv"),
              to = paste0('/share/hiv/spectrum_input/', spec.name, '/',inputs[input.x],"/",loc_i,".csv"), overwrite = T)
  }
}









  
