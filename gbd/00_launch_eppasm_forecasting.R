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
run.name <- "220926_albatross"
spec.name <- "200713_yuka"
compare.run <- c("200713_yuka")

proj.end <- 2050
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
testing = FALSE
test = NULL
run_eppasm = T

### Paths
input.dir <- paste0("/ihme/hiv/epp_input/", gbdyear, '/', run.name, "/")
dir.create(input.dir, recursive = TRUE, showWarnings = FALSE)
dir <- paste0("/ihme/hiv/epp_output/", gbdyear, '/', run.name, "/")
dir.create(dir, showWarnings = FALSE)
dir.table <- fread(paste0('/share/hiv/epp_input/gbd20//dir_table_log_gbd20.csv'))

run.table <- fread(paste0('/share/hiv/epp_input/gbd20//eppasm_run_table.csv'))

### Functions
source(paste0('/ihme/homes/', user, '/rt-shared-functions/cluster_functions.R'))
source(paste0(root,"/Project/Mortality/shared/functions/check_loc_results.r"))
library(mortdb, lib = "/ihme/mortality/share/r/4")
library(mortdb, lib = "/mnt/team/mortality/pub/shared/r/4")

### Tables
loc.table <- data.table(get_locations(hiv_metadata = T))

### Code
epp.list <- sort(loc.table[epp == 1 & grepl('1', group), ihme_loc_id])
loc.list <- epp.list


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

if(array.job){
  epp.string <- paste0("qsub -l m_mem_free=7G -l fthread=1 -l h_rt=24:00:00 -l archive=True -q all.q -P ", cluster.project, " ",
                     "-e /share/temp/sgeoutput/", user, "/errors ",
                     "-o /share/temp/sgeoutput/", user, "/output ",
                     "-N ", "eppasm_", run.name, ' ',
                     "-tc 100 ",
                     "-t 1:", n.draws, " ",
                     "-hold_jid eppasm_prep_inputs_", run.name," ",
                     code.dir, "gbd/singR_shell.sh ",
                     code.dir, "gbd/main_forecasting.R ",
                     run.name, " ", array.job)
print(epp.string)
system(epp.string)
}

x = list.files('/mnt/share/hiv/epp_output/gbd20/220926_albatross')
x = x[!x %in% c('fit', 'dt_objects')]
library(assertable)
prob.locs = c('TZA', 'COD', 'DOM', "ETH_44857", "ETH_44862", 'LBR', 'LSO', "NGA_25320", "NGA_25322", "NGA_25326", "NGA_25328",
              "NGA_25332", "NGA_25338", "NGA_25340", "NGA_25342","NGA_25350","NGA_25352", "PNG","SEN",
              "SLE","SWZ","UGA","ZAF_482", "ZAF_483")
x = x[!x %in% prob.locs & !grepl('KEN', x) & !grepl('ZAF', x)]
for (loc in x){
  print(loc)
  dt = fread(paste0('/mnt/share/hiv/epp_output/gbd20/220926_albatross/', loc, '/1.csv'))
  id.cols = c('age', 'sex', 'year', 'run_num')
  val.check = colnames(dt)
  val.check = val.check[!val.check %in% id.cols]
  assert_values(dt, val.check, 'gte', 0)
  
}

if(run_eppasm & !array.job){
for(loc in loc.list) {    ## Run EPPASM
    submit_array_job(script = paste0(code.dir, 'gbd/main_forecasting.R'), n_jobs = n.draws,
                     queue = 'all.q', memory = '7G', threads = 1, time = "1:00:00",  name = paste0(loc, '_', run.name, '_eppasm'),
                     archive = F, args = c(run.name, loc, proj.end, paediatric, TRUE))
}

  #   epp.string <- paste0("qsub -l m_mem_free=7G -l fthread=1 -l h_rt=24:00:00 -l archive=True -q all.q -P ", cluster.project, " ",
  #                        "-e /share/temp/sgeoutput/", user, "/errors ",
  #                        "-o /share/temp/sgeoutput/", user, "/output ",
  #                        "-N ", loc,"_",run.name, "_eppasm ",
  #                        "-tc 100 ",
  #                        "-t 1:", n.draws, " ",
  #                        "-hold_jid eppasm_prep_inputs_", run.name," ",
  #                        code.dir, "gbd/singR_shell.sh ",
  #                        code.dir, "gbd/main_forecasting.R ",
  #                        run.name, " ", array.job," ", loc, " ", proj.end, " ", paediatric)
  #                        
  # 
  #                     
  # print(epp.string)
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









  
