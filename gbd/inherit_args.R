if(parallel){
  # Arguments ---------------------------------------
  args <- commandArgs(trailingOnly = TRUE)
  print(args)
  if(length(args) == 0){
    array.job = FALSE
    run.name <- "201015_socialdets_sens"
    loc <- 'AGO'
    stop.year <- 2022
    j <- 1
    paediatric <- TRUE
    cores = 1
  }else{
    run.name <- args[1]
    array.job <- as.logical(args[2])
  }
  
  if(!array.job & length(args) > 0){
    loc <- args[3]
    stop.year <- as.integer(args[4])
    j <- as.integer(Sys.getenv("SGE_TASK_ID"))
    paediatric <- as.logical(args[5])
  }
  if(array.job){
    cores = 20
  }
  
  gbdyear <- 'gbd20'
  stop.year = 2022
  
  run.table <- fread(paste0('/share/hiv/epp_input/gbd20//eppasm_run_table.csv'))
  in_run_table = F
  if(in_run_table){
    c.args <- run.table[run_name==run.name]
  }else{
    #use the GBD20 final run toggles if the run isn't in the run table
    c.args <- run.table[run_name=='200713_yuka']
  }
  
  geoadjust <- c.args[['geoadjust']]
  anc.sub <- c.args[['anc_sub']]
  anc.backcast <- c.args[['anc_backcast']]
  age.prev <- c.args[['age_prev']]
  popadjust <- c.args[['popadjust']]
  anc.rt <- c.args[['anc_rt']]
  epp.mod <- c.args[['epp_mod']]
  geoadjust <- c.args[['anc_sub']]
  no_anc <- c.args[['no_anc']]
  start.year <- c.args[['start.year']]
  trans.params.sub <- c.args[['trans.params.sub']]
  pop.sub <- c.args[['pop.sub']]
  art.sub <- c.args[['art.sub']]
  prev_sub <- c.args[['prev_sub']]
  sexincrr.sub <- c.args[['sexincrr.sub']]
  plot.draw <- c.args[['plot.draw']]
  anc.prior.sub <- c.args[['anc.prior.sub']]
  test <- c.args[['test']]
  anc.prior.sub <- c.args[['anc.prior.sub']]
  prev_sub <- c.args[['prev_sub']]
  sexincrr.sub <- c.args[['sexincrr.sub']]
  
  lbd.anc <- T
  ped_toggle = TRUE
  paediatric = TRUE
  
  # Array job ---------------------------------------
  if(array.job){
    array.dt <- fread(paste0('/ihme/hiv/epp_input/gbd20/',run.name,'/array_table.csv'))
    task_id <- as.integer(Sys.getenv("SGE_TASK_ID"))
    j <- array.dt[task_id,draws]
    draws <- array.dt[j, draws]
    draws <- c(as.numeric(draws) : (as.numeric(draws) + (cores - 1)))
    file_name <- array.dt[task_id,loc_scalar]
    foi_scalar <- array.dt[task_id,scale_foi]
    loc <- array.dt[task_id,ihme_loc_id]
  }else{
    foi_scalar = 1
  }
}else{
  # Arguments ---------------------------------------
  args <- commandArgs(trailingOnly = TRUE)
  print(args)
  if(length(args) == 0){
    array.job = FALSE
    run.name <- "201014_socialdets_rvec"
    loc <- 'AGO'
    stop.year <- 2022
    j <- 1
    paediatric <- TRUE
  }else{
    run.name <- args[1]
    array.job <- as.logical(args[2])
  }
  
  if(!array.job & length(args) > 0){
    loc <- args[3]
    stop.year <- as.integer(args[4])
    j <- as.integer(Sys.getenv("SGE_TASK_ID"))
    paediatric <- as.logical(args[5])
  }
  
  gbdyear <- 'gbd20'
  stop.year = 2022
  
  run.table <- fread(paste0('/share/hiv/epp_input/gbd20//eppasm_run_table.csv'))
  in_run_table = F
  if(in_run_table){
    c.args <- run.table[run_name==run.name]
  }else{
    #use the GBD20 final run toggles if the run isn't in the run table
    c.args <- run.table[run_name=='200713_yuka']
  }
  
  geoadjust <- c.args[['geoadjust']]
  anc.sub <- c.args[['anc_sub']]
  anc.backcast <- c.args[['anc_backcast']]
  age.prev <- c.args[['age_prev']]
  popadjust <- c.args[['popadjust']]
  anc.rt <- c.args[['anc_rt']]
  epp.mod <- c.args[['epp_mod']]
  geoadjust <- c.args[['anc_sub']]
  no_anc <- c.args[['no_anc']]
  start.year <- c.args[['start.year']]
  trans.params.sub <- c.args[['trans.params.sub']]
  pop.sub <- c.args[['pop.sub']]
  art.sub <- c.args[['art.sub']]
  prev_sub <- c.args[['prev_sub']]
  sexincrr.sub <- c.args[['sexincrr.sub']]
  plot.draw <- c.args[['plot.draw']]
  anc.prior.sub <- c.args[['anc.prior.sub']]
  test <- c.args[['test']]
  anc.prior.sub <- c.args[['anc.prior.sub']]
  prev_sub <- c.args[['prev_sub']]
  sexincrr.sub <- c.args[['sexincrr.sub']]
  
  lbd.anc <- T
  ped_toggle = TRUE
  paediatric = TRUE
  
  # Array job ---------------------------------------
  if(array.job){
    array.dt <- fread(paste0('/ihme/hiv/epp_input/gbd20/',run.name,'/array_table.csv'))
    task_id <- as.integer(Sys.getenv("SGE_TASK_ID"))
    j <- array.dt[task_id,draws]
    file_name <- array.dt[task_id,loc_scalar]
    foi_scalar <- array.dt[task_id,scale_foi]
    loc <- array.dt[task_id,ihme_loc_id]
  }else{
    file_name <- loc
    foi_scalar = 1
  }
  
  out.dir <- paste0('/ihme/hiv/epp_output/',gbdyear,'/', run.name, "/", file_name)
}