ind.locs <- loc.table[epp == 1 & grepl('IND',ihme_loc_id), ihme_loc_id]


get_results <- function(loc_val){
  loc = loc_val
  ## Used in basically every script
  Sys.umask(mode = "0002")
  windows <- Sys.info()[1][["sysname"]]=="Windows"
  root <- ifelse(windows,"J:/","/home/j/")
  user <- ifelse(windows, Sys.getenv("USERNAME"), Sys.getenv("USER"))
  
  
  # Arguments ---------------------------------------

    array.job = FALSE
    run.name <- "200713_yuka"
    stop.year <- 2022
    j <- 1
    paediatric <- TRUE

  
  eppasm_dir <- paste0(ifelse(windows, "H:", paste0("/ihme/homes/", user)), "/eppasm/")
  setwd(eppasm_dir)
  devtools::load_all()
  gbdeppaiml_dir <- paste0(ifelse(windows, "H:", paste0("/ihme/homes/", user)), "/gbdeppaiml/")
  setwd(gbdeppaiml_dir)
  devtools::load_all()
  
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
    combo_num <- array.dt[task_id,combo]
    loc <- array.dt[task_id,ihme_loc_id]
    pred.mat <- readRDS('/ihme/homes/mwalte10/hiv_gbd2019/requests/haidong_proj/maggie/pref_mat.RDS')
    foi_scalar <- unique(pred.mat[ihme_loc_id == loc & combo == combo_num])[,.(year_id, scalar)]
  }else{
    file_name <- loc
    foi_scalar = 1
  }
  
  out.dir <- paste0('/ihme/hiv/epp_output/',gbdyear,'/', run.name, "/", file_name)
  
  source('/ihme/homes/mwalte10/gbdeppaiml/gbd/data_prep.R')
  # Location specific toggles ---------------------------------------
  # ANC data bias adjustment
  ##### These locations do not have information from LBD team estimates
  ##### ZAF ANC data are considered nationally representative so no GeoADjust - this could be challenged in the future
  no_geo_adj <-  c(loc.table[epp ==1 & grepl("IND",ihme_loc_id),ihme_loc_id],
                   "PNG","HTI","DOM", 'CPV', loc.table[epp ==1 & grepl("ZAF",ihme_loc_id),ihme_loc_id], 
                   'STP', 'KEN_35626', 'MRT', 'COM')
  if(geoadjust & !loc %in% no_geo_adj | loc %in% c('ZWE', 'MWI')){
    geoadjust  <- TRUE
  } else {
    geoadjust  <- FALSE
  }
  print(paste0(loc, ' geoadjust set to ', geoadjust))
  
  # LBD Adjustments
  ##### Location that don't undergo LBD adjustment, set to TRUE as a default above
  if(!loc %in% unlist(strsplit(list.files('/share/hiv/data/PJNZ_EPPASM_prepped_subpop/lbd_anc/2019/'), '.rds')) | loc %in% c('ZAF', 'PNG') | grepl('IND', loc)){
    lbd.anc <- FALSE
  }
  if(run.name == '201012_ancrt'){
    lbd.anc = F
  }
  
  print(paste0(loc, ' lbd.anc set to ', lbd.anc))
  
  
  # No Sex incrr substitution
  if(loc %in% c("MAR","MRT","COM")){
    sexincrr.sub <- FALSE
  }
  
  
  # Prepare the dt object ---------------------------------------
  dt <- read_spec_object(loc, j, start.year, stop.year, trans.params.sub,
                         pop.sub, anc.sub,  prev.sub = prev_sub, art.sub = TRUE,
                         sexincrr.sub = sexincrr.sub,  age.prev = age.prev, paediatric = TRUE,
                         anc.prior.sub = TRUE, lbd.anc = lbd.anc,
                         geoadjust = geoadjust, use_2019 = TRUE,
                         test.sub_prev_granular = test,
                         anc.rt = FALSE
                         # anc.backcast,
  )
  ###Switched to a binomial model, so we can now handle observations of zero
  # mod <- data.table(attr(dt, 'eppd')$hhs)[prev == 0.0005,se := 0]
  # mod[prev == 0.0005, prev := 0]
  # attr(dt, 'eppd')$hhs <- data.frame(mod)
  
  ###Extends inputs to the projection year as well as does some site specific changes. This should probably be examined by cycle

    ###########################################################################
    # Remove duplicates from ancsitedat -------------------------------------------
    ###########################################################################
    ancsitedat <- data.table(attr(dt,'eppd')$ancsitedat)
    if('subpop' %in% colnames(ancsitedat)){
      if(nrow(unique(ancsitedat[,.(site, subpop, year, used, prev, n, type, agegr, age,agspan)])) != nrow(ancsitedat) ){
        ancsitedat <- ancsitedat[!duplicated(ancsitedat[,.(site, subpop, year, used, prev, n, type, agegr, age,agspan)]),]
        attr(dt, 'eppd')$ancsitedat <- data.frame(ancsitedat)
        return('Removed duplicates from sitedat')
      }
      
    }else{
      if(nrow(unique(ancsitedat[,.(site,  year, used, prev, n, type, agegr, age,agspan)])) != nrow(ancsitedat) ){
        ancsitedat <- ancsitedat[!duplicated(ancsitedat[,.(site,  year, used, prev, n, type, agegr, age,agspan)]),]
        attr(dt, 'eppd')$ancsitedat <- data.frame(ancsitedat)
        return('Removed duplicates from sitedat')
      }
    }
    
    
  
  ###Extends inputs to the projection year as well as does some site specific changes. This should probably be examined by cycle
}
library(parallel)
x <- lapply(ind.locs, get_results)
