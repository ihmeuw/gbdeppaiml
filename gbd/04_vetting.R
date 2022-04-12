## ---------------------------
## Script name: 04_vetting.R
## Purpose of script: Vet input data for EPP-ASM locations
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

# Toggles ---------------------------------------
new_run  = '210415_zanfona' ; new.run = new_run
old_run = '200713_yuka' ; old.run = old_run
epp.locs <- (loc.table[epp == 1, ihme_loc_id])
compare.inputs = F
compare.results = T

if(compare.inputs){
  compare_demo_inputs = T
  unaids_inputs = F
  compare_dt_obj = F
}
if(compare.results){
  age_specific_outputs = T
  compare_covariates = T
}


# Vet UNAIDS inputs ---------------------------------------
#ned to create script that plots the unaids inputs against each other
if(unaids_inputs){
  ###this script can be used as a basis, should hand off to Ned
  source('/ihme/homes/mwalte10/hiv_gbd2020/vetting/dt_vetting.R')
  source('/ihme/homes/mwalte10/hiv_gbd2020/vetting/vet_inputs.R')
  source('/ihme/homes/mwalte10/hiv_gbd2020/vetting/vetting.R')
  
  
}

# Compare run inputs ---------------------------------------
if(compare_demo_inputs){

  compare_run_inputs <- function(loc, new.run, old.run){
    dt <- NULL
    if(file.exists(paste0('/ihme/hiv/epp_input/gbd20/',old.run,'/births/',loc, '.csv'))){
      old <- fread(paste0('/ihme/hiv/epp_input/gbd20/',old.run,'/births/',loc, '.csv'))
      new <- fread(paste0('/ihme/hiv/epp_input/gbd20/',new.run,'/births/',loc, '.csv'))
      dt <- rbind(dt, old[,run := old.run], new[,run:= new.run], fill = T)
      setnames(dt, 'population', 'value')
      dt[,input := 'births']
    }
    
    if(file.exists(paste0('/ihme/hiv/epp_input/gbd20/',old.run,'/SRB/',loc, '.csv'))){
      old <- fread(paste0('/ihme/hiv/epp_input/gbd20/',old.run,'/SRB/',loc, '.csv'))
      new <- fread(paste0('/ihme/hiv/epp_input/gbd20/',new.run,'/SRB/',loc, '.csv'))
      srb <- rbind(old[,run := old.run], new[,run := new.run])
      srb[,input:='SRB']
      srb <- melt(srb, id.vars = c('year_id', 'location_id', 'run_id', 'input', 'run'))
      srb[variable == 'female_srb', sex_id := 2]
      srb[variable == 'male_srb', sex_id := 1]
      srb[,variable := NULL]
      dt <- rbind(dt, srb, fill = T)
    }
    
    age.map <- get_age_map(type = 'all')
    if(file.exists(paste0('/ihme/hiv/epp_input/gbd20/',old.run,'/population_splits/',loc, '.csv')) &
       file.exists(paste0('/ihme/hiv/epp_input/gbd20/',old.run,'/population_single_age/',loc, '.csv')) ){
      old <- rbind(fread(paste0('/ihme/hiv/epp_input/gbd20/',old.run,'/population_splits/',loc, '.csv')),
                   fread(paste0('/ihme/hiv/epp_input/gbd20/',old.run,'/population_single_age/',loc, '.csv')))
      new <- rbind(fread(paste0('/ihme/hiv/epp_input/gbd20/',new.run,'/population_splits/',loc, '.csv')),
                   fread(paste0('/ihme/hiv/epp_input/gbd20/',new.run,'/population_single_age/',loc, '.csv')))
      pop <- rbind(old[,run := old.run], new[,run:= new.run], fill = T)
      pop[,input := 'population']
      pop <- merge(pop, age.map[,.(age_group_id,age_group_name_short)])
      pop[age_group_name_short %in% c('enn', 'lnn',  '1m', '6m'), age_group_name_short := 0]
      pop[age_group_name_short %in% c('12m'), age_group_name_short := 1]
      pop <- pop[age_group_name_short %in% as.character(c(0:80)), ]
      pop[,age_group_name_short := as.integer(age_group_name_short)]
      pop[,age_group_name_short :=  as.character(age_group_name_short - age_group_name_short%%5)]
      pop <- merge(pop[,age_group_id := NULL], age.map[age_group_id %in% c(c(6:21), 28),.(age_group_id, age_group_name_short)])
      pop[,value := sum(population), by = c('run', 'age_group_id', 'sex_id', 'year_id')]
      pop <- unique(pop[,.(age_group_id, year_id, sex_id, value, run, input)])
      dt <- rbind(dt, pop, fill = T)
    }
    if(file.exists(paste0('/ihme/hiv/epp_input/gbd20/',old.run,'/migration/',loc, '.csv'))){
      old <- fread(paste0('/ihme/hiv/epp_input/gbd20/',old.run,'/migration/',loc, '.csv'))
      new <- fread(paste0('/ihme/hiv/epp_input/gbd20/',new.run,'/migration/',loc, '.csv'))
      mig <- rbind(old[,run := old.run], new[,run := new.run])
      mig <- mig[,input := 'migration']
      mig[input == 'migration',age_group_name_short :=  as.character(age - age%%5)]
      mig <- merge(mig, age.map[age_group_id %in% c(c(6:21), 28),.(age_group_id, age_group_name_short)], all.x = T)
      mig[input == 'migration',value := sum(value), by = c('run', 'age_group_id', 'sex', 'year')]
      mig <- unique(mig[,.(sex,year,value, run, age_group_id, input)])
      setnames(mig, 'sex', 'sex_id')
      setnames(mig, 'year', 'year_id')
      dt <- rbind(dt, mig, fill= T)
    }
    
    if(file.exists(paste0('/ihme/hiv/epp_input/gbd20/',old.run,'/ASFR/',loc, '.csv'))){
      old <- fread(paste0('/ihme/hiv/epp_input/gbd20/',old.run,'/ASFR/',loc, '.csv'))
      new <- fread(paste0('/ihme/hiv/epp_input/gbd20/',new.run,'/ASFR/',loc, '.csv'))
      asfr <- rbind(old[,run := old.run], new[,run := new.run])
      setnames(asfr, 'age', 'age_group_name_short')
      asfr[,age_group_name_short := as.character(age_group_name_short)]
      asfr <- merge(asfr, age.map[age_group_id %in% c(c(6:21), 28),.(age_group_id, age_group_name_short)], all.x = T)
      setnames(asfr, 'year', 'year_id')
      dt <- rbind(dt, asfr[,input := 'ASFR'], fill = T)
      
    }
    dt[is.na(sex_id), sex_id := 3]
    dt <- dt[,.(age_group_id, year_id, value, run, input, sex_id)]
    dt <- merge(dt, age.map[age_group_id %in% c(c(6:21), 28),.(age_group_id, age_group_name_short)])
    dt[,value := as.numeric(value)]
    
    inputs <- unique(dt[,input])
    dir.create(paste0('/ihme/hiv/epp_input/gbd20/', new.run, '/input_plots/'), showWarnings = F)
    pdf(paste0('/ihme/hiv/epp_input/gbd20/', new.run, '/input_plots/', loc, '.pdf'), width = 11, height = 8)
    for(input.x in inputs){
      gg <- ggplot(dt[input == input.x,], aes(year_id, value, col = factor(run))) + geom_line(aes(linetype = factor(sex_id))) + 
        facet_wrap(~age_group_name_short, scales = 'free_y') + ggtitle(paste0(input.x, ' for ', loc))
      print(gg)
    }
    dev.off()
    
  }
  
  lapply(epp.locs, compare_run_inputs, new.run = new_run, old.run = old_run)
  
}

# Compare dt objects ---------------------------------------
if(compare_dt_obj){
  dt_obj_compare <- function(loc, new.run, old.run){
    if(!file.exists(paste0('/ihme/hiv/epp_output/gbd20/',new.run,'/dt_objects/',loc,'_dt.RDS'))){next}
    
    new <- readRDS(paste0('/ihme/hiv/epp_output/gbd20/',new.run,'/dt_objects/',loc,'_dt.RDS'))
    if(!file.exists(paste0('/ihme/hiv/epp_output/gbd20/',old.run,'/dt_objects/',loc,'_dt.RDS'))){next}
    old <- readRDS(paste0('/ihme/hiv/epp_output/gbd20/',old.run,'/dt_objects/',loc,'_dt.RDS'))
    
    new.pop <- attr(new, 'specfp')$entrantpop
    old.pop <- attr(old, 'specfp')$entrantpop
    new.pop <- data.table(melt(new.pop))[,model := 'new']
    old.pop <- data.table(melt(old.pop))[,model := 'old']
    pop <- rbind(new.pop, old.pop)
    if(any(colnames(pop) == 'X2')){
      entrant_pop.gg <- ggplot(pop, aes(X2, value, col = factor(model))) + geom_line() + facet_wrap(~X1)
      
    }else{
      entrant_pop.gg <- ggplot(pop, aes(Var2, value, col = factor(model))) + geom_line() + facet_wrap(~Var1)
      
    }
    print('entrant')
    
    new.pop <- attr(new, 'specfp')$art_mort
    old.pop <- attr(old, 'specfp')$art_mort
    men.old <- data.table(melt(old.pop[,,1,1]))[,model := 'old']
    men.old[,sex := 1]
    men.new <- data.table(melt(new.pop[,,1,1]))[,model := 'new']
    men.new[,sex := 1]
    women.old <- data.table(melt(old.pop[,,1,2]))[,model := 'old']
    women.old[,sex := 2]
    women.new <- data.table(melt(new.pop[,,1,2]))[,model := 'new']
    women.new[,sex := 2]
    dt <- rbind(men.old, men.new, women.old, women.new)
    art_mort.gg <- ggplot(dt, aes(x = cd4stage, y = value, col = factor(model))) + geom_line(aes(linetype = factor(artdur))) + facet_wrap(~sex)
    print('art_mort')
    
    new.dt <- attr(new, 'specfp')$pmtct_num
    old.dt <- attr(old, 'specfp')$pmtct_num
    new.dt <- data.table(melt(new.dt, id.vars = 'year'))[,model := 'new']
    old.dt <- data.table(melt(old.dt, id.vars = 'year'))[,model := 'old']
    dt <- rbind(new.dt, old.dt)
    pmtct.gg <- ggplot(dt, aes(x = year, y = value, col = factor(model))) + geom_line() + facet_wrap(~variable)
    print('pmtct')
    
    new.anc <- data.table(attr(new, 'eppd')$ancsitedat)
    old.anc <- data.table(attr(old, 'eppd')$ancsitedat)
    new.anc[,model := 'new'] ; old.anc[,model := 'old']
    anc <- rbind(new.anc, old.anc, fill = T)
    if(!any(colnames(anc)=='offset')){
      anc[,offset:=0]
    }
    ss <- copy(anc[,.(year, n, model)])
    offset <- copy(anc[,.(year, offset, model)])
    setnames(anc, 'prev', 'value') ; setnames(ss, 'n', 'value') ; setnames(offset, 'offset', 'value')
    anc[,var := 'prev'] ; ss[,var := 'sample size'] ; offset[,var := 'offset']
    anc.temp = copy(anc[,.(year, value, model, var)])
    rm(anc)
    anc <- rbind(anc.temp, ss, offset, fill = T)
    anc <- as.data.table(anc)
    anc.gg <- ggplot(anc, aes(year, value, col = factor(model))) + geom_point() + facet_wrap(~var, scales = 'free')
    print('anc')
    
    dir.create(paste0('/ihme/hiv/epp_input/gbd20/', new.run, '/dt_obj_vet/'), recursive = T, showWarnings = F)
    pdf(paste0('/ihme/hiv/epp_input/gbd20/', new.run, '/dt_obj_vet/', loc, '.pdf'))
    print(entrant_pop.gg) ; print(art_mort.gg) ; print(pmtct.gg); print(anc.gg)
    dev.off()
    print(loc)
    
  }
  
  lapply(epp.locs, dt_obj_compare, new.run = new_run, old.run = old_run)
}

if(compare_art){
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
}

# Compare covariates ---------------------------------------
if(compare_covariates){
  run.name_base = "210415_zanfona"
  out_dir <- paste0("/ihme/hiv/epp_output/gbd20/",run.name_base, '/covariate_plots/')
  gbd_year = "gbd20"
  dir.create(paste0(out_dir,gbd_year,"/"), recursive = T)
  uploaded = F
  
  if(uploaded){
    get_covariate_changes <- function(
      covariate,
      run_base = run_base,
      gbd_round_base=gbd_step_base,
      decomp_step_base=decomp_step_base,
      gbd_round_compare = gbd_round_compare,
      decomp_step_compare = decomp_step_compare,
      run_compare = run_compare,
      year_check = 1990:2022,
      write = FALSE,
      locations = locations,
      rate = F
    ){
      
      locations <- get_locations(hiv_metadata = TRUE)[level==3,.(location_name,ihme_loc_id,location_id,group)]
      
      
      gbd1 =  get_covariate_estimates(
        covariate_id=covariate,
        location_id=locations$location_id,
        gbd_round_id=gbd_round_base,
        decomp_step = decomp_step_base
      )
      
      gbd2 = get_covariate_estimates(
        covariate_id=covariate,
        location_id=locations$location_id,
        gbd_round_id=gbd_round_compare ,
        decomp_step = decomp_step_compare
      )
      
      
      gbd1[,run := "V1"]
      gbd2[,run := "V2"]
      
      id.vars = c("location_id","year_id","run","sex_id")
      
      dt = melt(gbd1[,.(location_id,year_id,sex_id,run,mean_value)], id.vars = id.vars)
      
      dt = rbind(dt,
                 melt(gbd2[,.(location_id,year_id,sex_id,run,mean_value)], id.vars = id.vars)
      )
      
      dt_cast = data.table::dcast(dt, location_id + year_id +  sex_id  ~ run)
      dt_cast = merge(dt_cast,pops, by = c("location_id","year_id","sex_id"))
      if(!rate){
        dt_cast[,V1 := V1 * population]
        dt_cast[,V2 := V2 * population]
      }
      
      
      dt_cast[,pct_change := (V2 - V1)/V1*100]
      dt_cast[abs(pct_change) <= 20, change := "less20"]
      dt_cast[abs(pct_change) >= 20 & abs(pct_change) <= 40, change := "20to40"]
      dt_cast[abs(pct_change) >= 40 , change := "40plus"]
      dt_cast <- merge(dt_cast ,locations, by = "location_id")
      
      #Write out > 20% changes
      big_change = dt_cast[change != "less20"] 
      big_change = big_change[year_id %in% year_check,.(year_id,ihme_loc_id,pct_change, group,V1,V2)]
      setnames(big_change,c("V1","V2"), c(run_base,run_compare))
      
      if(write) write.csv(big_change,paste0(out_dir,covariate,"_",run_base,"VS",run_compare), row.names = FALSE)
      
      setnames(dt_cast,c("V1","V2"), c(run_base,run_compare))
      
      return(dt_cast)
      
    }
  }else{
    get_covariate_changes <- function(
      covariate,
      run_base = run_base,
      gbd_round_base=gbd_step_base,
      decomp_step_base=decomp_step_base,
      run.name_base,
      gbd_round_compare = gbd_round_compare,
      decomp_step_compare = decomp_step_compare,
      run_compare = run_compare,
      year_check = 1990:2022,
      write = FALSE,
      locations = locations,
      rate = F
    ){
      
      locations <- get_locations(hiv_metadata = TRUE)[level==3,.(location_name,ihme_loc_id,location_id,group)]
      
      
      if(covariate == 5){
        out.dir <- paste0("/ihme/hiv/gbd_results/covariates/hiv_death_adult_15_59/")
        gbd1 <- fread(paste0(out.dir, run.name_base, "_compiled_hiv_sims.csv"))
      }
      gbd2 = get_covariate_estimates(
        covariate_id=covariate,
        location_id=locations$location_id,
        gbd_round_id=gbd_round_compare ,
        decomp_step = decomp_step_compare
      )
      
      
      gbd1[,run := "V1"]
      gbd2[,run := "V2"]
      
      id.vars = c("location_id","year_id","run","sex_id")
      
      dt = melt(gbd1[,.(location_id,year_id,sex_id,run,mean_value)], id.vars = id.vars)
      
      dt = rbind(dt,
                 melt(gbd2[,.(location_id,year_id,sex_id,run,mean_value)], id.vars = id.vars)
      )
      
      dt_cast = data.table::dcast(dt, location_id + year_id +  sex_id  ~ run)
      dt_cast = merge(dt_cast,pops, by = c("location_id","year_id","sex_id"))
      if(!rate){
        dt_cast[,V1 := V1 * population]
        dt_cast[,V2 := V2 * population]
      }
      
      
      dt_cast[,pct_change := (V2 - V1)/V1*100]
      dt_cast[abs(pct_change) <= 20, change := "less20"]
      dt_cast[abs(pct_change) >= 20 & abs(pct_change) <= 40, change := "20to40"]
      dt_cast[abs(pct_change) >= 40 , change := "40plus"]
      dt_cast <- merge(dt_cast ,locations, by = "location_id")
      
      #Write out > 20% changes
      big_change = dt_cast[change != "less20"] 
      big_change = big_change[year_id %in% year_check,.(year_id,ihme_loc_id,pct_change, group,V1,V2)]
      setnames(big_change,c("V1","V2"), c(run_base,run_compare))
      
      if(write) write.csv(big_change,paste0(out_dir,covariate,"_",run_base,"VS",run_compare), row.names = FALSE)
      
      setnames(dt_cast,c("V1","V2"), c(run_base,run_compare))
      
      return(dt_cast)
      
    }
  }
  
  
  ##Locations
  locations <- get_locations(hiv_metadata = TRUE)[level==3,.(location_name,ihme_loc_id,location_id,group)]
  pops = get_population(year_id = -1, sex_id = 1:3, location_id = locations$location_id, decomp_step = "iterative", gbd_round_id = 7)
  
  
  #u5 is covariate 214
  #hiv15q145 is covariate 5
  covariate = 5
  run_base = "gbd20"
  gbd_round_base = 7 
  decomp_step_base="iterative"
  gbd_round_compare = 7
  decomp_step_compare = "iterative"
  run_compare = "gbd20"
  rate = F
  run.name_base = "210415_zanfona"
  
  if(!uploaded){
    dt_adult = get_covariate_changes(covariate = 5,
                                     run_base = run_base,
                                     gbd_round_base = gbd_round_base,
                                     decomp_step_base = decomp_step_base,
                                     gbd_round_compare = gbd_round_compare,
                                     decomp_step_compare = decomp_step_compare,
                                     run_compare = run_compare,
                                     rate = rate,
                                     run.name_base = run.name_base)
  }else{
    dt_adult = get_covariate_changes(covariate = 5,
                                     run_base = run_base,
                                     gbd_round_base = gbd_round_base,
                                     decomp_step_base = decomp_step_base,
                                     gbd_round_compare = gbd_round_compare,
                                     decomp_step_compare = decomp_step_compare,
                                     run_compare = run_compare,
                                     rate = rate)
  }
  
  
  dt_child = get_covariate_changes(covariate = 214,
                                   run_base = run_base,
                                   gbd_round_base = gbd_round_base,
                                   decomp_step_base = decomp_step_base,
                                   gbd_round_compare = gbd_round_compare,
                                   decomp_step_compare = decomp_step_compare,
                                   run_compare = run_compare,
                                   rate = rate)
  
  
  ## Plots ##
  dt <- dt_adult
  if(!rate){
    max_val = 150000
    thresh_val = 25000 
  }else{
    max_val = 0.015
    thresh_val = 0.0025
  }
  plot_title = "Adult Crude Death Rate by Sex, Impact of COVID effects on Death Rate"
  run_compare_name <- '200713_yuka'
  library('ggrepel')
  plot.dir <- paste0('/ihme/hiv/spectrum_plots/',  run_compare_name, '/')
  pdf(paste0(plot.dir,"_adult_deaths.pdf"), width = 10,height = 8)
  gg <- ggplot(dt[year_id  >= 1995 & !is.na(change) & !is.na(`210415_zanfona`)],aes(get(run_compare),get(run_base),color = change, label = ihme_loc_id)) + 
    geom_point(size=0.5) + geom_abline(intercept = 0, slope = 1) + xlim(0,max_val) + ylim(0,max_val) + 
    geom_text_repel(data = dt[year_id %in% c(2020,2021) & change != 'less20'],aes(label = ihme_loc_id), color="black",size=2.5) + 
    theme_bw() +  facet_wrap(~sex_id) +
    xlab(run_compare) + ylab(run_base) + ggtitle(plot_title)
  # ggplotly(gg)
  print(gg)
  
  dev.off()
  
  dt <- dt_child
  max_val = 150000
  thresh_val = 15000 
  plot_title = "Under 5 Death Rate"
  pdf(paste0(plot.dir,"_child_deaths.pdf"), width = 10,height = 8)
  gg <- ggplot(dt[year_id  >= 1995 & !is.na(change)],aes(get(run_compare),get(run_base),color = change, label = ihme_loc_id)) + 
    geom_point(size=0.5) + geom_abline(intercept = 0, slope = 1) + xlim(0,max_val) + ylim(0,max_val) + 
    #    geom_text_repel(data = dt[get(run_base) > thresh_val & change != "less20"& year_id %in% c(2000,2010,2019)],aes(label = ihme_loc_id), color="black",size=2.5) + theme_bw() +  facet_wrap(~sex_id) +
    geom_text_repel(data = dt[get(run_base) > thresh_val  & pct_change > 0.05 & year_id %in% c(2000,2010,2019)],aes(label = ihme_loc_id), color="black",size=2.5) + theme_bw() +  facet_wrap(~sex_id) +
    xlab(run_compare) + ylab(run_base) + ggtitle(plot_title)
  # ggplotly(gg)
  print(gg)
  dev.off()
  
}


# Age specific outputs ---------------------------------------
if(age_specific_outputs){
  age_map <- get_age_map(gbd_year = 7)
  
  pdiff_age <- function(loc, new.run, old.run){
    run.list <- c(new.run, old.run)
    
    cur.dt.list <- list()
    for(run in run.list){
      print(run)
      input.path <- paste0("/ihme/hiv/spectrum_prepped/summary/",run,"/locations/")
      if(run == '200316_windchime'){
        input.path <- paste0(root, "WORK/04_epi/01_database/02_data/hiv/spectrum/summary/",run,"/locations/")
      }
      
      
      if(file.exists(paste0(input.path, loc, "_spectrum_prep.csv"))){
        cur.dt <- fread(paste0(input.path, loc, "_spectrum_prep.csv"))[sex_id==3]
        cur.dt[,c("ihme_loc_id", "clinic", "type"):=.(loc, NA, "line")]
        cur.dt[,source := run]
        cur.dt <- cur.dt[measure != 'Spec_Deaths']
        cur.dt <- cur.dt[measure != 'Spec_Deaths']
        cur.dt <- cur.dt[measure != 'Incidence']
        cur.dt <- cur.dt[metric == 'Count']
        cur.dt[measure=="Scaled_Inc", c('source','measure') := .(run,"Incidence")]
        cur.dt[measure == 'ART', line_type := 'ART']
        cur.dt[is.na(line_type), line_type := 'All']
        if(run == old.run){
          setnames(cur.dt, 'mean', 'old_run')
          cur.dt <- cur.dt[,mget(c('year_id', 'sex_id', 'age_group_id', 'measure', 'old_run'))]
          
        }
        if(run == new.run){
          setnames(cur.dt, 'mean', 'new_run')
          cur.dt <- cur.dt[,mget(c('year_id', 'sex_id', 'age_group_id', 'measure', 'new_run'))]
          
        }
        cur.dt.list[[run]] = cur.dt
      }
    }
    
    cur.dt <- merge(cur.dt.list[[1]], cur.dt.list[[2]])
    cur.dt[,gbd_epp_pdiff := 100 *abs(new_run - old_run) / old_run]
    
    cur.dt <- cur.dt[old_run != 0,]
    
    cur.dt[gbd_epp_pdiff < 20, level_pdiff:= 'lessthan20']
    cur.dt[gbd_epp_pdiff  > 20 , level_pdiff:= '20to40']
    cur.dt[gbd_epp_pdiff > 40, level_pdiff:= '40plus']
    cur.dt <- merge(cur.dt, age_map, by = 'age_group_id')
    
    dir.create(paste0('/ihme/hiv/epp_output/gbd20/',new.run,'/vetting/',loc), recursive = T, showWarnings = F)
    pdf(paste0('/ihme/hiv/epp_output/gbd20/',new.run,'/vetting/',loc,'.pdf'), height = 10, width = 10)
    measure.list <- unique(cur.dt[,measure])
    for(measure.x in measure.list){
      gg <- ggplot(cur.dt[measure == measure.x], aes(x = old_run, y = new_run, col = as.factor(level_pdiff))) + geom_point() + facet_wrap(~age_group_name, scales = 'free') +
        geom_abline(intercept = 1, slope = 1) + ggtitle(paste0(loc.table[ihme_loc_id == loc, plot_name], ' ', measure.x))
      print(gg)
    }
    graphics.off()
    
  }
  
  lapply(epp.locs, pdiff_age, new.run = new_run, old.run = old_run)
  
}




