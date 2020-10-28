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
compare_demo_inputs = T
unaids_inputs = F
compare_covariates = F

# Vet UNAIDS inputs ---------------------------------------
#ned to create script that plots the unaids inputs against each other
if(unaids_inputs){
  
}

# Compare run inputs ---------------------------------------
if(compare_demo_inputs){
  new_run  = '200713_yuka'
  old_run = '200505_xylo'
  epp.locs <- (loc.table[epp == 1, ihme_loc_id])
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

# Compare covariates ---------------------------------------
if(compare_covariates){
  run.name_base = "200713_yuka"
  out_dir <- paste0("/ihme/hiv/epp_output/gbd20/",run.name_base, '/covariate_plots/')
  gbd_year = "gbd20"
  dir.create(paste0(out_dir,gbd_year,"/"))
  uploaded = T
  
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
  gbd_round_compare =6
  decomp_step_compare = "step4"
  run_compare = "gbd19"
  rate = F
  run.name_base = "200713_yuka"
  
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
  plot_title = "Adult Crude Death Rate by Sex"
  run_compare_name <- '200713_yuka'
  plot.dir <- paste0('/ihme/hiv/spectrum_plots/',  run_compare_name, '/')
  pdf(paste0(plot.dir,"_adult_deaths.pdf"), width = 10,height = 8)
  gg <- ggplot(dt[year_id  >= 1995 & !is.na(change)],aes(get(run_compare),get(run_base),color = change, label = ihme_loc_id)) + 
    geom_point(size=0.5) + geom_abline(intercept = 0, slope = 1) + xlim(0,max_val) + ylim(0,max_val) + 
    #geom_text_repel(data = dt[get(run_base) > thresh_val & change != "less20"& year_id %in% c(2000,2010,2019)],aes(label = ihme_loc_id), color="black",size=2.5) + theme_bw() +  facet_wrap(~sex_id) +
    # geom_text_repel(data = dt[get(run_base) > thresh_val & change != "less20" & year_id %in% c(2000,2010,2019)| ihme_loc_id == 'VNM' & pct_change == max(dt[ihme_loc_id == 'VNM',pct_change], na.rm = T),],
    #                 aes(label = ihme_loc_id), col = 'black', size=5) + 
    theme_bw() +  facet_wrap(~sex_id) +
    #geom_text_repel(data = dt[ihme_loc_id == 'VNM',],aes(label = ihme_loc_id), color="black",size=2.5) + theme_bw() +  facet_wrap(~sex_id) +
    
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
