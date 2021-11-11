#' Plot results of EPPASM
#'
#' @param output model output
#' @param eppd data input to eppasm
#'
#'
compare_spec.func <- function(run_vec = c('190630_rhino_ind', '200316_windchime'), stage = 'stage_1'){
compare.dt.s2 <- c() 
for(run in run_vec){
  temp.loc <- loc.table[parent_id == loc.table[ihme_loc_id == loc, location_id], ihme_loc_id][1]
  ##changing to compare to stage on temporarily
  compare.dt.run <- fread(paste0('/ihme/hiv/spectrum_draws/', run, '/compiled/', stage,'/summary/', temp.loc, '_all_age.csv'))
  compare.dt.run <- compare.dt.run[,.(type = 'line', year, indicator = variable, 
                                      model = paste0('Spectrum, ', stage, ', ', run), mean = value, lower = NA, upper = NA)]
  compare.dt.run[indicator == 'mort_rate', indicator := 'Deaths']
  compare.dt.run[indicator == 'inc_rate', indicator := 'Incidence']
  compare.dt.run[indicator == 'prev_rate', indicator := 'Prevalence']
  compare.dt.run <- compare.dt.run[indicator != 'art_rate']
  compare.dt.s2 <- rbind(compare.dt.s2, compare.dt.run)
}

return(compare.dt.s2)

}


compare_epp.func <- function(run_vec = c('190630_rhino_ind', '200316_windchime')){
  epp_comp <- c()
  for(run in run_vec){
    epp_inc <- paste0(root,"/WORK/04_epi/01_database/02_data/hiv/04_models/gbd2015/02_inputs/incidence_draws/",run,"/", loc, '_SPU_inc_draws.csv')
    epp_prev <- paste0(root,"/WORK/04_epi/01_database/02_data/hiv/04_models/gbd2015/02_inputs/prevalence_draws/",run,"/", loc, '_SPU_prev_draws.csv')
    # epp_death <- paste0(root,"/WORK/04_epi/01_database/02_data/hiv/04_models/gbd2015/02_inputs/death_draws/",run,"/", loc, '_SPU_death_draws.csv')
    
    epp_inc <- fread(epp_inc)
    epp_prev <- fread(epp_prev)
    # epp_death <- fread(epp_death)
    ##Average incidence and prevalences across draws
    epp_inc[,mean := rowSums(epp_inc[,2:101]) / 100 / 100]
    epp_prev[,mean := rowSums(epp_prev[,2:101]) / 100 / 100]
    # epp_death[,mean := rowSums(epp_death[,2:101]) / 100 / 100]
    
    epp_inc <- epp_inc[,.(year, mean)]
    epp_prev <- epp_prev[,.(year, mean)]
    # epp_death <- epp_death[,.(year, prev)]
    epp_inc[,indicator := 'Incidence']
    epp_prev[,indicator := 'Prevalence']
    # epp_death[,indicator := 'death']
    
    # epp <- rbind(epp_inc, epp_prev, epp_death)
    epp <- rbind(epp_inc, epp_prev)
    epp[,type := 'line']
    epp[,model := paste0('EPP, ', run)]
    epp[,lower := NA]
    epp[,upper := NA]
    
    
    epp_comp <- rbind(epp_comp, epp)
  }
  return(epp_comp)
  
}




plot_15to49_draw <- function(loc, output, eppd, run.name, compare.run = '190630_rhino2', un.comparison = TRUE, paediatric = FALSE){
  ## Get data used in fitting model
  #If India child get parent
  #####RIGHT NOW GOING TO SET A RUN.NAME.OLD ARGUMENT, THIS WILL NEED TO BE UPDATED AS WE HAVE NEW DATA
  
  if(loc %in% loc.table[grepl("IND",ihme_loc_id) & epp != 1,ihme_loc_id]){
    parent_id1 <- loc.table[ihme_loc_id==loc,parent_id]
    loc1 <- loc.table[location_id==parent_id1,ihme_loc_id]
    data <- fread(paste0('/share/hiv/epp_input/gbd20/', run.name, '/fit_data/', loc1, '.csv'))
    } else {
    data <- fread(paste0('/share/hiv/epp_input/gbd20/', run.name, '/fit_data/', loc, '.csv'))

  }
  
  data <- data[agegr == '15-49']
  data[, c('agegr', 'sex') := NULL]
  un.data <- paste0(root, "WORK/04_epi/01_database/02_data/hiv/data/prepped/GBD17_comparison_data.csv")
  
  if(un.comparison){
    compare.dt.unaids <- fread(un.data)
    compare.dt.unaids <- compare.dt.unaids[age_group_id == 22 & sex_id == 3 & measure %in% c('Incidence', 'Prevalence') & 
                                             metric == 'Rate' & source=="UNAIDS17" & ihme_loc_id==loc]
  }
  
  if(nrow(compare.dt.unaids) == 0){
    un.comparison <- FALSE
  }
  
  
  ## Comparison run
  if(file.exists(paste0('/snfs1/WORK/04_epi/01_database/02_data/hiv/spectrum/summary/', compare.run, '/locations/', loc, '_spectrum_prep.csv'))){
    compare.dt <- fread(paste0('/snfs1/WORK/04_epi/01_database/02_data/hiv/spectrum/summary/', compare.run, '/locations/', loc, '_spectrum_prep.csv'))
    compare.dt <- compare.dt[age_group_id == 24 & sex_id == 3 & measure %in% c('Incidence', 'Prevalence') & metric == 'Rate']
    compare.dt <- compare.dt[,.(type = 'line', year = year_id, indicator = measure, model = ifelse(compare.run == '180702_numbat_combined', 'GBD2017', compare.run), mean, lower, upper)]
  } else {
    compare.dt <- NULL
  }
  
  cur.dt <- get_summary(output, loc, run.name, paediatric)
  cur.dt <- cur.dt[age_group_id == 24 & sex == 'both' & measure %in% c('Incidence', 'Prevalence') & metric == 'Rate',.(type = 'line', year, indicator = measure, model = run.name, mean, lower = NA, upper = NA)]
  
  if(un.comparison == TRUE) {
    compare.dt.unaids <- compare.dt.unaids[,.(type = 'line', year = year_id, indicator = measure, model = "UNAIDS17", 
                                              mean=mean/100, lower=lower/100, upper=upper/100)]
    plot.dt <- rbind(data, compare.dt, cur.dt, compare.dt.unaids, use.names = T)
    plot.dt[,model := factor(model)]
    color.list <- c('blue', 'red', 'purple')
    names(color.list) <- c(run.name, ifelse(compare.run == '180702_numbat_combined', 'GBD2017', compare.run), "UNAIDS17")
    
  } else {
    
    plot.dt <- rbind(data, compare.dt, cur.dt, use.names = T)
    plot.dt[,model := factor(model)]
    color.list <- c('blue', 'red')
    names(color.list) <- c(run.name, ifelse(compare.run == '180702_numbat_combined', 'GBD2017', compare.run))
    
  }
  
  
  pdf(paste0('/ihme/hiv/epp_output/gbd19/', run.name, "/", loc, '/', i, '.pdf'), width = 10, height = 6)
  gg <- ggplot()
  if(nrow(plot.dt[model == 'ANC Site']) > 0){
    gg <- gg + geom_point(data = plot.dt[model == 'ANC Site'], aes(x = year, y = mean, shape = 'ANC Site'), alpha = 0.2)
  }
  gg <- gg + geom_line(data = plot.dt[type == 'line'], aes(x = year, y = mean, color = model)) +
    geom_ribbon(data = plot.dt[type == 'line'], aes(x = year, ymin = lower, ymax = upper,  fill = model), alpha = 0.2) +
    facet_wrap(~indicator, scales = 'free_y') +
    theme_bw() +
    scale_fill_manual(values=color.list) + scale_colour_manual(values=color.list)  +
    xlab("Year") + ylab("Mean") + ggtitle(paste0(loc.table[ihme_loc_id == loc, plot_name], ' EPPASM Results'))
  if(nrow(plot.dt[model == 'Household Survey']) > 0){
    gg <- gg + geom_point(data = plot.dt[model == 'Household Survey'], aes(x = year, y = mean, shape = 'Household Survey'))
    gg <- gg + geom_errorbar(data = plot.dt[model == 'Household Survey'], aes(x = year, ymin = lower, ymax = upper))
  }
  
  print(gg)
  dev.off()
}

plot_15to49 <- function(loc="IND_4873",  
                        run.vec = c('/gbd19/190630_rhino2', '/gbd20/200713_yuka/'),
                        base.run = '200713_yuka',
                        names = c('GBD19', 'GBD20'),
                        gbdyear = "gbd20",
                        loc_name = NULL){
  x <- data.table(cbind(name = names, run_name = run.vec))
  meas.list <- c('Incidence', 'Prevalence', 'Deaths', 'ART')

  
  if(is.null(loc_name)){
    loc_name = loc
  }
  # Load in all runs ---------------------------------------
  dt <- c()
  for(run in x[,run_name]){
    if(file.exists(paste0('/ihme/hiv/epp_output/', run, '/summary_files/', loc, '.csv'))){
      compare.dt <- fread(paste0('/ihme/hiv/epp_output/', run, '/summary_files/', loc, '.csv'))
    }else{
      compare.dt <- fread(paste0('/ihme/hiv/epp_output/', run, '/summary_files/', loc_name, '.csv'))
    }
    run <- x[run_name == run, name]
    compare.dt <- compare.dt[age_group_id == 24 & sex == 'both' & measure %in% meas.list & metric == "Rate",.(type = 'line', year, indicator = measure, model = run, mean, lower, upper)]
    dt <- rbind(compare.dt, dt) 
  }
  compare.dt <- dt
  
  # Load in data ---------------------------------------
  compare_data <- list()
  for(run in x[,run_name]){
    data <- fread(paste0('/share/hiv/epp_input/', run, '/fit_data/', loc, '.csv'))
    data <- data[metric == 'Rate']
    data[, c('age_group_id', 'metric', 'ihme_loc_id') := NULL]
    run <- x[run_name == run, name]
    data[,run := run]
    compare_data <- rbind(compare_data, data)
  }
  data = compare_data
  
  anc <- data[model == 'ANC Site']
  anc[,sex := NULL]
  cur.dt <- fread(paste0('/share/hiv/epp_output/', gbdyear, '/', base.run, '/compiled/', loc, '.csv'))
  ## post stratify age-specific data using Spectrum population
  if(nrow(data[age != '15-49']) > 0){
    pop.dt <- copy(cur.dt)
    pop.dt <- pop.dt[age %in% 15:59]
    
    male.pop <- pop.dt[sex == 'male' & run_num == 1, .(pop, year, age)]
    male.pop[,age := paste0(age - age%%5)]
    male.pop <- male.pop[,total := sum(pop), by = c('year', 'age')]
    male.pop[,sex := 'male']
    male.pop[,pop := NULL]
    
    female.pop <- pop.dt[sex == 'female' & run_num == 1, .(pop, year, age)]
    female.pop[,age := paste0(age - age%%5)]
    female.pop <- female.pop[,total := sum(pop), by = c('year', 'age')]
    female.pop[,sex := 'female']
    female.pop[,pop := NULL]
    
    
    aggre_pop <- rbind(unique(male.pop), unique(female.pop))
    age_agg_pop <- copy(aggre_pop)
    age_agg_pop <- age_agg_pop [,total := sum(total), by = c('year', 'sex')]
    age_agg_pop[,age := '15-49']
    aggre_pop <- rbind(aggre_pop, unique(age_agg_pop))
    setnames(aggre_pop, 'total', 'pop')
    
    pop.dt <- aggre_pop
    data.pre.agg <- data[age %in%   c('15-49', '15-64') & model == 'Household Survey']
    data.pre.agg[,pop := NA]
    data.agg <- merge(pop.dt, data[model == 'Household Survey'], by = c( 'age', 'sex','year'), fill = T)
    data.agg <- rbind(data.agg[,.(year, model, indicator, mean, pop)], data.pre.agg[,.(year, model, indicator, mean, pop)])
    data.agg <- data.agg[,.(mean = weighted.mean(x = mean, w = pop)), by =c('year', 'model', 'indicator') ]
    
    data.agg[,upper := NA]
    data.agg[,lower := NA]    
    ui.dt <- fread(paste0('/share/hiv/data/prevalence_surveys/GBD2017_prevalence_surveys_15to49.csv'))
    ui.dt <- ui.dt[iso3 == loc]
    data.agg <- merge(data.agg, ui.dt[,.(year, se)], by = 'year', all.x = T)
    data.agg[, upper := mean + (1.96 * se)] 
    data.agg[, lower := ifelse(mean - (1.96 * se) < 0, 0, mean - (1.96 * se))]
    data.agg[, se := NULL]
    data.agg[,type := 'point']
    data <- data.agg[model != "ANC Site"]
  }
  data <- rbind(data, anc, fill = T)
  data[,indicator := 'Prevalence']
  
  
  # UNAIDS data, this needs to be reextracted  ---------------------------------------
  compare.dt.unaids <- fread('/share/hiv/data/UNAIDS_extract/UNAIDS_results_2019.csv')
  compare.dt.unaids <- compare.dt.unaids[age_group_id == 24 & sex_id == 3 & measure %in% meas.list &
                                           metric == 'Rate' & ihme_loc_id==loc_name]
  compare.dt.unaids <- compare.dt.unaids[,.(type = 'line', year = year_id, indicator = measure, model = "UNAIDS",
                                            mean, lower, upper)]
  
  
  # If switching models ---------------------------------------
  if(!loc_name %in% loc.table[epp == 1, ihme_loc_id] | grepl('IND', loc)){
    compare.dt.s2 <- compare_spec.func(run_vec = c('190630_rhino_combined'), stage = 'stage_2')
    compare_epp <- compare_epp.func(run_vec = c('190630_rhino_ind'))
  }else{
    compare.dt.s2 = NULL
    compare_epp <- NULL
  }
  
  plot.dt <- rbind(data,compare.dt, compare.dt.s2, compare_epp,use.names = T, fill = T)
  
  plot.dt[,model := factor(model)]
  
  # Add on Rvec ---------------------------------------
  rvec.full <- list()
  # for(run in x[,run_name]){
  #   # if(!grepl('socialdets', base.run)){
  #     if(file.exists(paste0('/ihme/hiv/epp_output/', run, '/fit/', loc, '.RDS'))){
  #     parm <- readRDS(paste0('/ihme/hiv/epp_output/', run, '/fit/', loc, '.RDS'))
  #   }else{
  #     parm <- readRDS(paste0('/ihme/hiv/epp_output/', run, '/fit/', loc_name, '.RDS'))
  #   }
  #   years <- seq(1970, 2022, length.out = length(parm$rvec))
  #   rvec <- data.table(cbind(mean = parm$rvec, year = years))
  #   # }else{
  #   #   if(file.exists(paste0('/ihme/hiv/epp_output/gbd20/', base.run,'/scaled_foi/', loc, '.RDS'))){
  #   #     rvec <- readRDS(paste0('/ihme/hiv/epp_output/gbd20/', base.run,'/scaled_foi/', loc, '.RDS'))
  #   #   }else{
  #   #     rvec <- readRDS(paste0('/ihme/hiv/epp_output/gbd20/', base.run,'/scaled_foi/', loc_name, '.RDS'))
  #   #   }
  #   # }
  #   rvec[,year := floor(year)]
  #   rvec <- rvec[year %in% c(1970:2022), mean := mean(mean), by = 'year']
  #   rvec <- unique(rvec)
  #   rvec[,indicator := 'Transmission rate']
  #   rvec[,type := 'line']
  #   rvec[, model := x[run_name == run,name]]
  #   rvec.full <- rbind(rvec.full, rvec)
  # }
  # plot.dt <- rbind( plot.dt, rvec.full, fill = T)
  plot.dt <- rbind( plot.dt, fill = T)
  
  
  color.list <- c('blue', 'red', 'green','purple','orange','black', 'darkgreen', 'red')
  names(color.list) <- c(unique(plot.dt$model)[1:2], 'current run', 'GBD20', 'EPP', 'rlogistic', 'Probit', 'Binomial')
  plot.dt[,model := factor(model)]

  
  dir.create(paste0('/ihme/hiv/epp_output/', gbdyear, '/', base.run, '/15to49_plots/'), recursive = TRUE)
  pdf(paste0('/ihme/hiv/epp_output/', gbdyear, '/', base.run, '/15to49_plots/', loc, '.pdf'), width = 10, height = 6)
  
  gg <- ggplot()

  # gg <- gg + geom_ribbon(data = plot.dt[type == 'line'], aes(year, ymin = lower, ymax = upper, fill = model), alpha = 0.2)
  
  gg <- gg + geom_line(data = plot.dt[type == 'line'], aes(x = year, y = mean, color = model),  alpha = 0.8) 
   # facet_wrap(~indicator, scales = 'free') +
    gg <- gg + geom_point(data = plot.dt[type == 'ancss'], aes(x = year, y = mean, color = run), alpha = 0.2) +
  
    theme_bw() +
    scale_fill_manual(values=color.list) + scale_colour_manual(values=color.list)  +
    xlab("Year") + ylab("Mean") #+ ggtitle(paste0(loc.table[ihme_loc_id == loc, plot_name], ' EPPASM Results'))
  # if(nrow(plot.dt[model == 'ANC Site']) > 0){
  #   if(nrow(plot.dt[type == 'ancss']) > 0){
  #     gg <- gg + geom_point(data = plot.dt[type == 'ancss'], aes(x = year, y = mean, shape = 'ANC-SS'), alpha = 1)
  #   }else{
  #     gg <- gg + geom_point(data = plot.dt[model == 'ANC Site'], aes(x = year, y = mean, shape = 'ANC Site'), alpha = 1)
  #   }


    # if(nrow(plot.dt[type == 'ancrt']) > 0){
    #   gg <- gg + geom_point(data = plot.dt[type == 'ancrt'], aes(x = year, y = mean, shape = 'ANC-RT'), alpha = 0.2)
    # }
  
  if(nrow(plot.dt[model == 'Household Survey']) > 0){
    gg <- gg + geom_point(data = plot.dt[model == 'Household Survey'], aes(x = year, y = mean, shape = 'Household Survey'),size=3)
    gg <- gg + geom_errorbar(data = plot.dt[model == 'Household Survey'], aes(x = year, ymin = lower, ymax = upper))
  }
  if(nrow(plot.dt[model == 'HH Survey Agg']) > 0){
    gg <- gg + geom_point(data = plot.dt[model == 'HH Survey Agg'], aes(x = year, y = mean, shape = 'HH Survey Old'),size=3)
  }
  
  if(nrow(plot.dt[model == 'VR']) > 0){
    gg <- gg + geom_point(data = plot.dt[model == 'VR'], aes(x = year, y = mean, shape = 'VR'), size = 0.75)
  }
  
  gg <- gg + theme(axis.text = element_text(size = 16), axis.title = element_text(size = 18), legend.text = element_text(size = 16),
                   legend.title = element_text(size = 18)) + ggtitle(loc.table[ihme_loc_id == loc, plot_name]) + facet_wrap(~indicator, scales = 'free')
  
  print(gg)
  dev.off()
  
  
}


plot_rvec_array <- function(loc="KEN_35618",  
                        new.run = '200316_windchime_testing3'){
  array.dt <- fread(paste0('/ihme/hiv/epp_input/gbd20/',new.run,'/array_table.csv'))
  loc_scalars <- array.dt[ihme_loc_id == loc, loc_scalar]
  ##### 
  #add on rvecs
  rvec.full <- list()
  for(loc_scale in loc_scalars){
    if(!file.exists(paste0('/ihme/hiv/epp_output/gbd20/', new.run, '/fit/', loc_scale, '.RDS'))){
      next
    }
    parm <- readRDS(paste0('/ihme/hiv/epp_output/gbd20/', new.run, '/fit/', loc_scale, '.RDS'))
    years <- seq(1970, 2022, length.out = length(parm$rvec))
    rvec <- data.table(cbind(mean = parm$rvec, year = years))
    rvec[,year := floor(year)]
    rvec <- rvec[year %in% c(1970:2022), mean := mean(mean), by = 'year']
    rvec <- unique(rvec)
    rvec[,indicator := 'Trans rate']
    rvec[,type := 'line']
    rvec[, model := loc_scale]
    rvec.full <- rbind(rvec.full, rvec)
  }
  plot.dt <- rvec.full
  

  plot.dt[,model := factor(model)]
  
  dir.create(paste0('/ihme/hiv/epp_output/gbd20/', new.run, '/rvec/'), recursive = TRUE)
  
  pdf(paste0('/ihme/hiv/epp_output/gbd20/', new.run, '/rvec/', loc, '.pdf'), width = 10, height = 6)
  
  gg <- ggplot()
  gg <- gg + geom_line(data = plot.dt[type == 'line' & year > 1999], aes(x = year, y = mean, color = model)) +
    facet_wrap(~indicator, scales = 'free_y') +
    theme_bw() +
    xlab("Year") + ylab("Mean") + ggtitle(paste0(loc.table[ihme_loc_id == loc, plot_name], ' EPPASM Results'))
  print(gg)
  dev.off()
  
  
}

## intended for comparing eppasm fits for group 2 countries to ciba/spectrum
plot_spec_compare <- function(loc, run.name, paediatric = FALSE, c.metric = 'Rate'){
  age.map <- fread(paste0('/ihme/hiv/epp_input/gbd19/', run.name, "/age_map.csv"))
  if(loc %in% loc.table[grepl("IND",ihme_loc_id) & epp != 1,ihme_loc_id]){
    parent_id1 <- loc.table[ihme_loc_id==loc,parent_id]
    loc1 <- loc.table[location_id==parent_id1,ihme_loc_id]
    data <- fread(paste0('/share/hiv/epp_input/gbd19/', run.name, '/fit_data/', loc1, '.csv'))
  } else {
    data <- fread(paste0('/share/hiv/epp_input/gbd19/', run.name, '/fit_data/', loc, '.csv'))
  }
  
  if('Case Report' %in% data$model & c.metric == 'Rate'){
    pop.dt <- fread(paste0('/share/hiv/epp_input/gbd19/', run.name, '/population_single_age/', loc, '.csv'))
    diagn.dt <- data[model == 'Case Report']
    if(length(unique(diagn.dt$sex)) == 1){
      pop.dt <- pop.dt[,.(population = sum(population)), by = c('year_id')]
      setnames(pop.dt, 'year_id', 'year')
      rate.dt <- merge(diagn.dt, pop.dt, by = 'year')
      rate.dt[, mean := mean/population]
      rate.dt[, metric := 'Rate']
      rate.dt[, population := NULL]
    }else{
      ##TODO
      diagn.dt[, sex := ifelse(sex_id == 1, 'male', 'female')]
      pop.dt <- pop.dt[,.(population = sum(population)), by = c('year_id', 'sex_id')]
    }
    data <- rbind(data, rate.dt, use.names = T)
  }
  ## TODO fix 80+ VR
  if('VR' %in% data$model){
    data.80 <- data[age_group_id >= 30 & model == 'VR']
    data.80[, age_group_id := 21]
    data.80[, age := '80']
    data.80.count <- data.80[metric == 'Count']
    data.80.count <- data.80.count[,.(mean = sum(mean), lower = NA, upper = NA, type = 'point', model = 'VR', metric = 'Count', indicator = 'Deaths'), by = c('ihme_loc_id', 'sex', 'age', 'age_group_id', 'year')]
    data <- data[!(age_group_id >= 30 & model == 'VR')]
    data <- rbind(data, data.80.count, use.names = T)
  }
  data <- data[age_group_id %in% c(4:22) & metric == c.metric]
  data[, c('age_group_id', 'metric', 'ihme_loc_id') := NULL]
  
  ## we only have unaids all-ages results in rate space
  unaids.dt <- fread('/share/hiv/data/UNAIDS_extract/UNAIDS_results_2018.csv')
  unaids.dt <- unaids.dt[ihme_loc_id == loc & age_group_id == 22 & metric == c.metric,.(age = 'All', sex = 'both', type = 'line',
                                                                                        indicator = measure, model = 'UNAIDS18', mean, lower, upper, year = year_id)]

  cur.dt <- fread(paste0('/share/hiv/epp_output/gbd19/', run.name, '/compiled/', loc, '.csv'))
  cur.dt <- get_summary(cur.dt, loc, run.name, paediatric)
  cur.dt <- cur.dt[!age_group_id == 24 & measure %in% c('Incidence', 'Prevalence', 'Deaths') & metric == c.metric,
                   .(age, sex, type = 'line', year, indicator = measure, model = run.name, mean, lower, upper)]
  
  compare.dt <- fread(paste0('/share/hiv/spectrum_draws/180702_numbat_combined/compiled/stage_2/', loc, '_ART_data.csv'))
  compare.dt[,pop_hiv := pop_lt200 + pop_200to350 + pop_gt350 + pop_art]
  compare.dt <- compare.dt[,.(hiv_deaths, new_hiv, pop_hiv, year, sex, age, run_num)]
  compare.dt <- melt(compare.dt, id.vars = c('year', 'sex', 'age', 'run_num'))
  all.age <- compare.dt[,.(value = sum(value), age = 'All'), by = c('year', 'sex', 'run_num', 'variable')]
  both.sex <- compare.dt[,.(value = sum(value), sex = 'both'), by = c('year', 'age', 'run_num', 'variable')]
  all.both <- both.sex[,.(value = sum(value), age = 'All'), by = c('sex', 'year', 'run_num', 'variable')]
  compare.dt <- rbindlist(list(all.both, both.sex, all.age, compare.dt), use.names = T)
  compare.dt <- compare.dt[,.(mean = mean(value), upper = quantile(value, 0.975), lower = quantile(value, 0.025), model = 'GBD 2017 CIBA/Spectrum', type = 'line'), by = c('age', 'sex', 'year', 'variable')]
  compare.dt[variable == 'hiv_deaths', indicator := 'Deaths']
  compare.dt[variable == 'pop_hiv', indicator := 'Prevalence']
  compare.dt[variable == 'new_hiv', indicator := 'Incidence']
  compare.dt[, variable := NULL]

  
  both.dt <- rbind(data, cur.dt, unaids.dt, compare.dt, use.names = T)
  both.dt[,model := factor(model)]
  color.list <- c('blue', 'red', 'green', 'yellow', 'purple')
  names(color.list) <- c(run.name,  'GBD 2017 CIBA/Spectrum', compare.run, 'STGPR', 'UNAIDS18')
  if(!paediatric){
    both.dt <- both.dt[!age %in% c('enn', 'lnn', 'pnn', '1', '5','10', '0'),]
    both.dt[,age := factor(age, levels=c(paste0(seq(15, 80, 5)), 'All'))]
  }else{
    both.dt[,age := factor(age, levels=c('enn', 'lnn', 'pnn', '1', paste0( seq(5, 80, 5)), 'All'))]
  }
  
  for(c.indicator in c('Incidence', 'Prevalence', 'Deaths')){
    pdf(paste0('/ihme/hiv/epp_output/gbd19/', run.name, '/spec_compare_plots/', c.indicator, '/', loc, '.pdf'), width = 10, height = 6)
    for(c.sex in c('male', 'female', 'both')){
      plot.dt <- both.dt[sex == c.sex & indicator == c.indicator]
      gg <- ggplot()
      
      if(nrow(plot.dt[model == 'ANC Site']) > 0){
        gg <- gg + geom_point(data = plot.dt[model == 'ANC Site'], aes(x = year, y = mean, shape = 'ANC Site'), alpha = 0.2)
      }
      
      gg <- gg + geom_line(data = plot.dt[type == 'line'], aes(x = year, y = mean, color = model)) +
        geom_ribbon(data = plot.dt[type == 'line'], aes(x = year, ymin = lower, ymax = upper,  fill = model), alpha = 0.2) +
        facet_wrap(~age, scales = 'free_y') +
        theme_bw() +
        scale_fill_manual(values=color.list) + scale_colour_manual(values=color.list)  +
        xlab("Year") + ylab("Mean") + ggtitle(paste0(loc.table[ihme_loc_id == loc, plot_name], ' EPPASM ', c.sex, ' ', c.indicator))
      
      if(nrow(plot.dt[model == 'Household Survey']) > 0){
        gg <- gg + geom_point(data = plot.dt[model == 'Household Survey'], aes(x = year, y = mean, shape = 'Household Survey'))
        gg <- gg + geom_errorbar(data = plot.dt[model == 'Household Survey'], aes(x = year, ymin = lower, ymax = upper))
      }
      if(nrow(plot.dt[model == 'VR']) > 0){
        gg <- gg + geom_point(data = plot.dt[model == 'VR'], aes(x = year, y = mean, shape = 'VR'), size = 0.75)
      }
      if(nrow(plot.dt[model == 'Case Report']) > 0){
        gg <- gg + geom_point(data = plot.dt[model == 'Case Report'], aes(x = year, y = mean, shape = 'Case Report'), size = 0.75)
      }
      
      print(gg)
    }
    dev.off()
  }
}


plot_age_specific <- function(loc,  
                              compare.run = c('2020_ind_test_agg8', '2020_ind_test_agg7'),
                              c.metric = 'Rate', run.name.new, gbdyear = 'gbd20'){
  ###format 
  if(grepl('socialdets', run.name.new)){
    loc_name = unlist(strsplit(loc, '_'))[[1]]
  }else{
    loc_name = loc
  }
  group = loc.table[ihme_loc_id == loc, group]
  group =1
  # final_runs <- data.table('gbd_year' = c('gbd19','gbd20',gbdyear, rep(gbdyear, length(compare.run))), 
  #                          'run' = c('190630_rhino2', '200713_yuka', run.name.new, compare.run),
  #                          'run_name' = c('Final GBD19', 'Final GBD20', 'Current Run', compare.run))
  final_runs <- data.table('gbd_year' = c('gbd20',gbdyear, rep(gbdyear, length(compare.run))), 
                           'run' = c('200713_yuka', run.name.new, compare.run),
                           'run_name' = c('Final GBD20', 'Current Run', compare.run))
  # if(!is.null(compare.run)){
     compare = T
  # }else{
  #   compare = F
  # }
  
  ###load in fit_data
  age.map.old <- fread(paste0('/ihme/hiv/epp_input/gbd19/', '190630_rhino2', "/age_map.csv"))
  if(loc %in% loc.table[grepl("IND",ihme_loc_id) & epp != 1,ihme_loc_id]){
    parent_id1 <- loc.table[ihme_loc_id==loc,parent_id]
    loc1 <- loc.table.old[location_id==parent_id1,ihme_loc_id]
    data <- fread(paste0('/share/hiv/epp_input/gbd19/', run.name.old, '/fit_data/', loc1, '.csv'))
  } else {
    if(file.exists(paste0('/share/hiv/epp_input/gbd20/', run.name.new, '/fit_data/', loc_name, '.csv'))){
        data <- fread(paste0('/share/hiv/epp_input/gbd20/', run.name.new, '/fit_data/', loc_name, '.csv'))
      }else{
        print('Using 2019 fit data')
      data <- fread(paste0('/share/hiv/epp_input/gbd19/', run.name.old, '/fit_data/', loc, '.csv'))
    }
  }
  
  ###format fit_data
  data <- data[age %in% seq(15, 45, by = 5) | age == '15-49',]
  data[mean == 0, upper := 0]
  data <- data[age_group_id %in% c(4:22,24) & metric == c.metric]
  data[, c('age_group_id', 'metric', 'ihme_loc_id') := NULL]
  
  ###do Group 2 modifications
  if(grepl('2', group)){
    if('Case Report' %in% data$model & c.metric == 'Rate'){
      pop.dt <- fread(paste0('/share/hiv/epp_input/gbd19/', run.name.old, '/population_single_age/', loc_name, '.csv'))
      diagn.dt <- data[model == 'Case Report']
      if(length(unique(diagn.dt$sex)) == 1){
        pop.dt <- pop.dt[,.(population = sum(population)), by = c('year_id')]
        setnames(pop.dt, 'year_id', 'year')
        rate.dt <- merge(diagn.dt, pop.dt, by = 'year')
        rate.dt[, mean := mean/population]
        rate.dt[, metric := 'Rate']
        rate.dt[, population := NULL]
      }else{
        diagn.dt[, sex := ifelse(sex_id == 1, 'male', 'female')]
        pop.dt <- pop.dt[,.(population = sum(population)), by = c('year_id', 'sex_id')]
      }
      data <- rbind(data, rate.dt, use.names = T)
    }
    
    if('Deaths' %in% data$indicator){
      ## TODO could add CI of STGPR
      stgpr <- fread('/ihme/hiv/st_gpr/spectrum_gpr_results.csv')
      stgpr <- stgpr[location_id == loc.table[ihme_loc_id == loc, location_id] & age_group_id %in% 8:22, .(age_group_id, year = year_id, sex = ifelse(sex_id == 1, 'male', 'female'), mean = gpr_mean / 100, type = 'line',
                                                                                                           lower = NA, upper = NA, model = 'STGPR', indicator = 'Deaths')]
      stgpr <- merge(stgpr, age.map.old[,.(age_group_id, age = age_group_name_short)], by = 'age_group_id')
      if(c.metric == 'Count'){
        pop.dt <- fread(paste0('/share/hiv/epp_input/gbd20/', run.name.old, '/population/', loc, '.csv'))
        setnames(pop.dt, 'year_id', 'year')
        pop.dt[,sex:= ifelse(sex_id == 1, 'male', 'female')]
        stgpr.count <- merge(stgpr, pop.dt[,.(age_group_id, sex, year, population)], by = c('age_group_id', 'sex','year'))
        stgpr.count[, mean := mean*population]
        stgpr.count[, population := NULL]
        stgpr <- stgpr.count
      }
      stgpr[, age_group_id := NULL]
      data <- rbind(data, stgpr, use.names = T)
    }
    
    ## Comparison run, changing to unraked results, 03/04/2020
    # if(file.exists(paste0('/snfs1/WORK/04_epi/01_database/02_data/hiv/spectrum/summary/190630_rhino_combined/locations/', loc, '_spectrum_prep.csv'))){
    #   compare.dt.19 <- fread(paste0('/snfs1/WORK/04_epi/01_database/02_data/hiv/spectrum/summary/190630_rhino_combined/locations/', loc, '_spectrum_prep.csv'))
    #   compare.dt.19 <- compare.dt.19[!age_group_id > 24 & measure %in% c('Incidence', 'Prevalence', 'Deaths') & metric == c.metric]
    #   compare.dt.19 <- merge(compare.dt.19, age.map.old[,.(age_group_id,age = age_group_name_short)], by = 'age_group_id', all.x = T)
    #   compare.dt.19[sex_id == 1, sex := 'male']
    #   compare.dt.19[sex_id == 2, sex := 'female']
    #   compare.dt.19[sex_id == 3, sex := 'both']
    #   compare.dt.19[age_group_id == 24, age := '15 to 49']
    #   compare.dt.19 <- compare.dt.19[,.(age, sex, type = 'line', year = year_id, 
    #                                     indicator = measure, model = 'GBD2019', mean, lower, upper)]
    # 
    # }else{
    #   compare.dt.19 <- NULL
    # }
  }

  ###read in all runs
  if(compare){
    compare.dt <- list()
    for(run.x in unique(final_runs[,run])){
      if(file.exists(paste0('/ihme/hiv/epp_output/', unique(final_runs[run == run.x, gbd_year]), '/', run.x, '/summary_files/', loc, '.csv'))){

        sum_file <- fread(paste0('/ihme/hiv/epp_output/', unique(final_runs[run == run.x, gbd_year]), '/', run.x, '/summary_files/', loc, '.csv'))
      }else{
        sum_file <- fread(paste0('/ihme/hiv/epp_output/', unique(final_runs[run == run.x, gbd_year]), '/', run.x, '/summary_files/', loc_name, '.csv'))
        
      }
      sum_file <- sum_file[measure %in% c('Incidence', 'Prevalence', 'Deaths') & metric == c.metric,
                                .(age, sex, type = 'line', year, indicator = measure, model = unique(final_runs[run == run.x, run_name]), mean, lower, upper)]
      compare.dt <- rbind(compare.dt, sum_file)
    }
  }else{
    compare.dt <- NULL
  }


  ## we only have unaids all-ages results in rate space
    unaids.dt <- fread('/share/hiv/data/UNAIDS_extract/UNAIDS_results_2019.csv')
    unaids.dt <- unaids.dt[ihme_loc_id == loc & age_group_id  %in% c(22,24) & metric == c.metric,.(age_group_id, sex = 'both', 
                                                                                                   type = 'line',indicator = measure, 
                                                                                                   model = 'UNAIDS19', mean, lower, upper, year = year_id)]
    unaids.dt[age_group_id == 22, age := 'All']
    unaids.dt[age_group_id == 24, age := '15 to 49']
    unaids.dt[, age_group_id := NULL]
    unaids.dt[,mean:=as.numeric(mean)] ; unaids.dt[,lower := as.numeric(lower)] ; unaids.dt[,upper := as.numeric(upper)]
  
  ##prep for plotting
  both.dt <- rbind(data,  compare.dt, unaids.dt,use.names = T)
  both.dt[,model := factor(model)]
  both.dt[age == '15-49', age := '15 to 49']
  color.list <- c('green','purple','red','black', 'red')
  color.list <- color.list[1:length(unique(both.dt$model))]
  names(color.list) <- unique(both.dt$model)
  both.dt[,age := factor(age, levels=c('enn', 'lnn', 'x_388', 'x_389', 'pnn', '1', paste0( seq(5, 80, 5)), 'All', '15 to 49'))]
  both.dt <- both.dt[!age %in% c('x_388', 'x_389'),]

  

  for(c.indicator in c('Incidence', 'Prevalence', 'Deaths')){
    dir.create(paste0('/ihme/hiv/epp_output/', gbd_year_new, '/', run.name.new, '/age_specific_plots/', c.indicator, '/'),recursive = TRUE)
    pdf(paste0('/ihme/hiv/epp_output/', gbd_year_new, '/', run.name.new, '/age_specific_plots/', c.indicator, '/', loc, '.pdf'), width = 10, height = 6)
      
    for(c.sex in c('male', 'female', 'both')){
      plot.dt <- both.dt[sex == c.sex & indicator == c.indicator & year > 1979]
      
      gg <- ggplot()
      
      if(nrow(plot.dt[model == 'ANC Site']) > 0){
        gg <- gg + geom_point(data = plot.dt[model == 'ANC Site'], aes(x = year, y = mean, shape = 'ANC Site'), alpha = 0.2)
      }
      
      gg <- gg + geom_line(data = plot.dt[type == 'line'], aes(x = year, y = mean, color = model), alpha = 0.8) +
        # geom_ribbon(data = plot.dt[type == 'line'], aes(x = year, ymin = lower, ymax = upper,  fill = model), alpha = 0.2) +
        facet_wrap(~age, scales = 'free_y') +
        theme_bw() +
        scale_fill_manual(values=color.list) + scale_colour_manual(values=color.list)  +
        xlab("Year") + ylab("Mean") + ggtitle(paste0(loc.table[ihme_loc_id == loc, plot_name], ' EPPASM ', c.sex, ' ', c.indicator))
      
      if(nrow(plot.dt[model == 'Household Survey']) > 0){
        gg <- gg + geom_point(data = plot.dt[model == 'Household Survey'], aes(x = year, y = mean, shape = 'Household Survey'))
        gg <- gg + geom_errorbar(data = plot.dt[model == 'Household Survey'], aes(x = year, ymin = lower, ymax = upper))
      }
      if(nrow(plot.dt[model == 'VR']) > 0){
        gg <- gg + geom_point(data = plot.dt[model == 'VR'], aes(x = year, y = mean, shape = 'VR'), size = 0.75)
      }
      if(nrow(plot.dt[model == 'Case Report']) > 0){
        gg <- gg + geom_point(data = plot.dt[model == 'Case Report'], aes(x = year, y = mean, shape = 'Case Report'), size = 0.75)
      }
      gg <- gg + theme(axis.text.y = element_text(size = 12), axis.text.x = element_text(angle = 45, size = 12, vjust = 0.8), axis.title = element_text(size = 18), legend.text = element_text(size = 16),
                    legend.title = element_text(size = 18)) + ggtitle(paste0(loc.table[ihme_loc_id == loc, plot_name], ' ', c.indicator, ', ', c.sex))
      print(gg)
    }
    dev.off()
  }
}



plot_birthprev <- function(loc, gbdyear, run.name.new, compare.run){
  final_runs <- data.table('gbd_year' = c('gbd20',gbdyear, rep(gbdyear, length(compare.run))), 
                           'run' = c('200713_yuka', run.name.new, compare.run),
                           'run_name' = c('Final GBD20', 'Current Run', compare.run))
  
  
      plot.dt<- list()
      for(run.x in unique(final_runs[,run])){
        cur.dt <- fread(paste0('/share/hiv/epp_output/', unique(final_runs[run == run.x, gbd_year]), '/', run.x, '/compiled/', loc, '.csv'))
        cur.dt <- cur.dt[,.(birth_prev = sum(birth_prev), hiv_births = sum(hiv_births), total_births = sum(total_births)), by = c('year', 'run_num')]
        cur.dt <- cur.dt[,.(birth_prev = mean(birth_prev), hiv_births = mean(hiv_births), total_births = mean(total_births)), by = c('year')]
        cur.dt[, model := unique(final_runs[run == run.x, run_name])[1]]
        
        cur.dt[, perinatal_transmission_rate := birth_prev/hiv_births]
        cur.dt[,pregprev := hiv_births / total_births]
        cur.dt[,birth_prev_rate := birth_prev/total_births]
        cur.dt <- melt(cur.dt, id.vars = c('year', 'model'))
        plot.dt <- rbind(plot.dt, cur.dt)
      }
  
  ###load in pmtct data
  pmtct <- list()
  find.files <- list()
  for(c.year in c('covid','UNAIDS_2019', 'UNAIDS_2018', 'UNAIDS_2017', 'UNAIDS_2016', 'UNAIDS_2015', '140520')){
    find.files[[c.year]] <- file.exists( paste0('/ihme/hiv/data/UNAIDS_extrapolated/GBD20/PMTCT/', c.year,'/', loc, '_PMTCT_ART_cov.csv'))
  }
  find.files <- data.table(names = names(find.files), exists = unlist(find.files))
  for(year.x in find.files[exists == 'TRUE', names]){
    dt <- paste0('/ihme/hiv/data/UNAIDS_extrapolated/GBD20/PMTCT/', year.x,'/', loc, '_PMTCT_ART_cov.csv') %>% fread
    dt <- melt(dt, id.vars= 'year')
    dt[,model:= year.x]
    pmtct <- rbind(dt, pmtct)
  }
  
  
  ### prep plot
  plot.dt[variable == 'total_births', variable := 'total births']
  plot.dt[variable == 'perinatal_transmission_rate', variable := 'perinatal transmission rate']
  plot.dt[variable == 'hiv_births', variable := 'births to HIV+ women']
  plot.dt[variable == 'birth_prev', variable := 'prevalence at birth (count)']
  plot.dt[variable == 'birth_prev_rate', variable := 'prevalence at birth (rate)']
  plot.dt[variable == 'pregprev', variable := 'pregnant women prevalence (rate)']
  
  plot.dt[, variable_f := factor(variable, levels = c('total births', 'pregnant women prevalence (rate)', 'births to HIV+ women',
                                                      'perinatal transmission rate', 'prevalence at birth (rate)', 'prevalence at birth (count)'))]
  plot.dt[,model := as.factor(model)]
  color.list <- c('blue', 'red', 'green')
  names(color.list) <- unique(plot.dt$model)
  
  if(!dir.exists(paste0('/ihme/hiv/epp_output/', gbdyear, '/', run.name.new, '/paeds_plots/'))){dir.create(paste0('/ihme/hiv/epp_output/', gbdyear, '/', run.name.new, '/paeds_plots/'), recursive= TRUE)}
 
    pdf(paste0('/ihme/hiv/epp_output/', gbdyear, '/', run.name.new, '/paeds_plots/', loc, '.pdf'), width = 10, height = 6)
    
  
  gg <- ggplot()
  gg <- gg + geom_line(data = plot.dt, aes(x = year, y = value, color = model))
  gg <- gg + facet_wrap(~variable_f, scales = 'free')
  gg <- gg + xlab("Year") + ylab("Mean") + ggtitle(paste0(loc.table[ihme_loc_id == loc, plot_name], ' EPPASM Births Results'))
  gg <- gg + theme_bw()
  print(gg)
  
  
  pmtct.gg <- ggplot() + geom_line(data = pmtct, aes(year, value, col = (model))) + facet_wrap(~variable, scales= 'free')
  pmtct.gg <- pmtct.gg + ggtitle(loc) + theme_bw()
  print(pmtct.gg)
  
  dev.off()
}

plot_age_sex_incrr <- function(loc, run.name){
  sexincrr.pr.mean <- log(1.38)
  ## Informative priors based on estimates for 11 countries with 3+ surveys, this is all relative to 25 to 29
  ageincrr.pr.mean <- c(-1.4, -0.28, 0.3, 0.3, -0.3, -0.6, -0.2, 0.05, -0.4, -0.45, -0.6, -0.7)
  # ageincrr.pr.mean <- c(5, -0.28, 0.3, 0.3, -0.3, -0.6, -0.2, 0.05, -0.4, -0.45, -0.6, -0.7)
  
  if(grepl('IND', loc)){
    ageincrr.pr.sd <- c(0.5, 0.4, 0.23, 0.3, 0.3, 0.3, 0.3, 0.3, 0.2, 0.2, 0.2, 0.2)
    
  }else{
    ageincrr.pr.sd <- c(0.5, 0.4, 0.23, 0.3, 0.3, 0.3, 0.3, 0.3, 0.2, 0.2, 0.2, 0.2)
    
  }
  incrr_trend_mean <- c(0.0, 0.035, -0.02, -0.09, -0.016, -0.06)
  incrr_trend_sd <- c(0.07, 0.07, 0.1, 0.1, 0.08, 0.08)
  years <- 1970:2022
  ## Incidence rate ratios for age 50 plus, relative to 15-49
  incrr_50plus_logdiff <- cbind(male   = log(0.493510) - log(c(0.358980, 0.282400, 0.259240, 0.264920, 0.254790, 0.164140, 0.000000)),
                                female = log(0.440260) - log(c(0.336720, 0.239470, 0.167890, 0.146590, 0.171350, 0.000000, 0.000000)))
  
  theta.files <- list.files(paste0('/ihme/hiv/epp_output/gbd20/', run.name, "/", loc))

  theta.files <- theta.files[grepl('theta', theta.files)]
  theta.dt <- rbindlist(lapply(theta.files, function(ff){
    draw.dt <- fread(paste0('/ihme/hiv/epp_output/gbd20/', run.name, "/", loc, '/', ff))
    draw.dt[,i := .I]
    draw.dt[, draw := paste0('draw_', gsub('.csv', '', gsub('theta_', '', ff)))]
    return(draw.dt)
  }))
  theta.dt <- theta.dt[,.(theta = mean(theta), upper = quantile(theta, 0.975), lower = quantile(theta, 0.025)), by = 'i']
  theta <- theta.dt[,theta]
  theta_incrr <- theta[(length(theta) - (NPARAM_REGINCRR) + 1):length(theta)]
  # theta_incrr <- theta[(length(theta) - (NPARAM_RW2 + NPARAM_LININCRR) + 1):length(theta)]
  incrr_sex <- rep(0, length(years))
  incrr_sex[] <- exp(theta_incrr[1])
  
  # Still don't totally understand this penalty - on difference in difference b/w age groups?
  sigma_agepen <- 0.4
  
  logincrr_age <- array(0, c(14, 2))
  # logincrr_age[c(1:2, 4:7), ] <- theta_incrr[2:13]
  logincrr_age[c(1:2, 4:7), ] <- theta[11:16]
  
  logincrr_age[8:14, ] <- sweep(-incrr_50plus_logdiff, 2,
                                logincrr_age[7, ], "+")
  
  ## Smooth 5-year age group IRRs to 1-year IRRs
  incrr_age <- beers_Amat %*% exp(logincrr_age)
  incrr_age[incrr_age < 0] <- 0
  
  incrr_age <- array(incrr_age, c(dim(incrr_age), length(years)))
  
  par <- theta[17:23]
 # par <- theta_incrr[NPARAM_RW2+1:NPARAM_LININCRR]
  
  logincrr_trend <- par
  sexadjust <- approx(c(2002, 2007, 2012), c(-5, 0, 5)*c(par[1], 0, par[2]), years, rule=2)$y
  incrr_sex <- incrr_sex * exp(sexadjust)
  incrr_sex.dt <- data.table(value = incrr_sex, year = years, type = 'posterior')
  
  ## adjustment to age IRRs among 15-24
  # m15to24_adjust <- approx(c(2002, 2007, 2012), c(-5, 0, 5)*c(par[3], 0, par[4]), years, rule=2)$y
  # f15to24_adjust <- approx(c(2002, 2007, 2012), c(-5, 0, 5)*c(par[5], 0, par[6]), years, rule=2)$y
  m15to24_adjust <- approx(c(2002, 2007, 2012), c(-5, 0, 5)*c(par[3], 0, par[4]), years, rule=2)$y
  f15to24_adjust <- approx(c(2002, 2007, 2012), c(-5, 0, 5)*c(par[5], 0, par[6]), years, rule=2)$y
  incrr_age[1:10,,] <- sweep(incrr_age[1:10,,,drop=FALSE], 2:3, exp(rbind(m15to24_adjust, f15to24_adjust)), "*")  
  
  # prep log incrr and adjustments for plotting
  adj.dt <- data.table(year = years, sexadjust = sexadjust, m15to24 = m15to24_adjust, f15to24 = f15to24_adjust)
  adj.dt <- melt(adj.dt, id.vars= 'year')
  adj.dt[,type := 'posterior']
  logincrr_age.dt <- data.table(male = logincrr_age[1:7,1], female = logincrr_age[1:7, 2])
  logincrr_age.dt[, age := seq(15, 45, 5)]
  logincrr_age.dt <- melt(logincrr_age.dt, id.vars = 'age')
  logincrr_age.dt[,type := 'posterior']
  loginc.prior <- array(0, c(7, 2))
  loginc.prior[c(1:2, 4:7), ] <- ageincrr.pr.mean
  loginc.prior <- data.table(male = loginc.prior[1:7,1], female = loginc.prior[1:7, 2])
  loginc.prior[, age := seq(15, 45, 5)]
  loginc.prior <- melt(loginc.prior, id.vars = 'age')
  loginc.prior[,type := 'prior']  
  
  prior.upper <- array(0, c(7, 2))
  prior.upper[c(1:2, 4:7), ] <- ageincrr.pr.mean + (1.96 * ageincrr.pr.sd)
  prior.upper <- data.table(male = prior.upper[1:7,1], female = prior.upper[1:7, 2])
  prior.upper[, age := seq(15, 45, 5)]
  prior.upper <- melt(prior.upper, id.vars = 'age')
  prior.lower <- array(0, c(7, 2))
  prior.lower[c(1:2, 4:7), ] <- ageincrr.pr.mean - (1.96 * ageincrr.pr.sd)
  prior.lower <- data.table(male = prior.lower[1:7,1], female = prior.lower[1:7, 2])
  prior.lower[, age := seq(15, 45, 5)]
  prior.lower <- melt(prior.lower, id.vars = 'age')
  loginc.prior$upper <- prior.upper$value
  loginc.prior$lower <- prior.lower$value
  logincrr.dt <- rbind(logincrr_age.dt, loginc.prior, fill = T)
  
  ## priors
  sexadjust <- approx(c(2002, 2007, 2012), c(-5, 0, 5)*c(incrr_trend_mean[1], 0, incrr_trend_mean[2]), years, rule=2)$y
  m15to24_adjust <- approx(c(2002, 2007, 2012), c(-5, 0, 5)*c(incrr_trend_mean[3], 0, incrr_trend_mean[4]), years, rule=2)$y
  f15to24_adjust <- approx(c(2002, 2007, 2012), c(-5, 0, 5)*c(incrr_trend_mean[5], 0, incrr_trend_mean[6]), years, rule=2)$y
  sexadjust.upper <- approx(c(2002, 2007, 2012), c(-5, 0, 5)*c(incrr_trend_mean[1] + 1.96 * incrr_trend_sd[1], 0, incrr_trend_mean[2]+ 1.96 * incrr_trend_sd[2]), years, rule=2)$y
  m15to24.upper <- approx(c(2002, 2007, 2012), c(-5, 0, 5)*c(incrr_trend_mean[3]+ 1.96 * incrr_trend_sd[3], 0, incrr_trend_mean[4]+ 1.96 * incrr_trend_sd[4]), years, rule=2)$y
  f15to24.upper <- approx(c(2002, 2007, 2012), c(-5, 0, 5)*c(incrr_trend_mean[5]+ 1.96 * incrr_trend_sd[5], 0, incrr_trend_mean[6]+ 1.96 * incrr_trend_sd[6]), years, rule=2)$y 
  sexadjust.lower <- approx(c(2002, 2007, 2012), c(-5, 0, 5)*c(incrr_trend_mean[1] - 1.96 * incrr_trend_sd[1], 0, incrr_trend_mean[2] - 1.96 * incrr_trend_sd[2]), years, rule=2)$y
  m15to24.lower <- approx(c(2002, 2007, 2012), c(-5, 0, 5)*c(incrr_trend_mean[3] - 1.96 * incrr_trend_sd[3], 0, incrr_trend_mean[4] - 1.96 * incrr_trend_sd[4]), years, rule=2)$y
  f15to24.lower <- approx(c(2002, 2007, 2012), c(-5, 0, 5)*c(incrr_trend_mean[5] - 1.96 * incrr_trend_sd[5], 0, incrr_trend_mean[6] - 1.96 * incrr_trend_sd[6]), years, rule=2)$y
  prior.dt <- data.table(year = years, sexadjust = sexadjust, m15to24 = m15to24_adjust, f15to24 = f15to24_adjust)
  prior.dt <- melt(prior.dt, id.vars= 'year')
  prior.dt[,type := 'prior']
  prior.dt[variable == 'sexadjust', upper := sexadjust.upper]
  prior.dt[variable == 'sexadjust', lower := sexadjust.lower]
  prior.dt[variable == 'f15to24', upper := f15to24.upper]
  prior.dt[variable == 'f15to24', lower := f15to24.lower]
  prior.dt[variable == 'm15to24', upper := m15to24.upper]
  prior.dt[variable == 'm15to24', lower := m15to24.lower]
  adj.dt <- rbind(prior.dt, adj.dt, use.names = T, fill = T)
  incrr_sex.prior <- data.table(value = (exp(sexadjust) * 1.38), year = years, type = 'prior')
  incrr_sex.dt <- rbind(incrr_sex.dt, incrr_sex.prior, use.names = T)
  
  incrr_age.dt <- rbindlist(lapply(1:length(years), function(i){
    data.table(male = incrr_age[,1,i], female = incrr_age[,2,i], year = years[i], age = 15:80)
  }))
  incrr_age.dt <- incrr_age.dt[year %in% seq(2000, 2015, 5)]
  incrr_age.dt <- melt(incrr_age.dt, id.vars = c('year', 'age'))
  
  
  pdf(paste0('/ihme/hiv/epp_output/gbd20/', run.name, "/", loc, '/ageincrr.pdf'), width = 10, height = 6)  
  gg <- ggplot(logincrr.dt) +
    geom_line(data = logincrr.dt, aes(x = age, y= value, linetype = factor(type))) +
    geom_ribbon(data = logincrr.dt[type == 'prior'], aes(x = age, ymax = upper, ymin = lower, alpha = 0.2)) +
    facet_wrap(~variable, scales = 'free') +
    ggtitle(paste0(loc.table[ihme_loc_id == loc, plot_name], ' log INCRR priors and posteriors')) +
    theme_bw()
  print(gg)
  
  gg <- ggplot(adj.dt) + 
    geom_line(data = adj.dt, aes(x = year, y = value, linetype = factor(type))) + 
    geom_line(data = adj.dt[type == 'prior'], aes(x = year, y = upper, linetype = factor(type))) +
    geom_line(data = adj.dt[type == 'prior'], aes(x = year, y = lower, linetype = factor(type))) +
    facet_wrap(~variable, scales = 'free') +
    ggtitle(paste0(loc.table[ihme_loc_id == loc, plot_name], ' Age/Sex INCRR log adjustments over time Priors and Posteriors')) +
    theme_bw()
  print(gg)
  
  gg <- ggplot(incrr_age.dt) + 
    geom_line(aes(x = age, y = value)) +
    facet_wrap(~ variable + year, scales = 'free', nrow = 2) +
    ggtitle(paste0(loc.table[ihme_loc_id == loc, plot_name], ' Age INCRR')) +
    theme_bw()
  print(gg)  
  
  gg <- ggplot(incrr_sex.dt) + 
    geom_line(aes(x = year, y = value, linetype = factor(type))) +
    ggtitle(paste0(loc.table[ihme_loc_id == loc, plot_name], ' Sex INCRR')) +
    theme_bw()
  print(gg)  
  
  dev.off()
}


summarize_theta <- function(loc, run.name){
  theta.files <- list.files(paste0('/ihme/hiv/epp_output/gbd20/', run.name, "/", loc))
  theta.files <- theta.files[grepl('theta', theta.files)]
  theta.dt <- rbindlist(lapply(theta.files, function(ff){
    draw.dt <- fread(paste0('/ihme/hiv/epp_output/gbd20/', run.name, "/", loc, '/', ff))
    draw.dt[,i := .I]
    draw.dt[, draw := paste0('draw_', gsub('.csv', '', gsub('theta_', '', ff)))]
    return(draw.dt)
  }))
  theta.dt <- theta.dt[,.(theta = mean(theta), upper = quantile(theta, 0.975), lower = quantile(theta, 0.025)), by = 'i']
  
  return(theta.dt)
}
