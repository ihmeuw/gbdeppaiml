## ---------------------------
## Script name: 
## Purpose of script:
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

source(paste0(ifelse(windows, "H:", paste0("/ihme/homes/", user)), "/gbdeppaiml/gbd/00_req_packages.R"))

plot_15to49_new_compare <- function(loc="AGO",  
                        new.run = c('201007_socialdets_sens',
                                    '201013_socialdets_sens'),
                        names.list = c('rhybrid', 'rlogistic'),
                        paediatric =TRUE,  gbdyear = "gbd20"){
  compare.locs <- fread('/ihme/hiv/epp_input/gbd20/201007_socialdets_sens/array_table.csv')
  compare.run <- unique(compare.locs[ihme_loc_id == loc,loc_scalar])
  names(names.list) <- new.run


  data <- fread(paste0('/share/hiv/epp_input/', gbdyear, '/', new.run[2], '/fit_data/', loc, '.csv'))
  data <- data[metric == 'Rate']
  anc <- data[model == 'ANC Site']
  anc[,sex := NULL]
  
  if(!any(grep(loc, list.files('/share/hiv/epp_output/gbd20/201007_socialdets_sens/compiled/')))){
    return('no compiled files')
  }else{
    cur.dt <- fread(paste0('/share/hiv/epp_output/gbd20/201007_socialdets_sens/compiled/',loc,'_1.csv'))
    
  }
  ## post stratify age-specific data using Spectrum population
  if(nrow(data[age != '15-49']) > 0){
    pop.dt <- copy(cur.dt)
    pop.dt <- pop.dt[age %in% 15:59]
    
    male.pop <- pop.dt[sex == 'male' & run_num == 1, .(pop, year, age)]
    male.pop[,age := paste0(age - age%%5)]
    male.pop <- male.pop[,total := sum(pop), by = c('year', 'age')]
    # male.pop <- cbind(unique(male.pop[,year]), unique(male.pop[,total]))
    # colnames(male.pop) <- c('year', 'pop')
    # male.pop <- data.table(male.pop)
    male.pop[,sex := 'male']
    male.pop[,pop := NULL]
    
    female.pop <- pop.dt[sex == 'female' & run_num == 1, .(pop, year, age)]
    female.pop[,age := paste0(age - age%%5)]
    female.pop <- female.pop[,total := sum(pop), by = c('year', 'age')]
    # female.pop <- cbind(unique(female.pop[,year]), unique(female.pop[,total]))
    # colnames(female.pop) <- c('year', 'pop')
    # female.pop <- data.table(female.pop)
    female.pop[,sex := 'female']
    female.pop[,pop := NULL]
    
    
    aggre_pop <- rbind(unique(male.pop), unique(female.pop))
    age_agg_pop <- copy(aggre_pop)
    age_agg_pop <- age_agg_pop [,total := sum(total), by = c('year', 'sex')]
    age_agg_pop[,age := '15-49']
    aggre_pop <- rbind(aggre_pop, unique(age_agg_pop))
    setnames(aggre_pop, 'total', 'pop')
    
    # pop.dt[,age := paste0(age - age%%5)]
    # pop.dt <- pop.dt[,.(pop = sum(pop)), by = c('age', 'sex', 'year', 'run_num')]
    # pop.dt <- pop.dt[,.(pop = mean(pop)), by = c('age', 'sex', 'year')]
    # pop.dt <- rbind(pop.dt, aggre_pop)
    pop.dt <- aggre_pop
    # pop.dt <- pop.dt[,.(pop = sum(pop)), by = c('age', 'sex', 'year')]
    data.pre.agg <- data[age %in%   c('15-49', '15-64') & model == 'Household Survey']
    data.pre.agg[,pop := NA]
    data.agg <- merge(pop.dt, data[model == 'Household Survey'], by = c( 'age', 'sex','year'), fill = T)
    data.agg <- rbind(data.agg[,.(year, model, indicator, mean, pop)], data.pre.agg[,.(year, model, indicator, mean, pop)])
    data.agg <- data.agg[,.(mean = weighted.mean(x = mean, w = pop)), by =c('year', 'model', 'indicator') ]
    
    ##TODO - what to do with upper and lower here?
    
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
  data <- rbind(data, anc[,age := NULL], fill = T)
  data[,type := model]
  data.new <- list()
  for(run in compare.run){
    data.new[[run]] <- data.table(copy(data))[,model := run]
  }
  data <- rbindlist(data.new)
  
  meas.list <- c('Incidence', 'Prevalence', 'Deaths')
  
  
  dt <- list()
   for(run.name in new.run){ 
     dt_locs <- list()
     for(run in compare.run){
      if(!file.exists(paste0('/share/hiv/epp_output/gbd20/',run.name,'/compiled/', run, '.csv'))){
        next
      }
        compare.dt <- fread(paste0('/share/hiv/epp_output/gbd20/',run.name,'/compiled/', run, '.csv'))
        loc_iter <- run
        compare.dt <- get_summary(compare.dt, loc, run.name.old = run.name, run.name.new = run.name, paediatric, old.splits = F, test_run = NULL, loc_name = loc_iter)
        compare.dt <- compare.dt[age_group_id == 24 & sex == 'both' & measure %in% meas.list & metric == "Rate",.(type = 'line', year, indicator = measure, model = run, mean, lower, upper, run = names.list[run.name])]
        dt_locs <- rbind(compare.dt, dt_locs) 
      }
    dt <- rbind(dt, dt_locs)
    }
  
  plot.dt <- rbind(data,dt,use.names = T, fill = T)
  
  plot.dt[,model := factor(model)]
  if(any(colnames(plot.dt) == 'x')){
    plot.dt[,x:= NULL]
  }
  color.list <- c('blue', 'red', 'green','purple','orange','black', 'darkgreen')
  plot.dt[,model := factor(model)]

  
  dir.create(paste0('/ihme/hiv/epp_output/', gbdyear, '/', new.run[2], '/15to49_plots/'), recursive = TRUE)
  pdf(paste0('/ihme/hiv/epp_output/', gbdyear, '/', new.run[2], '/15to49_plots/', loc, '.pdf'), width = 10, height = 6)
  
  gg <- ggplot()
  if(nrow(plot.dt[type == 'ANC Site']) > 0){
    gg <- gg + geom_point(data = plot.dt[type == 'ANC Site'], aes(x = year, y = mean, shape = 'ANC Site'), alpha = 0.2)
  }
  gg <- gg + geom_line(data = plot.dt[type == 'line'], aes(x = year, y = mean, color = run)) +
    facet_wrap(model~indicator, scales = 'free_y') +
    theme_bw() +
    # scale_fill_manual(values=color.list) + scale_colour_manual(values=color.list)  +
    xlab("Year") + ylab("Mean") + ggtitle(paste0(loc.table[ihme_loc_id == loc, plot_name], ' EPPASM Results'))
  if(nrow(plot.dt[type == 'Household Survey']) > 0){
    gg <- gg + geom_point(data = plot.dt[type == 'Household Survey'], aes(x = year, y = mean, shape = 'Household Survey'),size=3)
    # gg <- gg + geom_errorbar(data = plot.dt[model == 'Household Survey'], aes(x = year, ymin = lower, ymax = upper))
  }
  if(nrow(plot.dt[model == 'HH Survey Agg']) > 0){
    gg <- gg + geom_point(data = plot.dt[model == 'HH Survey Agg'], aes(x = year, y = mean, shape = 'HH Survey Old'),size=3)
    #gg <- gg + geom_errorbar(data = plot.dt[model == 'HH Survey Old'], aes(x = year, ymin = lower, ymax = upper),color="darkgrey")
  }
  
  if(nrow(plot.dt[model == 'VR']) > 0){
    gg <- gg + geom_point(data = plot.dt[model == 'VR'], aes(x = year, y = mean, shape = 'VR'), size = 0.75)
  }
  
  print(gg)
  dev.off()
  print(paste0(loc, ' is done'))
  
}

compare.locs <- fread('/ihme/hiv/epp_input/gbd20/201007_socialdets_sens/array_table.csv')
locs <- unique(compare.locs[,ihme_loc_id])
library(parallel)
# lapply(locs, plot_15to49_new)
# for(loc in locs[2:length(locs)]){
#   plot_15to49_new(loc)
# }

plot_15to49_new <- function(loc="AGO",  
                                    new.run = '201007_socialdets_sens',
                                    names.list = 'new method',
                                    paediatric =TRUE,  gbdyear = "gbd20"){
  compare.locs <- fread('/ihme/hiv/epp_input/gbd20/201007_socialdets_sens/array_table.csv')
  compare.run <- unique(compare.locs[ihme_loc_id == loc,loc_scalar])
  names(names.list) <- new.run
  
  
  data <- fread(paste0('/share/hiv/epp_input/', gbdyear, '/', new.run[1], '/fit_data/', loc, '.csv'))
  data <- data[metric == 'Rate']
  anc <- data[model == 'ANC Site']
  anc[,sex := NULL]
  
  if(!any(grep(loc, list.files('/share/hiv/epp_output/gbd20/201007_socialdets_sens/compiled/')))){
    return('no compiled files')
  }else{
    cur.dt <- fread(paste0('/share/hiv/epp_output/gbd20/201007_socialdets_sens/compiled/',loc,'_1.csv'))
    
  }
  ## post stratify age-specific data using Spectrum population
  if(nrow(data[age != '15-49']) > 0){
    pop.dt <- copy(cur.dt)
    pop.dt <- pop.dt[age %in% 15:59]
    
    male.pop <- pop.dt[sex == 'male' & run_num == 1, .(pop, year, age)]
    male.pop[,age := paste0(age - age%%5)]
    male.pop <- male.pop[,total := sum(pop), by = c('year', 'age')]
    # male.pop <- cbind(unique(male.pop[,year]), unique(male.pop[,total]))
    # colnames(male.pop) <- c('year', 'pop')
    # male.pop <- data.table(male.pop)
    male.pop[,sex := 'male']
    male.pop[,pop := NULL]
    
    female.pop <- pop.dt[sex == 'female' & run_num == 1, .(pop, year, age)]
    female.pop[,age := paste0(age - age%%5)]
    female.pop <- female.pop[,total := sum(pop), by = c('year', 'age')]
    # female.pop <- cbind(unique(female.pop[,year]), unique(female.pop[,total]))
    # colnames(female.pop) <- c('year', 'pop')
    # female.pop <- data.table(female.pop)
    female.pop[,sex := 'female']
    female.pop[,pop := NULL]
    
    
    aggre_pop <- rbind(unique(male.pop), unique(female.pop))
    age_agg_pop <- copy(aggre_pop)
    age_agg_pop <- age_agg_pop [,total := sum(total), by = c('year', 'sex')]
    age_agg_pop[,age := '15-49']
    aggre_pop <- rbind(aggre_pop, unique(age_agg_pop))
    setnames(aggre_pop, 'total', 'pop')
    
    # pop.dt[,age := paste0(age - age%%5)]
    # pop.dt <- pop.dt[,.(pop = sum(pop)), by = c('age', 'sex', 'year', 'run_num')]
    # pop.dt <- pop.dt[,.(pop = mean(pop)), by = c('age', 'sex', 'year')]
    # pop.dt <- rbind(pop.dt, aggre_pop)
    pop.dt <- aggre_pop
    # pop.dt <- pop.dt[,.(pop = sum(pop)), by = c('age', 'sex', 'year')]
    data.pre.agg <- data[age %in%   c('15-49', '15-64') & model == 'Household Survey']
    data.pre.agg[,pop := NA]
    data.agg <- merge(pop.dt, data[model == 'Household Survey'], by = c( 'age', 'sex','year'), fill = T)
    data.agg <- rbind(data.agg[,.(year, model, indicator, mean, pop)], data.pre.agg[,.(year, model, indicator, mean, pop)])
    data.agg <- data.agg[,.(mean = weighted.mean(x = mean, w = pop)), by =c('year', 'model', 'indicator') ]
    
    ##TODO - what to do with upper and lower here?
    
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
  data <- rbind(data, anc[,age := NULL], fill = T)
  data[,type := model]
  data.new <- list()
  for(run in compare.run){
    data.new[[run]] <- data.table(copy(data))[,model := run]
  }
  data <- rbindlist(data.new)
  
  meas.list <- c('Incidence', 'Prevalence', 'Deaths')
  
  
  dt <- list()
  for(run.name in new.run){ 
    dt_locs <- list()
    for(run in compare.run){
      if(!file.exists(paste0('/share/hiv/epp_output/gbd20/',run.name,'/compiled/', run, '.csv'))){
        next
      }
      compare.dt <- fread(paste0('/share/hiv/epp_output/gbd20/',run.name,'/compiled/', run, '.csv'))
      loc_iter <- run
      compare.dt <- get_summary(compare.dt, loc, run.name.old = run.name, run.name.new = run.name, paediatric, old.splits = F, test_run = NULL, loc_name = loc_iter)
      compare.dt <- compare.dt[age_group_id == 24 & sex == 'both' & measure %in% meas.list & metric == "Rate",.(type = 'line', year, indicator = measure, model = run, mean, lower, upper, run = names.list[run.name])]
      dt_locs <- rbind(compare.dt, dt_locs) 
    }
    dt <- rbind(dt, dt_locs)
  }
  
  plot.dt <- rbind(data,dt,use.names = T, fill = T)
  
  plot.dt[,model := factor(model)]
  if(any(colnames(plot.dt) == 'x')){
    plot.dt[,x:= NULL]
  }
  color.list <- c('blue', 'red', 'green','purple','orange','black', 'darkgreen')
  plot.dt[,model := factor(model)]
  
  
  dir.create(paste0('/ihme/hiv/epp_output/', gbdyear, '/', new.run[1], '/15to49_plots/'), recursive = TRUE)
  pdf(paste0('/ihme/hiv/epp_output/', gbdyear, '/', new.run[1], '/15to49_plots/', loc, '.pdf'), width = 10, height = 6)
  
  gg <- ggplot()
  if(nrow(plot.dt[type == 'ANC Site']) > 0){
    gg <- gg + geom_point(data = plot.dt[type == 'ANC Site'], aes(x = year, y = mean, shape = 'ANC Site'), alpha = 0.2)
  }
  gg <- gg + geom_line(data = plot.dt[type == 'line'], aes(x = year, y = mean, color = model)) +
    facet_wrap(~indicator, scales = 'free_y') +
    theme_bw() +
    # scale_fill_manual(values=color.list) + scale_colour_manual(values=color.list)  +
    xlab("Year") + ylab("Mean") + ggtitle(paste0(loc.table[ihme_loc_id == loc, plot_name], ' EPPASM Results'))
  if(nrow(plot.dt[type == 'Household Survey']) > 0){
    gg <- gg + geom_point(data = plot.dt[type == 'Household Survey'], aes(x = year, y = mean, shape = 'Household Survey'),size=3)
    # gg <- gg + geom_errorbar(data = plot.dt[model == 'Household Survey'], aes(x = year, ymin = lower, ymax = upper))
  }
  if(nrow(plot.dt[model == 'HH Survey Agg']) > 0){
    gg <- gg + geom_point(data = plot.dt[model == 'HH Survey Agg'], aes(x = year, y = mean, shape = 'HH Survey Old'),size=3)
    #gg <- gg + geom_errorbar(data = plot.dt[model == 'HH Survey Old'], aes(x = year, ymin = lower, ymax = upper),color="darkgrey")
  }
  
  if(nrow(plot.dt[model == 'VR']) > 0){
    gg <- gg + geom_point(data = plot.dt[model == 'VR'], aes(x = year, y = mean, shape = 'VR'), size = 0.75)
  }
  
  print(gg)
  dev.off()
  print(paste0(loc, ' is done'))
  
}

compare.locs <- fread('/ihme/hiv/epp_input/gbd20/201007_socialdets_sens/array_table.csv')
locs <- unique(compare.locs[,ihme_loc_id])
library(parallel)
mclapply(locs, plot_15to49_new, mc.cores = 5)
