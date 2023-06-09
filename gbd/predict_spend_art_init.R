rm(list=ls())
gc()
user <- Sys.info()[["user"]]
jpath <- ifelse(Sys.info()[1]=="Windows", "J:/", "/home/j/")
hpath <- ifelse(Sys.info()[1]=="Windows", "H:/", paste0("/homes/", user, "/"))
list.of.packages <- c("data.table","ggplot2","parallel","gtools","haven")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)
jpath <- ifelse(Sys.info()[1]=="Windows", "J:/", "/home/j/")
cores <- 1
source(paste0(jpath,"Project/forecasting/hiv/ref/multi_plot.R"))
library(mortdb, lib = '/mnt/team/mortality/pub/shared/r/4')
library(parallel)
library(lme4) 

run.name <- '230217_condor'
gbdyear = 'gbd20'
out.dir = paste0("/share/hiv/spectrum_plots/",run.name)
loc.table <- data.table(get_locations(hiv_metadata = T))
epp.list <- sort(loc.table[epp == 1 & grepl('1', group), ihme_loc_id])
loc.list <- epp.list
loc.list = loc.list[!grepl('IND', loc.list) & !grepl('KEN', loc.list)]
n.draws  = 50

art.init = data.table()
for(loc in loc.list){
  print(loc)
  artinit = rbindlist(lapply(1:n.draws, function(j){
    return(tryCatch(fread(paste0('/ihme/hiv/epp_output/', gbdyear, '/', run.name, '/artinit_onart/', loc,'/', j, '.csv')),
                    error=function(e) NULL))
  })
  )
  artinit[,V1 := NULL]
  if(file.exists(paste0('/share/hiv/data/PJNZ_prepped/2019/', loc, '.rds'))) {
    dt <- readRDS(paste0('/share/hiv/data/PJNZ_prepped/2019/', loc, '.rds'))
    y = 2019
    
  } else if(file.exists(paste0('/share/hiv/data/PJNZ_prepped/2018/', loc, '.rds'))) {
    dt <- readRDS(paste0('/share/hiv/data/PJNZ_prepped/2018/', loc, '.rds'))
    y = 2018
    
  } else if(file.exists(paste0('/share/hiv/data/PJNZ_prepped/2015/', loc, '.rds'))){
    dt <- readRDS(paste0('/share/hiv/data/PJNZ_prepped/2015/', loc, '.rds'))
    y = 2015
    
  } else if(file.exists(paste0('/share/hiv/data/PJNZ_EPPASM_prepped_subpop/', loc, '.rds'))) {
    dt <- readRDS(paste0('/share/hiv/data/PJNZ_EPPASM_prepped_subpop/', loc, '.rds'))
    y = 2013
    
  } else if(file.exists(paste0('/share/hiv/data/PJNZ_EPPASM_prepped/', loc, '.rds'))) {
    dt <- readRDS(paste0('/share/hiv/data/PJNZ_EPPASM_prepped/', loc, '.rds'))
    y = 2013
  } 
  spec = attr(dt, 'specfp')
  art = spec$art15plus_isperc
  for(i in 42:dim(art)[2]){
    if(!art[1,i-1] & art[1,i]){
      y = as.numeric(colnames(art)[i])
      break
    }
  }
  artinit[, perc_switch := y]
  artinit[,ihme_loc_id := loc]
  art.init = rbind(art.init, artinit)
}


# write.csv(art.init,paste0('/ihme/hiv/epp_output/', gbdyear, '/', run.name, '/artinit_onart/compiled_locations.csv'), row.names = F)
art.init = fread(paste0('/ihme/hiv/epp_output/', gbdyear, '/', run.name, '/artinit_onart/compiled_locations.csv'))

# Sum ART initiations over the year, find the mean # on ART over the year
year.dt = art.init[,.(art_init = sum(art_init), on_art = mean(on_art)), by = c('year', 'sex', 'draw', 'ihme_loc_id', 'perc_switch')]

# Read in spend (per capita)
drivers = fread(paste0('/home/j/Project/forecasting/hiv/data/20230202/forecasted_inputs_reference.csv'))
drivers[,V1 := NULL]    
drivers = drivers[variable == "Spend"]
setnames(drivers, c('year_id', 'pred_mean'), c('year', 'spend'))
drivers[,variable := NULL]
## pull in population to make it total spend
pop.dt = data.table()
for(loc in unique(art.init$ihme_loc_id)){
  print(loc)
  pop.loc = fread(paste0('/mnt/share/hiv/epp_input/', gbdyear, '/', run.name, '/population_single_age/', loc, '.csv'))
  pop.loc = pop.loc[,.(population = sum(population)), by = 'year_id']
  setnames(pop.loc, 'year_id', 'year')
  pop.loc[,ihme_loc_id := loc]
  pop.dt = rbind(pop.dt, pop.loc)
}
drivers = merge(drivers, pop.dt, by = c('ihme_loc_id', 'year'))
drivers[,spend := spend * population]
year.dt = merge(year.dt, drivers, by = c('ihme_loc_id', 'year'))

# Subset to years of interest
# Do we want to use the mean across draws?
fit.dt = year.dt[year %in% 2005:2016]
#sum across both sexes
fit.dt = fit.dt[,.(art_init = sum(art_init), on_art = sum(on_art)), by = c('year', 'ihme_loc_id', 'draw', 'spend')]
fit.dt = fit.dt[,.(art_init = mean(art_init), on_art = mean(on_art)), by = c('year', 'ihme_loc_id', 'spend')]
fit.dt = merge(fit.dt, loc.table[,.(ihme_loc_id, region_name)], by = 'ihme_loc_id')
fit1 = lm(spend ~ 1 + on_art + art_init + on_art*region_name, data= fit.dt)
int = summary(fit1)$coef[1]
b1 = summary(fit1)$coef[2]
b2 = summary(fit1)$coef[3]
pred.dt = year.dt[,.(art_init = sum(art_init), on_art = sum(on_art)), by = c('year', 'ihme_loc_id', 'draw', 'spend', 'perc_switch') ]
pred.dt[, pred_spend := int + (b1 * on_art) + (b2 * art_init)]
pred.dt[,sex := 'both']
pred.dt = pred.dt[,.(mean = mean(pred_spend), upper = quantile(pred_spend, probs = c(0.975)), lower = quantile(pred_spend, probs = c(0.025))), by = c('ihme_loc_id', 'year', 'sex', 'perc_switch')]
pred.dt[,variable := 'predicted_spend']

## art price
price = fread(paste0('/home/j/Project/forecasting/hiv/data/20230202/forecasted_inputs_reference.csv'))
price = price[variable == 'ART Price']

plot.dt = melt(year.dt, id.vars = c('ihme_loc_id', 'year', 'draw', 'perc_switch', 'sex'))
plot.dt = plot.dt[,.(mean = mean(value), upper = quantile(value, probs = c(0.975)), lower = quantile(value, probs = c(0.025))), by = c('ihme_loc_id', 'year', 'perc_switch', 'variable', 'sex')]
plot.dt = rbind(plot.dt, pred.dt, use.names = T)
plot.dt = plot.dt[grepl('spend', variable)]
# pdf(paste0("/homes/tahvif/",run.name, "_predicted_spend.pdf"))
for(loc in unique(year.dt$ihme_loc_id)){
  print(loc)
  plot.loc.dt = plot.dt[ihme_loc_id == loc]
  gg =  ggplot(plot.loc.dt) +
    geom_line(aes(x = year, y = mean, color = factor(variable))) +
    ggtitle(paste0(loc)) +
    theme_bw()
  print(gg)
}
dev.off()




