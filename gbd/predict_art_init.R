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

run.name <- '221223_bittern'
gbdyear = 'gbd20'
out.dir = paste0("/share/hiv/spectrum_plots/",run.name)
loc.table <- data.table(get_locations(hiv_metadata = T))
epp.list <- sort(loc.table[epp == 1 & grepl('1', group), ihme_loc_id])
loc.list <- epp.list
loc.list = loc.list[!grepl('IND', loc.list) & !grepl('KEN', loc.list)]
n.draws  = 50

pdf(paste0("/homes/tahvif/",run.name, "_art_init_prop.pdf"))
pred.init = data.table()
for(loc in loc.list){
  print(loc)
  # print(loc)
  # artinit = rbindlist(lapply(1:n.draws, function(j){
  #   return(tryCatch(fread(paste0('/ihme/hiv/epp_output/', gbdyear, '/', run.name, '/artinit/', loc,'/', j, '.csv')),
  #                   error=function(e) NULL))
  # })
  # )

  plot.artinit = artinit[ihme_loc_id == loc]
  loc_perc_switch = plot.artinit[year == 2005 & sex == 'male', perc_switch] + 1
  # artinit = artinit[year %in% 2000:2022]
  # artinit = artinit[,.(mean = mean(art_init), upper=quantile(art_init,probs=c(.975)),lower=quantile(art_init,probs=c(.025))), by = c('year', 'sex')]
  # artinit[,ihme_loc_id := loc]
  # pred.init = rbind(pred.init, artinit)
  gg =  ggplot(plot.artinit) +
    geom_line(aes(x = year, y = mean, color = factor(sex))) +
    geom_vline(xintercept = loc_perc_switch, linetype = 'dashed') + 
    geom_ribbon(aes(x = year, ymin = lower, ymax = upper, fill = factor(sex)), alpha = 0.2) +
    ggtitle(paste0(loc.table[ihme_loc_id == loc, location_name])) +
    theme_bw()
  print(gg)
}
dev.off()

# write.csv(pred.init,paste0('/ihme/hiv/epp_output/', gbdyear, '/', run.name, '/artinit/pred_init_tenth_year_full_time_series.csv'), row.names = F)
pred.init = fread(paste0('/ihme/hiv/epp_output/', gbdyear, '/', run.name, '/artinit/pred_init_tenth_year_full_time_series.csv'))

# identify year of percent switch
for(loc in unique(pred.init$ihme_loc_id)){
  print(loc)
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
  for(i in 2:dim(art)[2]){
    if(!art[1,i-1] & art[1,i]){
      y = as.numeric(colnames(art)[i]) - 1
      break
    }
  }
  pred.init[ihme_loc_id == loc, perc_switch := y]
  
}

reg.dt = merge(pred.init, loc.table[,.(region_name, ihme_loc_id)], by = 'ihme_loc_id')
# pdf(paste0("/homes/tahvif/",run.name, "_art_init_regional.pdf"))
# for(rr in unique(reg.dt$region_name)){
#   print(rr)
#   plot.dt = reg.dt[region_name == rr]
#   plot.dt = plot.dt[!grepl('NGA', ihme_loc_id) & !grepl('ETH', ihme_loc_id)]
#   gg =  ggplot(plot.dt) +
#     geom_line(aes(x = year, y = mean, color = factor(ihme_loc_id))) +
#     geom_ribbon(aes(x = year, ymin = lower, ymax = upper, fill = factor(ihme_loc_id)), alpha = 0.2) +
#     facet_wrap(~sex) +
#     ggtitle(paste0(rr)) +
#     theme_bw()
#   print(gg)
# }
# dev.off()
drivers = fread(paste0('/home/j/Project/forecasting/hiv/data/20230202/forecasted_inputs_reference.csv'))
drivers[,V1 := NULL]    
drivers = drivers[variable %in% c("Spend", "ART Price")]
art.price = dcast.data.table(drivers, ihme_loc_id + year_id ~ variable)
setnames(art.price, 'ART Price', 'price')
art.price[,dose_equivalents := Spend/price]
art.price = art.price[,.(ihme_loc_id, year_id, pred_mean = dose_equivalents, variable = 'dose_equivalents')]
drivers = drivers[variable == 'Spend']
drivers = rbind(drivers, art.price, use.names = T)
drivers = dcast.data.table(drivers, ihme_loc_id + year_id ~ variable)
setnames(drivers, c('year_id', 'Spend'), c('year', 'spend'))
pred.dt = merge(reg.dt, drivers, by = c('year', 'ihme_loc_id'))
pred.dt = pred.dt[year <= perc_switch]
pred.dt[,country := substr(ihme_loc_id, 1, 3)]
fit1 = lmer(mean ~ spend + (1 | region_name), data= pred.dt)
fit2 = lmer(mean ~ (1| country) + (spend | region_name), data = pred.dt)
int.mean <- summary(fit1)$coef[1]
int.se <- summary(fit1)$vcov[1,1]**0.5
re <- ranef(fit1, condVar = T)
region.var <- attr(re[[1]], "postVar")[1,1,]
region.dt <- data.table(region_name = sort(unique(pred.dt$region_name)), region_mean = re[[1]]$`(Intercept)`, region_sd = region.var ** 0.5)
region.dt[, region_val := region_mean + int.mean]
drivers = merge(drivers, loc.table[,.(region_name, ihme_loc_id)], by = 'ihme_loc_id')
loc.dt = merge(region.dt, drivers[,.(ihme_loc_id, region_name, spend, year)], by = 'region_name')
loc.dt[,pred_art_init := (summary(fit1)$coef[2,1] * spend) + region_val]

int.mean = summary(fit2)$coef[1,1]
int.loc <- summary(fit2)$coef[,1]
re2 = ranef(fit2, condVar = T)
region.dt.2 <- data.table(region_name = sort(unique(pred.dt$region_name)), region_int = re2[[1]]$`(Intercept)`, region_slope = re2[[1]]$`spend`)
loc.dt.2 = unique(pred.dt[,.(country, region_name)])
loc.dt.2[,loc_int := int.loc]
region.dt.2 = merge(region.dt.2, loc.dt.2, by = 'region_name')
drivers[,country := substr(ihme_loc_id, 1, 3)]
loc.dt.2 = merge(region.dt.2, drivers[,.(ihme_loc_id, spend, year, country)], by = 'country')
loc.dt.2[, pred_art_init_2 := (spend*region_slope) + loc_int + region_int]


drivers = merge(drivers, loc.dt[,.(ihme_loc_id, year, pred_art_init)], by = c('year', 'ihme_loc_id'))
drivers = merge(drivers, loc.dt.2[,.(ihme_loc_id, year, pred_art_init_2)], by = c('year', 'ihme_loc_id'))
drivers[,country := NULL]
plot.drivers = drivers[year %in% 2000:2040]
plot.drivers = melt(plot.drivers, id.vars = c('year', 'ihme_loc_id', 'region_name'))
plot.drivers = plot.drivers[!variable == 'dose_equivalents']

plot.dt = rbind(reg.dt[,.(ihme_loc_id, variable = sex, value = mean, year, region_name)], plot.drivers, use.names = T)
plot.dt[variable == 'male', variable := 'art_init_male']
plot.dt[variable == 'female', variable := 'art_init_female']
plot.dt = merge(plot.dt, unique(pred.dt[,.(ihme_loc_id, perc_switch)]), by = 'ihme_loc_id')
plot.dt = plot.dt[!(variable %in% c('art_init_male', 'art_init_female') & year > perc_switch)]
plot.dt[variable == 'spend', value := value/100]
plot.dt = plot.dt[!grepl('KEN', ihme_loc_id)]
plot.dt[variable %in% c('art_init_male', 'art_init_female', 'spend'), linetype := 'actual']
plot.dt[!variable %in% c('art_init_male', 'art_init_female', 'spend'), linetype := 'predicted']
plot.dt = plot.dt[!variable == 'pred_art_init_2']
plot.dt[variable == 'pred_art_init_2', variable := 'pred_art_init']
pdf(paste0("/homes/tahvif/",run.name, "_predicted_art_init.pdf"))
for(loc in unique(reg.dt$ihme_loc_id)){
  print(loc)
  plot.loc.dt = plot.dt[ihme_loc_id == loc]
  gg =  ggplot(plot.loc.dt) +
    geom_line(aes(x = year, y = value, color = factor(variable), linetype = factor(linetype))) +
    ggtitle(paste0(loc)) +
    theme_bw()
  print(gg)
}
dev.off()


# HAQI vs ART initiation over time
source("/ihme/cc_resources/libraries/current/r/get_covariate_estimates.R")
haqi <- get_covariate_estimates(covariate_id = 1099,
                                gbd_round_id = 7,
                                decomp_step = "iterative",
                                year_id = 2000:2040)
haqi = merge(haqi, loc.table[,.(ihme_loc_id, location_id)], by = 'location_id')
setnames(haqi, c('mean_value', 'lower_value', 'upper_value', 'year_id'), c('mean', 'lower', 'upper', 'year'))
haqi[,variable := 'HAQI']
haqi = haqi[ihme_loc_id %in% unique(pred.dt$ihme_loc_id)]
plot.haqi = copy(pred.init)
setnames(plot.haqi, 'sex', 'variable')
plot.haqi[variable == 'male', variable := 'art_init_male']
plot.haqi[variable == 'female', variable := 'art_init_female']
haqi[,perc_switch := NA]
haqi.cov = haqi[,.(ihme_loc_id, haqi = mean, year)]
haqi.pred = merge(pred.dt, haqi.cov, by = c('year', 'ihme_loc_id'))
fit3 = lmer(mean ~ spend + haqi + (1 | region_name), data= haqi.pred)
int.mean <- summary(fit3)$coef[1]
re3 <- ranef(fit3, condVar = T)
region.dt <- data.table(region_name = sort(unique(pred.dt$region_name)), region_mean = re3[[1]]$`(Intercept)`)
region.dt[, region_val := region_mean + int.mean]
loc.dt = merge(region.dt, haqi.pred[,.(ihme_loc_id, region_name, spend, haqi, year)], by = 'region_name')
loc.dt[,pred_art_init_haqi := (summary(fit3)$coef[2,1] * spend) + region_val + summary(fit3)$coef[3,1]]


plot.haqi = rbind(haqi[,.(year, ihme_loc_id, mean, upper, lower, variable, perc_switch)], plot.haqi, use.names = T)
plot.haqi[variable == 'HAQI',mean := mean/100]
plot.haqi[variable == 'HAQI',upper := upper/100]
plot.haqi[variable == 'HAQI',lower := lower/100]
plot.haqi = plot.haqi[year >= 2005]
pdf(paste0("/homes/tahvif/",run.name, "_art_init_vs_haqi.pdf"))
for(loc in sort(unique(plot.haqi$ihme_loc_id))){
  plot.artinit = plot.haqi[ihme_loc_id == loc]
  loc_perc_switch = plot.artinit[year == 2005 & variable == 'art_init_male', perc_switch] + 1
  # artinit = artinit[year %in% 2000:2022]
  # artinit = artinit[,.(mean = mean(art_init), upper=quantile(art_init,probs=c(.975)),lower=quantile(art_init,probs=c(.025))), by = c('year', 'sex')]
  # artinit[,ihme_loc_id := loc]
  # pred.init = rbind(pred.init, artinit)
  gg =  ggplot(plot.artinit) +
    geom_line(aes(x = year, y = mean, color = factor(variable))) +
    geom_vline(xintercept = loc_perc_switch, linetype = 'dashed') + 
    geom_ribbon(aes(x = year, ymin = lower, ymax = upper, fill = factor(variable)), alpha = 0.2) +
    ggtitle(paste0(loc.table[ihme_loc_id == loc, location_name])) +
    theme_bw()
  print(gg)
}
dev.off()

smooth.pred = copy(reg.dt[,.(year, ihme_loc_id, sex, mean)])
smooth.pred = rbind(smooth.pred, data.table(expand.grid(year = 2023:2040, ihme_loc_id = unique(smooth.pred$ihme_loc_id),
            sex = c('male', 'female'), mean = NA)), use.names = T)
smooth_dt <- rbindlist(lapply(c(2,3,4), function(i) {
  copy_dt <- copy(smooth.pred)
  copy_dt[, smooth := ksmooth(year, mean, kernel = "normal", range.x = c(2000, 2040), n.points = 41, bandwidth = i)$y, by = c('sex', 'ihme_loc_id')]
  copy_dt[, bandwidth := i]
}))

smooth_dt_loc = smooth_dt[ihme_loc_id == loc]
gg = ggplot(smooth_dt_loc) +
  geom_line(aes(x = year, y = smooth, color = factor(bandwidth)), linetype = 'dashed') +
  geom_line(aes(x = year, y = mean), linetype = 'solid') + 
  ggtitle(paste0(loc.table[ihme_loc_id == loc, location_name])) +
  facet_wrap(~sex)
print(gg)

