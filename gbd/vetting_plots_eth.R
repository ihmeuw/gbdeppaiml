##Set up ---------------------------
## Purpose of script: Format UNAIDS 2020 data
##
## Author: Deepa Jahagirdar
## Date Created:
## Email: djahag@uw.edu
##
##
## Notes:
##
Sys.umask(mode = "0002")
windows <- Sys.info()[1][["sysname"]]=="Windows"
root <- ifelse(windows,"J:/","/home/j/")
user <- ifelse(windows, Sys.getenv("USERNAME"), Sys.getenv("USER"))
package = c("data.table","ggplot2","dplyr")
lapply(package, require, character.only = TRUE)
library(mortdb, lib = "/share/mortality/shared/r")
loc.table = get_locations(hiv_metadata = TRUE)
source("/share/cc_resources/libraries/current/r/get_population.R")
##Format UNAIDS data
dt = fread("/ihme/hiv/UNAIDS_2020_data.csv", header = TRUE)
dt = dt[,1:(ncol(dt)-1)] ##Last column is percent change
dt = melt(dt, id.vars = c("Country"))
dt[,value := gsub("<","",value)]
dt[,value := gsub(" ","",value)]
dt[,value := as.numeric(value)]
uncertainty = dt[grepl("_",variable)]
dt = dt[!grepl("_",variable)]
setnames(dt,"Country","location_name")
dt = merge(dt,loc.table[,.(ihme_loc_id,location_name,group,level)])
dt = dt[group == c("1A") & !ihme_loc_id == "NGA_25344"]
##Bring in 2019 deaths
get_deaths = function(loc){
  print(loc)
  run.name = "190630_rhino_combined"
  g1 = fread(paste0(root, "WORK/04_epi/01_database/02_data/hiv/spectrum/summary/", run.name, "/locations/",loc,"_spectrum_prep.csv"))
  g1 = g1[,cycle := "GBD 19"]
  run.name = "180702_numbat_combined"
  g2 = fread(paste0(root, "WORK/04_epi/01_database/02_data/hiv/spectrum/summary/", run.name, "/locations/",loc,"_spectrum_prep.csv"))
  g2 = g2[,cycle := "GBD 17"]
  run.name = "200713_yuka"
  g3 = fread(paste0("/share/hiv/spectrum_prepped/summary/", run.name, "/locations/",loc,"_spectrum_prep.csv"))
  g3 = g3[,cycle := "GBD 20"]
  g.all = rbind(g1,g2,g3)
  g.all = g.all[measure %in% c("Deaths","Spec_Deaths") &
                  metric == "Count" & sex_id == 3 & age_group_id %in% c(22,24),
                .(year = year_id, mean, lower, upper, cycle,measure, age_group_id)]
  g.all[,c("mean","lower","upper") := .(mean = round(mean,0), lower = round(lower,0), upper = round(upper,0))]
  g.all[,ihme_loc_id := loc]
  return(g.all)
}
gbd_dat = rbindlist(lapply(unique(dt$ihme_loc_id), get_deaths))
setnames(dt,"variable","year")
setnames(dt,"value","unaids")
dt[,year := as.numeric(as.character(year))]
all_dt = merge(gbd_dat,dt,by =c("ihme_loc_id","year"))
##Differences
all_dt[,perc_diff := (unaids - mean)/mean]
all_dt
all_dt[abs(perc_diff) < 0.2, perc_diff_cat := 'less20']
all_dt[abs(perc_diff) >= 0.2 & abs(perc_diff)  < 0.4, perc_diff_cat := '20to40']
all_dt[abs(perc_diff) >= 0.4, perc_diff_cat := 'more40']
max_val = max(c(all_dt[,mean], all_dt[,unaids]))
thresh_val = 1.4e5
max_val <- log(max_val)
min_val= min(c(all_dt[,mean], all_dt[,unaids]))
min_val = log(min_val)
thresh_val <- log(thresh_val)
library(ggrepel)
# gg <- ggplot(all_dt, aes(log(mean), log(unaids))) + geom_point(aes(shape = as.factor(cycle), col = as.factor(perc_diff_cat))) +
#   facet_grid(measure~age_group_id) + geom_abline(intercept = 0, slope = 1)  + xlim(min_val,max_val) + ylim(min_val,max_val) +
#   geom_text_repel(data = all_dt[log(mean )> thresh_val & perc_diff_cat != "less20" & year %in% c(2010, 2013,2015,2017,2019),],
#                   aes(label = paste0(ihme_loc_id, '_', year)), color="black",size=2.5) + theme_bw()
#
# print(gg)
max_val = max(c(all_dt[,mean], all_dt[,unaids]))
thresh_val = 1.4e5
min_val= min(c(all_dt[,mean], all_dt[,unaids]))
lines <- data.table(mean = c(min_val: max_val), lower = c(min_val: max_val) * -0.2 + c(min_val: max_val),
                    upper = c(min_val: max_val) * 0.2 + c(min_val: max_val), type = c('less20'))
lines2 <- data.table(mean = c(min_val: max_val), lower = c(min_val: max_val) * -0.4 + c(min_val: max_val),
                     upper = c(min_val: max_val) * 0.4 + c(min_val: max_val), type = c('less40'))
lines <- rbind(lines, lines2)
# gg <- ggplot() + geom_point(data = all_dt, aes(mean, unaids, col = as.factor(cycle))) +
#   facet_grid(measure~age_group_id) +
#   geom_abline(intercept = 0, slope = 1)  +
#   geom_ribbon(data = lines, aes(x = mean,ymin = lower, ymax = upper, fill = factor(type)), alpha=0.2)+
#   coord_cartesian(ylim = c(min_val, max_val), xlim = c(min_val,max_val)) +
#   geom_text_repel(data = all_dt[(mean )> thresh_val & perc_diff_cat != "less20" & year %in% c(2010, 2013,2015,2017,2019),],
#                   aes(label = paste0(ihme_loc_id, '_', year)), color="black",size=2.5) + theme_bw()
# print(gg)
gg <- ggplot() + geom_point(data = all_dt[age_group_id == 22,], aes(log(mean), log(unaids), col = as.factor(cycle))) +
  facet_wrap(~measure) +
  geom_abline(intercept = 0, slope = 1)  +
  geom_ribbon(data = lines, aes(x = log(mean),ymin = log(lower), ymax = log(upper), fill = factor(type)), alpha=0.2)+
  coord_cartesian(ylim = c(log(min_val), log(max_val)), xlim = c(log(min_val),log(max_val))) +
  geom_text_repel(data = all_dt[abs(perc_diff) > 0.4 & year %in% c(2017) & mean > 5000,],
                  aes(x = log(mean),y = log(unaids), label = paste0(ihme_loc_id, '_', year)), color="black",size=2.5) + theme_bw()
print(gg)


new_dt <- all_dt[year == 2017 & age_group_id ==22,.(location_name, mean, lower, upper, cycle, measure, unaids)]
new_dt[,mean := paste0(mean, ' (', lower, ', ', upper, ')')]
new_dt <- new_dt[,.(location_name, mean, cycle, measure, unaids)]
new_dt <- dcast(new_dt, location_name + measure + unaids ~ cycle, value.var = 'mean')
new_dt[location_name %in% loc.table[region_id == 174 & level == 3, location_name] & measure == 'Deaths']




