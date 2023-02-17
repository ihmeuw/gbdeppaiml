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

run.name <- '221223_bittern'
out.dir = paste0("/share/hiv/spectrum_plots/",c.fbd_version)
loc.table <- data.table(get_locations(hiv_metadata = T))
epp.list <- sort(loc.table[epp == 1 & grepl('1', group), ihme_loc_id])
loc.list <- epp.list
loc.list = loc.list[!grepl('IND', loc.list) & !grepl('KEN', loc.list)]
n.draws  = 50

pdf(paste0("/homes/tahvif/",run.name, "_art_init_prop.pdf"))
for(loc in loc.list){
  print(loc)
  artinit = rbindlist(lapply(1:50, function(j){
    return(tryCatch(fread(paste0('/ihme/hiv/epp_output/', gbdyear, '/', run.name, '/artinit/', loc,'/', j, '.csv')),
                    error=function(e) NULL))
  })
  )
  artinit = artinit[year <= 2019]
  # pred.f = artinit[sex == 'female' & year %in% 2005:2015]
  # sp.f = smooth.spline(pred.f$year, pred.f$art_init, spar = 0.5)
  # pred.m = artinit[sex == 'male' & year %in% 2005:2015]
  # sp.m = smooth.spline(pred.m$year, pred.m$art_init, spar = 0.5)
  artinit = artinit[,.(mean = mean(art_init), upper=quantile(art_init,probs=c(.975)),lower=quantile(art_init,probs=c(.025))), by = c('year', 'sex')]
  # artinit[sex == 'female' & year >= 2005, pred := predict(sp.f, year)$y]
  # artinit[sex == 'male' & year >= 2005, pred := predict(sp.m, year)$y]
  gg =  ggplot(artinit) +
    geom_line(aes(x = year, y = mean, color = factor(sex))) +
    geom_line(linetype = "dashed", aes(x = year, y = pred, color = factor(sex))) +
    geom_ribbon(aes(x = year, ymin = lower, ymax = upper, fill = factor(sex)), alpha = 0.2) +
    ggtitle(paste0(loc)) +
    theme_bw()
  print(gg)
}
dev.off()

