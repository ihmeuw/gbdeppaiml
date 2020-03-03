### Setup
rm(list=ls())
windows <- Sys.info()[1][["sysname"]]=="Windows"
root <- ifelse(windows,"J:/","/home/j/")
user <- ifelse(windows, Sys.getenv("USERNAME"), Sys.getenv("USER"))
code.dir <- paste0(ifelse(windows, "H:", paste0("/homes/", user)), "/gbdeppaiml/")

## Packages
library(data.table); library(mvtnorm); library(survey); library(ggplot2)

## Arguments
args <- commandArgs(trailingOnly = TRUE)
print(args)
if(length(args) > 0) {
  loc <- args[1]
  run.name <- args[2]
  paediatric <- as.logical(args[3])
  compare.run <- args[4]
  if(compare.run == 'NA'){
    compare.run <- NA
  }
} else {
  run.name <- "200213_violin"
  loc <- "ZWE"

  draw.fill <- TRUE

  paediatric <- TRUE
  compare.run <- '190630_rhino2'
}

gbd_year_new <- "gbd20"
if(compare.run == '190630_rhino2'){
  gbd_year_old <- "gbd19"
  
}else{
  gbd_year_old <- "gbd20"
  
}
gbdyear <- 'gbd20'
run.name.old <- compare.run
### Functions
library(mortdb, lib = "/ihme/mortality/shared/r")
setwd(paste0(ifelse(windows, "H:", paste0("/homes/", user)), "/eppasm/"))
devtools::load_all()
setwd(code.dir)
devtools::load_all()






loc.table <- fread(paste0('/share/hiv/epp_input/gbd20/', run.name, '/location_table.csv'))
if(run.name.old != '190630_rhino2'){
  loc.table.old <- fread(paste0('/share/hiv/epp_input/gbd20/', run.name.old, '/location_table.csv'))
  
}else{
  loc.table.old <- fread(paste0('/share/hiv/epp_input/gbd19/', run.name.old, '/location_table.csv'))
  
}
if(loc %in% c('STP', 'COM', 'MAR')){
  compare.run = NA
}
# 
# ## 15-49 plots
dir.create(paste0('/ihme/hiv/epp_output/gbd20/', run.name, '/15to49_plots/'), recursive = TRUE, showWarnings = FALSE)
plot_15to49(loc,new.run = run.name, paediatric, plot.deaths = TRUE, compare.stage2 = F, compare.run = compare.run)

#Age-specific plots
for(c.indicator in c( 'Prevalence','Incidence','Deaths')){
  dir.create(paste0('/ihme/hiv/epp_output/gbd20/', run.name, '/age_specific_plots/', c.indicator, '/'), recursive = TRUE, showWarnings = FALSE)
}
plot_age_specific(loc, run.name.new=run.name, paediatric=TRUE, run.name.old = '190630_rhino2', compare.run = compare.run, gbdyear = gbd_year_new)

# ## Birth prevalence
dir.create(paste0('/ihme/hiv/epp_output/gbd20/', run.name, '/paeds_plots/'), showWarnings = F)
plot_birthprev(loc,run.name.new =  run.name, run.name.old = run.name.old)


# CIBA/Spectrum comparison plots
# for(c.indicator in c('Incidence', 'Prevalence', 'Deaths')){
#   dir.create(paste0('/ihme/hiv/epp_output/gbd19/', run.name, '/spec_compare_plots/', c.indicator, '/'), recursive = TRUE, showWarnings = FALSE)
# }
# plot_spec_compare(loc, run.name, paediatric, c.metric = 'Count')
# #


# 
# ## HIV CDR plots - Haidong sometimes asks for these
# dir.create(paste0('/ihme/hiv/epp_output/gbd19/', run.name, '/hivq15_plots/'), recursive = TRUE, showWarnings = FALSE)
# dt_old <- fread(paste0("/ihme/hiv/gbd_results/covariates/hiv_death_adult_15_59/compiled_hiv_sims_GBD2017.csv"))
# dt_new <- fread(paste0("/ihme/hiv/gbd_results/covariates/hiv_death_adult_15_59/compiled_hiv_sims.csv"))
# dt_old$run <- 'GBD17'
# dt_new$run <- 'GBD19'
# 
# color.list <- c('blue', 'red')
# names(color.list) <- c(run.name, 'GBD2017')
# 
# run.list <- list(dt_old,dt_new)
# combined.dt  = rbindlist(run.list)
# combined.dt$sex_id = as.factor(combined.dt$sex_id)
# 
# loc_id <- loc.table[ihme_loc_id==loc,location_id]
# combined.dt <- combined.dt[location_id==loc_id]
# plot_title <- loc.table[ihme_loc_id==loc,plot_name]
# 
# 
# combined.dt <- combined.dt[order(year_id,sex_id,run)]
# combined.dt <- combined.dt[year_id <= 2017 & year_id >= 1975]
# comb2 <- dcast(combined.dt[,.(run,mean_value, year_id,sex_id)], year_id ~ run + sex_id , value.var = c("mean_value"))
# comb2[,perc_ch1 := (GBD17_1-GBD19_1)/GBD19_1]
# comb2[,perc_ch2 := (GBD17_2-GBD19_2)/GBD19_2]
# comb2 <- melt(comb2[,.(perc_ch1,perc_ch2,year_id)],id.vars = "year_id")
# comb2[,variable := ifelse(variable=="perc_ch1",1,2)]
# setnames(comb2,c("variable","value"),c("sex_id","perc_change"))
# comb2[,sex_id := as.integer(sex_id)]
# combined.dt <- merge(combined.dt,comb2,by=c("year_id","sex_id"))

# 
# comb2 <- dcast(combined.dt[,.(run,mean_value, year_id,sex_id)], year_id ~ run + sex_id , value.var = c("mean_value"))
# comb2[,perc_ch1 := (GBD17_1-GBD19_1)]
# comb2[,perc_ch2 := (GBD17_2-GBD19_2)]
# comb2 <- melt(comb2[,.(perc_ch1,perc_ch2,year_id)],id.vars = "year_id")
# comb2[,variable := ifelse(variable=="perc_ch1",1,2)]
# setnames(comb2,c("variable","value"),c("sex_id","abs_change"))
# comb2[,sex_id := as.integer(sex_id)]
# 
# combined.dt <- merge(combined.dt,comb2,by=c("year_id","sex_id"))
# 
# d4 <- melt(combined.dt[,.(year_id,sex_id,mean_value,perc_change,abs_change,run)], id.vars=c("year_id","sex_id","run"))
# 
# if(grepl("IND",loc)){
#   cutoff  <- 1990
# } else {
#   cutoff <- 1980
# }
# 
# pdf(paste0('/ihme/hiv/epp_output/gbd19/', run.name, '/hivq15_plots/', loc, '.pdf'), width = 10, height = 6)
# gg <- ggplot(d4[year_id >= cutoff]) + geom_line(aes(year_id,value, color=run)) +
#     facet_wrap(~variable + sex_id,scales = "free_y",nrow=3) +
#     ggtitle(plot_title) + theme_bw() 
# print(gg)
# dev.off()
# 
# comb2 <- dcast(combined.dt[,.(run,mean_value, year_id,sex_id)], year_id ~ run + sex_id , value.var = c("mean_value"))
# comb2[,perc_ch1 := (GBD17_1-GBD19_1)]
# comb2[,perc_ch2 := (GBD17_2-GBD19_2)]
# comb2 <- melt(comb2[,.(perc_ch1,perc_ch2,year_id)],id.vars = "year_id")
# comb2[,variable := ifelse(variable=="perc_ch1",1,2)]
# setnames(comb2,c("variable","value"),c("sex_id","abs_change"))
# comb2[,sex_id := as.integer(sex_id)]
# 
# combined.dt <- merge(combined.dt,comb2,by=c("year_id","sex_id"))
# 
# d4 <- melt(combined.dt[,.(year_id,sex_id,mean_value,perc_change,abs_change,run)], id.vars=c("year_id","sex_id","run"))
# 
# if(grepl("IND",loc)){
#   cutoff  <- 1990
# } else {
#   cutoff <- 1980
# }
# 
# pdf(paste0('/ihme/hiv/epp_output/gbd19/', run.name, '/hivq15_plots/', loc, '.pdf'), width = 10, height = 6)
# gg <- ggplot(d4[year_id >= cutoff]) + geom_line(aes(year_id,value, color=run)) +
#     facet_wrap(~variable + sex_id,scales = "free_y",nrow=3) +
#     ggtitle(plot_title) + theme_bw() 
# print(gg)
# dev.off()
# 














