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
eppasm_dir <- paste0(ifelse(windows, "H:", paste0("/ihme/homes/", user)), "/eppasm/")
setwd(eppasm_dir)
devtools::load_all()
gbdeppaiml_dir <- paste0(ifelse(windows, "H:", paste0("/ihme/homes/", user)), "/gbdeppaiml/")
setwd(gbdeppaiml_dir)
devtools::load_all()

args <- commandArgs(trailingOnly = TRUE)
if(length(args) > 0) {
  loc <- args[1]
  run.name <- args[2]
  gbdyear <- args[3]
  compare.run <- args[4]
  forecast.run <- args[5]
} else {
  loc = 'BDI'
  run.name = '200713_yuka'
  gbdyear = 'gbd20'
  compare.run = 'gbd19/190630_rhino_combined'
  forecast.run = '20210429_forecasting'
}



read.files <- function(dir.path, pattern=".csv", verbose=F) {
  file.list <- list.files(dir.path, pattern=pattern, recursive="T")
  if(verbose == T) {
    print(paste0("Files: ", paste(file.list, collapse=", ")))
  }
  dt.list <- mclapply(file.list, function(file) {
    dt <- fread(paste0(dir.path, file))
  }, mc.cores=ifelse(Sys.info()[1]=="Windows", 1, 2))
  if(verbose == T) {
    print("Files read")
  }
  dt <- rbindlist(dt.list)
  if(verbose == T) {
    print("Files bound")
  }
  return(dt)
}

dir.create(paste0('/ihme/hiv/epp_output/', gbdyear, '/', run.name, '/detailed_art_coverage/', loc, '/'), recursive = T)
compare.dt <- paste0('/ihme/hiv/epp_output/',compare.run,'/detailed_art_coverage/', loc, '/')
compare.dt <- read.files(compare.dt, pattern = '_cov_data.csv', verbose = T)

get_detailed_art_coverage <- function(iso, draw, runname, gbd_year){
  fp <- readRDS(paste0('/ihme/hiv/epp_output/', gbd_year, '/', runname, '/dt_objects/', iso, '_dt.RDS'))
  fp <- attr(fp, 'specfp')
  result <- readRDS(paste0('/ihme/hiv/epp_output/', gbd_year, '/', runname, '/fit/', iso, '/', draw, '.RDS'))
  mod <- mod_dimnames(result, fp$ss, paediatric = T)
  hp1 <- hivpop_singleage(mod, fp$ss)
  x <- melt(hp1)
  x <- dcast(x, cd4stage + agegr + sex + year + artdur ~ L1, value.var = 'value') %>% data.table()
  x_hivpop <- x[,.(cd4stage, agegr, sex, year, hivpop1)]
  x <- x[!is.na(artpop1),.(cd4stage, agegr, sex, year,  artpop1)]
  x[,artpop1 := sum(artpop1), by = c('cd4stage', 'agegr', 'sex', 'year')]
  x <- merge(unique(x), x_hivpop[!is.na(hivpop1),], by = c('cd4stage', 'agegr', 'sex', 'year'))
  x[,run_num := draw]
  cd4_dt <- data.table(cd4stage = unique(x$cd4stage), CD4 = c("GT500CD4", "350to500CD4", "250to349CD4", "200to249CD4", "100to199CD4", "50to99CD4",   "LT50CD4"))
  x <- merge(x, cd4_dt, by = 'cd4stage')
  x <- data.table(x)
  x[,cd4stage := NULL]
  setnames(x, c('agegr', 'artpop1', 'hivpop1'), c('age', 'pop_1', 'pop_0'))
  x[,iso3 := iso]
  write.csv(x, paste0('/ihme/hiv/epp_output/', gbd_year, '/', runname, '/detailed_art_coverage/', iso, '/', draw, '_cov_data.csv'), row.names = F)
  return(x)
}

detailed_dt <- mclapply(c(1:999), get_detailed_art_coverage, iso = loc, runname = run.name, gbd_year = gbdyear, mc.cores = ifelse(!windows, 2, 10))
detailed_dt <- rbindlist(detailed_dt)
detailed_dt[,pop_1 := mean(pop_1), by = c('age', 'sex', 'year', 'CD4', 'iso3')]
detailed_dt[,pop_0 := mean(pop_0), by = c('age', 'sex', 'year', 'CD4', 'iso3')]
detailed_dt <- unique(detailed_dt[,run_num := NULL])

compare.dt[,pop_1 := mean(pop_1), by = c('age', 'sex', 'year', 'CD4', 'iso3')]
compare.dt[,pop_0 := mean(pop_0), by = c('age', 'sex', 'year', 'CD4', 'iso3')]
compare.dt <- unique(compare.dt[,run_num := NULL])
detailed_dt[,run := 'current']
compare.dt[,run := 'previous']
dt <- rbind(detailed_dt, compare.dt)

dt <- melt(dt, id.vars = c('age', 'year', 'sex', 'CD4', 'iso3', 'run'))
dt[,value := sum(value), by = c('year', 'CD4', 'iso3', 'run', 'variable')]
dt <- unique(dt[,.(year, CD4, iso3, run, variable, value)])

dir.create(paste0('/ihme/hiv/spectrum_plots/', forecast.run, '/detailed_ART_coverage/'), recursive = T)
pdf(paste0('/ihme/hiv/spectrum_plots/', forecast.run, '/detailed_ART_coverage/', loc, '.pdf'), width = 11, height = 8)
gg <- ggplot(dt, aes(year, value, col = as.factor(run))) + geom_line() + facet_grid(variable~CD4) + 
 labs(title = paste0(loc.table[ihme_loc_id == loc, plot_name], ', detailed ART coverage'), 
      subtitle = "All ages and sexes")
print(gg)
dev.off()











