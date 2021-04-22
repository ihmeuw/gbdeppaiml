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

run.name = '201012_ancrt'

loc.list <- list.files('/ihme/hiv/epp_output/gbd20/201012_ancrt/15to49_plots/')
loc.list <- unlist(lapply(loc.list, gsub, pattern = '.pdf', replacement = ''))
params.rt <- list()
for(loc in loc.list){
  if(!file.exists(paste0('/ihme/hiv/epp_output/gbd20/201012_ancrt/fit/',loc, '.RDS'))){
    next
  }
  rt <- readRDS(paste0('/ihme/hiv/epp_output/gbd20/201012_ancrt/fit/',loc, '.RDS'))
  rt_reg <- data.table(melt(rt$rvec))[,run := 'ancrt']
  rt_reg[,var :='rvec']
  rt_reg[,time := seq(1:nrow(rt_reg))]
  
  rt_beta <- data.table(melt(rt$ancrtsite.beta))[,run := 'ancrt']
  rt_beta[,var :='ancrtsite.beta']
  
  rt_age <- data.table(melt(rt$logincrr_age))[,run := 'ancrt']
  rt_age[,var :='age']
  setnames(rt_age, c('Var1', 'Var2'), c('age', 'sex'))
  
  rt <- data.table(rt$ancbias)[,run := 'ancrt']
  setnames(rt, 'V1', 'value')
  rt[,var := 'ancbias']
  
  rt <- rbind(rt_reg, rt_age, rt_beta, rt, fill = T)
  
  dt <- readRDS(paste0('/ihme/hiv/epp_output/gbd20/200713_yuka/fit/',loc, '.RDS'))
  dt_reg <- data.table(melt(dt$rvec))[,run := 'yuka']
  dt_reg[,var :='rvec']  
  dt_reg[,time := seq(1:nrow(dt_reg))]

  
  dt_age <- data.table(melt(dt$logincrr_age))[,run := 'yuka']
  dt_age[,var :='age']
  setnames(dt_age, c('Var1', 'Var2'), c('age', 'sex'))
  
  dt <- data.table(dt$ancbias)[,run := 'yuka']
  setnames(dt, 'V1', 'value')
  dt[,var := 'ancbias']
  dt <- rbind(dt_reg, dt_age, dt, fill = T)
  
  
  dt <- data.table(rbind(rt, dt))
  dt[,loc := loc]
  params.rt <- rbind(params.rt, dt) 
  
}

ggplot(params.rt[var == 'ancrtsite.beta',], aes(loc, value)) + geom_col() + ggtitle('ancrtsite.beta')

ggplot(params.rt[var == 'rvec',], aes(x = time, y = value, col = as.factor(loc))) + geom_line() + ggtitle('rvec') + facet_wrap(~run)


priors <- data.table(prior = 0.15, var = 'ancbias')

params.rt <- merge(params.rt, priors, fill = T)

####
#we want to know the one's on ancbias, where we insert the prior on (0.15, 1)
##at the bottom of gbd data sub

dt <- params.rt[var == 'ancbias',]

ggplot(dt[loc != 'AGO'], aes(loc, value, fill = run)) + geom_bar(position="dodge", stat="identity") + geom_hline(yintercept = unique(dt$prior))




# Graph tracking ss site over time ---------------------------------------

dt <- readRDS('/ihme/hiv/epp_output/gbd20/201012_ancrt/dt_objects/BDI_dt.RDS')
dt <- data.table(attr(dt, 'eppd')$ancsitedat)

ggplot(dt, aes(year, prev, col = as.factor(site))) + geom_line(aes(linetype = as.factor(type))) +
  geom_point() + ggtitle('BDI') + guides(color = FALSE)


dt <- readRDS('/ihme/hiv/epp_output/gbd20/201012_ancrt/dt_objects/CAF_dt.RDS')
dt <- data.table(attr(dt, 'eppd')$ancsitedat)

ggplot(dt, aes(year, prev, col = as.factor(site))) + geom_line(aes(linetype = as.factor(type))) +
  geom_point() + ggtitle('CAF') + guides(color = FALSE) 


dt <- readRDS('/ihme/hiv/epp_output/gbd20/201012_ancrt/dt_objects/BWA_dt.RDS')
dt <- data.table(attr(dt, 'eppd')$ancsitedat)

dt <- list()
for(loc in loc.list){
  if(!file.exists(paste0('/ihme/hiv/epp_output/gbd20/201012_ancrt/dt_objects/',loc,'_dt.RDS'))){
    next
  }
  dt.x <- readRDS(paste0('/ihme/hiv/epp_output/gbd20/201012_ancrt/dt_objects/',loc,'_dt.RDS'))
  dt.x <- data.table(attr(dt.x, 'eppd')$ancsitedat)
  dt.x[,loc := loc]
  dt <- rbind(dt, dt.x, fill =T)
}

ggplot(dt, aes(year, prev, col = as.factor(site))) + geom_line(aes(linetype = as.factor(type))) +
  geom_point(data = dt[type == 'ancrt',], aes(year, prev, col = as.factor(site))) + guides(color = FALSE) + facet_wrap(~loc, scales = 'free')












