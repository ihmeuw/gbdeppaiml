#######################################################################################
### Setup
#######################################################################################
rm(list=ls())
gc()
root <- ifelse(Sys.info()[1]=="Windows", "J:/", "/home/j/")
list.of.packages <- c("data.table","ggplot2","haven","parallel", "assertable","feather")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)
code.dir <- ifelse(Sys.info()[1]=="Windows", "H:/", paste0("/homes/", Sys.info()['user'], "/"))
hpath <- ifelse(Sys.info()[1]=="Windows", "H:/", paste0("/homes/", Sys.info()['user'], "/"))
jpath <- ifelse(Sys.info()[1]=="Windows", "J:/", "/home/j/")

### Arguments
args <- commandArgs(trailingOnly = TRUE)
if(length(args) > 0) {
  c.fbd_version <- args[1]
  c.gbd_version <- args[2]
  c.scenario <- args[3]
  end_year <- as.integer(args[4])
  cores <- as.integer(args[5])
} else {

  c.fbd_version <- "230222_dove"
  c.gbd_version <- "200713_yuka"
  c.scenario <- 'reference'
  end_year <- 2050
  cores <- 25
}

library('dplyr')
# c.args <- read.csv(paste0(code.dir,"hiv_forecasting_inputs/run_versions_2020.csv"))
# c.args <- data.table(c.args)
# c.args <- c.args[fbd_version==c.fbd_version]
# c.gbd_version <- c.args[["gbd_version"]]
# c.gbd_input_version <- c.args[["gbd_input_version"]]
# extension.year <- c.args[["extension_year"]]
# c.draws <- c.args[["draws"]]
# end_year <- as.numeric(end_year)
# 
# 
# stage.list <- c("stage_1")
# 
### Functions
invisible(sapply(list.files("/share/cc_resources/libraries/current/r/", full.names = T), source))
library(mortdb, lib = "/mnt/team/mortality/pub/shared/r/4")
source("/share/cc_resources/libraries/current/r/get_population.R")
source(paste0(hpath,"/hiv_forecasting_inputs/find_child_locs.R"))

scenario.dict <- data.table(scenarios = c("reference","worse","better"), code = c(0,-1,1))
scenario.dict <- scenario.dict[code != 2,]

### Tables
loc.table <- data.table(get_locations(hiv_metadata = T, level = "all", gbd_year = 2020))
loc.list <- readRDS(paste0('/ihme/hiv/spectrum_input/20230202_forecasting/loc_list.RDS'))

#######################################################################################
###Prep Data
#######################################################################################
###Child ART
proc_dat <- function(loc) {
  print(loc)
for(loc in loc.list){
  print(loc)
  children = find.children(loc)
  if(length(children) == 0){
    children = loc
  }

  ##2. Read in files, test 1 child to find
  all_child = rbindlist(lapply(children,function(iso){
  
    
    print(iso)
    if(grepl('IND', iso)){
      dirs <- list(
        paste0("/share/hiv/spectrum_draws/",c.gbd_version,"/compiled/stage_2/", iso, "_coverage.csv"),
        paste0("/share/hiv/spectrum_draws/",c.gbd_version,"/compiled/stage_1/", iso, "_coverage.csv"),
        paste0("/share/hiv/epp_output/gbd20/",c.gbd_version,"/compiled/", iso, "_coverage.csv")
      )
    }else{
      dirs <- list(
        paste0("/share/hiv/spectrum_draws/",c.gbd_version,"/compiled/stage_1/", iso, "_coverage.csv"),
        paste0("/share/hiv/epp_output/gbd20/",c.gbd_version,"/compiled/", iso, "_coverage.csv")
      )
    }


    for(path in dirs) {
      
      if(file.exists(path)) {
        print(path)
        break
      }
    }
    if(!file.exists(path)){
      parent <- data.table()
      return(parent)
    }
    
    data <- fread(path) 
    data <- data.table(data)
    data[,eligible_pop := as.numeric(eligible_pop)]
    data <- data[,.(coverage=mean(coverage),
                    eligible_pop=mean(eligible_pop)),
                 by=.(age,sex,year,type)]
    data = data[order(year)]
    data[,ihme_loc_id := iso]
    return(data)
    
  }))
  
  ##3. Aggregate to the parent location
  if(nrow(all_child) == 0){
    parent <- data.table()
    return(parent)
  }
  parent = all_child[,list(coverage = sum(coverage), 
                           eligible_pop = sum(eligible_pop)), by = .(age,sex,year,type)]
  parent$ihme_loc_id <- loc
  print(which(loc == loc.list) / length(loc.list))
  final = parent
  spec = rbind(spec, final, use.names = T)
}
  return(parent)
}
# spec.list <- data.table()
# for(loc in loc.list){
#   x <- proc_dat(loc)
#   spec.list <- rbind(spec.list, x)
# }
spec.list <- mclapply(loc.list, proc_dat, mc.cores = 20)
# spec.list <- spec.list[unlist(lapply(spec.list, is.data.table))]

spec <- rbindlist(spec.list)
spec <- data.table(spec)
spec <- spec[year > 1979]
SAVE <- spec
spec[,id := 1:nrow(spec)]
spec <- spec[!ihme_loc_id %in% c( 'NGA'),]
spec[ihme_loc_id == 'COG' & year == 1980 & is.na(eligible_pop), eligible_pop := 0]
spec_na_copy <- spec[is.na(eligible_pop)]

spec <- unique(spec)

child.art <- spec[type=="ART" & age =="child"]
child.art <- child.art[,.(coverage = sum(coverage), 
                          eligible_pop = sum(eligible_pop)), by = c('age', 'year', 'type', 'ihme_loc_id')]
child.art[,value := ifelse(eligible_pop == 0, 0, coverage / eligible_pop)]
child.art[value > 1,value := 1]
setnames(child.art,"year","year_id")
child.art <- child.art[,.SD,.SDcols=c("ihme_loc_id","year_id","value")]
child.art[,variable:="Child ART"]
child.art[,roc_year:=2010]


###Cotrim
cotrim <- spec[type=="CTX" & age =="child"]
cotrim <- cotrim[,.(coverage = sum(coverage), 
                    eligible_pop = sum(eligible_pop)), by = c('age', 'year', 'type', 'ihme_loc_id')]
cotrim[,value := ifelse(eligible_pop == 0, 0, coverage / eligible_pop)]
cotrim[value > 1,value := 1]
setnames(cotrim,"year","year_id")
cotrim <-cotrim[,.SD,.SDcols=c("ihme_loc_id","year_id","value")]
cotrim[,variable:="Cotrim"]
cotrim[,roc_year:=2010]

###PMTCT
pmtct <- spec[(!(type %in% c("CTX","ART"))) & age =="adult" & sex=="female"]
pmtct<- dcast.data.table(pmtct,ihme_loc_id+year+eligible_pop~type,value.var="coverage")
pmtct_melted <- melt(pmtct, id.vars = c('ihme_loc_id', 'year', 'eligible_pop'))
pmtct_melted[,value := ifelse(is.na(value), 0, value)]
pmtct<- dcast.data.table(pmtct_melted,ihme_loc_id+year+eligible_pop~variable,value.var="value")

#Prenatal -- proportional rake all elements if over one
pmtct[,prenatal :=  singleDoseNevir + dualARV + optionA  + optionB]
#rake 
pmtct[,rake_prenatal := prenatal /eligible_pop]
for (c.var in c("dualARV","optionA", "optionB","singleDoseNevir")) {
pmtct[rake_prenatal>1,paste0(c.var):=get(c.var)/rake_prenatal]  
}

#recalculate
pmtct[,prenatal :=  singleDoseNevir + dualARV + optionA  + optionB]
#Postnatal
pmtct[,postnatal := optionA_BF  + optionB_BF]
##Rake postnatal
pmtct[, rake_postnatal := postnatal/eligible_pop]

for (c.var in c("optionA_BF","optionB_BF")) {
  pmtct[rake_postnatal > 1,paste0(c.var):=get(c.var)/rake_postnatal]  
}

#recalculate
pmtct[,postnatal :=  optionA_BF + optionB_BF]

pmtct.nga <- pmtct[ihme_loc_id == 'NGA_25322',]
pmtct.nga.2019 <- pmtct.nga[year == 2019,]
pmtct.nga.2019[,year := 2020]
keep_nga <- rbind(pmtct.nga[year != 2020], unique(pmtct.nga.2019), fill = T)
keep_nga[,keep := NULL]
pmtct <- rbind(pmtct[ihme_loc_id != 'NGA_25322',], keep_nga)

pmtct.final <- pmtct[,.SD,.SDcols=c("ihme_loc_id","year","prenatal","postnatal","tripleARTbefPreg", "tripleARTdurPreg", "eligible_pop")]
pmtct.final <- melt.data.table(pmtct.final,id.vars=c("ihme_loc_id","year","eligible_pop"),value.name = "num")
                               
pmtct.final[,value:=ifelse(eligible_pop == 0, 0, num/eligible_pop)]
pmtct.final[value > 1,value := 1]
pmtct.final[,variable:=paste0("pmtct ",variable)]
setnames(pmtct.final,"year","year_id")
pmtct.final <-pmtct.final[,.SD,.SDcols=c("ihme_loc_id","year_id","value","variable")]
pmtct.final[,roc_year:=2012]
##added this 5/12/2021
pmtct.final = pmtct.final[!ihme_loc_id %in% unique(pmtct.final[is.na(value), ihme_loc_id]),]
pmtct.bwa <- pmtct.final[ihme_loc_id == 'BWA',]
pmtct.bwa.2020 <- pmtct.bwa[year_id == 2020,]
pmtct.bwa.2020[,keep := max(value), by = 'variable']
keep_bwa <- rbind(pmtct.bwa[year_id != 2020], unique(pmtct.bwa.2020[value == keep]), fill = T)
keep_bwa[,keep := NULL]
pmtct.final <- rbind(pmtct.final[ihme_loc_id != 'BWA',], keep_bwa)
pmtct.bdi <- pmtct.final[ihme_loc_id == 'BDI',]
pmtct.bdi.2020 <- pmtct.bdi[year_id == 2020,]
pmtct.bdi.2020[,keep := max(value), by = 'variable']
keep_bdi <- rbind(pmtct.bdi[year_id != 2020], unique(pmtct.bdi.2020[value == keep]), fill = T)
keep_bdi[,keep := NULL]
pmtct.final <- rbind(pmtct.final[ihme_loc_id != 'BDI',], keep_bdi)

pmtct.bdi <- pmtct.final[ihme_loc_id == 'BDI',]
pmtct.bdi.2012 <- pmtct.bdi[year_id == 2012,]
pmtct.bdi.2012[,keep := max(value), by = 'variable']
keep_bdi <- rbind(pmtct.bdi[year_id != 2012], unique(pmtct.bdi.2012[value == keep]), fill = T)
keep_bdi[,keep := NULL]
pmtct.final <- rbind(pmtct.final[ihme_loc_id != 'BDI',], keep_bdi)

assert_values(pmtct.final, names(pmtct.final), 'not_na')


#######################################################################################
###Fit Rate of Change Models
#######################################################################################
data <- rbind(child.art,cotrim,pmtct.final)
SAVE_no_covid <- data

data <- data[year_id == extension.year | year_id == roc_year]
data[,n_years:=extension.year-roc_year]
data[year_id == roc_year,year_id:=9999]
data.w <- dcast.data.table(unique(data),ihme_loc_id+variable+n_years~year_id)

setnames(data.w,"9999","roc_year")
setnames(data.w,paste0(extension.year),"extension_year")

data.w[variable %in% c("Child ART","Cotrim","pmtct postnatal","pmtct prenatal"),roc:= (boot::logit(extension_year) - boot::logit(roc_year)) / (n_years)]
data.w[!(variable %in% c("Child ART","Cotrim","pmtct postnatal","pmtct prenatal")),roc:= (log(extension_year) - log(roc_year)) / (n_years)]
data.w <- data.w[!is.na(extension_year) & is.finite(roc) & !is.na(roc)]
  
summ.rocs <- data.w[!is.na(roc) & is.finite(roc),
                    .(p05=(quantile(roc,probs=.05,na.rm=T)),
                     p10=(quantile(roc,probs=.1,na.rm=T)),
                     p20=(quantile(roc,probs=.2,na.rm=T)),
                     p45=(quantile(roc,probs=.45,na.rm=T)),
                     p55=(quantile(roc,probs=.55,na.rm=T)),
                     p60=(quantile(roc,probs=.6,na.rm=T)),
                     p70=(quantile(roc,probs=.7,na.rm=T)),
                     p80=(quantile(roc,probs=.8,na.rm=T)),
                     p90=(quantile(roc,probs=.9,na.rm=T)),
                     p95=(quantile(roc,probs=.95,na.rm=T))
),by=.(variable)]

dir.create(paste0(jpath,"Project/forecasting/hiv/data/",c.fbd_version))
write.csv(summ.rocs,paste0(jpath,"Project/forecasting/hiv/data/",c.fbd_version,"/forecasted_input_rocs_", c.scenario, ".csv"))


#Load Past Data
data <- rbind(art.price,child.art,cotrim,pmtct.final, financ.data, ldi)

#make template, merge
data <- data[order(variable,ihme_loc_id,year_id)]
extension.dt <- data[year_id == extension.year]
n = end_year - extension.year
replicated.dt <- extension.dt[rep(seq_len(nrow(extension.dt)), n), ]
for (i in 1:n) {
  replicated.dt[seq(((i-1)*nrow(extension.dt) + 1), i*nrow(extension.dt)), year_id := (extension.year + i)]
}
replicated.dt = replicated.dt[,.(year_id, variable, roc_year, ihme_loc_id)]
data <- rbind(data[year_id <= extension.year], replicated.dt, fill = T)
data <- data[order(variable,ihme_loc_id,year_id)]
data <- data[order(year_id),]


#Make Predictions
make_pred <- function(c.draw,dt) {
  print(c.draw)  
  dt[,roc:=runif(n=nrow(dt),min=roc_low,max=roc_up)]
  dt[,pred:=value]  
  dt <- dt[order(variable,ihme_loc_id,year_id)]
  growth <- c()
  for (c.yr in seq(extension.year+1,end_year,1)) {
    ## Update - make sure these equations are correct
    dt[!(variable %in% c("Child ART","Cotrim","pmtct postnatal","pmtct prenatal", "pmtct tripleARTbefPreg", "pmtct tripleARTdurPreg")),growth:=(shift(pred)*roc)+shift(pred)]   
    dt[(variable %in% c("Child ART","Cotrim","pmtct postnatal","pmtct prenatal", "pmtct tripleARTbefPreg", "pmtct tripleARTdurPreg")),growth:=boot::inv.logit(boot::logit(shift(pred)) + roc)]   
    dt[year_id==c.yr,pred:=growth]  
  }
  # dt[!(variable %in% c("Child ART","Cotrim","pmtct postnatal","pmtct prenatal")),growth2:=pred * exp(roc * (year_id - extension.year))]
  dt[,draw:=c.draw]
}

rm(list = c("merge.ldi", "spec.list", "spec"))
fit.data <- merge(data,summ.rocs,by=c("variable"),all=T)

if(c.fbd_version == "20200805" | c.fbd_version == '20210129' | c.fbd_version == '20210429'){
  
  if (c.scenario %in% c("reference","covid_ref","no_covid")) {
    setnames(fit.data,"p45","roc_low")  
    setnames(fit.data,"p55","roc_up") 
  }
  
  if (c.scenario %in% c("better","covid_better")) {
    setnames(fit.data,"p80","roc_low")  
    setnames(fit.data,"p90","roc_up")
    fit.data[variable=="ART Price",roc_low:=p10]
    fit.data[variable=="ART Price",roc_up:=p20]
  }
  if (c.scenario %in% c("worse","covid_worse")) {
    setnames(fit.data,"p10","roc_low")  
    setnames(fit.data,"p20","roc_up")
    fit.data[variable=="ART Price",roc_low:=p80]
    fit.data[variable=="ART Price",roc_up:=p90]
  }  
} else {
  
  if (c.scenario == "reference" ) {
    setnames(fit.data,"p45","roc_low")  
    setnames(fit.data,"p55","roc_up") 
  }
  
  if (c.scenario %in% c("better")) {
    setnames(fit.data,"p80","roc_low")  
    setnames(fit.data,"p90","roc_up")
    fit.data[variable=="ART Price",roc_low:=p10]
    fit.data[variable=="ART Price",roc_up:=p20]
  }
  if (c.scenario %in% c("worse")) {
    setnames(fit.data,"p10","roc_low")  
    setnames(fit.data,"p20","roc_up")
    fit.data[variable=="ART Price",roc_low:=p80]
    fit.data[variable=="ART Price",roc_up:=p90]
  }  
  
}




n.draws <- 50
fit.data[,paste0('p', c('05', seq(10,95, 5))) := NULL]
# ldi.data <- data[variable == 'LDI']
# spend.data <- data[variable == "Spend"]
# # fit.data <- fit.data[!variable %in% c('Spend', 'LDI')]
# fit.data <- fit.data[!variable %in% c('LDI')]
preds <- rbindlist(mclapply(1:n.draws,make_pred,dt=fit.data,mc.cores = 20, mc.preschedule = F))
preds[,value := ifelse(is.na(value), pred, value)]
preds[,value := ifelse(is.nan(pred), 0, value)]

pred.draws <-  preds[,.SD,.SDcols=c("ihme_loc_id","year_id","variable","pred","value","draw","roc","roc_year")]


pred.summ <- pred.draws[,.(pred_mean=mean(pred,na.rm=T)),by=.(ihme_loc_id,year_id,variable,roc_year)]
# 
# ldi.data[, pred_mean := value]
# # spend.data[,pred_mean := value]
#  pred.summ <- rbind(pred.summ, ldi.data, use.names = T)
# pred.summ <- rbind(pred.summ, spend.data, use.names = T)


#######################################################################################
###Save Files
#######################################################################################
pred.summ <- pred.summ[,.SD,.SDcols=c("ihme_loc_id","year_id","variable","pred_mean")]
pred.summ <- unique(pred.summ)
pred.summ[,pred_mean := ifelse(is.nan(pred_mean), 0, pred_mean)]
dir.create(showWarnings = F, path=paste0(jpath,"Project/forecasting/hiv/data/",c.fbd_version),recursive = T)
# pred.summ_prior <- fread(paste0(jpath,"Project/forecasting/hiv/data/",c.fbd_version,"/forecasted_inputs_", c.scenario, ".csv"))
# pred.summ <- rbind(pred.summ, pred.summ_prior[,.(ihme_loc_id, year_id, variable, pred_mean)])
write.csv(pred.summ,paste0(jpath,"Project/forecasting/hiv/data/",c.fbd_version,"/forecasted_inputs_", c.scenario, ".csv"))

#save child ART and Cotrim 
child <- pred.summ[variable %in% c("Child ART","Cotrim")]
setnames(child,"year_id","year")
#make percent
child[,pred_mean:=pred_mean*100]
child <- dcast.data.table(child,ihme_loc_id+year~variable)
setnames(child,"Child ART","ART_cov_pct")
setnames(child,"Cotrim","Cotrim_cov_pct")
child[,ART_cov_num:=0]
child[,Cotrim_cov_num:=0]

dir.create(showWarnings = F, path=paste0('/share/hiv/spectrum_input/', c.fbd_version, '_', c.scenario, '/childARTcoverage'),recursive = T)
mclapply(loc.list, function(c.iso) {
  # x <- fread(paste0('/ihme/hiv/spectrum_input/', c.fbd_version, '_', c.scenario, '/childARTcoverage/', c.iso, '.csv'))
  # fp = paste0('/ihme/hiv/spectrum_input/', c.fbd_version, '_', c.scenario, '/childARTcoverage/', c.iso, '.csv')
  # if(length(x$year) == 81){
  #   next
  # }
  # system(paste0('rm ', fp))
  #
  print(c.iso)
  temp <- child[ihme_loc_id == c.iso]
  temp[,ihme_loc_id:=NULL]
  assert_values(temp,names(temp), test = "not_nan")
  assert_values(temp,names(temp), test = "not_na")
  write.csv(temp, paste0('/share/hiv/spectrum_input/', c.fbd_version, '_', c.scenario, '/childARTcoverage/' ,c.iso,".csv"),row.names=F)
}, mc.cores = cores)

# pred.summ <- read.csv(paste0(jpath,"Project/forecasting/hiv/data/",c.fbd_version,"/forecasted_inputs_", c.scenario, ".csv"))
pred.summ <- data.table(pred.summ)
pred.summ[,V1 := NULL]
pred.summ <- unique(pred.summ)

#split PMTCT and save
pmtct.vars <- c('singleDoseNevir', 'prenat_optionA', 'prenat_optionB', 'dualARV', 'postnat_optionB', 'postnat_optionA', 'tripleARTbefPreg', 'tripleARTdurPreg')
pmtct.pred <- dcast.data.table(pred.summ[grepl('pmtct', variable)],ihme_loc_id+year_id~variable, value.var = 'pred_mean')
pmtct.props <- unique(pmtct[year == (extension.year - 1), .(prenatal = sum(prenatal), postnatal = sum(postnatal), dualARV = sum(dualARV), 
                                               optionA = sum(optionA), optionA_BF = sum(optionA_BF), optionB = sum(optionB), 
                                               optionB_BF = sum(optionB_BF), singleDoseNevir = sum(singleDoseNevir),  ihme_loc_id)])
pmtct.props[,dualARV := ifelse(prenatal == 0, 0, dualARV/prenatal)]
pmtct.props[,prenat_optionA := ifelse(prenatal == 0, 0, optionA/prenatal)]
pmtct.props[,prenat_optionB := ifelse(prenatal == 0, 0, optionB/prenatal)]
pmtct.props[,singleDoseNevir := ifelse(prenatal == 0, 0, singleDoseNevir/prenatal)]
pmtct.props[, postnat_optionA:= ifelse(postnatal == 0, 0, optionA_BF/postnatal)]
pmtct.props[, postnat_optionB:= ifelse(postnatal == 0, 0, optionB_BF/postnatal)]
pmtct.props[,c('prenatal', 'postnatal', 'optionA', 'optionB', 'optionA_BF', 'optionB_BF') := NULL]
pmtct.props <- unique(pmtct.props)
setnames(pmtct.pred,"year_id","year")
names(pmtct.pred) <- gsub('pmtct ', '', names(pmtct.pred))
pmtct.pred <- merge(pmtct.props, pmtct.pred, by = c('ihme_loc_id'))
## split prenatal
for(c.var in c('singleDoseNevir', 'prenat_optionA', 'prenat_optionB', 'dualARV')){
  pmtct.pred[, paste0(c.var, '_pct') := prenatal * get(c.var)]
}
## split postnatal
for(c.var in c('postnat_optionB', 'postnat_optionA')){
  pmtct.pred[, paste0(c.var, '_pct') := postnatal * get(c.var)]
}
setnames(pmtct.pred, c('tripleARTbefPreg', 'tripleARTdurPreg'), c('tripleARTbefPreg_pct', 'tripleARTdurPreg_pct'))
pmtct.pred <- pmtct.pred[,c('ihme_loc_id', 'year', paste0(pmtct.vars, '_pct')), with = F]
pmtct.pred <- pmtct.pred[year >= extension.year]
for(c.var in pmtct.vars){
  pmtct.pred[, paste0(c.var, '_num') := 0]
  pmtct.pred[, paste0(c.var, '_pct') := get(paste0(c.var, '_pct')) * 100]

}
pmtct.pred <- unique(pmtct.pred)
dir.create(showWarnings = F, path=paste0('/share/hiv/spectrum_input/', c.fbd_version, '_', c.scenario, '/PMTCT'),recursive = T)
redo <- c()
for(c.iso in loc.list){
  print(c.iso)
  loc.pred <- pmtct.pred[ihme_loc_id == c.iso]
 if(nrow(loc.pred) == 0){
   redo <- c(redo, c.iso)
   next
   }
  loc.pred[, ihme_loc_id := NULL]
  loc.pred <- melt(loc.pred, id.vars = 'year')
  loc.pred[,value := ifelse(is.nan(value), 0, value)]
  loc.pred <- dcast(loc.pred, year~variable)
  #Note that PMTCT was not updated between GBD 17 and GBD 19, hence pulling from this folder
  #Aggregate past PMTCTs created just for forecast
  if(file.exists(paste0("/share/hiv/spectrum_input/20200505_forecasting/PMTCT/",c.iso,".csv")) |
     file.exists(paste0('/share/hiv/spectrum_input/190415_orca/PMTCT/', c.iso, '.csv'))){
    # if(!c.iso %in% run.locs){
    #   print("using aggregated inputs")
    #
    #   # if(!file.exists(paste0("/share/hiv/spectrum_input/20200505_forecasting/PMTCT/",c.iso,".csv"))){
    #   #   next
    #   # }
    #   # loc.past <- fread(paste0("/share/hiv/spectrum_input/20200505_forecasting/PMTCT/",c.iso,".csv"))
    #
    #
    # } else {
      if(file.exists(paste0('/share/hiv/spectrum_input/190415_orca/PMTCT/', c.iso, '.csv'))){
        loc.past <- fread(paste0('/share/hiv/spectrum_input/190415_orca/PMTCT/', c.iso, '.csv'))
      }else{
        loc.past <- read.csv(paste0("/share/hiv/spectrum_input/20200505_forecasting/PMTCT/",c.iso,".csv"))
        loc.past <- data.table(loc.past)
      }
    # }

  }else{
    children.names <- find.children(c.iso)
    children <- paste0('/ihme/hiv/spectrum_input/190415_orca/PMTCT/', children.names, '.csv')

    loc.past <- list()
    for(file in children){
      x <- read.csv(file)
      x <- data.table(x)
      x[,ihme_loc_id := children.names[which(children == file)]]
      loc.past <- rbind(loc.past, x)
    }
    loc.past <- melt(loc.past, id.vars = c('year', 'ihme_loc_id'))
    pop.loc <- get_population(age_group_id = 22, location_id = loc.table[ihme_loc_id %in% children.names, location_id],
                              year_id = unique(loc.past$year), gbd_round_id = 7, decomp_step = 'iterative')
    pop.loc[,total := sum(population), by = 'year_id']
    pop.loc[,pct := population / total, by = 'year_id']
    pop.loc <- pop.loc[,.(year_id, location_id, pct)]
    pop.loc <- merge(pop.loc, loc.table[,.(ihme_loc_id, location_id)])
    setnames(pop.loc, 'year_id', 'year')
    loc.past <- merge(loc.past, pop.loc, by = c('year', 'ihme_loc_id'))
    loc.past[,value := value * pct, by = c('year','ihme_loc_id', 'variable')]
    loc.past[,value := sum(value), by = c('year','variable')]
    loc.past <- unique(loc.past[,.(year, variable, value)])
    loc.past <- dcast(loc.past, year ~ variable, value.var = 'value')

  }

  loc.past <- unique(loc.past)
  loc.past <- loc.past[year <= extension.year]
  if(extension.year %in% loc.pred$year & extension.year %in% loc.past$year){
    loc.pred <- loc.pred[year!=extension.year]
  }
  loc.final <- rbind(loc.pred, loc.past, use.names = T, fill = T)
  ##will probably need to take this out?
  loc.final[is.na(loc.final)] <- 0
  loc.final <- loc.final[order(year)]
  id.vars <- list(year = min(loc.past$year):end_year)
  # if(max(loc.final$year) != 2050){
  #   next
  # }
  assert_ids(loc.final, id.vars)
  assert_values(loc.final,names(loc.final),"not_na")
  write.csv(loc.final, paste0('/share/hiv/spectrum_input/', c.fbd_version, '_', c.scenario, '/PMTCT/' ,c.iso,".csv"),row.names=F)


}



#Example for presentation and to check was sensibility
# c.fbd_version = "20200720"
# c.scenario = "covid_sens"
# plot_dt = fread(paste0(jpath,"Project/forecasting/hiv/data/",c.fbd_version,"/forecasted_inputs_", c.scenario, ".csv"))
# ggplot(plot_dt[ihme_loc_id %in% c("BWA","MWI","LUX","CAN","GNQ") &
# variable == "Child ART"],aes(year_id,pred_mean)) + geom_line(aes(color=ihme_loc_id)) +
# geom_vline(xintercept=2019,linetype="dotted")  + theme_bw() + scale_color_manual(name=NULL,
#                                                                breaks=c("BWA","CAN","LUX","MWI","GNQ"),
#                                                                labels=c("Botswana","Canada","Luxembourg","Malawi","Eq Guinea"),
#                                                                values=c("maroon","darkblue","darkgrey","orange","darkgreen")) +
# labs(y="Mean Child ART Coverage",x=NULL)




