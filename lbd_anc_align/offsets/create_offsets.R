#### ANC Areal Conversation 

rm(list=ls())
windows <- Sys.info()[1][["sysname"]]=="Windows"
root <- ifelse(windows,"J:/","/home/j/")
user <- ifelse(windows, Sys.getenv("USERNAME"), Sys.getenv("USER"))
code.dir <- paste0(ifelse(windows, "H:", paste0("/homes/", user)), "/hiv_gbd2019/anc_bias/")

h_root <- '/homes/mwalte10/'
# load packages, install if missing
lib.loc <- paste0(h_root,"R/",R.Version()$platform,"/",R.Version()$major,".",R.Version()$minor)
dir.create(lib.loc,recursive=T, showWarnings = F)
.libPaths(c(lib.loc,.libPaths()))
packages <- c("data.table","sp", 'sf', "stringr", 'tidyverse', 'lwgeom')
library(gridExtra)
for(p in packages){
  if(p %in% rownames(installed.packages())==FALSE){
    install.packages(p)
  }
  library(p, character.only = T)
}

## Arguments
args <- commandArgs(trailingOnly = TRUE)
print(args)
if(length(args) > 0) {
  loc <- args[1]
} else {
  loc <- "BWA"
}


save_out_shapefiles <- FALSE
covs <- "hiv_test"
measures <- "mean"
agg_method = "pop_weight"
shapefile_field <- "GAUL_CODE"

library("slackr",lib.loc =paste0("/homes/", user,"/rlibs/"))
library(mortdb, lib = "/home/j/WORK/02_mortality/shared/r")
loc.table <- get_locations()

source(paste0(code.dir,"/geo_extract_functions.R"))
library(data.table); library(rgdal)

##File paths
out_dir_areal <- paste0(root,"WORK/04_epi/01_database/02_data/hiv/04_models/gbd2015/02_inputs/lbd_extraction/04_04_2019_extraction/areal_sites/")
geo_repository <- paste0(ifelse(windows, "H:", paste0("/homes/", user)), "/lbd_hiv/data/")
core_repo = paste0("/homes/",user,"/lbd_core/")
shapefile_directory <- paste0(root,"WORK/11_geospatial/05_survey shapefile library/Shapefile directory/")
out_dir_geo <- "/ihme/hiv/data/shapefiles/"

lbd_map <- fread(paste0(geo_repository,"/anc/2_geomatching/final_geo_codebook.csv"))
lbd_map <- merge(lbd_map,loc.table[,.(ihme_loc_id,location_name)],by.x="iso3",by.y="ihme_loc_id",all.x=TRUE)
loc.list <- unique(lbd_map$iso3)

#Save location specific shapefiles to reduce size of large files - this only needs to be done once and completed 6/7/2019
# if(save_out_shapefiles){
# for(shp in c("gadm_36_ad1","admin2013_2","admin2013_1")){
#   if(shp == "gadm_36_ad1") id.var<-"ADM0_NAME" else id.var <-"COUNTRY_ID"
#     print(id.var)
#     locs <- lbd_map[shapefile==shp,iso3]; locations <- lbd_map[shapefile==shp,location_name]
#     print(locs);print(locations)
#     loc_shp <- readOGR(paste0(shapefile_directory,shp,".shp"))
#     loc_shp <- loc_shp[which(loc_shp@data[,id.var] %in% c(locs,locations)),]
#     writeOGR(obj=loc_shp, dsn=out_dir_geo, layer=paste0(shp,"_limited.shp"), driver="ESRI Shapefile")
#    }
# }   
loc <- "BWA"
# Load cell pred object - DO THIS OUTSIDE THE FUNCTION
message("Load cell pred object")
rd <- "2019_02_26_20_11_35"
rr <- "sssa"
mod.dir <- paste0("/share/geospatial/mbg/hiv/hiv_test/output/",rd,"/")
region_draws <- paste0("hiv_test_cell_draws_eb_bin0_",rr,"_0.RData") 
load(paste0(mod.dir,region_draws))
shp <- "BWA_adm2"
shapefile_path <- paste0(shapefile_directory, shp,".shp")

for(loc in loc.list){
  
  location <- unique(lbd_map[iso3==loc,location_name])
  final_geo <- lbd_map[iso3==loc & is.na(latitude)]
  shp_path <- unique(final_geo$shapefile)
  
  
  agg_covs <- list()
  
  for (shp in shp_path){
    print(shp)
    
    if (is.na(shp) | !file.exists(shapefile_path)){
      print(paste0("no geofile for ",loc))
      next
    }
    
    #shp <- shp_path[1]
    #shapefile_path <- paste0(shapefile_directory, shp,".shp")
    
    
    if(shp %in% c("gadm_36_ad1","admin2013_2","admin2013_1")){
      shapefile_path <- paste0(out_dir_geo,shp,"_limited.shp")
      loc_shp <- readOGR(shapefile_path)
      if(shp == "gadm_36_ad1") id.var<-"ADM0_NAME" else id.var<-"COUNTRY_ID"
      loc_shp <- loc_shp[which(loc_shp@data[,id.var] %in% c(loc,location)),]
    } else {
      loc_shp <- readOGR(shapefile_path)
    }
    
    
    df <- loc_shp@data
    
    aggregated_covs <- frac_agg_covs(covs=covs, measures=measures, shapefile_path=shapefile_path, 
                                     shapefile_field=shapefile_field, core_repo=core_repo, agg_method=agg_method)
    aggregated_covs$shapefile <- shp
    agg_covs <- rbind(agg_covs, aggregated_covs,fill=TRUE)
    
  }
  if(!is.null(dim(agg_covs))){ 
    # write.csv(agg_covs,paste0(out_dir_areal,loc,".csv"),row.names = FALSE)
    write.csv(agg_covs,paste0('/homes/mwalte10/hiv_gbd2019/lbd_anc_align/areal_sites/',loc,".csv"),row.names = FALSE)
    
  }
  
}




### Functions
## GBD
code.dir <- paste0(ifelse(windows, "H:", paste0("/ihme/homes/", user)), "/hiv_gbd2019/02_EPP2019/")

source(paste0(code.dir,"gbd/prep_data.R"))
source(paste0(code.dir,"gbd/prep_output.R"))
source(paste0(code.dir,"gbd/data_sub.R"))
source(paste0(code.dir,"gbd/plot_fit.R"))
source(paste0(code.dir,"gbd/ind_data_prep.R"))


## EPP
source(paste0(code.dir,"R/epp.R"))
source(paste0(code.dir,"R/fit-model.R"))
source(paste0(code.dir,"R/generics.R"))
source(paste0(code.dir,"R/IMIS.R"))
source(paste0(code.dir,"R/likelihood.R"))
source(paste0(code.dir,"R/read-epp-files.R"))
start.year <- 1970
trans.params.sub <- TRUE
pop.sub <- TRUE
art.sub <- TRUE
prev.sub <- TRUE
sexincrr.sub <- TRUE
plot.draw <- FALSE
anc.prior.sub <- TRUE
lbd.anc <- TRUE
age.prev <- FALSE
gbdyear <- 'gbd20'
run.name <- '191002_sitar'
stop.year <- 2020
j <- 1
anc.sub <- FALSE


gen.pop.dict <- c("General Population", "General population", "GP", 
                  "GENERAL POPULATION", "GEN. POPL.", "General population(Low Risk)", 
                  "Remaining Pop", "population feminine restante","Pop féminine restante","Rift Valley", 
                  "Western","Eastern","Central","Coast","Nyanza","Nairobi",
                  "Female remaining pop")

if(grepl("ZAF",loc) | grepl("IND",loc)){
  dt <- readRDS(paste0('/share/hiv/data/PJNZ_EPPASM_prepped/', loc, '.rds'))
} else {
  dt <- readRDS(paste0('/share/hiv/data/PJNZ_EPPASM_prepped_subpop/lbd_anc/', loc, '.rds'))
}

anc.dt <- dt %>% data.table()

new.anc <- readRDS("/home/j/WORK/11_geospatial/10_mbg/hiv/unaids_anc/anc_data_2018_10_26.rds")
loc1 <- substring(loc,1,3)
new.anc <- setDT(new.anc)[country==loc]
setnames(new.anc,c("country"),c("iso3"))

all.dat <- new.anc
all.dat <- all.dat[,.(site,year,anc_hiv_test,N,group)]
setnames(all.dat,c("anc_hiv_test","N","group"),c("prev","n","subpop"))
all.dat[,c('type','agegr','age','agspan') := NA]
all.dat$type <- rep("ancss",nrow(all.dat));
all.dat$agegr <- rep("15-49",nrow(all.dat)); all.dat$age <- rep("15",nrow(all.dat)); all.dat$agspan=rep("35",nrow(all.dat))
all.dat$used <- TRUE
all.dat$source <- "LBD"

anc.dt$source <- "GBD"



#Find different site years by subpopulation (if exists)
all.dat[,site_year := paste0(site,year)]
anc.dt$site_year <- paste0(anc.dt$site,anc.dt$year)


##Merge with source
full_geo <- fread(paste0(geo_repository,"/anc/2_geomatching/final_geo_codebook.csv"))[iso3==loc]
full_geo[,data_source := paste0("UNAIDS files - ",UNAIDS_year)]

lbd.anc <- read.csv("/snfs1/WORK/04_epi/01_database/02_data/hiv/04_models/gbd2015/02_inputs/lbd_extraction/04_04_2019_extraction/anc_mean_est_final.csv") %>% data.table()
lbd.anc <- lbd.anc[iso3==loc]
setnames(new.anc,c("country"),c("iso3"))
all.dat <- merge(new.anc,lbd.anc,all.x=TRUE)




### Tables
loc.table <- data.table(get_locations(hiv_metadata = T))

lbd_loc <- loc
sub_locs <- loc.table[grepl(lbd_loc,ihme_loc_id) & epp==1,ihme_loc_id]

site.dat.list <- list()
loc_ids <- list()
subpop_names <- list()

# Pull most recent GBD ANC site data and collapse over sublocs if necessary
# Do not try to match to high risk populations because LBD does not have 
## Code
# Prep data and collapse location subpopulations
for(loc in sub_locs){
  print(loc)
  
  dt <- read_spec_object(loc, j, start.year, stop.year, trans.params.sub, 
                         pop.sub, anc.sub, anc.backcast, prev.sub = TRUE, art.sub = TRUE, 
                         sexincrr.sub = TRUE, popadjust, age.prev = age.prev, paediatric = TRUE, 
                         anc.prior.sub = TRUE, lbd.anc =T)
  site.dat.subpop <- list()
  
  
  gen.pop.dict <- c("General Population", "General population", "GP", 
                    "GENERAL POPULATION", "GEN. POPL.", "General population(Low Risk)", 
                    "Remaining Pop", "population feminine restante","Pop féminine restante","Rift Valley", 
                    "Western","Eastern","Central","Coast","Nyanza","Nairobi",
                    "Female remaining pop")
  
  
  #Formatting
    #site.dat <- attr(dt[[subpop]], "eppd")$anc.prev
    site.dat <- attr(dt, "eppd")$ancsitedat
    site.dat.subpop <- site.dat

  
  site.dat.list <- site.dat.subpop
  loc_ids <- rbind(loc_ids,matrix(rep(loc,nrow(site.dat.subpop)),ncol=1))
  
}


years <- site.dat.list$year;  ihme_loc_id <- loc_ids
site.dat <- as.data.frame(site.dat.list)
site.dat$site <- site.dat.list$site
site.dat$ihme_loc_id <- unlist(ihme_loc_id)
site.dat <- data.table(unique(site.dat))
gbd.anc.all <- cbind(as.character(site.dat$site), site.dat$year, site.dat$prev, site.dat$ihme_loc_id, site.dat$n, site.dat$type,
                     site.dat$agegr, site.dat$age, site.dat$agspan) %>% as.data.table()
colnames(gbd.anc.all) <- c('site', 'year_id', 'mean', 'ihme_loc_id', 'n', 'type', 'agegr' ,'age','agspan')


##Flag high risk data that will not get matched to LBD data
gbd.anc.all[,high_risk := FALSE]
gbd.anc.all[!subpop %in% c(loc,sub_locs,gen.pop.dict),high_risk := TRUE] 
gbd.anc.all[clinic_match %in% c("Pseudo site","Pseudo sites","pseudo site"),high_risk:=TRUE]

#Remove high risk data to complete LBD matching (but bind it later)
gbd.anc <- gbd.anc.all[high_risk==FALSE]

loc <- lbd_loc
lbd.anc <- read.csv("/snfs1/WORK/04_epi/01_database/02_data/hiv/04_models/gbd2015/02_inputs/lbd_extraction/04_04_2019_extraction/anc_mean_est_final.csv") %>% data.table()
lbd.anc <- lbd.anc[iso3==loc]

#######################################
######  General character fixes  ######
#######################################
lbd.anc$site <- gsub("&apos;",'\'',lbd.anc$site) 
lbd.anc$site <- gsub("\xe9","é", lbd.anc$site)

new.sites <- c()
for (i in 1:length(lbd.anc$site)) {
  test.length <- length(unlist(strsplit(as.character(lbd.anc$site[i]), split = " ")))
  if(test.length > 2){
    new.sites[i] <- paste(unlist(strsplit(as.character(lbd.anc$site[i]), split = " "))[1], unlist(strsplit(as.character(lbd.anc$site[i]), split = " "))[(length(unlist(strsplit(as.character(lbd.anc$site[i]), split = " "))) - 1)])
  }else{
    new.sites[i] <- unlist(strsplit(as.character(lbd.anc$site[i]), split = " "))[1]
  }
}

lbd.anc$site <- new.sites

######################################
######  Loc specific  fixes     ######
######################################

if (loc == "UGA"){
  gbd.anc$clinic_match <- gsub(substr(gbd.anc$clinic_match,1,1),'',gbd.anc$clinic_match)
  
}

if (loc=="BEN"){
  lbd.anc[site=="CM Porto-Novo",site:= "CM Porton-Novo"]   
}

if (loc=="BFA"){
  lbd.anc[site=="Ziniar\xe9 (%)",site:= "Ziniaré (%)"]   
}

if (loc=="CMR"){
  lbd.anc[site=="CMA Tyo",site:= "CMA  Tyo"]   
}

if (loc=="COG"){
  lbd.anc$site <- gsub("\\?","é",lbd.anc$site)
}

if (loc=="MLI"){
  lbd.anc[site=="CSCOM KLELE",site:="CSCOM KLELA"]
}

if (loc=="SDN"){
  dict <- data.table(site=c("Umrowaba  N Kordofan","Muglad  S Kordofan","Sinnar  Sinnar","Dabah  Northern","Barbar  River Nile","Sinkat  Red Sea"),revised=c("Umrowaba_N Kordofan","Muglad_S Kordofan","Sinnar_Sinnar","Dabah_Northern","Barbar_River Nile","Sinkat_Red Sea"))
  lbd.anc <- lbd.anc %>% left_join(dict) %>% data.table()
  lbd.anc[,site := ifelse(!is.na(revised),revised,site)]
  lbd.anc[,revised:=NULL]
  
}

if (loc=="MDG"){
  dict <- data.table(site=c("Antsirabe (%)","Sambava (%)","Miarinarvio (%)" ),revised=c("Antsirabe  (%)","Sambava  (%)","Miarinarivo (%)"))
  lbd.anc <- lbd.anc %>% left_join(dict) %>% data.table()
  lbd.anc[,site := ifelse(!is.na(revised),revised,site)]
  lbd.anc[,revised:=NULL]
}

if (loc=="RWA"){
  gbd.anc$clinic_match <- gsub("\\\"","",gbd.anc$clinic_match)
}

#Check for differences
gbd_diff <- setdiff(unique(gbd.anc$clinic_match), unique(lbd.anc$site))
#setdiff(unique(lbd.anc$site), unique(gbd.anc$clinic_match))
gbd.anc[,lbd_areal := 0]
gbd.anc[,lbd_missing := 0]

if(loc=="NGA"){
  gbd.anc$clinic_match[gbd.anc$clinic_match %in% gbd_diff] <- gsub(substr(gbd_diff,1,1),'',gbd_diff) 
  lbd.anc[site=="Osun (Iragbere)",site := "Osun  (Iragbere)"]
  gbd_diff <- setdiff(unique(gbd.anc$clinic_match), unique(lbd.anc$site))
}

##ID areal data from original geo codebook (which is usually why GBD sites > LBD sites; the areal data gets dropped when original geo codebook merged)
if(length(gbd_diff) > 0){
  full_geo <- fread(paste0(geo_repository,"/anc/2_geomatching/final_geo_codebook.csv"))[iso3==loc]
  areal <- full_geo[site %in% gbd_diff & is.na(latitude)]
  remaining_diff <- gbd_diff[!gbd_diff %in% areal$site]
  if(length(remaining_diff) > 0){
    warning("Remaining Differences: ",paste0(remaining_diff))
  }
  gbd.anc[site %in% areal$site, lbd_areal := 1]
  gbd.anc[site %in% remaining_diff, lbd_missing := 1]
  #write.csv(areal, paste0('/snfs1/WORK/04_epi/01_database/02_data/hiv/04_models/gbd2015/02_inputs/lbd_extraction/04_04_2019_extraction/areal_sites/',loc, '.csv'), row.names=FALSE)
  
}

##ID missingness from LBD
missing <- lbd.anc[is.na(site_pred) & site %in% gbd.anc$site]

if(nrow(missing) > 0){
  write.csv(missing,paste0('/snfs1/WORK/04_epi/01_database/02_data/hiv/04_models/gbd2015/02_inputs/lbd_extraction/04_04_2019_extraction/missing_lbd_preds/',loc, '.csv'), row.names=FALSE)
}

##Add back subpopulations
if(nrow(gbd.anc) != nrow(gbd.anc.all)){
  gbd.anc.all[,c("lbd_missing","lbd_areal") := NA]
  gbd.anc <- rbind(gbd.anc,gbd.anc.all[high_risk==TRUE])
  if(nrow(gbd.anc) != nrow(gbd.anc.all)){stop("gen pop + high risk matrix size mismatch")}
}

###############################
######  Combined data    ######
###############################
setnames(lbd.anc, c('site', 'year'), c('clinic', 'year_id'))
setnames(gbd.anc, c('site'), c('clinic'))
lbd.anc[,iso3 := ifelse(!is.na(iso3_adm1), as.character(iso3_adm1),as.character(iso3))]


lbd.anc <- unique(lbd.anc)

if(length(unique(lbd.anc$iso3)) > 1){
  lbd.anc <- lbd.anc[!is.na(iso3_adm1)]
}

lbd.anc[,c('loc_id', 'subnational', 'country') := NULL]
## Pre 2000 we don't have any LBD estimates, so we just merge on the clinic metadata
####NOTE, I TOOK OUT THE MATCH ON LATITUDE AND LONGITUDE HERE BECAUSE IT CAN BE MISSING OR MULTIPLE VALUES
#################NOT SURE WHAT TO DO HERE NOW
# pre.2000 <- merge(gbd.anc[year_id < 2000], unique(lbd.anc[,.(clinic,iso3)]), by.x= c('clinic','ihme_loc_id'), by.y=c('clinic','iso3'),all.x = TRUE)
# pre.2000[,c('site_pred', 'adm0_mean', 'adm0_lower', 'adm0_upper','adm1_mean', 'adm1_lower', 'adm1_upper') := NA]
# pre.2000[,c("iso3_adm1" ,  "loc_id_adm1") := NA]

## Post 2000 merge on site-years
gbd.anc$year_id <- as.integer(gbd.anc$year_id)
post.2000 <- merge(gbd.anc[year_id >= 2000], lbd.anc, by.x = c('year_id', 'clinic', 'ihme_loc_id'), by.y = c('year_id', 'clinic','iso3'),all.x = TRUE)
post.2000[,c('latitude','longitude') := NULL]

setdiff(colnames(post.2000),colnames(pre.2000))
#both.dt <- rbind(pre.2000, post.2000, use.names = T)
both.dt <- rbind( post.2000, use.names = T)
both.dt[,clinic_match := NULL]

both.dt <- unique(both.dt)

###FINAL CHECK for site names
setdiff(site.dat$site,both.dt$clinic)

#######################################
######   Check for duplicates    ######
#######################################
split.list <- split(both.dt,both.dt$ihme_loc_id)
lapply(split.list,function(x) aggregate(year_id ~ clinic, data = x, function(x) x[duplicated(x)]))

##Save files out at the estimation level
lapply(split.list, function(x) write.csv(x, paste0('/homes/mwalte10/hiv_gbd2019/lbd_anc_align/offsets/', loc, '_ANC_matched.csv'), row.names = F))

##Figure out quantiles for capping 
all.dat <- rbindlist(lapply(list.files(paste0("/homes/mwalte10/hiv_gbd2019/lbd_anc_align/offsets/"),pattern=".csv"),function(file){
  gg <- fread(paste0("/homes/mwalte10/hiv_gbd2019/lbd_anc_align/offsets/",file))
  gg[,offset := qnorm(adm0_mean)-qnorm(site_pred)]
  return(gg)
}),fill=TRUE)

all.dt <- all.dat[!is.na(offset)]
anc_site_dat.colnames <- c('site', 'year', 'used', 'prev','n' ,'subpop','type' ,'agegr' ,'age','agspan','offset', 'country')
setnames(all.dt, 'year_id', 'year')
setnames(all.dt, 'ihme_loc_id', 'country')
setnames(all.dt, 'mean', 'prev')
setnames(all.dt, 'clinic', 'site')
all.dt[,'lbd_areal' := NULL]
all.dt[,'lbd_missing' := NULL]
all.dt[,'iso3_adm1' := NULL]
all.dt[,'loc_id_adm1' := NULL]
all.dt[,'adm0_mean' := NULL]
all.dt[,'adm0_lower' := NULL]
all.dt[,'adm0_upper' := NULL]
all.dt[,'adm1_mean' := NULL]
all.dt[,'adm1_lower' := NULL]
all.dt[,'adm1_upper' := NULL]
all.dt[,'used' := TRUE]


save(all.dt, file = paste0('/share/hiv/data/PJNZ_EPPASM_prepped_subpop/lbd_anc/offset/', loc, '.rds'))
quantile(all.dt$offset,c(0.15,0.85))























































