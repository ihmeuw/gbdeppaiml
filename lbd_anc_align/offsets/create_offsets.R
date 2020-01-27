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
packages <- c("data.table","sp", 'sf', "stringr", 'tidyverse', 'lwgeom', 'doParallel')
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
# Load cell pred object - DO THIS OUTSIDE THE FUNCTION
message("Load cell pred object")
rd <- "2019_02_26_30_11_35"
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
    shapefile_path <- paste0(shapefile_directory, shp,".shp")
    
    if (is.na(shp) | !file.exists(shapefile_path)){
      print(paste0("no geofile for ",loc))
      next
    }
    
    #shp <- shp_path[1]
    
    
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
run.name <- '191224_trumpet'
stop.year <- 2022
j <- 1
anc.sub <- FALSE

for(loc in loc.list[119:length(loc.list)]){
gen.pop.dict <- c("General Population", "General population", "GP", 
                  "GENERAL POPULATION", "GEN. POPL.", "General population(Low Risk)", 
                  "Remaining Pop", "population feminine restante","Pop féminine restante","Rift Valley", 
                  "Western","Eastern","Central","Coast","Nyanza","Nairobi",
                  "Female remaining pop")

if(grepl("ZAF",loc) | grepl("IND",loc)){
  dt <- readRDS(paste0('/share/hiv/data/PJNZ_EPPASM_prepped/', loc, '.rds'))
} else {
  dt <- readRDS(paste0('/share/hiv/data/PJNZ_EPPASM_prepped_subpop/lbd_anc/2019/', loc, '.rds'))
}

anc.dt <- dt %>% data.table()

new.anc <- readRDS("/home/j/WORK/11_geospatial/10_mbg/hiv/unaids_anc/anc_data_2020_01_23.rds")
loc1 <- substring(loc,1,3)
if(grepl('ETH', loc) | grepl('KEN', loc) | grepl('NGA', loc)){
  country <- unlist(strsplit(loc, split = '_'))[1]
  new.anc <- setDT(new.anc)[country==country]
  full_geo <- fread(paste0(geo_repository,"/anc/2_geomatching/final_geo_codebook.csv"))[iso3==country]
  full_geo[,data_source := paste0("UNAIDS files - ",UNAIDS_year)]
}else{
  new.anc <- setDT(new.anc)[country==loc]
  full_geo <- fread(paste0(geo_repository,"/anc/2_geomatching/final_geo_codebook.csv"))[iso3==loc]
  full_geo[,data_source := paste0("UNAIDS files - ",UNAIDS_year)]
}
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


lbd.anc <- read.csv("/snfs1/WORK/04_epi/01_database/02_data/hiv/04_models/gbd2015/02_inputs/lbd_extraction/04_04_2019_extraction/anc_mean_est_final.csv") %>% data.table()
if(grepl('ETH', loc) | grepl('KEN', loc) | grepl('NGA', loc)){
  lbd.anc <- lbd.anc[iso3_adm1 == loc]
}else{
  lbd.anc <- lbd.anc[iso3==loc]
}
all.dat <- merge(all.dat,lbd.anc,all.x=TRUE)
if(grepl('ETH', loc) | grepl('KEN', loc) | grepl('NGA', loc)){
  all.dat <- all.dat[,iso3 := iso3_adm1]
}

#setnames(all.dat, 'N', 'n')



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
# for(loc in sub_locs){
#   print(loc)
# 
#   dt <- readRDS(paste0('/share/hiv/data/PJNZ_EPPASM_prepped_subpop/lbd_anc/2019/', loc, '.rds'))
#   site.dat.subpop <- list()
# 
# 
#   gen.pop.dict <- c("General Population", "General population", "GP",
#                     "GENERAL POPULATION", "GEN. POPL.", "General population(Low Risk)",
#                     "Remaining Pop", "population feminine restante","Pop féminine restante","Rift Valley",
#                     "Western","Eastern","Central","Coast","Nyanza","Nairobi",
#                     "Female remaining pop")
# 
# 
#   #Formatting
#     #site.dat <- attr(dt[[subpop]], "eppd")$anc.prev
#     site.dat.subpop <- dt
# 
# 
#   site.dat.list <- rbind(dt,site.dat.list)
#   loc_ids <- rbind(loc_ids,matrix(rep(loc,nrow(site.dat.subpop)),ncol=1))
# 
# }

site.dat.list <- anc.dt


years <- site.dat.list$year;  ihme_loc_id <- loc
gbd.anc.all  <- data.table(unique(site.dat.list))


##Flag high risk data that will not get matched to LBD data
gbd.anc.all[,high_risk := FALSE]
gbd.anc.all[!subpop %in% c(loc,sub_locs,gen.pop.dict, 'Urban', 'Rural'),high_risk := TRUE] 
gbd.anc.all[is.na(subpop),high_risk:=FALSE]
#Remove high risk data to complete LBD matching (but bind it later)
gbd.anc <- gbd.anc.all[high_risk==FALSE]

lbd.anc <- all.dat
#######################################
######  General character fixes  ######
#######################################
lbd.anc$site <- gsub("&apos;",'\'',lbd.anc$site) 
lbd.anc$site <- gsub("\xe9","é", lbd.anc$site)



######################################
######  Loc specific  fixes     ######
######################################

if(loc == "LBR"){
  lbd.anc$site[lbd.anc$iso3 == "LBR" & lbd.anc$site == "Barclay.Health.Center"] <- "Barclay Health Center"
  lbd.anc$site[lbd.anc$iso3 == "LBR" & lbd.anc$site == "C.B..Dunbar.Health.Center...."] <- "C.B. Dunbar Health Center (%)"
  lbd.anc$site[lbd.anc$iso3 == "LBR" & lbd.anc$site == "C.H..Rennie.Hospital...."] <- "C.H. Rennie Hospital (%)"
  lbd.anc$site[lbd.anc$iso3 == "LBR" & lbd.anc$site == "Duport.Road.Health.Center...."] <- "Duport Road Health Center (%)"
  lbd.anc$site[lbd.anc$iso3 == "LBR" & lbd.anc$site == "Firestone.Hospital"] <- "Firestone Hospital"
  lbd.anc$site[lbd.anc$iso3 == "LBR" & lbd.anc$site == "Fish.Town.Health.Center"] <- "Fish Town Health Center"
  lbd.anc$site[lbd.anc$iso3 == "LBR" & lbd.anc$site == "Ganta.Hospital"] <- "Ganta Hospital"
  lbd.anc$site[lbd.anc$iso3 == "LBR" & lbd.anc$site == "JJ.Dossen.Hospital"] <- "JJ Dossen Hospital"
  lbd.anc$site[lbd.anc$iso3 == "LBR" & lbd.anc$site == "John.F.Kennedy.Hospital"] <- "John F Kennedy Hospital"
  lbd.anc$site[lbd.anc$iso3 == "LBR" & lbd.anc$site == "Liberia.Govt..Hospital.Tubmanburg"] <- "Liberia Govt. Hospital-Tubmanburg"
  lbd.anc$site[lbd.anc$iso3 == "LBR" & lbd.anc$site == "Liberia.Govt.Hosp.Buchanan"] <- "Liberia Govt Hosp-Buchanan"
  lbd.anc$site[lbd.anc$iso3 == "LBR" & lbd.anc$site == "Martha.Tubman.Hospital"] <- "Martha Tubman Hospital"
  lbd.anc$site[lbd.anc$iso3 == "LBR" & lbd.anc$site == "Phebe.Hospital"] <- "Phebe Hospital"
  lbd.anc$site[lbd.anc$iso3 == "LBR" & lbd.anc$site == "Redemption.Hospital"] <- "Redemption Hospital"
  lbd.anc$site[lbd.anc$iso3 == "LBR" & lbd.anc$site == "site.22"] <- "site 22"
  lbd.anc$site[lbd.anc$iso3 == "LBR" & lbd.anc$site == "St.Francis.Hospital"] <- "St Francis Hospital"
  lbd.anc$site[lbd.anc$iso3 == "LBR" & lbd.anc$site == "Star.of.the.Sea.Health.Center"] <- "Star of the Sea Health Center"
  lbd.anc$site[lbd.anc$iso3 == "LBR" & lbd.anc$site == "Voinjama.Health.Center"] <- "Voinjama Health Center"
}
if(loc == "MDG"){
  lbd.anc$site[lbd.anc$iso3 == "MDG" & lbd.anc$site == "Miarinarivo (%)"] <- "Miarinarivo (%)" # niot working
  lbd.anc$site[lbd.anc$iso3 == "MDG" & lbd.anc$site == "Sambava  (%)"] <- "Sambava (%)"
  lbd.anc$site[lbd.anc$iso3 == "MDG" & lbd.anc$site == "Antsirabe  (%)"] <- "Antsirabe (%)"
}
if(loc == "NGA"){
  lbd.anc$site[lbd.anc$iso3 == "NGA" & lbd.anc$site == "Osun  (Iragbere)"] <- "Osun (Iragbere)"
}
if(loc == "ZAF"){
  lbd.anc$site[lbd.anc$iso3 == "ZAF" & lbd.anc$site == "NULL"] <- "KWAZULU-NATAL PROVINCE" 
  
}
if(loc == "COG"){
  lbd.anc$site[lbd.anc$iso3 == "COG" & lbd.anc$site == "Cuvette.ouest"] <- "Cuvette ouest"
  lbd.anc$site[lbd.anc$iso3 == "COG" & lbd.anc$site == "Mouyondzi...."] <- "Mouyondzi (%)"
  lbd.anc$site[lbd.anc$iso3 == "COG" & lbd.anc$site == "Owando...."] <- "Owando (%)"
  lbd.anc$site[lbd.anc$iso3 == "COG" & lbd.anc$site == "Sibiti...."] <- "Sibiti (%)"
}
if(loc == "CMR"){
  lbd.anc$site[lbd.anc$iso3 == "CMR" & lbd.anc$site == "CMA  Tyo"] <- "CMA Tyo"
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

if(loc == 'ZAF'){lbd.anc$site[lbd.anc$iso3 == "ZAF" & lbd.anc$site =="NULL"] <- "KWAZULU-NATAL PROVINCE"
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
  gbd.anc$site <- gsub("\\\"","",gbd.anc$site)
}

if(loc == 'RWA' | loc == 'TZA'){
  lbd.anc <- subset(lbd.anc, !(iso3 == "RWA" & site == "Gikondo A"& year != 2013))
  lbd.anc$site[lbd.anc$iso3 == "RWA"& lbd.anc$site == "Gikondo A"] <- "Gikondo"
  lbd.anc <- subset(lbd.anc, !(iso3 == "RWA" & site == "Biryogo A"& year != 2013))
  lbd.anc$site[lbd.anc$iso3 == "RWA"& lbd.anc$site == "Biryogo A"] <- "Biryogo"
  lbd.anc$site[lbd.anc$iso3 == "RWA"& lbd.anc$site == "CHK"] <- "Kigali CHK"
  lbd.anc$site[lbd.anc$iso3 == "RWA"& lbd.anc$site == "Ruhengeri 2"] <- "Ruhengeri"
  lbd.anc$site[lbd.anc$iso3 == "TZA"& lbd.anc$site == "Kagera (Lukole Refugee Camp)"] <- "Lukole Refugee"

}

#Check for differences
gbd_diff <- setdiff(unique(gbd.anc$site), unique(lbd.anc$site))
#setdiff(unique(lbd.anc$site), unique(gbd.anc$site))
gbd.anc[,lbd_areal := 0]
gbd.anc[,lbd_missing := 0]

if(loc1=="NGA" & length(gbd_diff) != 0){
  gbd.anc$site[gbd.anc$site %in% gbd_diff] <- gsub(substr(gbd_diff,1,1),'',gbd_diff) 
  lbd.anc[site=="Osun (Iragbere)",site := "Osun  (Iragbere)"]
  gbd_diff <- setdiff(unique(gbd.anc$site), unique(lbd.anc$site))
}

##ID areal data from original geo codebook (which is usually why GBD sites > LBD sites; the areal data gets dropped when original geo codebook merged)
# if(length(gbd_diff) > 0){
#   full_geo <- fread(paste0(geo_repository,"/anc/2_geomatching/final_geo_codebook.csv"))[iso3==loc]
#   areal <- full_geo[site %in% gbd_diff & is.na(latitude)]
#   remaining_diff <- gbd_diff[!gbd_diff %in% areal$site]
#   if(length(remaining_diff) > 0){
#     warning("Remaining Differences: ",paste0(remaining_diff))
#   }
#   gbd.anc[site %in% areal$site, lbd_areal := 1]
#   gbd.anc[site %in% remaining_diff, lbd_missing := 1]
#   #write.csv(areal, paste0('/snfs1/WORK/04_epi/01_database/02_data/hiv/04_models/gbd2015/02_inputs/lbd_extraction/04_04_2019_extraction/areal_sites/',loc, '.csv'), row.names=FALSE)
#   
# }

##ID missingness from LBD
missing <- lbd.anc[is.na(site_pred) & site %in% gbd.anc$site]


##Add back subpopulations
if(nrow(gbd.anc) != nrow(gbd.anc.all)){
  gbd.anc.all[,c("lbd_missing","lbd_areal") := NA]
  gbd.anc <- rbind(gbd.anc,gbd.anc.all[high_risk==TRUE])
  if(nrow(gbd.anc) != nrow(gbd.anc.all)){stop("gen pop + high risk matrix size mismatch")}
}

###############################
######  Combined data    ######
###############################
setnames(lbd.anc, c('site'), c('clinic'))


lbd.anc <- unique(lbd.anc)

if(length(unique(lbd.anc$iso3)) > 1){
  lbd.anc <- lbd.anc[!is.na(iso3_adm1)]
}

lbd.anc[,c('loc_id', 'subnational', 'country') := NULL]
lbd.anc[,site_year := paste0(clinic, year)]
## Pre 2000 we don't have any LBD estimates, so we just merge on the clinic metadata
####NOTE, I TOOK OUT THE MATCH ON LATITUDE AND LONGITUDE HERE BECAUSE IT CAN BE MISSING OR MULTIPLE VALUES
#################NOT SURE WHAT TO DO HERE NOW
setnames(gbd.anc, 'site','clinic')
if('group' %in% colnames(lbd.anc)){
  setnames(lbd.anc, 'group','subpop')
  
}
pre.2000 <- merge(gbd.anc[year < 2000], unique(lbd.anc), by= c('site_year', 'year', 'subpop','clinic', 'n'), all.x = TRUE)
pre.2000[,c('adm1_mean', 'adm1_lower', 'adm1_upper') := NULL]
pre.2000[,c("iso3_adm1" ,  "loc_id_adm1") := NA]

## Post 2000 merge on site-years
post.2000 <- merge(gbd.anc[year >= 2000], lbd.anc, by = c('site_year', 'year','subpop','clinic','n'), all.x = TRUE)
post.2000[,c('latitude','longitude') := NULL]
post.2000[,c('adm1_mean', 'adm1_lower', 'adm1_upper') := NULL]


setdiff(colnames(post.2000),colnames(pre.2000))
both.dt <- rbind(pre.2000, post.2000, use.names = T, fill = T)

both.dt <- unique(both.dt)
both.dt <- rbind(both.dt, gbd.anc[clinic %in% setdiff(gbd.anc$site,both.dt$clinic),],fill = T)

###FINAL CHECK for site names
setdiff(gbd.anc$clinic,both.dt$clinic)
setnames(both.dt, 'year', 'year_id')
both.dt <- both.dt[,.(site, year_id, used, prev, n, clinic, subpop, type, agegr, age, agspan, offset, ihme_loc_id, high_risk, site_pred, adm0_mean, adm0_lower, adm0_upper)]

#######################################
######   Check for duplicates    ######
#######################################
split.list <- split(both.dt,both.dt$ihme_loc_id)
lapply(split.list,function(x) aggregate(year_id ~ clinic, data = x, function(x) x[duplicated(x)]))

##Save files out at the estimation level
lapply(split.list, function(x) write.csv(x, paste0('/share/hiv/data/PJNZ_prepped/lbd_anc/2019/', loc, '_ANC_matched.csv'), row.names = F))

##Figure out quantiles for capping 
all.dat <- rbindlist(lapply(list.files(paste0("/share/hiv/data/PJNZ_prepped/lbd_anc/2019/"),pattern=".csv"),function(file){
  gg <- fread(paste0("/share/hiv/data/PJNZ_prepped/lbd_anc/2019/",file))
  gg[,offset := qnorm(adm0_mean)-qnorm(site_pred)]
  return(gg)
}),fill=TRUE)

all.dt <- as.data.table(split.list)
colnames(all.dt) <- gsub(paste0(loc, '.'),'',colnames(all.dat))
# setnames(all.dt, 'year_id', 'year')
# setnames(all.dt, 'ihme_loc_id', 'country')
all.dt[,'lbd_areal' := NULL]
all.dt[,'lbd_missing' := NULL]
all.dt[,'iso3_adm1' := NULL]
all.dt[,'loc_id_adm1' := NULL]
all.dt[,'adm1_mean' := NULL]
all.dt[,'adm1_lower' := NULL]
all.dt[,'adm1_upper' := NULL]
all.dt[,'used' := TRUE]


saveRDS(all.dt, file = paste0('/share/hiv/data/PJNZ_prepped/lbd_anc/2019/', loc, '.rds'))
}




















































