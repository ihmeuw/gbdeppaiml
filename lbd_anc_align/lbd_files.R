## ---------------------------
## Script name: 
## Purpose of script:
##
## Author: Maggie Walters
## Date Created: 2018-04-11
## Email: mwalte10@uw.edu
## ---------------------------
##
## Notes: this should only need to be run once. formats the lbd data to be matched with the lbd data. 
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
code.dir <- paste0(ifelse(windows, "H:", paste0("/ihme/homes/", user)), "/hiv_gbd2019/")
input_table <- fread(paste0(ifelse(windows, "H:", paste0("/ihme/homes/", user)), '/gbdeppaiml/lbd_anc_align/inputs.csv'))
c.args <- input_table[run_name==run.name]
run_name <- run.name
lbd.anc <- c.args[['lbd.anc']]
anc_no_offset <- c.args[['anc_no_offset']]
anc_offset <- c.args[['anc_offset']]
lbd_anc_data <- c.args[['lbd_anc_data']]
codetable <- c.args[['codetable']]
codetable <- paste0(ifelse(windows, "H:", paste0("/ihme/homes/", user)), codetable)
add_info <- c.args[['add_info']]
add_info <- paste0(ifelse(windows, "H:", paste0("/ihme/homes/", user)), add_info)
geo_codebook <- c.args[['geo_codebook']]
geo_codebook <- paste0(ifelse(windows, "H:", paste0("/ihme/homes/", user)), geo_codebook)
UNAIDS_year <- 2020
geo_repository <- c.args[['geo_repository']]
sf_dir <- c.args[['sf_dir']]
lbd_core <- c.args[['lbd_core']]
lbd_core <- paste0(ifelse(windows, "H:", paste0("/ihme/homes/", user)), lbd_core)
areal_sites <- c.args[['areal_sites']]
rd <- c.args[['rd']]
lbd_anc_mean_est <- c.args[['lbd_anc_mean_est']]



save_out_shapefiles <- FALSE
covs <- "hiv_test"
measures <- "mean"
agg_method = "pop_weight"
shapefile_field <- "GAUL_CODE"

library("slackr",lib.loc =paste0("/homes/", user,"/rlibs/"))
library(mortdb, lib = "/home/j/WORK/02_mortality/shared/r")
loc.table <- get_locations(hiv_metadata = T)

source(paste0(code.dir,"/geo_extract_functions.R"))
library(data.table); library(rgdal)

##File paths
out_dir_areal <- paste0(root, areal_sites)
geo_repository <- paste0(ifelse(windows, "H:", paste0("/homes/", user)), geo_repository)
##comes from https://stash.ihme.washington.edu/projects/GEOSP/repos/lbd_hiv/browse/data/anc/2_geomatching/final_geo_codebook_2_12_2020.csv
geo_repository <- paste0(geo_repository, "/anc/2_geomatching/final_geo_codebook.csv")
core_repo = paste0("/homes/",user,"/lbd_core/")
shapefile_directory <- paste0(root,sf_dir)
out_dir_geo <- "/ihme/hiv/data/shapefiles/"

lbd_map <- fread(paste0(geo_repository))
lbd_map <- merge(lbd_map,loc.table[,.(ihme_loc_id,location_name)],by.x="iso3",by.y="ihme_loc_id",all.x=TRUE)
loc.list <- unique(lbd_map$iso3)

if(rd != '/share/geospatial/mbg/hiv/hiv_test/output/2019_02_26_30_11_35/'){
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
      write.csv(agg_covs,paste0('/homes/mwalte10/hiv_gbd2019/lbd_anc_align/areal_sites/',loc,".csv"),row.names = FALSE)
      
    }
    
  }
  
}


### Code
loc.table <- data.table(get_locations(hiv_metadata = T))
epp.list <- sort(loc.table[epp == 1 & grepl('1', group), ihme_loc_id])
loc.list <- setdiff(epp.list, c('ETH_44859', 'HTI','PNG', 'GNQ'))
loc.list <- loc.list[!grepl('IND', loc.list)]
loc.list <- loc.list[!grepl('ZAF', loc.list)]

for(loc in loc.list){
  gen.pop.dict <- c("General Population", "General population", "GP", 
                    "GENERAL POPULATION", "GEN. POPL.", "General population(Low Risk)", 'Pop restante',
                    "Remaining Pop", "population feminine restante","Pop féminine restante","Rift Valley", 
                    "Western","Eastern","Central","Coast","Nyanza","Nairobi",
                    "Female remaining pop", 'Urbain')
  
  new.anc <- readRDS(lbd_anc_data)
  loc1 <- substring(loc,1,3)
  if(grepl('ETH', loc) | grepl('KEN', loc) | grepl('NGA', loc) ){
    countr <- unlist(strsplit(loc, split = '_'))[1]
    new.anc <- as.data.table(new.anc)[country == countr,]
    full_geo <- fread(paste0(geo_repository))[iso3==countr]
    full_geo[,data_source := paste0("UNAIDS files - ",UNAIDS_year)]
    full_geo[,ihme_loc_id := paste0(iso3, '_', subnational)]
  }else{
    new.anc <- setDT(new.anc)[country==loc]
    full_geo <- fread(paste0(geo_repository))[iso3==loc]
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
  
  
  ##Merge with source
 lbd.anc <- read.csv(lbd_anc_mean_est) %>% data.table()
  if(grepl('ETH', loc) | grepl('KEN', loc) | grepl('NGA', loc)){
    if(grepl('KEN', loc)){
      lbd.anc <- lbd.anc
    }else{
      lbd.anc <- lbd.anc[iso3_adm1 == loc]
    }
  }else{
    lbd.anc <- lbd.anc[iso3==loc]
  }
  if(loc == 'ZMB'){
    lbd.anc <- lbd.anc[is.na(iso3_adm1)]
  }
  if(loc == 'MDG'){
    sites <- gsub("\\(%)", '', as.character(lbd.anc$site))
    sites <- gsub(' ', '',sites)
    lbd.anc$site <- sites
    all.dat$site <- gsub(' ', '',all.dat$site)
    
  }
  if(loc == 'UGA'){
    sites <- gsub("\\(%)", '', as.character(lbd.anc$site))
    sites <- strsplit(sites, split = ' ')
    for(x in 1:length(sites)){
      sites[x] <-  paste0(sites[[x]][which(sites[[x]] != '')], collapse =' ')
    }
    sites <- unlist(sites)
    sites[1] <- 'Kamwenge HC'
    lbd.anc$site <- sites
    
  }
  all.dat <- merge(all.dat,lbd.anc,by = c('site', 'year'),all.x=TRUE)
  if(grepl('ETH', loc) | grepl('KEN', loc) | grepl('NGA', loc) | grepl('ZAF', loc)){
    if(grepl('KEN', loc) | grepl('ZAF', loc)){
      all.dat <- all.dat[,iso3 := loc]
      
    }else{
      all.dat <- all.dat[,iso3 := iso3_adm1]
      
    }
  }
  
  lbd.anc <- all.dat
  print('here_1')
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
  
  if(loc == 'ZAF'){
    lbd.anc$site[lbd.anc$iso3 == "ZAF" & lbd.anc$site =="NULL"] <- "KWAZULU-NATAL PROVINCE"
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
  
  
  if(loc == 'RWA' | loc == 'TZA'){
    lbd.anc <- subset(lbd.anc, !(iso3 == "RWA" & site == "Gikondo A"& year != 2013))
    lbd.anc$site[lbd.anc$iso3 == "RWA"& lbd.anc$site == "Gikondo A"] <- "Gikondo"
    lbd.anc <- subset(lbd.anc, !(iso3 == "RWA" & site == "Biryogo A"& year != 2013))
    lbd.anc$site[lbd.anc$iso3 == "RWA"& lbd.anc$site == "Biryogo A"] <- "Biryogo"
    lbd.anc$site[lbd.anc$iso3 == "RWA"& lbd.anc$site == "CHK"] <- "Kigali CHK"
    lbd.anc$site[lbd.anc$iso3 == "RWA"& lbd.anc$site == "Ruhengeri 2"] <- "Ruhengeri"
    lbd.anc$site[lbd.anc$iso3 == "TZA"& lbd.anc$site == "Kagera (Lukole Refugee Camp)"] <- "Lukole Refugee"
    
  }
  
  if(loc1=="NGA"){
    lbd.anc[site=="Osun (Iragbere)",site := "Osun  (Iragbere)"]
  }
  
  
  ##ID missingness from LBD
  lbd.anc <- lbd.anc[!is.na(site_pred)]
  setnames(lbd.anc, c('site'), c('clinic'))
  lbd.anc <- unique(lbd.anc)
  lbd.anc[,c('loc_id', 'subnational', 'country') := NULL]
  lbd.anc[,site_year := paste0(clinic, year)]
  lbd.anc[,prev:=NULL]
  lbd.anc[,source := NULL]
  
  dir.create('/share/hiv/data/PJNZ_EPPASM_prepped_subpop/lbd_anc/lbd_data_cleaned/')
  saveRDS(lbd.anc, paste0('/share/hiv/data/PJNZ_EPPASM_prepped_subpop/lbd_anc/lbd_data_cleaned/',loc, '.RDS'))
}
