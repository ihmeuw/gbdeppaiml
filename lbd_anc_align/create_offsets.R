## ---------------------------
## Script name: create_offsets.R
## Purpose of script:Creates offsets that get pulled into read_spec_object
##
## Author: Maggie Walters
## Date Created: 2020-10-12
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

source(paste0(ifelse(windows, "H:", paste0("/ihme/homes/", user)), "/gbdeppaiml/gbd/00_req_packages.R"))

args <- commandArgs(trailingOnly = TRUE)
print(args)
if(length(args) == 0){
  run.name <- '210408_antman'
}else{
  run.name <- args[1]
}

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


loc.table <- data.table(get_locations(hiv_metadata = T))

### Code
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
dt <- readRDS(paste0(anc_no_offset, loc, '.rds'))

if(loc == 'GNQ'){
  dt[subpop == 'Guinea Ecuatorial', subpop := loc]
}

anc.dt <- dt %>% data.table()
anc.dt$source <- "GBD"
anc.dt$site_year <- paste0(anc.dt$site,anc.dt$year)
site.dat.list <- anc.dt
years <- site.dat.list$year;  ihme_loc_id <- loc
gbd.anc.all  <- data.table(unique(site.dat.list))
if(!grepl('ZAF', loc)){
  gbd.anc.all[,high_risk := FALSE]
  gbd.anc.all[!subpop %in% c(loc,gen.pop.dict, 'Urban', 'Rural', 'Urbaine', 'Rurale', 'TOTAL', 'POPULATION TOTALE'),high_risk := TRUE] 
  gbd.anc.all[is.na(subpop),high_risk:=FALSE]
  #Remove high risk data to complete LBD matching (but bind it later)
  gbd.anc <- gbd.anc.all[high_risk==FALSE]
}else{
  gbd.anc.all[,high_risk := FALSE]
  gbd.anc <- gbd.anc.all[high_risk==FALSE]
  
}

######################################
######  Loc specific  fixes     ######
######################################
if (loc=="RWA"){
  gbd.anc$site <- gsub("\\\"","",gbd.anc$site)
}

gbd.anc[,lbd_areal := 0]
gbd.anc[,lbd_missing := 0]

if(loc1=="NGA"){
  gbd.anc$site[gbd.anc$site %in% gbd_diff] <- gsub(substr(gbd_diff,1,1),'',gbd_diff) 
  gbd_diff <- setdiff(unique(gbd.anc$site), unique(lbd.anc$site))
}

if(nrow(gbd.anc) != nrow(gbd.anc.all)){
  gbd.anc.all[,c("lbd_missing","lbd_areal") := NA]
  gbd.anc <- rbind(gbd.anc,gbd.anc.all[high_risk==TRUE])
  if(nrow(gbd.anc) != nrow(gbd.anc.all)){stop("gen pop + high risk matrix size mismatch")}
}


lbd.anc <- readRDS(paste0('/share/hiv/data/PJNZ_EPPASM_prepped_subpop/lbd_anc/lbd_data_cleaned/',loc, '.RDS'))

setnames(gbd.anc, 'site','clinic')
if('group' %in% colnames(lbd.anc)){
  setnames(lbd.anc, 'group','subpop')
  
}
gbd.anc[, age := 15]
gbd.anc[, agegr := '15-49']
gbd.anc[, agspan := 35]
merge_on <- intersect(colnames(gbd.anc), colnames(lbd.anc))
merge_on <- merge_on[which(merge_on != 'source')]
merge_on <- merge_on[which(merge_on != 'prev')]
lbd.anc[,prev:=NULL]
gbd.anc[,source := NULL]
lbd.anc[,source := NULL]


if(!grepl(loc, 'ZAF')){
  both.dt <- list()
  for(subpop.x in unique(gbd.anc[,subpop])){
    pre.2000 <- merge(gbd.anc[subpop == subpop.x & year < 2000,], unique(lbd.anc[subpop == subpop.x,]), by= merge_on, all.x = TRUE)
    pre.2000[,c('adm1_mean', 'adm1_lower', 'adm1_upper') := NULL]
    pre.2000[,c("iso3_adm1" ,  "loc_id_adm1") := NA]
    
    ## Post 2000 merge on site-years
    post.2000 <- merge(gbd.anc[subpop == subpop.x & year >= 2000], unique(lbd.anc[subpop == subpop.x,]), by = merge_on, all.x = TRUE)
    post.2000[,c('latitude','longitude') := NULL]
    post.2000[,c('adm1_mean', 'adm1_lower', 'adm1_upper') := NULL]
    
    
    setdiff(colnames(post.2000),colnames(pre.2000))
    both.dt.sp <- rbind(pre.2000, post.2000, use.names = T, fill = T)
    both.dt.sp <- unique(both.dt.sp)
    both.dt <- rbind(both.dt, both.dt.sp)
  }
}else{
  both.dt <- list()
  lbd.anc[,age := as.numeric(age)]
  lbd.anc[,agspan := as.numeric(agspan)]
  
    pre.2000 <- merge(gbd.anc[year < 2000,], unique(lbd.anc), by= merge_on, all.x = TRUE)
    pre.2000[,c('adm1_mean', 'adm1_lower', 'adm1_upper') := NULL]
    pre.2000[,c("iso3_adm1" ,  "loc_id_adm1") := NA]
    
    ## Post 2000 merge on site-years
    post.2000 <- merge(gbd.anc[ year >= 2000], unique(lbd.anc), by = merge_on, all.x = TRUE)
    post.2000[,c('latitude','longitude') := NULL]
    post.2000[,c('adm1_mean', 'adm1_lower', 'adm1_upper') := NULL]
    
    
    setdiff(colnames(post.2000),colnames(pre.2000))
    both.dt.sp <- rbind(pre.2000, post.2000, use.names = T, fill = T)
    both.dt.sp <- unique(both.dt.sp)
    both.dt <- rbind(both.dt, both.dt.sp)

}






both.dt <- rbind(both.dt, gbd.anc[clinic %in% setdiff(gbd.anc$site,both.dt$clinic),],fill = T)

###FINAL CHECK for site names
setnames(both.dt, 'year', 'year_id')
if(any(colnames(both.dt) == 'subpop')){
  both.dt <- both.dt[,.( year_id, used, prev, n, clinic, subpop, type, agegr, age, agspan, offset, ihme_loc_id, high_risk, site_pred, adm0_mean, adm0_lower, adm0_upper)]
  
}else{
  both.dt <- both.dt[,.( year_id, used, prev, n, clinic, type, agegr, age, agspan,ihme_loc_id, high_risk, site_pred, adm0_mean, adm0_lower, adm0_upper)]
  
}

#######################################
######   Check for duplicates    ######
#######################################
split.list <- split(both.dt,both.dt$ihme_loc_id)
lapply(split.list,function(x) aggregate(year_id ~ clinic, data = x, function(x) x[duplicated(x)]))

##Save files out at the estimation level
lapply(split.list, function(x) write.csv(x, paste0(anc_offset, loc, '_ANC_matched.csv'), row.names = F))

##Figure out quantiles for capping 
all.dat <- rbindlist(lapply(list.files(paste0(anc_offset),pattern=".csv"),function(file){
  gg <- fread(paste0(anc_offset,file))
  gg[,offset := qnorm(adm0_mean)-qnorm(site_pred)]
  return(gg)
}),fill=TRUE)

all.dt <- as.data.table(split.list[[1]])
all.dt[,'used' := TRUE]

setnames(all.dt, 'clinic', 'site')
all.dt <- unique(all.dt)
saveRDS(all.dt, file = paste0(anc_offset, loc, '.rds'))
print(loc)
}

