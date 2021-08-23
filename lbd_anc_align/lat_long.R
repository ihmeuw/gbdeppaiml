## ---------------------------
## Script name: lat_long.R
## Purpose of script: Creates latitude and longitude table to geo-match ANC sites
##
## Author: Maggie Walters
## Date Created: 2021-08-23
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
libs <- c("data.table", "sp", "spatialEco")
sapply(libs, require, character.only = T)
out.dir <- '/ihme/homes/mwalte10/'

##load in GBD central shape file 
sf <- st_read('/home/j/DATA/SHAPE_FILES/GBD_geographies/master/GBD_2019/master/shapefiles/GBD2019_analysis_final.shp')

##subset to possible countries
poss <- loc.table[most_detailed == 1,ihme_loc_id]
poss <- poss[c(grep('ETH', poss), grep('NGA', poss), grep('KEN', poss))]
sf <- subset(sf, ihme_lc_id %in% poss)

##read in ANC data cleaned by LBD
anc_dat <- as.data.table(readRDS('/home/j/WORK/11_geospatial/10_mbg/hiv/unaids_anc/anc_data_2020_01_23.rds')) 
anc_dat <- subset(anc_dat, country %in% c("COM", "ETH", "KEN", "MAR", "MRT", "NGA", "ZAF") & point == 1)
coordinates(anc_dat) <- ~longitude+latitude
raster::crs(anc_dat) <- "+proj=longlat +datum=WGS84 +no_defs"

results <- point.in.poly(anc_dat, sf)
lat_long_table <- cbind(as.character(attr(results, 'data')$ihme_lc_id), coordinates(anc_dat))
write.csv(lat_long_table, file =  paste0(out.dir, 'lat_long_codetable.csv'), row.names = F)
