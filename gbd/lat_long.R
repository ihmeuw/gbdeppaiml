library(sf)
user <- ifelse(windows, Sys.getenv("USERNAME"), Sys.getenv("USER"))

h_root = paste0('/homes/', user)
lib.loc <- paste0(h_root,"R/",R.Version(),"/",R.Version(),".",R.Version())
dir.create(lib.loc,recursive=T, showWarnings = F)
.libPaths(c(lib.loc,.libPaths()))
##this takes a long time
packages <- c('spatialEco')
for(p in packages){
if(p %in% rownames(installed.packages())==FALSE){
install.packages(p)
}
library(p, character.only = T)
}

library(spatialEco)
sf <- st_read('/home/j/DATA/SHAPE_FILES/GBD_geographies/master/GBD_2019/master/shapefiles/GBD2019_analysis_final.shp')
poss <- loc.table[most_detailed == 1,ihme_loc_id]
poss <- poss[c(grep('ETH', poss), grep('NGA', poss), grep('KEN', poss))]
sf <- subset(sf, ihme_lc_id %in% poss)
anc_dat <- as.data.table(readRDS('/home/j/WORK/11_geospatial/10_mbg/hiv/unaids_anc/anc_data_2020_01_23.rds')) 
anc_dat <- subset(anc_dat, country %in% c("COM", "ETH", "KEN", "MAR", "MRT", "NGA", "ZAF") & point == 1)
coordinates(anc_dat) <- ~longitude+latitude
crs(anc_dat) <- "+proj=longlat +datum=WGS84 +no_defs"

results <- point.in.poly(anc_dat, sf)

lat_long_table <- cbind(as.character(attr(results, 'data')$ihme_lc_id), coordinates(anc_dat))
write.csv(lat_long_table, file =  paste0('/homes/', user, '/lat_long_codetable.csv'), row.names = F)
