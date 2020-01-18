rm(list=ls())
windows <- Sys.info()[1][["sysname"]]=="Windows"
root <- ifelse(windows,"J:/","/home/j/")
user <- ifelse(windows, Sys.getenv("USERNAME"), Sys.getenv("USER"))
code.dir <- paste0(ifelse(windows, "H:", paste0("/ihme/homes/", user)), "/hiv_gbd2019/")
date <- substr(gsub("-","",Sys.Date()),3,8)
output.dir <- '/ihme/hiv/data/PJNZ_EPPASM_prepped_subpop/lbd_anc/2019/'
dir.create(output.dir)
## Packages
library(data.table)





###easy locs, called keep
#load('/homes/mwalte10/lbd_anc_align/examine_first.RData')
loc.table <- get_locations(hiv_metadata = TRUE)
anc_dat <- as.data.table(readRDS('/home/j/WORK/11_geospatial/10_mbg/hiv/unaids_anc/anc_data_2019_08_13.rds')) 
keep <- unique(anc_dat$country)
keep <- cbind(keep, loc.table[ihme_loc_id %in% keep, unaids_recent])
colnames(keep) <- c('countries', 'years')
keep <- as.data.frame(keep)

library(mortdb, lib = '/ihme/mortality/shared/r')
loc.table <- get_locations(hiv_metadata = TRUE)
hiv_locs_nat <- loc.table[level == 3 & epp == 1, ihme_loc_id]
lat_long_table <- fread('/homes/mwalte10/lat_long_codetable.csv')
colnames(lat_long_table) <- c('ihme_loc_id','longitude', 'latitude')

tracking_sheet <- list()
nrow_sheet <- list()
  
libs <- c("data.table", "openxlsx", "raster", "foreign", "rgdal", "geosphere", "fossil", "dplyr", "rgeos", "car","plyr")
sapply(libs, require, character.only = T)

## Bind all UNAIDS Survaillance files together---------------------------------------------------------------------------------------------

additional <- read.csv("/ihme/homes/mwalte10/gbdeppaiml/lbd_anc_align/additional_info.csv")
## Merge Geography codebook and UNAIDS data -----------------------------------------------------------------------------------------------
geocodebook <- read.csv('/homes/mwalte10/gbdeppaiml/lbd_anc_align/final_geo_codebook.csv', stringsAsFactors = FALSE)
source('/homes/mwalte10/gbdeppaiml/lbd_anc_align/anc_snap.R')
geocodebook <- snap_codebook(geocodebook)

##added trim/whitespace code 
geocodebook$site <- gsub(' $',"",as.character(geocodebook$site))
geocodebook$site <- trimws(geocodebook$site)


additional <- merge(additional, geocodebook, by.x = 'Site',by.y = 'site')
additional <- as.data.table(additional)
additional <- additional[,.(Site, iso3, Prev, Year, Group, N, latitude, longitude, type_of_additional)]
additional <- merge(additional, lat_long_table, by = c('latitude', 'longitude'))
additional[,type:='ancss']

loc.table <- data.table(get_locations(hiv_metadata = T))

### Code
epp.list <- sort(loc.table[epp == 1 & grepl('1', group), ihme_loc_id])
loc.list <- epp.list

#We did not use EPP-ASM for India in GBD19, instead EPP + Spectrum
if(T){
  loc.list <- loc.list[!grepl("IND",loc.list)]
}
#################
#lbd only has 2017 for ZAF so going to use that


for (countries in loc.list) {

  if(file.exists(paste0('/share/hiv/data/PJNZ_prepped/2019/', countries, '.rds'))){
    df <- readRDS(paste0('/share/hiv/data/PJNZ_prepped/2019/', countries, '.rds'))
    
  }else{
    if(file.exists(paste0('/share/hiv/data/PJNZ_prepped/2018/', countries, '.rds'))){
      df <- readRDS(paste0('/share/hiv/data/PJNZ_prepped/2018/', countries, '.rds'))
      
    }else{
      if(file.exists(paste0('/share/hiv/data/PJNZ_EPPASM_prepped_subpop/', countries, '.rds'))){
        df <- readRDS(paste0('/share/hiv/data/PJNZ_EPPASM_prepped_subpop/', countries, '.rds'))
      }else{
        if(file.exists(paste0('/share/hiv/data/PJNZ_EPPASM_prepped/', countries, '.rds'))){
          df <- readRDS(paste0('/share/hiv/data/PJNZ_EPPASM_prepped/', countries, '.rds'))
        }
      }
    }
  }
  
  df <- attr(df, 'eppd')$ancsitedat
  #df <- subset(df, type == 'ancss')
  df$country <- countries
  df$ihme_loc_id <- countries
  
  nrow_1 <- nrow(df)
  
  #################################
  #change incorrect site names
  #################################
  if(countries == "LBR"){
    df$site[df$country == "LBR" & df$site == "Barclay.Health.Center"] <- "Barclay Health Center"
    df$site[df$country == "LBR" & df$site == "C.B..Dunbar.Health.Center...."] <- "C.B. Dunbar Health Center (%)"
    df$site[df$country == "LBR" & df$site == "C.H..Rennie.Hospital...."] <- "C.H. Rennie Hospital (%)"
    df$site[df$country == "LBR" & df$site == "Duport.Road.Health.Center...."] <- "Duport Road Health Center (%)"
    df$site[df$country == "LBR" & df$site == "Firestone.Hospital"] <- "Firestone Hospital"
    df$site[df$country == "LBR" & df$site == "Fish.Town.Health.Center"] <- "Fish Town Health Center"
    df$site[df$country == "LBR" & df$site == "Ganta.Hospital"] <- "Ganta Hospital"
    df$site[df$country == "LBR" & df$site == "JJ.Dossen.Hospital"] <- "JJ Dossen Hospital"
    df$site[df$country == "LBR" & df$site == "John.F.Kennedy.Hospital"] <- "John F Kennedy Hospital"
    df$site[df$country == "LBR" & df$site == "Liberia.Govt..Hospital.Tubmanburg"] <- "Liberia Govt. Hospital-Tubmanburg"
    df$site[df$country == "LBR" & df$site == "Liberia.Govt.Hosp.Buchanan"] <- "Liberia Govt Hosp-Buchanan"
    df$site[df$country == "LBR" & df$site == "Martha.Tubman.Hospital"] <- "Martha Tubman Hospital"
    df$site[df$country == "LBR" & df$site == "Phebe.Hospital"] <- "Phebe Hospital"
    df$site[df$country == "LBR" & df$site == "Redemption.Hospital"] <- "Redemption Hospital"
    df$site[df$country == "LBR" & df$site == "site.22"] <- "site 22"
    df$site[df$country == "LBR" & df$site == "St.Francis.Hospital"] <- "St Francis Hospital"
    df$site[df$country == "LBR" & df$site == "Star.of.the.Sea.Health.Center"] <- "Star of the Sea Health Center"
    df$site[df$country == "LBR" & df$site == "Voinjama.Health.Center"] <- "Voinjama Health Center"
  }
  if(countries == "MDG"){
    df$site[df$country == "MDG" & df$site == "Miarinarivo (%)"] <- "Miarinarivo (%)" # niot working
    df$site[df$country == "MDG" & df$site == "Sambava  (%)"] <- "Sambava (%)"
    df$site[df$country == "MDG" & df$site == "Antsirabe  (%)"] <- "Antsirabe (%)"
  }
  if(countries == "NGA"){
    df$site[df$country == "NGA" & df$site == "Osun  (Iragbere)"] <- "Osun (Iragbere)"
  }
  if(countries == "ZAF"){
    df$site[df$country == "ZAF" & df$site == "NULL"] <- "KWAZULU-NATAL PROVINCE" 
    
  }
  if(countries == "COG"){
    df$site[df$country == "COG" & df$site == "Cuvette.ouest"] <- "Cuvette ouest"
    df$site[df$country == "COG" & df$site == "Mouyondzi...."] <- "Mouyondzi (%)"
    df$site[df$country == "COG" & df$site == "Owando...."] <- "Owando (%)"
    df$site[df$country == "COG" & df$site == "Sibiti...."] <- "Sibiti (%)"
  }
  if(countries == "CMR"){
    df$site[df$country == "CMR" & df$site == "CMA  Tyo"] <- "CMA Tyo"
  }
  
  #################################
  #remove duplicate observations. GBD ALSO DOES THIS STEP
  #################################
  # {
  # nrow_before_dup <- nrow(df)
  # df_in <- subset(df, In == 1)
  # df_in <- unique(df_in)
  # df_not_in <- subset(df, In == 0)
  # df_not_in <- unique(df_not_in)
  # together <- rbind.fill(df_in, df_not_in)
  # together_dedupe <- unique(setDT(together)[order(Group,Site,Year, -In)], by = c('Group','Site','Year'))
  # df <- data.frame(together_dedupe)
  # df <- df[,!(names(df) %in% c("In"))]
  # nrow_after_dup <- nrow(df)
  # diff_1 <- nrow_before_dup - nrow_after_dup}
  # nrow_2 <- nrow(df)
  
  parent <- unlist(strsplit(countries , split = '_'))[1]
  #################################
  #Add additional data
  #################################
  if(countries %in% unique(additional$ihme_loc_id)){
    additional$Site <- trimws(additional$Site)
    #Incorporate Additional Data and Discrepencies
    new_data <- subset(additional, ihme_loc_id == as.character(countries) & type_of_additional %in% c(1,2))
    new_data <- new_data[,c("Group","Site", "Year", "Prev", "N", 'type')]
    setnames(new_data, new = 'subpop', old = 'Group')
    setnames(new_data, new = 'n', old = 'N')
    setnames(new_data, new = 'prev', old = 'Prev')
    setnames(new_data, new = 'site', old = 'Site')
    setnames(new_data, new = 'year', old = 'Year')
    new_data[,country := countries]
    new_data[,type:= 'ancss']
    new_data[,used := TRUE]
    new_data[,agegr := '15-49']
    new_data[,age:= 15]
    new_data[,agspan := 35]
    
    
    
    setDT(df)
    diff_2 <- nrow(new_data)
  }else{diff_2 <- 0}
  nrow_3 <- nrow(df)
  
  #################################
  #Replace sample sizes
  #################################
  if(countries %in% unique(additional$ihme_loc_id)){
    additional$Site <- trimws(additional$Site)
    #Incorporate Additional Data and Discrepencies
    replace_N <- subset(additional, ihme_loc_id == as.character(countries) & type_of_additional %in% c(3,34))
    replace_N <- replace_N[,c("Group","Site", "Year", "Prev", "N", 'type')]
    setnames(replace_N, new = 'subpop', old = 'Group')
    setnames(replace_N, new = 'n', old = 'N')
    setnames(replace_N, new = 'prev', old = 'Prev')
    setnames(replace_N, new = 'site', old = 'Site')
    setnames(replace_N, new = 'year', old = 'Year')
    replace_N[,country := countries]
    replace_N[,type:= 'ancss']
    replace_N[,used := TRUE]
    replace_N[,agegr := '15-49']
    replace_N[,age:= 15]
    replace_N[,agspan := 35]
    df <- rbind.fill(df, replace_N)
    diff_3 <- nrow(replace_N)
  }else{diff_3 <- 0}
  nrow_4 <- nrow(df)
  
  #################################
  #Replace prevalence
  #################################
  if(countries %in% unique(additional$ihme_loc_id)){
    replace_Prev <- subset(additional, ihme_loc_id == as.character(countries) & type_of_additional %in% c(4,34))
    setDT(replace_Prev)
    replace_Prev <- replace_Prev[,c("Group","Site", "Year", "Prev", "N", 'type')]
    setnames(replace_Prev, new = 'subpop', old = 'Group')
    setnames(replace_Prev, new = 'n', old = 'N')
    setnames(replace_Prev, new = 'prev', old = 'Prev')
    setnames(replace_Prev, new = 'site', old = 'Site')
    setnames(replace_Prev, new = 'year', old = 'Year')
    replace_Prev[,country := countries]
    replace_Prev[,type:= 'ancss']
    replace_Prev[,used := TRUE]
    replace_Prev[,agegr := '15-49']
    replace_Prev[,age:= 15]
    replace_Prev[,agspan := 35]
    df <- rbind.fill(df, replace_Prev)
    diff_4 <- nrow(replace_Prev)
  }else{diff_4 <- 0}
  nrow_5 <- nrow(df)
  
  #################################
  #Replace TGO duplicates
  #################################
  if(countries == 'TGO'){
    pre_tgo_dup <- nrow(df)
    # Remove duplicates from TGO
    df <- subset(df, !(country == "TGO" & subpop %in% c("POLULATION TOTALE","POPULATION TOTALE")))
    post_tgo_dup <- nrow(df)
    diff_5 <- pre_tgo_dup - post_tgo_dup
  }else{diff_5 <- 0}
  nrow_6 <- nrow(df)
  
  #################################
  #Replace duplicate rows after additional data is added
  #################################
  {
    pre_dup <- nrow(df)
    df <- unique(df)
    post_dup <- nrow(df)
    rm(new_data, replace_N,replace_Prev)
    diff_6 <- pre_dup - post_dup
  }
  nrow_7 <- nrow(df)
  
  #merge to geography information
  # all <- merge(geocodebook, df, by.x=c("country", "Group", "site"), by.y=c("country", "subpop", "site"), all.x=F, all.y=T)
  # all_copy <- all
  # all$site <- as.character(all$site)
  all <- df
  
  #################################
  #Rename ZAF blank site
  #################################
  if(countries == 'ZAF'){all$site[all$country == "ZAF" & all$site =="NULL"] <- "KWAZULU-NATAL PROVINCE"
  }
  
  #################################
  #Remove MOZ pseudosites
  #################################
  if(countries == "MOZ"){
    all_pre <- nrow(all)
    all <- subset(all, !(country == "MOZ" & site %in% c("pseudo site", "Pseudo site", "Pseudo sites")))
    all_post <- nrow(all)
    diff_7 <- all_pre - all_post
  }else{diff_7 = 0}
  nrow_8 <- nrow(all)
  
  #################################
  #Remove non-ANC data
  #################################
  if(countries == 'CPV' | countries == 'MAR' | countries == 'MDG' | 
     countries == 'MRT' | countries == 'NER' | countries == 'SDN' |
     countries == 'SEN'){
    anc_pre <- nrow(all)
    all <- subset(all, !(country == "CPV" & !(subpop %in% c("Pop f,minine restante","Pop feminina restante"))))
    all <- subset(all, !(country == "MAR" & !(subpop %in% c("Femmes"))))
    all <- subset(all, !(country == "MDG" & !(subpop %in% c("population feminine restante"))))
    all <- subset(all, !(country == "MRT" & !(subpop %in% c("Pop féminine restante"))))
    all <- subset(all, !(country == "NER" & !(subpop %in% c("Pop féminine restante"))))
    all <- subset(all, !(country == "SDN" & !(subpop %in% c("Female remaining pop"))))
    #all <- subset(all, !(country == "STP" & !(subpop %in% c("Pop Fem_restante"))))
    all <- subset(all, !(country == "SEN" & !(subpop %in% c("Pop féminine restante"))))
    anc_post <- nrow(all)
    diff_8 <- anc_pre - anc_post
  }else{diff_8 <- 0}
  nrow_9 <- nrow(all)
  
  all$site <- gsub('"',"",as.character(all$site))
  all$site <- gsub("\\(%\\)","", as.character(all$site))
  all$site <- gsub("\\.^\\s+|\\s+$", "", all$site)
  all$site <- unlist(lapply(strsplit((all$site), split=' '), function(x) paste(x[which(x != '')], collapse = ' ')))
  all$site <- unlist(lapply(strsplit((all$site), split=' '), function(x) paste(x[which(x != '(%)')], collapse = ' ')))
  
  
  #################################
  #Remove differently named duplicates
  #################################
  
  if(parent == 'ZAF'){
    all <- as.data.table(all)
    all[,subpop:='all']
    all <- as.data.frame(all)
  }
  if(countries == 'RWA' | countries == 'TZA'){
    dup_pre <- nrow(all)
    all <- subset(all, !(country == "RWA" & site == "Gikondo A"& year != 2013))
    all$site[all$country == "RWA"& all$site == "Gikondo A"] <- "Gikondo"
    all <- subset(all, !(country == "RWA" & site == "Biryogo A"& year != 2013))
    all$site[all$country == "RWA"& all$site == "Biryogo A"] <- "Biryogo"
    all$site[all$country == "RWA"& all$site == "CHK"] <- "Kigali CHK"
    all$site[all$country == "RWA"& all$site == "Ruhengeri 2"] <- "Ruhengeri"
    all$site[all$country == "TZA"& all$site == "Kagera (Lukole Refugee Camp)"] <- "Lukole Refugee"
    dup_post <- nrow(all)
    diff_9 <- dup_pre - dup_post 
  }else{diff_9 <- 0}
  nrow_10 <- nrow(all)
  
  #################################
  #Remove outliers
  #################################
  if(countries %in% c('CIV', 'CMR', 'COD', 'AGO',
                      'MOZ', 'MWI', 'CMR', 'CPV',
                      'ERI', 'KEN', 'TGO', 'ZAF',
                      'GHA'))
    {# Remove site-years with concerning data (see "Checking weird Trends in ANC Data")
    outlier_pre <- nrow(all)
    all <- subset(all, !(country == "CIV" & site == "MATERNITE HG ABOBO SUD")) #outlier
    all <- subset(all, !(country == "CMR" & site == "Lolodorf" & year == 2012)) #outlier
    all <- subset(all, !(country == "COD" & site == "S.Vicente" & year == 1996)) #outlier
    all <- subset(all, !(country == "AGO" & site %in% c("CMI Benguela - Benguela","Mat do Lobito - Benguela") & year == 2004)) #outlier
    all <- subset(all, !(country == "MOZ" & year >= 2013)) #RT vs SS (9 site-years)
    all <- subset(all, !(country == "MWI" & year == 2016)) #RT vs SS (3 site-years)
    #duplicates from anc checks output 8/8/2019
    #exact duplicates to reports (zeros)
    all <- subset(all, !(country == "CMR" & site == "Ndop" & year == 2002))
    all <- subset(all, !(country == "CPV" & site == "S.Vicente" & year == 1995))
    #all <- subset(all, !(country == "ERI" & site == "Akordet" & year == 1995 & data_source == "2017" ))
    all <- subset(all, !(country == "ERI" & site == "Akordet" & year == 2007 ))
    all <- subset(all, !(country == "KEN" & site == "Kisii" & year == 1992  ))
    all <- subset(all, !(country == "TGO" & site == "USP KATINDI" & year == 2011 ))
    all <- subset(all, !(country == "ZAF" & site == "Namakwa DM" & year == 2009 ))
    # removing duplicate site_year with different prev, this prev for 2018 was zero (6.0 from 2017 was closer)
    all <- subset(all, !(country == "GHA" & site == "Nadowli" & year == 2006 ))
    outlier_post <- nrow(all)
    diff_10 <- outlier_pre - outlier_post}else{diff_10 <- 0}
    nrow_11 <- nrow(all)  
  
  #################################
  #Replace sample size place holders
  #################################
  {# For countries with many repeated sample sizes at a set level, 
    #    change it to be the median of the others
    all$n[all$country == "BDI" & all$n == 300] <- 349
    all$n[all$country == "CMR" & all$n == 300] <- 100
    
    all$n[all$country == "COG" & all$n == 300] <- 121
    all$n[all$country == "LSO" & all$n == 300] <- 325
    all$n[all$country == "MLI" & all$n == 300] <- 299
    all$n[all$country == "TGO" & all$n == 300] <- 103
    all$n[all$country == "UGA" & all$n == 300] <- 470
    all$n[all$country == "ZWE" & all$n == 300] <- 333
    
    all$n[all$country == "AGO" & all$n == 500] <- 498

    # Only keep variables we need
    all <- all[,c("country", "subpop", "site",  "year","prev", "n", 'ihme_loc_id', 'type')]
    }
  
  #################################
  #Reformat remainder
  #################################
  ####create all_mapped for areas that we do subnats for
  ####for countries we don't do subnats for we can just keep all data and put into gbd format
  if(countries %in% hiv_locs_nat){ 
  all_mapped <- all
  # all_mapped$latitude[all_mapped$point == 0] <- NA
  # all_mapped$longitude[all_mapped$point == 0] <- NA
  all_mapped <- transform(all_mapped)
  all_mapped$prev <- all_mapped$prev * .01
  all_mapped <- unique(all_mapped)
  all_mapped$cluster_id <- 1:dim(all_mapped)[1]
  
  all_mapped <- data.frame(all_mapped)
  
  #remove duplcates
  all_mapped <- all_mapped[!duplicated(all_mapped), ]
  all_mapped <- as.data.table(all_mapped)
  
  anc_point_dat <- all_mapped[,point:= NULL]
  anc_point_dat[, agegr := "15-49"]
  anc_point_dat[, age := "15"]
  anc_point_dat[, agspan := "35"]
  ##not sure how to get region
  #anc_point_dat[, subpop := NA]
  anc_point_dat[, used := TRUE]
  anc_point_dat[, offset := NA]
  anc_site_dat.colnames <- c('site', 'year', 'used', 'prev','n' ,'subpop','type' ,'agegr' ,'age','agspan','offset', 'country', 'ihme_loc_id')
  anc_point_new.dat <- anc_point_dat[,anc_site_dat.colnames, with = FALSE]
  saveRDS(anc_point_new.dat, file = paste0(output.dir,countries, '.rds'))
  diff_11 <- 0
  }else{
    ######this removes sites that can't be geomatched, for countries where we do subnats for, should we keep them for nat estimates? 
    # all_mapped <- subset(all,!is.na(point)) 
    # all_mapped$latitude[all_mapped$point == 0] <- NA
    # all_mapped$longitude[all_mapped$point == 0] <- NA
    # all_mapped <- transform(all_mapped, latitude = as.numeric(as.character(latitude)),
    #                         longitude = as.numeric(as.character(longitude)))
    # all_mapped$location_code <- as.numeric(all_mapped$location_code)
    # all_mapped$source <- paste0("UNAIDS files - ", all_mapped$data_source)
    all_mapped <- all
    all_mapped$prev <- all_mapped$prev * .01
    all_mapped <- unique(all_mapped)
    all_mapped$cluster_id <- 1:dim(all_mapped)[1]
    
    all_mapped <- data.frame(all_mapped)
    
    #remove duplcates
    all_mapped <- all_mapped[!duplicated(all_mapped), ]
    

    if(parent == 'ZAF'){
      # all_point <- subset(all_mapped, point == 0)
      all_mapped <- all
      all_mapped <- as.data.table(all_mapped)
      anc_point_dat <- all_mapped[,point:= NULL]

      anc_point_dat[, agegr := "15-49"]
      anc_point_dat[, age := "15"]
      anc_point_dat[, agspan := "35"]
      ##not sure how to get region
      anc_point_dat[, subpop := NA]
      anc_point_dat[, used := TRUE]
      anc_point_dat[, offset := NA]
      anc_point_dat[, country := countries]
      #anc_point_dat[, type := "ancss"]
      anc_site_dat.colnames <- c('site', 'year', 'used', 'prev','n' ,'subpop','type' ,'agegr' ,'age','agspan','offset', 'country', 'ihme_loc_id')
      anc_point_new.dat <- anc_point_dat[,anc_site_dat.colnames, with = FALSE]
      saveRDS(anc_point_new.dat, file = paste0(output.dir, countries, '.rds'))

    }else{   
      ######get point data
      # temp_lat_long_code <- lat_long_table[grepl(countries, lat_long_table$ihme_loc_id),]
      # subnat_all <- merge(all_mapped, temp_lat_long_code, by = c('latitude', 'longitude'))
      # all_mapped <- as.data.table(subnat_all)
      # 
       anc_point_dat <- as.data.table(all_mapped)

      anc_point_dat[, agegr := "15-49"]
      anc_point_dat[, age := "15"]
      anc_point_dat[, agspan := "35"]
      ##not sure how to get region
      anc_point_dat[, subpop := NA]
      anc_point_dat[, used := TRUE]
      anc_point_dat[, offset := NA]
      anc_point_dat[, country := countries]
      anc_point_dat[,ihme_loc_id := countries]
      anc_site_dat.colnames <- c('site', 'year', 'used', 'prev','n' ,'subpop','type' ,'agegr' ,'age','agspan','offset', 'country', 'ihme_loc_id')
      anc_point_new.dat <- anc_point_dat[,anc_site_dat.colnames, with = FALSE]
      for(k in 1:length(unique(anc_point_new.dat$ihme_loc_id))){
        final_file <- subset(anc_point_new.dat, ihme_loc_id == unique(anc_point_new.dat$ihme_loc_id)[k])
        anc_site_dat.colnames <- c('site', 'year', 'used', 'prev','n' ,'subpop','type' ,'agegr' ,'age','agspan','offset', 'country')
        final_file <- final_file[,anc_site_dat.colnames, with = FALSE]
        saveRDS(final_file, file = paste0(output.dir, unique(anc_point_new.dat$ihme_loc_id)[k], '.rds'))
        assign(paste0('diff_11_', k), nrow(final_file))
      }
      }
    

  }
  
  # tracking_sheet[[i]] <- unlist(lapply(paste0('diff_', seq(2:11)), get))
  # nrow_sheet[[i]] <- unlist(lapply(paste0('nrow_', seq(1:11)), get))
  
}

tracking_sheet.dt <- do.call(cbind.data.frame, tracking_sheet)
colnames(tracking_sheet.dt) <- keep$countries
rownames.tracking.dt <- c('Duplicate observation removed (-)',
                          'Additional data added (+)',
                          'Sample size replaced',
                          'Prevalence replaced',
                          'TGO duplicates removed (-)',
                          'Duplicates after additional data is added are removed (-)',
                          'MOZ pseudosites removed (-)', 
                          'Non-ANC data removed',
                          'Differently named duplicates removed (-)',
                          'Outliers removed (-)',
                          'Point data added at the subnational level')
rownames(tracking_sheet.dt) <- rownames.tracking.dt

nrow_sheet.dt <- do.call(cbind.data.frame, nrow_sheet)
colnames(nrow_sheet.dt) <- keep$countries
rownames.nrow.dt <- c('Start', 
                          'Duplicate observation removed (-)',
                          'Additional data added (+)',
                          'Sample size replaced',
                          'Prevalence replaced',
                          'TGO duplicates removed (-)',
                          'Duplicates after additional data is added are removed (-)',
                          'MOZ pseudosites removed (-)', 
                          'Non-ANC data removed',
                          'Differently named duplicates removed (-)',
                          'Outliers removed (-)')
rownames(nrow_sheet.dt) <- rownames.nrow.dt


###################
#compare to gbd amounts, amounts_ancsitedat
load('/homes/mwalte10/amounts_old.RData')
amounts_ancsitedat <- as.data.table(amounts_ancsitedat)
gbd_amounts <- amounts_ancsitedat[locations %in% keep$countries, ]
gbd_amounts <- gbd_amounts[,1:2]


