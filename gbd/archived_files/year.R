for (i in 1:nrow(keep)) {
  keep <- as.data.frame(keep)
  countries <- keep$countries[i]
  if(keep$years[i] == "2018"){
    unaid_year <- 2018
    df <- data.frame()
    
    surveillance_files <- list.files(paste0("/home/j/WORK/11_geospatial/10_mbg/hiv/unaids_anc/anc_epp_extractions/", unaid_year,"/",countries),
                                     pattern=".csv$", ignore.case = T, full.names = T)
    
    for(j in 1:length(surveillance_files)){
      csv <- read.csv(surveillance_files[j], stringsAsFactors = FALSE)
      csv$Country <- rep(countries,dim(csv)[1])
      csv$data_source <- as.character(rep(unaid_year,dim(csv)[1]))
      csv$nid <- rep(365412,dim(csv)[1])
      colnames(csv)[1] <- "Group"
      csv$UNAIDS_file <- NULL
      df <- rbind.fill(df, csv)
    }
  }
  if(keep$years[i] == "2017"){
    unaid_year <- 2017
    df <- data.frame()
    
    surveillance_files <- list.files(paste0("/home/j/WORK/11_geospatial/10_mbg/hiv/unaids_anc/anc_epp_extractions/", unaid_year,"/",countries), 
                                     pattern=".csv$", ignore.case = T, full.names = T)
    for(j in 1:length(surveillance_files)){
      message(paste("processing file             ", j, " of ", length(surveillance_files)))
      csv <- read.csv(surveillance_files[j], stringsAsFactors = FALSE)
      csv$Country <- rep(countries,dim(csv)[1])
      csv$data_source <- as.character(rep(unaid_year,dim(csv)[1]))
      csv$nid <- rep(317607,dim(csv)[1])
      csv$UNAIDS_file <- NULL
      colnames(csv)[1] <- "Group"
      #colnames(csv)[colnames(csv)== "?..Group"] <- "Group"
      df <- rbind.fill(df, csv)
      
    }
  }
  if(keep$years[i] == "2016"){
    unaid_year <- 2016
    df <- data.frame()
    surveillance_files <- list.files(paste0("/home/j/WORK/11_geospatial/10_mbg/hiv/unaids_anc/anc_epp_extractions/", unaid_year,"/",countries), 
                                     pattern=".csv$", ignore.case = T, full.names = T)
    for(j in 1:length(surveillance_files)){
      message(paste("processing file             ", j, " of ", length(surveillance_files)))
      csv <- read.csv(surveillance_files[j], stringsAsFactors = FALSE)
      csv$Country <- rep(countries,dim(csv)[1])
      csv$data_source <- as.character(rep(unaid_year,dim(csv)[1]))
      csv$nid <- rep(306517,dim(csv)[1])
      csv$UNAIDS_file <- NULL
      colnames(csv)[1] <- "Group"
      #colnames(csv)[colnames(csv)== "?..Group"] <- "Group"
      df <- rbind.fill(df, csv)
    }
    
  }
  
  nrow_1 <- nrow(df)
  years_1 <- count_me(df$Year)
  #################################
  #change incorrect site names
  #################################
  if(countries == "LBR"){
    df$Site[df$Country == "LBR" & df$Site == "Barclay.Health.Center"] <- "Barclay Health Center"
    df$Site[df$Country == "LBR" & df$Site == "C.B..Dunbar.Health.Center...."] <- "C.B. Dunbar Health Center (%)"
    df$Site[df$Country == "LBR" & df$Site == "C.H..Rennie.Hospital...."] <- "C.H. Rennie Hospital (%)"
    df$Site[df$Country == "LBR" & df$Site == "Duport.Road.Health.Center...."] <- "Duport Road Health Center (%)"
    df$Site[df$Country == "LBR" & df$Site == "Firestone.Hospital"] <- "Firestone Hospital"
    df$Site[df$Country == "LBR" & df$Site == "Fish.Town.Health.Center"] <- "Fish Town Health Center"
    df$Site[df$Country == "LBR" & df$Site == "Ganta.Hospital"] <- "Ganta Hospital"
    df$Site[df$Country == "LBR" & df$Site == "JJ.Dossen.Hospital"] <- "JJ Dossen Hospital"
    df$Site[df$Country == "LBR" & df$Site == "John.F.Kennedy.Hospital"] <- "John F Kennedy Hospital"
    df$Site[df$Country == "LBR" & df$Site == "Liberia.Govt..Hospital.Tubmanburg"] <- "Liberia Govt. Hospital-Tubmanburg"
    df$Site[df$Country == "LBR" & df$Site == "Liberia.Govt.Hosp.Buchanan"] <- "Liberia Govt Hosp-Buchanan"
    df$Site[df$Country == "LBR" & df$Site == "Martha.Tubman.Hospital"] <- "Martha Tubman Hospital"
    df$Site[df$Country == "LBR" & df$Site == "Phebe.Hospital"] <- "Phebe Hospital"
    df$Site[df$Country == "LBR" & df$Site == "Redemption.Hospital"] <- "Redemption Hospital"
    df$Site[df$Country == "LBR" & df$Site == "Site.22"] <- "Site 22"
    df$Site[df$Country == "LBR" & df$Site == "St.Francis.Hospital"] <- "St Francis Hospital"
    df$Site[df$Country == "LBR" & df$Site == "Star.of.the.Sea.Health.Center"] <- "Star of the Sea Health Center"
    df$Site[df$Country == "LBR" & df$Site == "Voinjama.Health.Center"] <- "Voinjama Health Center"
  }
  if(countries == "MDG"){
    df$Site[df$Country == "MDG" & df$Site == "Miarinarivo (%)"] <- "Miarinarivo (%)" # niot working
    df$Site[df$Country == "MDG" & df$Site == "Sambava  (%)"] <- "Sambava (%)"
    df$Site[df$Country == "MDG" & df$Site == "Antsirabe  (%)"] <- "Antsirabe (%)"
  }
  if(countries == "NGA"){
    df$Site[df$Country == "NGA" & df$Site == "Osun  (Iragbere)"] <- "Osun (Iragbere)"
  }
  if(countries == "ZAF"){
    df$Site[df$Country == "ZAF" & df$Site == "NULL"] <- "KWAZULU-NATAL PROVINCE" 
    
  }
  if(countries == "COG"){
    df$Site[df$Country == "COG" & df$Site == "Cuvette.ouest"] <- "Cuvette ouest"
    df$Site[df$Country == "COG" & df$Site == "Mouyondzi...."] <- "Mouyondzi (%)"
    df$Site[df$Country == "COG" & df$Site == "Owando...."] <- "Owando (%)"
    df$Site[df$Country == "COG" & df$Site == "Sibiti...."] <- "Sibiti (%)"
  }
  if(countries == "CMR"){
    df$Site[df$Country == "CMR" & df$Site == "CMA  Tyo"] <- "CMA Tyo"
  }

  
  #################################
  #remove duplicate observations. GBD ALSO DOES THIS STEP
  #################################
  {
    nrow_before_dup <- nrow(df)
    df_in <- subset(df, In == 1)
    df_in <- unique(df_in)
    df_not_in <- subset(df, In == 0)
    df_not_in <- unique(df_not_in)
    together <- rbind.fill(df_in, df_not_in)
    together_dedupe <- unique(setDT(together)[order(Group,Site,Year, -In)], by = c('Group','Site','Year'))
    df <- data.frame(together_dedupe)
    df <- df[,!(names(df) %in% c("In"))]
    nrow_after_dup <- nrow(df)
    diff_1 <- nrow_before_dup - nrow_after_dup}
  nrow_2 <- nrow(df)
  years_2 <- count_me(df$Year)
  #################################
  #Add additional data
  #################################
  if(countries %in% unique(additional$Country)){
    additional$Site <- trimws(additional$Site)
    #Incorporate Additional Data and Discrepencies
    new_data <- subset(additional, Country == as.character(countries) & type_of_additional %in% c(1,2))
    new_data <- new_data[,c("Group","Site", "Year", "Prev", "N","Country","data_source","nid")]
    df <- rbind.fill(df, new_data)
    setDT(df)
    diff_2 <- nrow(new_data)
  }else{diff_2 <- 0}
  nrow_3 <- nrow(df)
  years_3 <- count_me(df$Year)
  
  #################################
  #Replace sample sizes
  #################################
  if(countries %in% unique(additional$Country)){
    additional$Site <- trimws(additional$Site)
    #Incorporate Additional Data and Discrepencies
    replace_N <- subset(additional, Country == as.character(countries) & type_of_additional %in% c(3,34))
    setDT(replace_N)
    setDT(df)
    df <- df[replace_N, on = .(Group,Site,Year), ':=' (N = i.N)]
    diff_3 <- nrow(replace_N)
  }else{diff_3 <- 0}
  nrow_4 <- nrow(df)
  years_4 <- count_me(df$Year)
  
  #################################
  #Replace prevalence
  #################################
  if(countries %in% unique(additional$Country)){
    replace_Prev <- subset(additional, Country == as.character(countries) & type_of_additional %in% c(4,34))
    setDT(replace_Prev)
    df <- df[replace_Prev, on = .(Group,Site,Year), ':=' (Prev = i.Prev, data_source = "report", nid = as.numeric(i.nid))]
    diff_4 <- nrow(replace_Prev)
  }else{diff_4 <- 0}
  nrow_5 <- nrow(df)
  years_5 <- count_me(df$Year)
  
  #################################
  #Replace TGO duplicates
  #################################
  if(countries == 'TGO'){
    pre_tgo_dup <- nrow(df)
    # Remove duplicates from TGO
    df <- subset(df, !(Country == "TGO" & Group %in% c("POLULATION TOTALE","POPULATION TOTALE")))
    post_tgo_dup <- nrow(df)
    diff_5 <- pre_tgo_dup - post_tgo_dup
  }else{diff_5 <- 0}
  nrow_6 <- nrow(df)
  years_6 <- count_me(df$Year)
  
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
  years_7 <- count_me(df$Year)
  
  #merge to geography information
  all <- merge(geocodebook, df, by.x=c("iso3", "group", "site"), by.y=c("Country", "Group", "Site"), all.x=F, all.y=T)
  all_copy <- all
  all$site <- as.character(all$site)
  
  #################################
  #Rename ZAF blank site
  #################################
  if(countries == 'ZAF'){all$site[all$iso3 == "ZAF" & all$site =="NULL"] <- "KWAZULU-NATAL PROVINCE"
  }
  
  #################################
  #Remove MOZ pseudosites
  #################################
  if(countries == "MOZ"){
    all_pre <- nrow(all)
    all <- subset(all, !(iso3 == "MOZ" & site %in% c("pseudo site", "Pseudo site", "Pseudo sites")))
    all_post <- nrow(all)
    diff_7 <- all_pre - all_post
  }else{diff_7 = 0}
  nrow_8 <- nrow(all)
  years_8 <- count_me(all$Year)
  #################################
  #Remove non-ANC data
  #################################
  if(countries == 'CPV' | countries == 'MAR' | countries == 'MDG' | 
     countries == 'MRT' | countries == 'NER' | countries == 'SDN' |
     countries == 'SEN'){
    anc_pre <- nrow(all)
    all <- subset(all, !(iso3 == "CPV" & !(group %in% c("Pop f,minine restante","Pop féminine restante"))))
    all <- subset(all, !(iso3 == "MAR" & !(group %in% c("Femmes"))))
    all <- subset(all, !(iso3 == "MDG" & !(group %in% c("population feminine restante"))))
    all <- subset(all, !(iso3 == "MRT" & !(group %in% c("Pop féminine restante"))))
    all <- subset(all, !(iso3 == "NER" & !(group %in% c("Pop féminine restante"))))
    all <- subset(all, !(iso3 == "SDN" & !(group %in% c("Female remaining pop"))))
    #all <- subset(all, !(iso3 == "STP" & !(group %in% c("Pop Fem_restante"))))
    all <- subset(all, !(iso3 == "SEN" & !(group %in% c("Pop féminine restante"))))
    anc_post <- nrow(all)
    diff_8 <- anc_pre - anc_post
  }else{diff_8 <- 0}
  nrow_9 <- nrow(all)
  years_9 <- count_me(all$Year)
  
  all$site <- gsub('"',"",as.character(all$site))
  all$site <- gsub("\\(%\\)","", as.character(all$site))
  all$site <- gsub("\\.^\\s+|\\s+$", "", all$site)
  
  #################################
  #Remove differently named duplicates
  #################################
  if(countries == 'RWA' | countries == 'TZA'){
    dup_pre <- nrow(all)
    all <- subset(all, !(iso3 == "RWA" & site == "Gikondo A"& Year != 2013))
    all$site[all$iso3 == "RWA"& all$site == "Gikondo A"] <- "Gikondo"
    all <- subset(all, !(iso3 == "RWA" & site == "Biryogo A"& Year != 2013))
    all$site[all$iso3 == "RWA"& all$site == "Biryogo A"] <- "Biryogo"
    all$site[all$iso3 == "RWA"& all$site == "CHK"] <- "Kigali CHK"
    all$site[all$iso3 == "RWA"& all$site == "Ruhengeri 2"] <- "Ruhengeri"
    all$site[all$iso3 == "TZA"& all$site == "Kagera (Lukole Refugee Camp)"] <- "Lukole Refugee"
    dup_post <- nrow(all)
    diff_9 <- dup_pre - dup_post 
  }else{diff_9 <- 0}
  nrow_10 <- nrow(all)
  years_10 <- count_me(all$Year)
  #################################
  #Remove outliers
  #################################
  if(countries %in% c('CIV', 'CMR', 'COD', 'AGO',
                      'MOZ', 'MWI', 'CMR', 'CPV',
                      'ERI', 'KEN', 'TGO', 'ZAF',
                      'GHA'))
  {# Remove site-years with concerning data (see "Checking weird Trends in ANC Data")
    outlier_pre <- nrow(all)
    all <- subset(all, !(iso3 == "CIV" & site == "MATERNITE HG ABOBO SUD")) #outlier
    all <- subset(all, !(iso3 == "CMR" & site == "Lolodorf" & Year == 2012)) #outlier
    all <- subset(all, !(iso3 == "COD" & site == "S.Vicente" & Year == 1996)) #outlier
    all <- subset(all, !(iso3 == "AGO" & site %in% c("CMI Benguela - Benguela","Mat do Lobito - Benguela") & Year == 2004)) #outlier
    all <- subset(all, !(iso3 == "MOZ" & Year >= 2013)) #RT vs SS (9 site-years)
    all <- subset(all, !(iso3 == "MWI" & Year == 2016)) #RT vs SS (3 site-years)
    #duplicates from anc checks output 8/8/2019
    #exact duplicates to reports (zeros)
    all <- subset(all, !(iso3 == "CMR" & site == "Ndop" & Year == 2002 & data_source == "2017" ))
    all <- subset(all, !(iso3 == "CPV" & site == "S.Vicente" & Year == 1995 & data_source == "2016"))
    #all <- subset(all, !(iso3 == "ERI" & site == "Akordet" & Year == 1995 & data_source == "2017" ))
    all <- subset(all, !(iso3 == "ERI" & site == "Akordet" & Year == 2007 & data_source == "2018" ))
    all <- subset(all, !(iso3 == "KEN" & site == "Kisii" & Year == 1992 & data_source == "2018" ))
    all <- subset(all, !(iso3 == "TGO" & site == "USP KATINDI" & Year == 2011 & data_source == "2017" ))
    all <- subset(all, !(iso3 == "ZAF" & site == "Namakwa DM" & Year == 2009 & data_source == "2017" ))
    # removing duplicate site_year with different prev, this prev for 2018 was zero (6.0 from 2017 was closer)
    all <- subset(all, !(iso3 == "GHA" & site == "Nadowli" & Year == 2006 & data_source == "2018" ))
    outlier_post <- nrow(all)
    diff_10 <- outlier_pre - outlier_post}else{diff_10 <- 0}
  nrow_11 <- nrow(all)  
  years_11 <- count_me(all$Year)
  #################################
  #Replace sample size place holders
  #################################
  {# For countries with many repeated sample sizes at a set level, 
    #    change it to be the median of the others
    all$N[all$iso3 == "BDI" & all$N == 300] <- 349
    all$N[all$iso3 == "CMR" & all$N == 300] <- 100
    
    all$N[all$iso3 == "COG" & all$N == 300] <- 121
    all$N[all$iso3 == "LSO" & all$N == 300] <- 325
    all$N[all$iso3 == "MLI" & all$N == 300] <- 299
    all$N[all$iso3 == "TGO" & all$N == 300] <- 103
    all$N[all$iso3 == "UGA" & all$N == 300] <- 470
    all$N[all$iso3 == "ZWE" & all$N == 300] <- 333
    
    all$N[all$iso3 == "AGO" & all$N == 500] <- 498
    
    # Only keep variables we need
    all <- all[,c("nid", "iso3", "group", "site", "point", "latitude","longitude",
                  "admin_level","shapefile","location_code","Year","Prev", "N","data_source")]}
  
  #################################
  #Reformat remainder
  #################################
  ####create all_mapped for areas that we do subnats for
  ####for countries we don't do subnats for we can just keep all data and put into gbd format
  if(countries %in% hiv_locs_nat){ 
    all_mapped <- all
    all_mapped$latitude[all_mapped$point == 0] <- NA
    all_mapped$longitude[all_mapped$point == 0] <- NA
    all_mapped <- transform(all_mapped, latitude = as.numeric(as.character(latitude)),
                            longitude = as.numeric(as.character(longitude)))
    all_mapped$shapefile <- as.character(all_mapped$shapefile)
    all_mapped$location_code <- as.numeric(all_mapped$location_code)
    all_mapped$source <- paste0("UNAIDS files - ", all_mapped$data_source)
    colnames(all_mapped)[colnames(all_mapped) == "Year"] <- "year"
    colnames(all_mapped)[colnames(all_mapped) == "Prev"] <- "anc_hiv_test"
    colnames(all_mapped)[colnames(all_mapped) == "iso3"] <- "country"
    all_mapped$N_obs <- all_mapped$N
    all_mapped$anc_hiv_test <- all_mapped$anc_hiv_test *.01
    all_mapped <- unique(all_mapped)
    all_mapped$cluster_id <- 1:dim(all_mapped)[1]
    
    all_mapped <- data.frame(all_mapped)
    
    #remove duplcates
    all_mapped <- all_mapped[!duplicated(all_mapped), ]
    all_mapped <- as.data.table(all_mapped)
    
    anc_point_dat <- all_mapped[,point:= NULL]
    setnames(anc_point_dat, old = 'N', new= 'n')
    setnames(anc_point_dat, old = 'anc_hiv_test', new= 'prev')
    anc_point_dat[, agegr := "15-49"]
    anc_point_dat[, age := "15"]
    anc_point_dat[, agspan := "35"]
    ##not sure how to get region
    anc_point_dat[, subpop := NA]
    anc_point_dat[, used := TRUE]
    anc_point_dat[, offset := NA]
    anc_point_dat[, type := "ancss"]
    anc_site_dat.colnames <- c('site', 'year', 'used', 'prev','n' ,'subpop','type' ,'agegr' ,'age','agspan','offset', 'country')
    anc_point_new.dat <- anc_point_dat[,anc_site_dat.colnames, with = FALSE]
    saveRDS(anc_point_new.dat, file = paste0(output.dir,countries, '.rds'))
    diff_11 <- 0
  }else{
    ######this removes sites that can't be geomatched, for countries where we do subnats for, should we keep them for nat estimates? 
    all_mapped <- subset(all,!is.na(point)) 
    all_mapped$latitude[all_mapped$point == 0] <- NA
    all_mapped$longitude[all_mapped$point == 0] <- NA
    all_mapped <- transform(all_mapped, latitude = as.numeric(as.character(latitude)),
                            longitude = as.numeric(as.character(longitude)))
    all_mapped$shapefile <- as.character(all_mapped$shapefile)
    all_mapped$location_code <- as.numeric(all_mapped$location_code)
    all_mapped$source <- paste0("UNAIDS files - ", all_mapped$data_source)
    colnames(all_mapped)[colnames(all_mapped) == "Year"] <- "year"
    colnames(all_mapped)[colnames(all_mapped) == "Prev"] <- "anc_hiv_test"
    colnames(all_mapped)[colnames(all_mapped) == "iso3"] <- "country"
    all_mapped$N_obs <- all_mapped$N
    all_mapped$anc_hiv_test <- all_mapped$anc_hiv_test *.01
    all_mapped <- unique(all_mapped)
    all_mapped$cluster_id <- 1:dim(all_mapped)[1]
    
    all_mapped <- data.frame(all_mapped)
    
    #remove duplcates
    all_mapped <- all_mapped[!duplicated(all_mapped), ]
    
    
    if(countries == 'ZAF'){
      all_point <- subset(all_mapped, point == 0)
      all_mapped <- as.data.table(all_point)
      anc_point_dat <- all_mapped[,point:= NULL]
      setnames(anc_point_dat, old = 'N', new= 'n')
      setnames(anc_point_dat, old = 'anc_hiv_test', new= 'prev')
      anc_point_dat[, agegr := "15-49"]
      anc_point_dat[, age := "15"]
      anc_point_dat[, agspan := "35"]
      ##not sure how to get region
      anc_point_dat[, subpop := NA]
      anc_point_dat[, used := TRUE]
      anc_point_dat[, offset := NA]
      anc_point_dat[, type := "ancss"]
      hiv_locs_subnat <- loc.table[ epp == 1, ihme_loc_id]
      hiv_locs_subnat <- loc.table[ level == 4 | level == 5, ihme_loc_id]
      zaf_locs <- hiv_locs_subnat[grep('ZAF', hiv_locs_subnat)]
      anc_point_dat <- as.data.table(anc_point_dat)
      for (k in 1:length(zaf_locs)) {
        x <-  anc_point_dat[,ihme_loc_ids := rep(zaf_locs[k], nrow(anc_point_dat))]
        anc_site_dat.colnames <- c('site', 'year', 'used', 'prev','n' ,'subpop','type' ,'agegr' ,'age','agspan','offset', 'country', 'ihme_loc_ids')
        anc_point_new.dat <- x[,anc_site_dat.colnames, with = FALSE]
        saveRDS(anc_point_new.dat, file = paste0(output.dir, zaf_locs[k], '.rds'))
        
        
      }
      
    }else{   
      ######get point data
      all_point <- subset(all_mapped, point == 1)
      temp_lat_long_code <- lat_long_table[grepl(countries, lat_long_table$ihme_loc_ids),]
      subnat_all <- merge(all_point, temp_lat_long_code, by = c('latitude', 'longitude'))
      all_mapped <- as.data.table(subnat_all)
      
      anc_point_dat <- all_mapped[,point:= NULL]
      setnames(anc_point_dat, old = 'N', new= 'n')
      setnames(anc_point_dat, old = 'anc_hiv_test', new= 'prev')
      anc_point_dat[, agegr := "15-49"]
      anc_point_dat[, age := "15"]
      anc_point_dat[, agspan := "35"]
      ##not sure how to get region
      anc_point_dat[, subpop := NA]
      anc_point_dat[, used := TRUE]
      anc_point_dat[, offset := NA]
      anc_point_dat[, type := "ancss"]
      anc_site_dat.colnames <- c('site', 'year', 'used', 'prev','n' ,'subpop','type' ,'agegr' ,'age','agspan','offset', 'country', 'ihme_loc_ids')
      anc_point_new.dat <- anc_point_dat[,anc_site_dat.colnames, with = FALSE]
      for(k in 1:length(unique(anc_point_new.dat$ihme_loc_ids))){
        final_file <- subset(anc_point_new.dat, ihme_loc_ids == unique(anc_point_new.dat$ihme_loc_ids)[k])
        anc_site_dat.colnames <- c('site', 'year', 'used', 'prev','n' ,'subpop','type' ,'agegr' ,'age','agspan','offset', 'country')
        final_file <- final_file[,anc_site_dat.colnames, with = FALSE]
        saveRDS(final_file, file = paste0(output.dir, unique(anc_point_new.dat$ihme_loc_ids)[k], '.rds'))
        assign(paste0('diff_11_', k), nrow(final_file))
      }}
    years_12 <- count_me(all_mapped$year)
    
  }
  
  tracking_sheet[[i]] <- unlist(lapply(paste0('diff_', seq(1:11)), get))
  nrow_sheet[[i]] <- unlist(lapply(paste0('nrow_', seq(1:11)), get))
  table <- lapply(paste0('years_', seq(1:11)), get)
  year_tracking[[i]] <- as.data.table(do.call(cbind, table))
  
}
years <- seq(1980, 2020)
count_me <- function(obj){
  counts <- c()
  for(i in 1:length(years)){
    counts[i] <- length(which(obj == years[i]))
    
  }
  return(counts)
}
