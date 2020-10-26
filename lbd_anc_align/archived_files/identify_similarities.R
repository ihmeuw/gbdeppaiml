######################################
#LOAD IN LBD UNAIDS FILES, lines 25 - 89 of 
#lbd_hiv/browse/data/anc/3_post_processing/1_anc_post_processing_final.R
######################################

df <- data.frame()

unaid_year <- 2018
countries <- c("BEN", "BFA", "CIV", "COM", "ERI" , "GHA", "GMB",
               "LBR", "MDG", "NAM" , "NGA", "TZA", "KEN") # add SEN later

length(countries)
for(i in 1:length(countries)){
  message(paste("processing", countries[i], i, " of ", length(countries)))
  surveillance_files <- list.files(paste0("/home/j/WORK/11_geospatial/10_mbg/hiv/unaids_anc/anc_epp_extractions/", unaid_year,"/",countries[i]),
                                   pattern=".csv$", ignore.case = T, full.names = T)
  
  for(j in 1:length(surveillance_files)){
    csv <- read.csv(surveillance_files[j], stringsAsFactors = FALSE)
    csv$Country <- rep(countries[i],dim(csv)[1])
    csv$data_source <- as.character(rep(unaid_year,dim(csv)[1]))
    csv$nid <- rep(365412,dim(csv)[1])
    colnames(csv)[1] <- "Group"
    csv$UNAIDS_file <- NULL
    df <- rbind.fill(df, csv)
  }
}

# Bind 2017 data to dataframe
unaid_year <- 2017
countries <- c("AGO","BDI", "CMR", "DJI", "COD","ETH", "LSO","MLI","MOZ","NGA", "RWA", "SOM", "UGA","SSD", "GAB", "TZA",
               "GHA", "ERI", "LBR", "CAF", "GMB","GNB","SWZ","MDG","GIN","NER", "ZAF","SDN","BWA","NAM","TGO",
               "MAR","TCD","MWI","BFA","COG","BEN","CIV","ZMB")

for(i in 1:length(countries)){
  message(paste("processing", countries[i], i, " of ", length(countries)))
  surveillance_files <- list.files(paste0("/home/j/WORK/11_geospatial/10_mbg/hiv/unaids_anc/anc_epp_extractions/", unaid_year,"/",countries[i]), 
                                   pattern=".csv$", ignore.case = T, full.names = T)
  for(j in 1:length(surveillance_files)){
    message(paste("processing file             ", j, " of ", length(surveillance_files)))
    csv <- read.csv(surveillance_files[j], stringsAsFactors = FALSE)
    csv$Country <- rep(countries[i],dim(csv)[1])
    csv$data_source <- as.character(rep(unaid_year,dim(csv)[1]))
    csv$nid <- rep(317607,dim(csv)[1])
    csv$UNAIDS_file <- NULL
    colnames(csv)[1] <- "Group"
    #colnames(csv)[colnames(csv)== "?..Group"] <- "Group"
    df <- rbind.fill(df, csv)
  }
}

unaid_year <- 2016
countries <- c("CPV","SLE","MRT","ZWE" )

for(i in 1:length(countries)){
  message(paste("processing", countries[i], i, " of ", length(countries)))
  surveillance_files <- list.files(paste0("/home/j/WORK/11_geospatial/10_mbg/hiv/unaids_anc/anc_epp_extractions/", unaid_year,"/",countries[i]), 
                                   pattern=".csv$", ignore.case = T, full.names = T)
  for(j in 1:length(surveillance_files)){
    message(paste("processing file             ", j, " of ", length(surveillance_files)))
    csv <- read.csv(surveillance_files[j], stringsAsFactors = FALSE)
    csv$Country <- rep(countries[i],dim(csv)[1])
    csv$data_source <- as.character(rep(unaid_year,dim(csv)[1]))
    csv$nid <- rep(306517,dim(csv)[1])
    csv$UNAIDS_file <- NULL
    colnames(csv)[1] <- "Group"
    #colnames(csv)[colnames(csv)== "?..Group"] <- "Group"
    df <- rbind.fill(df, csv)
  }
}

#LBR site differences in new CSV raw data
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

#more site Differences from new CSV raw data
df$Site[df$Country == "MDG" & df$Site == "Miarinarivo (%)"] <- "Miarinarivo (%)" # niot working
df$Site[df$Country == "MDG" & df$Site == "Sambava  (%)"] <- "Sambava (%)"
df$Site[df$Country == "MDG" & df$Site == "Antsirabe  (%)"] <- "Antsirabe (%)"
df$Site[df$Country == "NGA" & df$Site == "Osun  (Iragbere)"] <- "Osun (Iragbere)"
df$Site[df$Country == "ZAF" & df$Site == "NULL"] <- "KWAZULU-NATAL PROVINCE" #might need to change it in
df$Site[df$Country == "COG" & df$Site == "Cuvette.ouest"] <- "Cuvette ouest"
df$Site[df$Country == "COG" & df$Site == "Mouyondzi...."] <- "Mouyondzi (%)"
df$Site[df$Country == "COG" & df$Site == "Owando...."] <- "Owando (%)"
df$Site[df$Country == "COG" & df$Site == "Sibiti...."] <- "Sibiti (%)"
df$Site[df$Country == "CMR" & df$Site == "CMA  Tyo"] <- "CMA Tyo"


# Remove duplicate data - use the data that is "In" the file if discrepency for a site

df_in <- subset(df, In == 1)
df_in <- unique(df_in)

df <- df_in
#######################################
#Find the number of observations for each country
#######################################
lbd_countries <- unique(df$Country)
counts_all <- c()
for (i in 1:length(lbd_countries)) {
  counts_all[i] <- length(which(df$Country == lbd_countries[i]))
}
names(counts_all) <- lbd_countries

#######################################
#Load in gbd data
#######################################
gbd_counts <- c()
for (i in 1:length(lbd_countries)) {
  tryCatch({
    gbd <- readRDS(paste0('/share/hiv/data/PJNZ_EPPASM_prepped_subpop/', lbd_countries[i], '.rds'))
    gbd_counts[i] <- nrow(attr(gbd, 'eppd')$ancsitedat)
  }, error=function(e){})

}
names(gbd_counts) <- lbd_countries



#######################################
#Subset by year 
#######################################
subset_2017 <- subset(df, data_source == '2017')
lbd_countries_2017 <- unique(subset_2017$Country)
counts_2017 <- c()
for (i in 1:length(lbd_countries)) {
  counts_2017[i] <- length(which(subset_2017$Country == lbd_countries[i]))
}

subset_2016 <- subset(df, data_source == '2016')
lbd_countries_2016 <- unique(subset_2016$Country)
counts_2016 <- c()
for (i in 1:length(lbd_countries)) {
  counts_2016[i] <- length(which(subset_2016$Country == lbd_countries[i]))
}


subset_2018 <- subset(df, data_source == '2018')
lbd_countries_2018 <- unique(subset_2018$Country)
counts_2018 <- c()
for (i in 1:length(lbd_countries)) {
  counts_2018[i] <- length(which(subset_2018$Country == lbd_countries[i]))
}

data_2018 <- lbd_countries[which(gbd_counts - counts_2018 > -5 & gbd_counts - counts_2018 < 5)]
data_2017 <- lbd_countries[which(gbd_counts - counts_2017 > -5 & gbd_counts - counts_2017 < 5)]
data_2016 <- lbd_countries[which(gbd_counts - counts_2016 > -5 & gbd_counts - counts_2016 < 5)]

countries <- c(data_2016, data_2017, data_2018)
years <- c(rep(2016, length(data_2016)), rep(2017, length(data_2017)), rep(2018, length(data_2018)))
keep <- cbind(countries, years)
save(keep, file = '/homes/mwalte10/lbd_anc_align/examine_first.RData')


#######################################
#Examine differences
#######################################
unmatched <- setdiff(lbd_countries, keep[,1])
unmatched <- unmatched[-which(unmatched == 'NGA')]
unmatched <- unmatched[-which(unmatched == 'ETH')]
unmatched <- unmatched[-which(unmatched == 'KEN')]

missing_table <- cbind(unmatched, unname(gbd_counts[which(names(gbd_counts) %in% unmatched)]),  counts_2017[which(names(gbd_counts) %in% unmatched)], counts_2018[which(names(gbd_counts) %in% unmatched)], counts_all[which(names(gbd_counts) %in% unmatched)])
colnames(missing_table) <- c('loc', 'GBD_obs',  'LBD_2017', 'LBD_2018', 'LBD_all')
missing_table <- as.data.table(missing_table)
missing_table <- missing_table[-which(is.na(missing_table$GBD_obs) == TRUE)]

