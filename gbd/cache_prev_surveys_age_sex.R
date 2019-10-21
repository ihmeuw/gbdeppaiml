
### Setup
rm(list=ls())
windows <- Sys.info()[1][["sysname"]]=="Windows"
root <- ifelse(windows,"J:/","/home/j/")
user <- ifelse(windows, Sys.getenv("USERNAME"), Sys.getenv("USER"))
code.dir <- paste0(ifelse(windows, "H:", paste0("/homes/", user)), "/HIV/")

## Packages
library(data.table); library(survey)

## Arguments
args <- commandArgs(trailingOnly = TRUE)
if(length(args) > 0) {
  run.name <- args[1]
} else {
  run.name <- '190503_all'
}
### Paths
out.dir <- paste0("/ihme/hiv/epp_output/gbd19/", run.name, "/")
dir.create(out.dir, showWarnings = F)
out.path <- paste0(out.dir, "prev_surveys.csv")
supp.survey.path <- paste0(root, "WORK/04_epi/01_database/02_data/hiv/04_models/gbd2015/02_inputs/supplement_survey_data_2017.csv")

### Functions
library(mortdb, lib = "/home/j/WORK/02_mortality/shared/r")

### Tables
loc.table <- data.table(get_locations(hiv_metadata = T))

### Code
## GBD locs
gbd.locs <- loc.table$ihme_loc_id

#bring in geospatial microdata data
geos_dir <- "/ihme/limited_use/LIMITED_USE/LU_GEOSPATIAL/geo_matched/hiv_gbd/"
versions <- grep("[[:digit:]]*_[[:digit:]]*_[[:digit:]]*", list.files(geos_dir, full.names=F), value=T)
newest <- versions[which.max(as.Date(versions, format="%Y_%m_%d"))]
load(dir(paste0(geos_dir, newest), pattern=".Rdata", full.names=T)[1])
setnames(gbd_all, "country", "iso3")
data3 <- gbd_all[!is.na(iso3) & !is.na(hiv_test) & !is.na(hiv_weight),]

## Kenya
# Use admin2 data
data3[grepl("KEN", iso3) & !(admin_2_id == "KEN" | admin_2_id == "" | is.na(admin_2_id)), iso3 := admin_2_id]
# Exclude if not mapped to an admin2
data3 <- data3[iso3 != "KEN"]

# South Africa
data3[grepl("ZAF", iso3) & !is.na(admin_1_id), iso3 := admin_1_id]

# Ethiopia
data3[grepl("ETH", iso3) & !is.na(admin_1_id), iso3 := admin_1_id]

# Nigeria !!! Not currently mapped to admin_1_id !!!
data3[grepl("NGA", iso3) & !is.na(admin_1_id), iso3 := admin_1_id]

# India
data3[grepl("IND", iso3) & !is.na(admin_1_id), iso3 := admin_1_id]

# Repeat India states U/R
ind.copy <- copy(data3[grepl("IND", iso3)])
ind.copy[, iso3 := admin_1_urban_id]
data3 <- rbind(data3, ind.copy)

## Nigeria
## TODO: Update Ubcov extraction to actually match admin_1_id
ngadata <- data3[grepl('NGA', iso3)]
data3 <- data3[!grepl('NGA', iso3)]
ngadata[, temp := tolower(admin_1)]
ngadata[, temp := paste0(toupper(substr(temp, 1, 1)), substr(temp, 2, length(temp)))]
ngadata[temp %in% c('Cross river', 'Cross-river'), temp := 'Cross River']
ngadata[temp %in% c('Fct abuja', 'Fct-abuja'), temp := 'FCT (Abuja)']
ngadata[temp %in% c('Akwa-ibom', 'Akwa ibom'), temp := 'Akwa Ibom']
nga.table <- loc.table[grepl('NGA_', ihme_loc_id),.(ihme_loc_id, temp = location_name)]
ngadata <- merge(ngadata, nga.table, by = 'temp', all.x = T)
ngadata[!is.na(temp), admin_1_id := ihme_loc_id]
ngadata[!is.na(temp), iso3 := admin_1_id]
ngadata[,c('temp', 'ihme_loc_id') := NULL]
data3 <- rbind(data3, ngadata, use.names = T)

data3[,loc_year := paste0(iso3,"_",year)] 

data3 <- data3[!is.na(sex_id) & !is.na(age_year)]

data3[, age_year := age_year - age_year %% 5]
data3[age_year > 80, age_year := 80]


#bring in report data (not extracted yet)
supp.survey <- fread(supp.survey.path)[iso3 %in% gbd.locs & outlier == 0]
supp.survey[, outlier := NULL]

## process microdata
#need this option unfortunately
options(survey.lonely.psu="adjust")
data4 <- rbindlist(lapply(unique(data3$loc_year), function(loc.year){
  print(loc.year)
  data5 <- data3[loc_year == loc.year]
  loc <- unique(data5$iso3)
  temp1 <- rbindlist(lapply(unique(data5$age_year), function(a) {
    print(a)
    data.a <- data5[age_year == a]
    temp2 <- rbindlist(lapply(unique(data.a$sex_id), function(sex) {
      print(sex)
      data.as <- data.a[sex_id == sex]
      # throw out single observations
      if(nrow(data.as) < 2) {
        return(data.table())
      }
      if(all(data.as$hiv_test == 0)) {
        # impute a half positive observation to get uncertainty for 0 prevalence
        hold <- copy(data.as[1,])
        hold$hiv_test <- 0.5
        data.as <- rbind(data.as, hold)
      }
      if(all(is.na(data.as$hh_id))) {
        data.as[, hh_id := .I]
      }
      if(nrow(data.as[is.na(strata),]) > 0){
        print(paste0((nrow(data.as[is.na(strata),])/nrow(data.as))*100,"% missing strata for ",loc.year))
        if(nrow(data.as[!is.na(strata),]) == 0) {
          data6 <- data.as
          s <- svydesign(ids = ~psu, weights = ~hiv_weight,data = data.as)
        } else {
          data6 <- data.as[!is.na(strata),]
          s <- svydesign(ids = ~psu, strata = ~strata, weights = ~hiv_weight,data = data6,check.strata = TRUE)	                
        }
        t <- svymean(~hiv_test,s)
        data.as <- data.as[,list(year = median(int_year), prev = weighted.mean(hiv_test, hiv_weight, na.rm = T), n = .N),
                           by='iso3,nid,survey_name']
        d <- data.table(iso3 = loc, year = data.as[,"year", with = F][[1]], age_year = a, sex_id = sex, prev =data.as[,"prev", with = F][[1]], se = SE(t)[[1]], n = nrow(data6))
      } else { 
        if(length(unique(data.as$psu)) == 1) {
          s <- svydesign(id = ~hh_id, strata = ~strata, weights = ~hiv_weight, data = data.as)
        } else {
          s <- svydesign(ids = ~psu, strata = ~strata, weights = ~hiv_weight, data = data.as, nest = TRUE)
        }	          	
        t <- svymean(~hiv_test,s)
        data13 <- data.as[,list(year = median(int_year), prev=weighted.mean(hiv_test, hiv_weight, na.rm=T), n=.N),
                          by='iso3,nid,survey_name']
        d <- data.table(iso3 = loc, year = data13[,"year", with = F][[1]], age_year = a, sex_id = sex, prev =coef(t)[[1]], se = SE(t)[[1]], n = nrow(data.as))
      }
      return(d)
    }))
    return(temp2)
  }))
  return(temp1)
}))
nid.dt <- unique(data3[, .(int_year, iso3, nid)])
nid.dt[, year := as.numeric(int_year)]
temp <- merge(data4, nid.dt, by = c("year", "iso3"), all.x = T)
out.dt <- rbind(temp, supp.survey, fill = T)

# Check for duplicates
loc.years <- data.table(melt(table(out.dt[, .(iso3, year, age_year, sex_id)])))
loc.years[value == 2]

# Outlier surveys
out.dt <- out.dt[!(grepl("ZAF", iso3) & year %in% c(2002, 2008))]
out.dt <- out.dt[!(iso3 %in% c("KEN_35623", "KEN_35662") & year == 2008)]

write.csv(out.dt, out.path, row.names = F)

### End