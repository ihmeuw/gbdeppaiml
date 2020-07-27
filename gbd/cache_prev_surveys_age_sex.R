### Setup
rm(list=ls())
windows <- Sys.info()[1][["sysname"]]=="Windows"
root <- ifelse(windows,"J:/","/home/j/")
user <- ifelse(windows, Sys.getenv("USERNAME"), Sys.getenv("USER"))
code.dir <- paste0(ifelse(windows, "H:", paste0("/homes/", user)), "/HIV/")

## Packages
library(data.table); library(survey); library(assertable)


### Paths
##out.dir <- paste0("/ihme/hiv/epp_output/gbd19/", run.name, "/")
out.dir <- paste0("/ihme/hiv/epp_input/gbd20/")
dir.create(out.dir, showWarnings = F)
out.path <- paste0("/ihme/hiv/epp_input/gbd20/prev_surveys_ind.csv")

### Change this to the newest extraction sheet
supp.survey.path <- paste0("/ihme/hiv/data/prevalence_surveys/avinav_GBD2019_prevalence_surveys_decomp4_FORUSE.csv")

### Functions
library(mortdb, lib = "/share/mortality/shared/r")

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

## Nigeria - no NGA surveys in the extraction currently so throws an error
## TODO: Update Ubcov extraction to actually match admin_1_id
# ngadata <- data3[grepl('NGA', iso3)]
# data3 <- data3[!grepl('NGA', iso3)]
# ngadata[, temp := tolower(admin_1)]
# ngadata[, temp := paste0(toupper(substr(temp, 1, 1)), substr(temp, 2, length(temp)))]
# ngadata[temp %in% c('Cross river', 'Cross-river'), temp := 'Cross River']
# ngadata[temp %in% c('Fct abuja', 'Fct-abuja'), temp := 'FCT (Abuja)']
# ngadata[temp %in% c('Akwa-ibom', 'Akwa ibom'), temp := 'Akwa Ibom']
# nga.table <- loc.table[grepl('NGA_', ihme_loc_id),.(ihme_loc_id, temp = location_name)]
# ngadata <- merge(ngadata, nga.table, by = 'temp', all.x = T)
# ngadata[!is.na(temp), admin_1_id := ihme_loc_id]
# ngadata[!is.na(temp), iso3 := admin_1_id]
# ngadata[,c('temp', 'ihme_loc_id') := NULL]
# data3 <- rbind(data3, ngadata, use.names = T)

data3 <- data3[!is.na(sex_id) & !is.na(age_year)]

data3[, age_year := age_year - age_year %% 5]
data3[age_year > 80, age_year := 80]

#Adjustments to separate 2 MWI surveys
data3[survey_name=="MWI_ICAP_PHIA", int_year := 2016]
data3[survey_name=="MWI_ICAP_PHIA", year := 2016]
data3[nid=="218581", int_year := 2015]

data3[,loc_year := paste0(iso3,"_",year)] 

#bring in report data (not extracted yet)
supp.survey <- fread(supp.survey.path)[iso3 %in% gbd.locs]


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
        data.as[,zero := NA]
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
        data.as <- data.as[,list(year = median(int_year), prev = weighted.mean(hiv_test, hiv_weight, na.rm = T), n = .N),by='iso3,nid,survey_name']
          if("zero" %in% colnames(data.as)){
            #GBD 20 change: use Wilson approximation for 0 SE
            new_se = sqrt(1/(4 * nrow(data.as)^2) * qnorm(0.975)^2)
            data.table(iso3 = loc, year = data.as[,"year", with = F][[1]], age_year = a, sex_id = sex, prev =data.as[,"prev", with = F][[1]], se = new_se, n = data.as[,"n", with = F][[1]])
          } else {
            d <- data.table(iso3 = loc, year = data.as[,"year", with = F][[1]], age_year = a, sex_id = sex, prev =data.as[,"prev", with = F][[1]], se = SE(t)[[1]], n = data.as[,"n", with = F][[1]])
        }
      } else { 
        if(length(unique(data.as$psu)) == 1) {
          s <- svydesign(id = ~hh_id, strata = ~strata, weights = ~hiv_weight, data = data.as)
        } else {
          s <- svydesign(ids = ~psu, strata = ~strata, weights = ~hiv_weight, data = data.as, nest = TRUE)
        }	
        
        t <- svymean(~hiv_test,s)
        data13 <- data.as[,list(year = median(int_year), prev=weighted.mean(hiv_test, hiv_weight, na.rm=T), n=.N),by='iso3,nid,survey_name']
        
        if("zero" %in% colnames(data.as)){
          #GBD 20 change: use Wilson approximation for 0 SE
          new_se = sqrt(1/(4 * nrow(data.as)^2) * qnorm(0.975)^2)
          d <- data.table(iso3 = loc, year = data13[,"year", with = F][[1]], age_year = a, sex_id = sex, prev = 0.00, se = new_se, n = data13[,"n", with = F][[1]])
        } else {
          d <- data.table(iso3 = loc, year = data13[,"year", with = F][[1]], age_year = a, sex_id = sex, prev = coef(t)[[1]], se = SE(t)[[1]], n = data13[,"n", with = F][[1]])
        }
       
      }
      return(d)
    }))
    return(temp2)
  }))
  return(temp1)
}))

#Some checks
nid.dt <- unique(data3[, .(int_year, iso3, nid)])
nid.dt[, year := as.numeric(int_year)]
temp <- merge(data4, nid.dt, by = c("year", "iso3"), all.x = T)


# Align years to facilitate EPP-ASM ane ID duplicates with microdata
supp.survey[iso3 == "AGO" & year == 2016, year := 2015]
supp.survey[iso3 == "TZA" & year == 2016, year := 2017]

temp[iso3 == "AGO" & year == 2016, year := 2015]
temp[iso3 == "ZWE" & year == 2011, year := 2010] 

#Format supplemental surveys and remove underlying nid_iso3_loc_years
temp[,nid_loc_year := paste0(nid,"_",iso3,"_",int_year)]
supp.survey[,loc_year := gsub(" ","",loc_year)]
supp.survey[,nid_loc_year := paste0(nid,"_",iso3,"_",year)]
supp.survey[,n := gsub(",","",n)]
supp.survey[,nid_loc_year := gsub(" ","",nid_loc_year)]
supp.survey.rm = supp.survey[!which(supp.survey$nid_loc_year %in% unique(temp$nid_loc_year) & nchar(supp.survey$age_year) <= 2 & !sex_id==3)]

#Bind them
out.dt = rbind(temp,supp.survey.rm,fill=T)

#Align years
#Check if a location has back to back surveys
adj_years = list()

for(loc_i in unique(out.dt$iso3)){
  print(loc_i)
  yrs = sort(out.dt[iso3 == loc_i, unique(year)])
  yrs = data.table(survey_years = yrs,difference = c(NA,diff(yrs)))
  if(nrow(yrs[difference == abs(1)]) >= 1){
    print(paste0("Double Check ",loc_i))
    adj_years <- rbind(adj_years,paste0(loc_i," year ",yrs[difference %in% c(0,1,-1),survey_years]))
  }
}

#Legitimate ones with adjacent years - 
#157064 is 2016 ZAF DHS, and 357200 is 2017 Human Sciences Research Council
#MWI had a PHIA and DHS
#CMR had a PHIA and DHS
#ZWE had 157066 is DHS 2015 for ZWE, 287631 is ICAP 2015 - 2016
#SWZ: problem because the report has a different NID, excluded below

# Outlier surveys or report data
out.dt <- out.dt[!(grepl("ZAF", iso3) & year %in% c(2002, 2008,2009))] #Historical decision, reason unclear
out.dt <- out.dt[!(iso3 %in% c("CMR") & year == 2018)] #DHS survey implausibly low
out.dt <- out.dt[!out.dt$nid == '409506'] ### Using microdata instad of report data for SWZ 2017
out.dt <- out.dt[!(grepl("ETH",iso3) & nid =='414568')] ##This surey was urban-only
out.dt <- out.dt[!(iso3=="ZMB" & age_year %in% c("15-59","50-59"))]
out.dt <- out.dt[!(iso3=="NAM" & year==2008)] #Survey was only in capital region

# ID rows with more than 1 prevalence value for the same id vars  - needs further investigation
save = aggregate(data = out.dt, prev ~ year + iso3 + age_year + sex_id, function(x) length(unique(x)))
if(nrow(save[save$prev > 1,]) > 0){
  stop("need to fix")
}


#CHanges: DOM, ZMB, a lot of Kenya, CMR, mWI, TZA, GIN, all ZAF, ZWE, NAM, 
#DOM: 2013 was duplicated earlier
#ZMB has some additional aggregqte rows, removing in case it throws off model
#GIN 2005 aggregate is doubled - should be fixed in raw extraction now
#ZAF had 2012 all doubled 
#CMR outliered here instead
#ZMB, age 40 year 2010 was doubled previously


#Use 15-49 for Kenya subnationals because strata are too small
#SUB IN KENYA FOR OLD PREVALENCE SHEET
kenya_extractions = fread("/share/hiv/data/prevalence_surveys/kenya_extractions.csv")[grepl("KEN",iso3)]
old = kenya_extractions[year != 2018 & sex_id==3]
new = kenya_extractions[year==2018]
all_keny = rbind(old,new)
out.dt = out.dt[!grepl("KEN",iso3)]
out.dt <- rbind(out.dt,all_keny[,.(year,iso3,age_year,sex_id,prev,se,n,nid)],fill=TRUE,use.names=TRUE)
out.dt <- out.dt[!(iso3 %in% c("KEN_35623", "KEN_35662") & year == 2008)] #Outlier values

#Use column for run locations only
# run_locs = loc.table[(epp==1 & !grepl("IND",ihme_loc_id)) | ihme_loc_id %in% c("MRT","COM","STP"),ihme_loc_id]
run_locs = loc.table[epp==1  | ihme_loc_id %in% c("MRT","COM","STP"),ihme_loc_id]

out.dt[,use:=TRUE]

save_all <- out.dt[!iso3 %in% run_locs]
save_all[,use:=FALSE]

out.dt <- out.dt[iso3 %in% run_locs]

#Situation 1: we have age-sex specific surveys, with the same NID as aggregate version
overlap = out.dt[age_year==15 & sex_id==1,unique(nid)]
out.dt[age_year %in% c("15-49","15-64")  & nid %in% unique(overlap),use := FALSE] #89 overlaping NIDs

#Situation 2: we have age-aggregate, sex specific surveys to differentiate from sex-aggregate with overlapping NIDs
overlap = out.dt[age_year %in% c("15-49","15-64") & sex_id==2,unique(nid)]
out.dt[age_year %in% c("15-49","15-64")  & sex_id==3 & nid %in% unique(overlap),use := FALSE] 

#The remaining ones should be checked manually to see what survey to use
combos = out.dt[use==TRUE & age_year %in% c("15-49","15-64") ,.(iso3,year)]
look_further <- list()
for(i in 1:nrow(combos)){
  if(nrow(out.dt[iso3 %in% combos[i,"iso3"] & year %in% combos[i,"year"] & use==TRUE]) > 2){
    look_further <- rbind(look_further,combos[i,])
  }
}

#Checked for GBD 2020: All fine
#Exception NER because we don't have SE or sample size for sex-specific survey
out.dt[iso3=="NER" & year==2002 & sex_id ==3, use := TRUE]
out.dt[iso3=="NER" & year==2002 & sex_id %in% c(1,2), use := FALSE]

#SItuation 4: We have age-sex-specific and age-specific, sex aggregate with same NID
overlap = out.dt[age_year==15 & sex_id==1,unique(nid)]
out.dt[age_year %in% 0:80 & sex_id==3 & nid %in% unique(overlap),use := FALSE] 

#Fix 0s and missing SE
#Formula given by Hmwe March 2020 (Wilson approximation)
out.dt[,n := as.numeric(n)]
out.dt[is.na(se) & n > 0 , se :=  sqrt(((1/n) * prev * (1 - prev )) + 1/(4 * n^2) * qnorm(0.975)^2)]
out.dt[prev == 0.000, se :=  sqrt(((1/n) * prev * (1 - prev )) + 1/(4 * n^2) * qnorm(0.975)^2)]
out.dt[prev == 0.000, prev := 0.0005]

#Limit the age groups to those used in EPP
out.dt[!age_year %in% c(15:59,"15-49","15-64"), use := FALSE]

#Bind back all surveys
out.dt = rbind(out.dt,save_all)
#out.dt[grepl("IND_",iso3) & age_year=="15-49", use := TRUE]
out.dt[prev == 0.000 & use==TRUE, prev := 0.0005]

write.csv(out.dt[,.(year,iso3,age_year,sex_id,prev,se,n,nid_loc_year, use)],"/ihme/hiv/epp_input/gbd20/prev_surveys_ind.csv",row.names = FALSE)

