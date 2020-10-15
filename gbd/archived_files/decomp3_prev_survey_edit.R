### Setup
rm(list=ls())
windows <- Sys.info()[1][["sysname"]]=="Windows"
root <- ifelse(windows,"J:/","/home/j/")
user <- ifelse(windows, Sys.getenv("USERNAME"), Sys.getenv("USER"))
code.dir <- paste0(ifelse(windows, "H:", paste0("/homes/", user)), "/gbdeppaiml/")

## Packages
library(data.table)

## Arguments
run.name <- "190503_all"

### Paths
input.dir <- paste0("/ihme/hiv/epp_input/gbd19/", run.name, "/")

### Functions
library(mortdb, lib = "/home/j/WORK/02_mortality/shared/r")

### Tables
loc.table <- data.table(get_locations(hiv_metadata = T))

### Code
epp.list <- sort(loc.table[epp == 1 & grepl('1', group), ihme_loc_id])

past = fread('/share/hiv/epp_input/decomp19/190415_orca/prev_surveys.csv')
cur = fread("/ihme/hiv/epp_output/gbd19/", run.name, "/prev_surveys.csv")

past.ken = past[grepl('KEN', iso3)]
cur = cur[!grepl('KEN', iso3)]
past.ken[, age_year := '15-49']
past.ken[, sex_id := 3]
cur <- rbind(cur, past.ken, use.names = T)
old <- fread('/homes/tahvif/age_prev_surveys.csv')
for(loc in epp.list){
    print(loc)
    cur.loc = cur[iso3 == loc]
    past.loc = past[iso3 == loc]
    print(setdiff(unique(cur.loc$year), unique(past.loc$year)))
    print(setdiff(unique(past.loc$year), unique(cur.loc$year)))

}
past[, age_year := '15-49']
past[, sex_id := 3]
cur <- cur[!(iso3 == 'AGO' & year == 2016)]
cur <- cur[!(iso3 == 'BDI' & year == 2016)]
cur <- rbind(cur, past[iso3 == 'BDI' & year == 2002], use.names = T)
cur <- rbind(cur, past[iso3 == 'BEN'], use.names = T)
cur <- rbind(cur, past[iso3 == 'CAF'], use.names = T)
cur <- rbind(cur, past[iso3 == 'CPV'], use.names = T)
cur <- rbind(cur, past[iso3 == 'DJI'], use.names = T)
cur <- rbind(cur, past[iso3 == 'ERI'], use.names = T)
cur <- rbind(cur, past[iso3 == 'GNQ'], use.names = T)
cur <- rbind(cur, past[iso3 == 'LSO' & year == 2016], use.names = T)
cur <- rbind(cur, past[iso3 == 'NER' & year == 2002], use.names = T)
cur <- rbind(cur, past[iso3 == 'RWA' & year == 2014], use.names = T)
cur <- cur[!(iso3 == 'SEN' & year == 2017)]
cur <- rbind(cur, past[iso3 == 'SWZ' & year %in% c(2011, 2016)], use.names = T)
cur <- rbind(cur, past[iso3 == 'TZA' & year == 2016], use.names = T)
cur <- rbind(cur, past[iso3 == 'UGA' & year %in% c(2005, 2016)], use.names = T)
cur <- cur[!(grepl('ZAF', iso3) & year == 2016)]
cur <- rbind(cur, past[iso3 == 'ZWE' & year == 2016], use.names = T)
cur <- rbind(cur, past[iso3 == 'SLE' & year %in% c(2002, 2005)], use.names = T)

write.csv(cur, paste0("/ihme/hiv/epp_input/gbd19/", run.name, "/prev_surveys.csv"), row.names = F)
