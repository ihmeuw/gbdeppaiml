################################################################################
## Purpose: 
## Date created: 
## Date modified:
## Author: Austin Carter, aucarter@uw.edu
## Run instructions: 
## Notes:
################################################################################

### Setup
rm(list=ls())
windows <- Sys.info()[1][["sysname"]]=="Windows"
root <- ifelse(windows,"J:/","/home/j/")
user <- ifelse(windows, Sys.getenv("USERNAME"), Sys.getenv("USER"))
code.dir <- paste0(ifelse(windows, "H:", paste0("/homes/", user)), "/gbdeppaiml/")

## Packages
library(data.table); library(ggplot2)

# Arguments
args <- commandArgs(trailingOnly = TRUE)
if(length(args) > 0) {
    run.name <- args[1]
} else {
    run.name <- "190129_rspline_1549dat"
}

### Paths
surv.path <- paste0("/share/hiv/epp_output/gbd19/", run.name, "/prev_surveys.csv")

pop.dir <- paste0('/ihme/hiv/epp_input/gbd19/', run.name, "/population_single_age/india_splitting_locs/")
pop.dir <- paste0('/ihme/hiv/epp_input/gbd19/', run.name, "/population_single_age/")

out.path = paste0('/ihme/hiv/epp_input/gbd19/', run.name, '/art_prop.csv')
plot.path <- paste0("/ihme/hiv/epp_output/gbd19/", run.name, "/art_prop.pdf")

### Functions
library(mortdb, lib = "/home/j/WORK/02_mortality/shared/r")

### Tables
loc.table <- data.table(get_locations(hiv_metadata = T))
# Values from NFHS-3 report https://dhsprogram.com/pubs/pdf/FRIND3/FRIND3-Vol1[Oct-17-2008].pdf
nat.urban06 <- 0.0035
nat.rural06 <- 0.0025

# Values from NFHS-4 report http://rchiips.org/NFHS/NFHS-4Reports/India.pdf
nat.urban15 <- 0.0038
nat.rural15 <- 0.0017

### Code
locs <- loc.table[grepl("IND", ihme_loc_id) & level == 5, ihme_loc_id]
loc.table[grepl("IND", ihme_loc_id) & level == 5,]
surv <- fread(surv.path)[iso3 %in% locs]

surv[, nyears := .N, by = iso3]
surv[year == 2016, year := 2015] # Because different years assigned collapsing of microdata

urban.locs <- loc.table[grepl("Urban", location_name), ihme_loc_id]
rural.locs <- loc.table[grepl("Rural", location_name), ihme_loc_id]

# Read in surveys and fill in missing years
for(loc in unique(surv[nyears != 2 & grepl("IND", iso3), iso3])) {
    subset.dt <- copy(surv[iso3 == loc])
    missing <- setdiff(unique(surv[grepl("IND", iso3), year]), unique(subset.dt$year))
    print(missing)
    if(loc %in% urban.locs){
        fill.dt <- data.table(iso3 = loc, year = missing, prev = ifelse(missing == 2006, nat.urban06, nat.urban15)) 
    } else {
        fill.dt <- data.table(iso3 = loc, year = missing, prev = ifelse(missing == 2006, nat.rural06, nat.rural15)) 
    }
    surv <- rbind(surv, fill.dt, fill = T)
}

# Apply population to survey rates
pop <- rbindlist(lapply(locs, function(loc) {
    #loc.id <- loc.table[ihme_loc_id == loc, location_id]
    #path <- paste0(pop.dir, loc.id, ".csv")
    path <- paste0(pop.dir, loc, ".csv")
    dt <- fread(path)
    collapse.dt <- dt[, .(population = sum(population)), by = .(year_id, location_id)]
}), fill = T)
pop <- merge(pop, loc.table[, .(ihme_loc_id, location_id)], by = "location_id")
setnames(pop, c("ihme_loc_id", "year_id"), c("iso3", "year"))
merge.dt <- merge(surv, pop, by = c("iso3", "year"))
merge.dt[, count := prev * population]


# Calculate proportions and output
merge.dt[, prop := count / sum(count), by = year]

out.dt <- copy(merge.dt[, .(prop = mean(prop)), by = iso3])		

# Plot
pdf(plot.path)
gg <- ggplot(out.dt) + geom_point(aes(x = iso3, y = prop))
print(gg)
dev.off()

## Add Kenya
prop.path <- paste0("/share/hiv/epp_input/gbd19/KEN_ART_props.csv")
prop.dt <- fread(prop.path)
ken.prop <- prop.dt[, .(ihme_loc_id, prop_pepfar)]
setnames(ken.prop, "prop_pepfar", "prop")
setnames(out.dt, "iso3", "ihme_loc_id")
out.dt <- rbind(out.dt, ken.prop)

write.csv(out.dt, out.path, row.names = F)

### End