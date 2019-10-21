### Setup
rm(list=ls())
windows <- Sys.info()[1][["sysname"]]=="Windows"
root <- ifelse(windows,"J:/","/home/j/")
user <- ifelse(windows, Sys.getenv("USERNAME"), Sys.getenv("USER"))
code.dir <- paste0(ifelse(windows, "H:", paste0("/homes/", user)), "/gbdeppaiml/prep_pjnz_data/")
date <- substr(gsub("-","",Sys.Date()),3,8)

## Packages
library(data.table)

## Arguments
cluster.project <- "proj_hiv"

### Functions
library(mortdb, lib = "/home/j/WORK/02_mortality/shared/r")


### Tables
loc.table <- data.table(get_locations(hiv_metadata = T))

### Code
epp.list <- sort(loc.table[epp == 1, ihme_loc_id])
loc.list <- epp.list


## Launch prepare locations file
for(loc in loc.list) {
    prep.files.string <- paste0("qsub -l m_mem_free=5G -l fthread=1 -l h_rt=12:00:00 -l archive -q all.q -P ", cluster.project, " ", 
                         "-e /share/temp/sgeoutput/", user, "/errors ",
                         "-o /share/temp/sgeoutput/", user, "/output ",
                         "-N ", loc, "_prep_data ",
                         "/homes/", user, "/gbdeppaiml/gbd/singR_shell.sh ", 
                         code.dir,"main_prep_data.R ", loc)
    print(prep.files.string)
    system(prep.files.string)
        
}
      
### End
