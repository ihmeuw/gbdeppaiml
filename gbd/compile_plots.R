### Setup
rm(list=ls())
windows <- Sys.info()[1][["sysname"]]=="Windows"
root <- ifelse(windows,"J:/","/home/j/")
user <- ifelse(windows, Sys.getenv("USERNAME"), Sys.getenv("USER"))
code.dir <- paste0(ifelse(windows, "H:", paste0("/homes/", user)), "/gbdeppaiml/")

## Packages
library(data.table); library(mvtnorm); library(survey);

## Arguments
args <- commandArgs(trailingOnly = TRUE)
print(args)
if(length(args) > 0) {
  run.name <- args[1]
  gbdyear <- args[2]
} else {
  run.name <- '201209_socialdets'
  gbdyear <- 'gbd20'
}

dir <- paste0("/ihme/hiv/epp_output/", gbdyear ,'/', run.name, "/")
###### Combine all plots of locations #####
setwd(paste0(dir, '/15to49_plots/'))
system(paste0("/usr/bin/ghostscript -dBATCH -dSAFER -DNOPAUSE -q -sDEVICE=pdfwrite -sOutputFile=", run.name, "_15to49.pdf -f $(ls | sort -n | xargs)"))
system(paste0("mv ", dir, "/15to49_plots/", run.name, "_15to49.pdf ", dir, '/', run.name, "_15to49.pdf"))


for(c.indicator in c('Incidence','Deaths', 'Prevalence')){
  setwd(paste0(dir, '/age_specific_plots/', c.indicator))
  system(paste0("/usr/bin/ghostscript -dBATCH -dSAFER -DNOPAUSE -q -sDEVICE=pdfwrite -sOutputFile=", run.name, "_", c.indicator, "_age_specific.pdf -f $(ls | sort -n | xargs)"))
  system(paste0("mv ", dir, "/age_specific_plots/", c.indicator, '/', run.name,  "_", c.indicator, "_age_specific.pdf ", dir, '/', run.name,  "_", c.indicator, "_age_specific.pdf"))
}


setwd(paste0(dir, '/paeds_plots/'))
system(paste0("/usr/bin/ghostscript -dBATCH -dSAFER -DNOPAUSE -q -sDEVICE=pdfwrite -sOutputFile=", run.name, "_pediatric.pdf -f $(ls | sort -n | xargs)"))
system(paste0("mv ", dir, "/paeds_plots/", run.name, "_pediatric.pdf ", dir, '/', run.name, "_pediatric.pdf"))
system(paste0("rm -r -f ", dir, "/paeds_plots/"))

