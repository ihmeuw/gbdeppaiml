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
} else {
  run.name <- '190630_ind_noanc'
}

dir <- paste0("/ihme/hiv/epp_output/gbd19/", run.name, "/")
###### Combine all plots of locations #####
setwd(paste0(dir, '/15to49_plots/'))
system(paste0("/usr/bin/ghostscript -dBATCH -dSAFER -DNOPAUSE -q -sDEVICE=pdfwrite -sOutputFile=", run.name, "_15to49.pdf -f $(ls | sort -n | xargs)"))
system(paste0("mv ", dir, "/15to49_plots/", run.name, "_15to49.pdf ", dir, '/', run.name, "_15to49.pdf"))
#system(paste0("rm -r -f ", dir, "/15to49_plots/"))


for(c.indicator in c('Incidence','Deaths', 'Prevalence')){
  setwd(paste0(dir, '/age_specific_plots/', c.indicator))
  system(paste0("/usr/bin/ghostscript -dBATCH -dSAFER -DNOPAUSE -q -sDEVICE=pdfwrite -sOutputFile=", run.name, "_", c.indicator, "_age_specific.pdf -f $(ls | sort -n | xargs)"))
  system(paste0("mv ", dir, "/age_specific_plots/", c.indicator, '/', run.name,  "_", c.indicator, "_age_specific.pdf ", dir, '/', run.name,  "_", c.indicator, "_age_specific.pdf"))
  #system(paste0("rm -r -f ", dir, "/age_specific_plots/", c.indicator))
}


setwd(paste0(dir, '/paeds_plots/'))
system(paste0("/usr/bin/ghostscript -dBATCH -dSAFER -DNOPAUSE -q -sDEVICE=pdfwrite -sOutputFile=", run.name, "_pediatric.pdf -f $(ls | sort -n | xargs)"))
system(paste0("mv ", dir, "/paeds_plots/", run.name, "_pediatric.pdf ", dir, '/', run.name, "_pediatric.pdf"))
system(paste0("rm -r -f ", dir, "/paeds_plots/"))


# setwd(paste0("/ihme/hiv/epp_output/gbd19/190630_rhino2/final_plots"))
# system(paste0("/usr/bin/ghostscript -dBATCH -dSAFER -DNOPAUSE -q -sDEVICE=pdfwrite -sOutputFile=", run.name, "_diagnostics.pdf -f $(ls | sort -n | xargs)"))
# system(paste0("mv /ihme/hiv/epp_output/gbd19/190630_rhino2/final_plots/190630_rino2_diagnostics.pdf ", dir, '/', run.name, "_diagnostics.pdf"))
# system(paste0("rm -r -f ", dir, "/paeds_plots/"))

# 
# setwd(paste0(dir, '/hivq15_plots/'))
# system(paste0("/usr/bin/ghostscript -dBATCH -dSAFER -DNOPAUSE -q -sDEVICE=pdfwrite -sOutputFile=", run.name, "_adult_cdr.pdf -f $(ls | sort -n | xargs)"))
# system(paste0("mv ", dir, "/hivq15_plots/", run.name, "_adult_cdr.pdf ", dir, '/', run.name,"_adult_cdr.pdf"))
#system(paste0("rm -r -f ", dir, "/age_specific_plots/", c.indicator))