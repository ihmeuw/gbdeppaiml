##Set up --------------------------- 
## Script name: compile_draws.R
## Purpose of script: Average across draws
##
## Author: Maggie Walters
## Date Created: 2018-04-11
## Email: mwalte10@uw.edu
## 
##
## Notes: Created by Tahvi Frank and modified for GBD20 by Maggie Walters
## Some arguments are likely to stay constant across runs, others we're more likely to test different options.
## The arguments that are more likely to vary are pulled from the eppasm run table
##

## Used in basically every script
rm(list = ls())
Sys.umask(mode = "0002")
windows <- Sys.info()[1][["sysname"]]=="Windows"
root <- ifelse(windows,"J:/","/home/j/")
user <- ifelse(windows, Sys.getenv("USERNAME"), Sys.getenv("USER"))

# source(paste0(ifelse(windows, "H:", paste0("/ihme/homes/", user)), "/gbdeppaiml/gbd/00_req_packages.R"))



args <- commandArgs(trailingOnly = TRUE)
print(args)
if(length(args) == 0){
  array.job = FALSE
  run.name = '220329_maggie'
  loc <- 'AGO'
  draw.fill <- F
  paediatric <- T
  n = 1000
}else{
  run.name <- args[1]
  array.job <- as.logical(args[2])
  draw.fill <- as.logical(args[4])
  paediatric <- as.logical(args[5])

}

test = NULL
gbdyear = 'gbd20'
h_root = '/homes/mwalte10/'
lib.loc <- paste0(h_root,"R/",R.Version(),"/",R.Version(),".",R.Version())
.libPaths(c(lib.loc,.libPaths()))
packages <- c('fastmatch')
for(p in packages){
  if(p %in% rownames(installed.packages())==FALSE){
    install.packages(p)
  }
  library(p, character.only = T)
}

#library(data.table); library(mvtnorm); library(survey); library(ggplot2); library(plyr); library(dplyr); library(assertable); 
library(parallel)
library(data.table)
gbdeppaiml_dir <- paste0(ifelse(windows, "H:", paste0("/ihme/homes/", user)), "/gbdeppaiml/")
eppasm_dir <- paste0(ifelse(windows, "H:", paste0("/ihme/homes/", user)), "/eppasm/")
library(mortdb, lib ="/mnt/team/mortality/pub/shared/r/4")
library(assertable)

# setwd(eppasm_dir)
# devtools::load_all()
# setwd(gbdeppaiml_dir)
# devtools::load_all()
if(!array.job & length(args) > 0){
  loc <- args[3]
  n = 1000
}

# Array job ---------------------------------------
if(array.job){
  array.dt <- fread(paste0('/ihme/hiv/epp_input/gbd20/',run.name,'/array_table.csv'))
  task_id <- as.integer(Sys.getenv("SGE_TASK_ID"))
  loc <- array.dt[task_id,loc_scalar]
}


## Functions
fill_draws <- function(fill.dt,type=NULL){
  missing <- setdiff(1:n, unique(fill.dt$run_num))
  print(length(missing))
  if(length(missing) > 0){
    have.draws <- unique(fill.dt$run_num)
    need.draws <- missing
    for(draw in need.draws) {
      print(draw)
      if(length(have.draws) > 1){
        replace.draw <- sample(have.draws, 1)
      }else{replace.draw <- have.draws}
      replace.dt <- fill.dt[run_num == replace.draw]
      replace.dt[, run_num := draw]
      fill.dt <- rbind(fill.dt, replace.dt)
    }
    if(type=="adult"){
    missing_spec <- data.table(ihme_loc_id=rep(loc,length(missing)), missing=need.draws)
    write.csv(missing_spec,paste0('/ihme/hiv/epp_output/', gbdyear, '/', run.name, "/", loc,"/missing_or_neg_draws.csv"),row.names = FALSE)

    }
  }
  return(fill.dt)
}

### Tables
loc.table <- data.table(get_locations(hiv_metadata = T))
print('loc.table loaded')


### Code


    if(!is.null(test)){
      draw.path <- paste0('/ihme/hiv/epp_output/', gbdyear, '/', run.name, "/", loc, '_', test)
      
    }else{
      draw.path <- paste0('/ihme/hiv/epp_output/', gbdyear, '/', run.name, "/", loc)
      
    }
    draw.list <- list.files(draw.path)
  
    
    print('draw.list exists')
    ## subset out additional outputs (theta, under-1 splits)
    ## this could probably be tidied up
    draw.list <- draw.list[grepl('.csv', draw.list) & !grepl('theta_', draw.list) & !grepl('under_', draw.list)]
   # draw.list <- draw.list[gsub('.csv', '', draw.list) %in% 1:1000]
    
    dt <- lapply(draw.list, function(draw){
      print(draw)
      draw.dt <- fread(paste0(draw.path, '/', draw))
    })
    dt <- rbindlist(dt)
    dt <- melt(dt, id.vars = c('age', 'sex', 'year', 'run_num'))
    dt[value <0, value := 0]
    dt <- dcast(dt, age + sex + year + run_num  ~ variable, value.var = 'value')
    ##Sometimes there are negative values, need to replace
    
    # dt.check <- lapply(dt,function(draw.dt)
    #   try(assert_values(draw.dt,colnames(draw.dt),test="gte",0))
    # )
    # 
    # dt.check <- unlist(lapply(dt.check,function(check) !class(check)=="try-error"))
    # dt <- rbindlist(dt[dt.check])
    print('negative values replaced if necessary')
    
    if(draw.fill){
      dt <- fill_draws(dt,type="adult")
    }
    
    print('fill_draws done')
    compiled.path <- paste0('/ihme/hiv/epp_output/', gbdyear, '/', run.name, "/compiled/")
    dir.create(compiled.path, recursive = TRUE, showWarnings = FALSE)
    if(!is.null(test)){
      write.csv(dt, paste0(compiled.path, loc, '_', test, '.csv'), row.names = F)
      
    }else{
      write.csv(dt, paste0(compiled.path, loc, '.csv'), row.names = F)
      
    }
    print('first file saved')
    print(compiled.path)
    
    ## under 1 splits
    if(paediatric){
      split.list <- list.files(draw.path)
      split.list <- split.list[grepl('under_', split.list)]
      split.dt <- lapply(split.list, function(draw){
       
        draw.dt <- fread(paste0(draw.path, '/', draw))
      })
      
      
      dt.check <- lapply(split.dt,function(draw.dt)
        try(assert_values(draw.dt,colnames(draw.dt),test="gte",0))
      )
      
      dt.check <- unlist(lapply(dt.check,function(check) !class(check)=="try-error"))
      split.dt <- rbindlist(split.dt[dt.check])
      
      if(draw.fill){
        split.dt <- fill_draws(split.dt,type="child")
      }
      
      if(!is.null(test)){
        write.csv(split.dt, paste0(compiled.path, loc,'_', test, '_under1_splits.csv'), row.names = F)
        
      }else{
        write.csv(split.dt, paste0(compiled.path, loc, '_under1_splits.csv'), row.names = F)
        
      }
    }




