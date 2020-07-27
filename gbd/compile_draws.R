### Setup
rm(list=ls())
windows <- Sys.info()[1][["sysname"]]=="Windows"
root <- ifelse(windows,"J:/","/home/j/")
user <- ifelse(windows, Sys.getenv("USERNAME"), Sys.getenv("USER"))
code.dir <- paste0(ifelse(windows, "H:", paste0("/homes/", user)), "/gbdeppaiml/")
## Packages
library(data.table); library(mvtnorm); library(survey);library(assertable)
library(mortdb, lib = "/share/mortality/shared/r")


## Arguments
args <- commandArgs(trailingOnly = TRUE)
print(args)
if(length(args) > 0) {
   run.name <- args[1]
  #run.name <- "191002_sitar"
  loc <- args[2]
  n <- as.integer(args[3])
  draw.fill <- as.logical(args[4])
  paediatric <- as.logical(args[5])
  gbdyear <- 'gbd20'
} else {
  run.name <- "200713_yuka"
  loc <- "KEN_44796"
  n <- 1000
  draw.fill <- TRUE
  paediatric <- TRUE
}
gbdyear <- 'gbd20'
test = NULL

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
    draw.list <- draw.list[gsub('.csv', '', draw.list) %in% 1:n]
    
    dt <- lapply(draw.list, function(draw){
      draw.dt <- fread(paste0(draw.path, '/', draw))
    })
    
    ##Sometimes there are negative values, need to replace
    
    dt.check <- lapply(dt,function(draw.dt)
      try(assert_values(draw.dt,colnames(draw.dt),test="gte",0))
    )
    
    dt.check <- unlist(lapply(dt.check,function(check) !class(check)=="try-error"))
    dt <- rbindlist(dt[dt.check])
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




