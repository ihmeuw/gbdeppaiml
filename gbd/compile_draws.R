### Setup
rm(list=ls())
windows <- Sys.info()[1][["sysname"]]=="Windows"
root <- ifelse(windows,"J:/","/home/j/")
user <- ifelse(windows, Sys.getenv("USERNAME"), Sys.getenv("USER"))
code.dir <- paste0(ifelse(windows, "H:", paste0("/homes/", user)), "/gbdeppaiml/")
## Packages
library(data.table); library(mvtnorm); library(survey)

## Arguments
args <- commandArgs(trailingOnly = TRUE)
print(args)
if(length(args) > 0) {
  run.name <- args[1]
  loc <- args[2]
  n <- as.integer(args[3])
  draw.fill <- as.logical(args[4])
  paediatric <- as.logical(args[5])
} else {
  run.name <- "191218_group2_tests"
  loc <- "PER"
  n <- 1
  draw.fill <- TRUE
  paediatric <- FALSE
}

## Functions
fill_draws <- function(fill.dt){
  missing <- setdiff(1:n, unique(fill.dt$run_num))
  if(length(missing) > 0){
    have.draws <- unique(fill.dt$run_num)
    need.draws <- missing
    for(draw in need.draws) {
      if(length(have.draws) > 1){
        replace.draw <- sample(have.draws, 1)
      }else{replace.draw <- have.draws}
      replace.dt <- fill.dt[run_num == replace.draw]
      replace.dt[, run_num := draw]
      fill.dt <- rbind(fill.dt, replace.dt)
    }
  }
  return(fill.dt)
}

draw.path <- paste0('/ihme/hiv/epp_output/gbd19/', run.name, "/", loc)
draw.list <- list.files(draw.path)
## subset out additional outputs (theta, under-1 splits)
## this could probably be tidied up
draw.list <- draw.list[grepl('.csv', draw.list) & !grepl('theta_', draw.list) & !grepl('under_', draw.list)]
draw.list <- draw.list[gsub('.csv', '', draw.list) %in% 1:n]
dt <- rbindlist(lapply(draw.list, function(draw){
  draw.dt <- fread(paste0(draw.path, '/', draw))
}))

if(draw.fill){
  dt <- fill_draws(dt)
}

compiled.path <- paste0('/ihme/hiv/epp_output/gbd19/', run.name, "/compiled/")
dir.create(compiled.path, recursive = TRUE, showWarnings = FALSE)
write.csv(dt, paste0(compiled.path, loc, '.csv'), row.names = F)

## under 1 splits
if(paediatric){
  split.list <- list.files(draw.path)
  split.list <- split.list[grepl('under_', split.list)]
  split.dt <- rbindlist(lapply(split.list, function(draw){
    draw.dt <- fread(paste0(draw.path, '/', draw))
  }))
  if(draw.fill){
    split.dt <- fill_draws(split.dt)
  }
  write.csv(split.dt, paste0(compiled.path, loc, '_under1_splits.csv'), row.names = F)


}
