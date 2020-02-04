library(mortdb, lib = "/ihme/mortality/shared/r")
library(data.table)
library(ggplot2)

args <- commandArgs(trailingOnly = TRUE)
print(args)
if(length(args) > 0) {
  new_fp <- args[1]
  old_fp <- args[2]
  loc <- args[3]
} else {
  new_fp <- 'gbd20/191224_trumpet'
  old_fp <- 'gbd19/190630_rhino2'
  loc <- 'MDG'
}

diag_dir <- paste0('/ihme/hiv/epp_output/', new_fp, '/diagnostic/')
dir.create(diag_dir)


diff_arise <- function(new_fp = 'gbd20/191224_trumpet', old_fp = 'gbd19/190630_rhino2', loc){
  if(!file.exists(paste0('/ihme/hiv/epp_output/', old_fp, '/dt_objects/', loc, '_dt.RDS'))){
    print(paste0('Dt object for the ', old_fp, ' run was not saved'))
  }
  old <- readRDS(paste0('/ihme/hiv/epp_output/', old_fp, '/dt_objects/', loc, '_dt.RDS'))
  old <- data.table(attr(old, 'eppd')$ancsitedat)
  old <- old[type == 'ancss']
  
  if(file.exists(paste0('/share/hiv/data/PJNZ_prepped/2019/', loc, '.rds'))){
    raw <- readRDS(paste0('/share/hiv/data/PJNZ_prepped/2019/', loc, '.rds'))
    
  }else{
    if(file.exists(paste0('/share/hiv/data/PJNZ_prepped/2018/', loc, '.rds'))){
      raw <- readRDS(paste0('/share/hiv/data/PJNZ_prepped/2018/', loc, '.rds'))
      
    }else{
      if(file.exists(paste0('/share/hiv/data/PJNZ_EPPASM_prepped_subpop/', loc, '.rds'))){
        raw <- readRDS(paste0('/share/hiv/data/PJNZ_EPPASM_prepped_subpop/', loc, '.rds'))
      }else{
        if(file.exists(paste0('/share/hiv/data/PJNZ_EPPASM_prepped/', loc, '.rds'))){
          raw <- readRDS(paste0('/share/hiv/data/PJNZ_EPPASM_prepped/', loc, '.rds'))
        }
      }
    }
  }
    
  raw <- data.table(attr(raw, 'eppd')$ancsitedat)
  raw <- raw[type == 'ancss']
  if(!file.exists(paste0('/ihme/hiv/epp_output/', new_fp, '/dt_objects/', loc, '_dt.RDS'))){
    print(paste0('Dt object for the ', new_fp, ' run was not saved'))
  }
  new <- readRDS(paste0('/ihme/hiv/epp_output/', new_fp, '/dt_objects/', loc, '_dt.RDS'))
  new <- data.table(attr(new, 'eppd')$ancsitedat)
  new <- new[type =='ancss']
  
  ##create a prev_year to find what are overlaps
  raw <- as.data.table(raw)
  old <- as.data.table(old)
  new <- as.data.table(new)
  
  raw[,prev_year := paste0(prev, '_', year)]
  old[,prev_year := paste0(prev, '_', year)]
  new[,prev_year := paste0(prev, '_', year)]
  
  raw.prevyear <- unique(raw$prev_year)
  old.prevyear <- unique(old$prev_year)
  new.prevyear <- unique(new$prev_year)
  
  raw.only <- intersect(setdiff(raw.prevyear, old.prevyear), setdiff(raw.prevyear, new.prevyear))
  old.only <- intersect(setdiff(old.prevyear, raw.prevyear), setdiff(old.prevyear, new.prevyear))
  new.only <- intersect(setdiff(new.prevyear, old.prevyear), setdiff(new.prevyear, raw.prevyear))
  
  raw_old <- intersect(raw.prevyear, old.prevyear)
  raw_new <- intersect(raw.prevyear, new.prevyear)
  old_new <- intersect(old.prevyear, new.prevyear)
  
  all.x <- intersect(old_new, raw_old)
  
  all <- raw[prev_year %in% all.x, .(year, prev)]
  all[,source:='all']
  all[,used := factor(1)]
  
  raw_old <- raw[prev_year %in% setdiff(raw_old, all.x), .(year, prev)]
  raw_old[,source := 'raw_old']
  raw_old[,used := factor(0)]
  
  
  raw_new <- raw[prev_year %in% setdiff(raw_new, all.x), .(year, prev)]
  raw_new[,source := 'raw_new']
  raw_new[,used := factor(1)]
  
  
  old_new <- old[prev_year %in% setdiff(old_new, all.x), .(year, prev)]
  old_new[,source := 'old_new']
  old_new[,used := factor(1)]
  
  
  raw.only <- raw[prev_year %in% raw.only, .(year, prev)]
  raw.only[,source := 'raw.only']
  raw.only[,used := factor(0)]
  
  old.only <- old[prev_year %in% old.only, .(year, prev)]
  old.only[,source := 'old.only']
  old.only[,used := factor(0)]
  
  new.only <- new[prev_year %in% new.only, .(year, prev)]
  new.only[,source := 'new.only']
  new.only[,used := factor(1)]
  

  
  
  final <- rbind(all, raw.only, old.only, new.only, raw_new, raw_old, old_new)
  final[,used := factor(used)]

  pdf(file = paste0('/ihme/hiv/epp_output/', new_fp, '/diagnostic/', loc, '.pdf'), width = 10, height = 6)
  
 gg <-  ggplot(data = final, aes(x = year, y = prev,  color = source,shape = as.factor(used))) + geom_point() +
    ggtitle(loc) 
    print(gg)
 graphics.off()
  
  
}


diff_arise(loc = loc, new_fp = new_fp, old_fp = old_fp)












































