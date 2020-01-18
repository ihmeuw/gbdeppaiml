new.dir <- '/ihme/hiv/data/PJNZ_EPPASM_prepped_subpop/lbd_anc/2019/'
dir.create(paste0(new.dir, 'offset/'))
old.dir <- '/ihme/hiv/data/PJNZ_EPPASM_prepped_subpop/lbd_anc/offset/'
loc.list <- gsub('.rds', '',list.files(old.dir)[grep('.rds', list.files(old.dir))])
for(loc in loc.list){
  if(!file.exists(paste0(new.dir, loc, '.rds'))){
    file.copy(from = paste0(old.dir, loc, '.rds'), to = paste0(new.dir, 'offset/', loc, '.rds'))
    next
  }
  new <- readRDS(paste0(new.dir, loc, '.rds'))
  old <- readRDS(paste0(old.dir, loc, '.rds'))
  new[,site_year := paste0(site, year)]
  #new[,subpop := NULL]
  new <- unique(new)
  
  keep.cols <- colnames(old)
  keep.cols <- keep.cols[-which(keep.cols %in% c('adm0_mean', 'adm0_lower', 'adm0_upper',
                                                 'ihme_loc_id', 'site_pred', 'high_risk',
                                                 'subpop'))]
  old <- old[,.(site_year, adm0_mean, adm0_lower, adm0_upper, site_pred)]
  
  dt <- merge(new, old, by = site_year)
  saveRDS(dt, paste0(new.dir, 'offset/', loc, '.rds'))
}
