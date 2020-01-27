dir.create('/ihme/hiv/epp_output/gbd20/191224_trumpet/diagnostic/')
source(paste0(root,"/Project/Mortality/shared/functions/check_loc_results.r"))
library(mortdb, lib = "/ihme/mortality/shared/r")

### Tables
loc.table <- data.table(get_locations(hiv_metadata = T))

### Code
epp.list <- sort(loc.table[epp == 1 & grepl('1', group), ihme_loc_id])
loc.list <- epp.list
loc.list <- loc.list[-which(loc.list == 'CPV')]
loc.list <- loc.list[-which(loc.list == 'DJI')]

if(!est_India){
  loc.list <- loc.list[!grepl("IND",loc.list)]
}
loc.list.gbd <- gsub('.rds', '', list.files('/ihme/hiv/data/PJNZ_EPPASM_prepped_subpop/lbd_anc/2019/'))
loc.list.gbd <- loc.list.gbd[-which(loc.list == 'offset')]
loc.list.gbd <- loc.list.gbd[-which(loc.list == 'NA')]

check_diff_plot <- function(new_name = '191224_trumpet', old_name = '190630_rhino2', loc){
  if(!file.exists(paste0('/ihme/hiv/data/PJNZ_EPPASM_prepped_subpop/lbd_anc/2019/', loc, '.rds'))){stop(paste0('new ', loc, ' not generated'))}
  new <- readRDS(paste0('/ihme/hiv/data/PJNZ_EPPASM_prepped_subpop/lbd_anc/2019/', loc, '.rds'))
  old <- readRDS(paste0('/ihme/hiv/data/PJNZ_EPPASM_prepped_subpop/', loc, '.rds'))
  # old <- fread(paste0('/home/j/WORK/11_geospatial/10_mbg/hiv/unaids_anc/anc_epp_extractions/2019/', loc, '/', loc, '_2019_UNAIDS_ANC_EXTRACTION.csv'))
  
  #old <- readRDS(paste0('/ihme/hiv/data/PJNZ_EPPASM_prepped/', loc, '.rds'))
  
  old <- attr(old, 'eppd')$ancsitedat
  old <- as.data.table(old)
  ##fix those that end with a space
  # setnames(old, 'Site', 'site')
  # setnames(old, 'Year', 'year')
  # setnames(old, 'Prev', 'prev')
  # setnames(old, 'N', 'n')
  # setnames(old, 'Group', 'subpop')
  old[,site := unlist(lapply(strsplit(as.character(old[,site]), split = ' '), paste, collapse = ' '))]
  ##fix those that start with a space
  old[unlist(lapply(strsplit(as.character(old[,site]), split = ' '), function(x) (x[1] == ''))), site := lapply(strsplit(as.character(old[,site]), split = ' ')[unlist(lapply(strsplit(as.character(old[,site]), split = ' '), function(x) (x[1] == '')))], function(x) paste(x[-1], collapse = ' '))]
  
  new <- as.data.table(new)
  old[,site_year := paste0(site, year, '_', prev)]
  new[,site_year := paste0(site, year, '_', prev)]
  new <- subset(new, type == 'ancss')
  new[,source := 'gbd']
  old[,source := 'lbd']
  
  old.only <- old[site_year %in% setdiff(old[,site_year], new[,site_year]),]
  new.only <- new[site_year %in% setdiff(new[,site_year], old[,site_year]),]
  both <- merge(old, new, by = c('site_year'))
  both[,source := 'both']
  both <- as.data.table(both)
  
  both <- both[,.(site_year, prev.x, source)]
  setnames(both, new = 'prev', old = 'prev.x')
  both[,x:= prev]
  both[,y:= prev]
  new <- new.only[,.(site_year, prev, source)]
  new[,x:=0.05]
  new[,y:=prev]
  old <- old.only[,.(site_year, prev, source)]
  old[,y:=0.05]
  old[,x:=prev]
  
  final <- rbind(both, new, old)
  
  pdf(file = paste0('/ihme/hiv/epp_output/gbd20/191224_trumpet/diagnostic/', loc, '.pdf'))
   ggplot(data = final, aes(x = x, y = y, color = source)) + geom_point() + ggtitle(loc)
  dev.off()
  
}
lapply(loc.list, check_diff_plot, new_name = '191224_trumpet', old_name = '190630_rhino2')


plot.dir <- paste0("/ihme/hiv/epp_output/gbd20/",'191224_trumpet',"/diagnostic/")

setwd(plot.dir)
# Combine location-specific plots
system(paste0("/usr/bin/ghostscript -dBATCH -dSAFER -DNOPAUSE -q -sDEVICE=pdfwrite -sOutputFile=plots.pdf -f *"))




diff_arise <- function(new_fp = 'gbd20/191224_trumpet', old_fp = 'gbd19/190630_rhino', loc){
  old <- readRDS(paste0('/ihme/hiv/epp_output/', old_fp, '/dt_objects/', loc, '_dt.RDS'))
  old <- attr(old, 'eppd')$ancsitedat
  
      if(file.exists(paste0('/share/hiv/data/PJNZ_EPPASM_prepped_subpop/', loc, '.rds'))){
        raw <- readRDS(paste0('/share/hiv/data/PJNZ_EPPASM_prepped_subpop/', loc, '.rds'))
      }else{
        if(file.exists(paste0('/share/hiv/data/PJNZ_EPPASM_prepped/', loc, '.rds'))){
          raw <- readRDS(paste0('/share/hiv/data/PJNZ_EPPASM_prepped/', loc, '.rds'))
        }
      }
    
  raw <- attr(raw, 'eppd')$ancsitedat
  new <- readRDS(paste0('/ihme/hiv/epp_output/', new_fp, '/dt_objects/', loc, '_dt.RDS'))
  new <- attr(new, 'eppd')$ancsitedat
  new$prev <- new$prev
  
  
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
  
  all <- intersect(old_new, raw_old)
  
  all <- raw[prev_year %in% all, .(year, prev)]
  all[,source:='all']
  all[,used := factor(1)]
  
  raw_old <- raw[prev_year %in% raw_old, .(year, prev)]
  raw_old[,source := 'raw_old']
  
  raw_new <- raw[prev_year %in% raw_new, .(year, prev)]
  raw_new[,source := 'raw_new']
  
  old_new <- old[prev_year %in% old_new, .(year, prev)]
  old_new[,source := 'old_new']
  
  raw.only <- raw[prev_year %in% raw.only, .(year, prev)]
  raw.only[,source := 'raw.only']
  raw.only[,used := factor(0)]
  
  old.only <- old[prev_year %in% old.only, .(year, prev)]
  old.only[,source := 'old.only']
  old.only[,used := factor(0)]
  
  new.only <- new[prev_year %in% new.only, .(year, prev)]
  new.only[,source := 'new.only']
  new.only[,used := factor(1)]
  

  
  
  final <- rbind(all, raw.only, old.only, new.only)
  final[,used := factor(used)]

  pdf(file = paste0('/ihme/hiv/epp_output/gbd20/191224_trumpet/diagnostic/', loc, '.pdf'), width = 10, height = 6)
  
 gg <-  ggplot(data = final, aes(x = year, y = prev,  color = source,shape = as.factor(used))) + geom_point() +
    ggtitle(loc) 
 print(gg)
  
  graphics.off()
  
  
}


lapply(loc.list[77:length(loc.list)], diff_arise, raw_fp = '191224_trumpet', lbd_fp = '', old_fp = '190630_rhino')
plot.dir <- paste0("/ihme/hiv/epp_output/gbd20/191224_trumpet/diagnostic/")
setwd(plot.dir)
# Combine location-specific plots
system(paste0("/usr/bin/ghostscript -dBATCH -dSAFER -DNOPAUSE -q -sDEVICE=pdfwrite -sOutputFile=plots.pdf -f *"))











































