set_toggles <- function(loc){
  if(loc == 'ETH_44862'){
    births <-    paste0('/ihme/hiv/epp_input/gbd19/190630_rhino2/births/', loc, '.csv')
    print(paste0(loc, ' births from rhino2 subbed in'))
    
  }
  
  # These locations do not have information from LBD team estimates
  # ZAF ANC data are considered nationally representative so no GeoADjust - this could be challenged in the future
  no_geo_adj <-  c(loc.table[epp ==1 & grepl("IND",ihme_loc_id),ihme_loc_id],
                   "PNG","HTI","DOM", 'CPV', loc.table[epp ==1 & grepl("ZAF",ihme_loc_id),ihme_loc_id], 'STP', 'KEN_35626', 'MRT', 'COM')
  
  if(loc %in% c('ZWE', 'MWI')){
    geoadj_test <- TRUE
  }else{
    geoadj_test <- FALSE
  }
  # ANC data bias adjustment
  if(geoadjust & !loc %in% no_geo_adj){
    geoadjust  <- TRUE
  } else {
    geoadjust  <- FALSE
  }
  print(paste0(loc, ' geoadjust set to ', geoadjust))
  
  if(!loc %in% unlist(strsplit(list.files('/share/hiv/data/PJNZ_EPPASM_prepped_subpop/lbd_anc/2019/'), '.rds'))){
    lbd.anc <- FALSE
  }
  if(grepl('IND',loc)){
    lbd.anc <- FALSE
  }
  
  if(grepl('ZAF', loc)){
    lbd.anc <- FALSE
  }
  if(grepl('PNG', loc)){
    lbd.anc <- FALSE
  }
  print(paste0(loc, ' lbd.anc set to ', lbd.anc))
  
  ##Need to figure out where to get these
  if(loc %in% c("MAR","MRT","COM")){
    sexincrr.sub <- FALSE
    print(paste0(loc, ' subincrr.sub switch to ', sexincrr.sub))
    }
  return(c(geoadjust, lbd.anc))
  
}









