##Maggie Walters
##mwalte10@uw.edu
##this function can be used to ensure that all inputs are the correct dimension given years

check_inputs <- function(dt_obj){
  specfp <- attr(dt_obj, 'specfp')
  names <- names(specfp)
  copied <- c()
  for(name in names){
    dim_num <- dim(specfp[[name]])
    if(!any(dim_num) == specfp[['SIM_YEARS']]){
      next
    }else{
      if(which(dim_num == specfp[['SIM_YEARS']]) == 2){
        if(dim_num[2] >= specfp[['SIM_YEARS']]){
          next
        }
        stop_year <- specfp[[name]][,specfp[['SIM_YEARS']]]
        penul_year <- specfp[[name]][,specfp[['SIM_YEARS']] - 1]
        if(stop_year == penul_year){
          copied <- append(copied, name)
          next
        }
      }
      
      
      if(which(dim_num == specfp[['SIM_YEARS']]) == 3){
        if(dim_num[3] >= specfp[['SIM_YEARS']]){
          next
        }
        stop_year <- specfp[[name]][,,specfp[['SIM_YEARS']]]
        penul_year <- specfp[[name]][,,specfp[['SIM_YEARS']] - 1]
        if(any(stop_year == penul_year)){
          copied <- append(copied, name)
          next
        }
      }
      
      if(which(dim_num == specfp[['SIM_YEARS']]) == 4){
        if(dim_num[4] >= specfp[['SIM_YEARS']]){
          next
        }
        stop_year <- specfp[[name]][,,,specfp[['SIM_YEARS']]]
        penul_year <- specfp[[name]][,,,specfp[['SIM_YEARS']] - 1]
        if(any(stop_year == penul_year)){
          copied <- append(copied, name)
          next
        }
      }
      
      
    }
  }
  
  return(copied)
  
}





modify_dt <- function(dt){

  inputs <- check_inputs(dt)
  if(!is.null(inputs)){
    print(paste0(inputs, ' need to be extended to the appropriate year'))
  }
  
  
  ###########################################################################
  # Remove duplicates from ancsitedat -------------------------------------------
  ##########################################################################
  
  ancsitedat <- data.table(attr(dt,'eppd')$ancsitedat)
  
  if(any(is.na(ancsitedat$prev))){
    ancsitedat <-  data.table(attr(dt, 'eppd')$ancsitedat)[!is.na(prev)]
    attr(dt, 'eppd')$ancsitedat <- data.frame(ancsitedat)
  }
  
  ancsitedat <- data.table(attr(dt,'eppd')$ancsitedat)
  
  if(any(colnames(ancsitedat) == 'year_id')){
    setnames(ancsitedat, 'year_id', 'year')
    attr(dt,'eppd')$ancsitedat <- data.frame(ancsitedat)
  }
  ancsitedat <- data.table(attr(dt,'eppd')$ancsitedat)
  
  ##find copied observations
  if('subpop' %in% colnames(ancsitedat)){
    ancsitedat[,id := paste0(site, '_', subpop, '_', year)]
    if(grepl('NGA', loc)){
      ancsitedat[,id := NULL]
      attr(dt,'eppd')$ancsitedat <- data.frame(ancsitedat)
      
    }else{
      zero_offset <- ancsitedat[offset == 0,]
      if(nrow(unique(zero_offset)) != nrow(zero_offset)){
        zero_offset[,length := length(unique(id)), by = c('site', 'subpop', 'year')]
        keep <- zero_offset[length > 1]
        keep[,length := NULL]
      }else{
        keep = NULL
      }
      ancsitedat = ancsitedat[id %in% unique(zero_offset$id) & offset != 0,]
      ancsitedat <- rbind(ancsitedat, keep)
      ancsitedat[,id := NULL]
      attr(dt,'eppd')$ancsitedat <- data.frame(ancsitedat)
    }

  }

  
  if('subpop' %in% colnames(ancsitedat)){
    if(nrow(unique(ancsitedat[,.(site, subpop, year, used, prev, n, type, agegr, age,agspan)])) != nrow(ancsitedat) ){
      ancsitedat <- ancsitedat[!duplicated(ancsitedat[,.(site, subpop, year, used, prev, n, type, agegr, age,agspan)]),]
      attr(dt, 'eppd')$ancsitedat <- data.frame(ancsitedat)
      print('Removed duplicates from sitedat')
    }
    
  }else{
    if(nrow(unique(ancsitedat[,.(site,  year, used, prev, n, type, agegr, age,agspan)])) != nrow(ancsitedat) ){
      ancsitedat <- ancsitedat[!duplicated(ancsitedat[,.(site,  year, used, prev, n, type, agegr, age,agspan)]),]
      attr(dt, 'eppd')$ancsitedat <- data.frame(ancsitedat)
      print('Removed duplicates from sitedat')
    }
  }
  
 


###########################################################################
# Location Specific corrections -------------------------------------------
###########################################################################
  if(loc == 'NGA_25322'){
    attr(dt, 'eppd')$ancsitedat <- as.data.frame(as.data.table(attr(dt, 'eppd')$ancsitedat)[prev < 0.2,])
    print(paste0(loc, ' some high prevalences were removed'))
    
  }
  if(grepl('ETH', loc)){
    attr(dt, 'eppd')$hhs <-  subset(attr(dt, 'eppd')$hhs, year != '2018')
    print(paste0(loc, ' removed 2018 survey'))

      check = cbind(attr(dt, 'eppd')$anc.prev,use = attr(dt, 'eppd')$anc.used)
      check = subset(check, check[,'use'] == 1, drop = FALSE)
      check = check[,-ncol(check),drop = FALSE]
      attr(dt,'eppd')$ancsitedat <-  attr(dt,'eppd')$ancsitedat[ attr(dt,'eppd')$ancsitedat$site %in% rownames(check),] 
      print(paste0(loc, ' dropping use = FALSE ANC data'))
    
  }
  if(loc == 'IND_4842'){
    sub_in <- readRDS('/ihme/hiv/epp_output/gbd20/200213_violin/dt_objects/IND_4862_dt.RDS')
    sub_in <- attr(sub_in, 'specfp')$paedsurv_artcd4dist
    attr(dt, 'specfp')$paedsurv_artcd4dist <- sub_in
    print(paste0(loc, ' subbed in dt object from violin run'))
    
  }
  if(loc=="NGA_25343"){
    attr(dt,"eppd")$ancrtcens <- attr(dt,"eppd")$ancrtcens[1:2,]
    print(paste0(loc, ' ancrtcens was modified'))
    
    
  }
  if(grepl('NGA', loc)){
    temp <- attr(dt, 'specfp')$paedsurv_artcd4dist
    temp[temp < 0] <- 0
    attr(dt, 'specfp')$paedsurv_artcd4dist <- temp
    print(paste0(loc, ' removed less than zero paed surv art cd4 dist'))
    
  }
  if(loc %in% c('CPV', 'LSO', 'MLI', 'TZA', 'UGA')){
    temp <- readRDS(paste0('/share/hiv/data/PJNZ_EPPASM_prepped_subpop/', loc, '.rds'))
    temp.artmxrr <- attr(temp, 'specfp')$artmx_timerr
    attr(dt, 'specfp')$artmx_timerr <- temp.artmxrr
    print(paste0(loc, ' old artmx_timerr subbed in'))
    
  }
  
  if(grepl("IND",loc)){
    if(no_anc){
      attr(dt,"eppd")$ancsitedat <- NULL
    }
    attr(dt, 'specfp')$art_alloc_mxweight <- 0.5
    print(paste0(loc, ' ART alloc mxweight modified'))
    
  }
  
  if(loc %in% "STP"){
    attr(dt, 'eppd')$ancsitedat <- attr(dt, 'eppd')$ancsitedat[attr(dt, 'eppd')$ancsitedat$subpop=="Pop Fem_restante",]
    attr(dt, 'eppd')$ancsitedat = unique(attr(dt, 'eppd')$ancsitedat)
    attr(dt, 'specfp')$art_alloc_mxweight <- 0.5
    print(paste0(loc, ' ART allco mxweight modified, subpop modified'))
  }
  
  if(loc %in% "COM"){
    attr(dt, 'eppd')$ancsitedat <- attr(dt, 'eppd')$ancsitedat[attr(dt, 'eppd')$ancsitedat$subpop=="Female Population",]
    attr(dt, 'eppd')$ancsitedat = unique(attr(dt, 'eppd')$ancsitedat)
    attr(dt, 'specfp')$art_alloc_mxweight <- 0.5
    print(paste0(loc, ' ART allco mxweight modified, subpop modified'))
  }
  
  if(loc %in% "MRT"){
    attr(dt, 'eppd')$ancsitedat <- attr(dt, 'eppd')$ancsitedat[attr(dt, 'eppd')$ancsitedat$subpop=="Pop féminine restante",]
    attr(dt, 'eppd')$ancsitedat = unique(attr(dt, 'eppd')$ancsitedat)
    attr(dt, 'specfp')$art_alloc_mxweight <- 0.5
    print(paste0(loc, ' ART allco mxweight modified, subpop modified'))

  }
  
  if(loc == 'TZA'){
    mod <- data.table(attr(dt, 'eppd')$hhs)
    mod[year == 2017, se := 0.015]
    attr(dt, 'eppd')$hhs <- data.frame(mod)
  }
  
###########################################################################
# Extend inputs to stop.year ----------------------------------------------
###########################################################################
  if(max(attr(dt, 'specfp')$pmtct_dropout$year) < stop.year & ped_toggle){
    add_on.year <- seq(max(attr(dt, 'specfp')$pmtct_dropout$year) + 1 , stop.year)
    add_on.dropouts <- attr(dt, 'specfp')$pmtct_dropout[attr(dt, 'specfp')$pmtct_dropout$year == max(attr(dt, 'specfp')$pmtct_dropout$year), 2:ncol(attr(dt, 'specfp')$pmtct_dropout)]
    attr(dt, 'specfp')$pmtct_dropout <- rbind(attr(dt, 'specfp')$pmtct_dropout, c(year = unlist(add_on.year), add_on.dropouts))
  }
  if(length(unique(attr(dt, 'specfp')$pmtct_dropout$year)) < attr(dt, 'specfp')$SIM_YEARS & ped_toggle){
    missing_years <- setdiff(seq(start.year, stop.year), attr(dt, 'specfp')$pmtct_dropout$year)
    temp.dt <- data.table( attr(dt, 'specfp')$pmtct_dropout)
    extend_back <- temp.dt[year == min(year),]
    list <- list()
    for(years in missing_years){
      x <- data.table(extend_back)
      list[[years - min(missing_years) + 1]] <- x[,year := years]
    }
    extend_back <- do.call(rbind, list)
    extend_back <- data.table(extend_back)
    extend_back[,year := missing_years]
    new <- rbind(attr(dt, 'specfp')$pmtct_dropout, extend_back)
    new <- new[order(year),]
    new <- as.data.frame(new)
    attr(dt, 'specfp')$pmtct_dropout <- new
  }
  
  if(dim(attr(dt, 'specfp')$artmx_timerr)[2] < attr(dt, 'specfp')$SIM_YEARS){
    diff <- dim(attr(dt, 'specfp')$artmx_timerr)[2] - attr(dt, 'specfp')$SIM_YEARS
    while(diff != 0){
      attr(dt, 'specfp')$artmx_timerr <-  abind::abind(attr(dt, 'specfp')$artmx_timerr, attr(dt, 'specfp')$artmx_timerr[,ncol(attr(dt, 'specfp')$artmx_timerr) ])
      diff <- dim(attr(dt, 'specfp')$artmx_timerr)[2] - attr(dt, 'specfp')$SIM_YEARS
      
    }
  }
  if(dim( attr(dt, 'specfp')$art15plus_isperc)[2] < attr(dt, 'specfp')$SIM_YEARS){
    diff <- dim( attr(dt, 'specfp')$art15plus_isperc)[2] - attr(dt, 'specfp')$SIM_YEARS
    while(diff != 0){
      attr(dt, 'specfp')$art15plus_isperc <-  abind::abind( attr(dt, 'specfp')$art15plus_isperc,  attr(dt, 'specfp')$art15plus_isperc[,ncol( attr(dt, 'specfp')$art15plus_isperc)])
      diff <- dim( attr(dt, 'specfp')$art15plus_isperc)[2] - attr(dt, 'specfp')$SIM_YEARS
      
    }
  }
  if(length( attr(dt, 'specfp')$specpop_percelig) < attr(dt, 'specfp')$SIM_YEARS){
    diff <- length( attr(dt, 'specfp')$specpop_percelig)- attr(dt, 'specfp')$SIM_YEARS
    while(diff != 0){
      attr(dt, 'specfp')$specpop_percelig <-  abind::abind(  attr(dt, 'specfp')$specpop_percelig,  (attr(dt, 'specfp')$specpop_percelig)[length( attr(dt, 'specfp')$specpop_percelig)])
      diff <- length( attr(dt, 'specfp')$specpop_percelig) - attr(dt, 'specfp')$SIM_YEARS
      
    }
  }
  if(length( attr(dt, 'specfp')$pw_artelig) < attr(dt, 'specfp')$SIM_YEARS){
    diff <- length( attr(dt, 'specfp')$pw_artelig)- attr(dt, 'specfp')$SIM_YEARS
    while(diff != 0){
      attr(dt, 'specfp')$pw_artelig<-  abind::abind(  attr(dt, 'specfp')$pw_artelig,  (attr(dt, 'specfp')$pw_artelig)[length( attr(dt, 'specfp')$pw_artelig)])
      diff <- length( attr(dt, 'specfp')$pw_artelig) - attr(dt, 'specfp')$SIM_YEARS
      
    }
  }
  if(length( attr(dt, 'specfp')$art_dropout) < attr(dt, 'specfp')$SIM_YEARS){
    diff <- length( attr(dt, 'specfp')$art_dropout)- attr(dt, 'specfp')$SIM_YEARS
    while(diff != 0){
      attr(dt, 'specfp')$art_dropout<-  abind::abind(  attr(dt, 'specfp')$art_dropout,  (attr(dt, 'specfp')$art_dropout)[length( attr(dt, 'specfp')$art_dropout)])
      diff <- length( attr(dt, 'specfp')$art_dropout) - attr(dt, 'specfp')$SIM_YEARS
      
    }
  }
  if(dim( attr(dt, 'specfp')$paedsurv_cd4dist)[3] < attr(dt, 'specfp')$SIM_YEARS){
    diff <- dim( attr(dt, 'specfp')$paedsurv_cd4dist)[3] - attr(dt, 'specfp')$SIM_YEARS
    while(diff != 0){
      attr(dt, 'specfp')$paedsurv_cd4dist <-  abind::abind( attr(dt, 'specfp')$paedsurv_cd4dist,  attr(dt, 'specfp')$paedsurv_cd4dist[,,dim( attr(dt, 'specfp')$paedsurv_cd4dist)[3]])
      diff <- dim( attr(dt, 'specfp')$paedsurv_cd4dist)[3] - attr(dt, 'specfp')$SIM_YEARS
      
    }
  }
  if(dim( attr(dt, 'specfp')$incrr_age)[3] < attr(dt, 'specfp')$SIM_YEARS){
    diff <- dim( attr(dt, 'specfp')$incrr_age)[3] - attr(dt, 'specfp')$SIM_YEARS
    while(diff != 0){
      attr(dt, 'specfp')$incrr_age <-  abind::abind( attr(dt, 'specfp')$incrr_age,  attr(dt, 'specfp')$incrr_age[,,dim( attr(dt, 'specfp')$incrr_age)[3]])
      diff <- dim( attr(dt, 'specfp')$incrr_age)[3] - attr(dt, 'specfp')$SIM_YEARS
      
    }
  }
  if(dim( attr(dt, 'specfp')$paedsurv_artcd4dist)[4] < attr(dt, 'specfp')$SIM_YEARS){
    diff <- dim( attr(dt, 'specfp')$paedsurv_artcd4dist)[4] - attr(dt, 'specfp')$SIM_YEARS
    while(diff != 0){
      attr(dt, 'specfp')$paedsurv_artcd4dist <-  abind::abind( attr(dt, 'specfp')$paedsurv_artcd4dist,  attr(dt, 'specfp')$paedsurv_artcd4dist[,,,dim( attr(dt, 'specfp')$paedsurv_artcd4dist)[4]])
      diff <- dim( attr(dt, 'specfp')$paedsurv_artcd4dist)[4] - attr(dt, 'specfp')$SIM_YEARS
      
    }
  }
  
  if(dim( attr(dt, 'specfp')$art15plus_num)[2] < attr(dt, 'specfp')$SIM_YEARS){
    diff <- dim( attr(dt, 'specfp')$art15plus_num)[2] - attr(dt, 'specfp')$SIM_YEARS
    while(diff != 0){
      attr(dt, 'specfp')$art15plus_num <-  abind::abind( attr(dt, 'specfp')$art15plus_num,  attr(dt, 'specfp')$art15plus_num[,ncol( attr(dt, 'specfp')$art15plus_num) - 1])
      diff <- dim( attr(dt, 'specfp')$art15plus_num)[2] - attr(dt, 'specfp')$SIM_YEARS
      
    }
  }
  if(length( attr(dt, 'specfp')$median_cd4init) < attr(dt, 'specfp')$SIM_YEARS){
    diff <- length( attr(dt, 'specfp')$median_cd4init)- attr(dt, 'specfp')$SIM_YEARS
    while(diff != 0){
      attr(dt, 'specfp')$median_cd4init<-  abind::abind(  attr(dt, 'specfp')$median_cd4init,  (attr(dt, 'specfp')$median_cd4init)[length( attr(dt, 'specfp')$median_cd4init)])
      diff <- length( attr(dt, 'specfp')$median_cd4init) - attr(dt, 'specfp')$SIM_YEARS
      
    }
  }
  if(length( attr(dt, 'specfp')$med_cd4init_input) < attr(dt, 'specfp')$SIM_YEARS){
    diff <- length( attr(dt, 'specfp')$med_cd4init_input)- attr(dt, 'specfp')$SIM_YEARS
    while(diff != 0){
      attr(dt, 'specfp')$med_cd4init_input<-  abind::abind(  attr(dt, 'specfp')$med_cd4init_input,  (attr(dt, 'specfp')$med_cd4init_input)[length( attr(dt, 'specfp')$med_cd4init_input)])
      diff <- length( attr(dt, 'specfp')$med_cd4init_input) - attr(dt, 'specfp')$SIM_YEARS
      
    }
  }
  if(length( attr(dt, 'specfp')$med_cd4init_cat) < attr(dt, 'specfp')$SIM_YEARS){
    diff <- length( attr(dt, 'specfp')$med_cd4init_cat)- attr(dt, 'specfp')$SIM_YEARS
    while(diff != 0){
      attr(dt, 'specfp')$med_cd4init_cat<-  abind::abind(  attr(dt, 'specfp')$med_cd4init_cat,  (attr(dt, 'specfp')$med_cd4init_cat)[length( attr(dt, 'specfp')$med_cd4init_cat)])
      diff <- length( attr(dt, 'specfp')$med_cd4init_cat) - attr(dt, 'specfp')$SIM_YEARS
      
    }
  }
  if(length( attr(dt, 'specfp')$verttrans_lag) < attr(dt, 'specfp')$SIM_YEARS){
    diff <- length( attr(dt, 'specfp')$verttrans_lag)- attr(dt, 'specfp')$SIM_YEARS
    while(diff != 0){
      attr(dt, 'specfp')$verttrans_lag<-  abind::abind(  attr(dt, 'specfp')$verttrans_lag,  (attr(dt, 'specfp')$verttrans_lag)[length( attr(dt, 'specfp')$verttrans_lag)])
      diff <- length( attr(dt, 'specfp')$verttrans_lag) - attr(dt, 'specfp')$SIM_YEARS
      
    }
  }
  
  if(length( attr(dt, 'specfp')$paedsurv_lag) < attr(dt, 'specfp')$SIM_YEARS){
    diff <- length( attr(dt, 'specfp')$paedsurv_lag)- attr(dt, 'specfp')$SIM_YEARS
    while(diff != 0){
      attr(dt, 'specfp')$paedsurv_lag<-  abind::abind(  attr(dt, 'specfp')$paedsurv_lag,  (attr(dt, 'specfp')$paedsurv_lag)[length( attr(dt, 'specfp')$paedsurv_lag)])
      diff <- length( attr(dt, 'specfp')$paedsurv_lag) - attr(dt, 'specfp')$SIM_YEARS
      
    }
  }
  
  if(length( attr(dt, 'specfp')$artcd4elig_idx) < attr(dt, 'specfp')$SIM_YEARS){
    diff <- length( attr(dt, 'specfp')$artcd4elig_idx)- attr(dt, 'specfp')$SIM_YEARS
    while(diff != 0){
      attr(dt, 'specfp')$artcd4elig_idx<-  abind::abind(  attr(dt, 'specfp')$artcd4elig_idx,  (attr(dt, 'specfp')$artcd4elig_idx)[length( attr(dt, 'specfp')$artcd4elig_idx)])
      diff <- length( attr(dt, 'specfp')$artcd4elig_idx) - attr(dt, 'specfp')$SIM_YEARS
      
    }
  }
  
  if(dim( attr(dt, 'specfp')$entrantprev)[2] < attr(dt, 'specfp')$SIM_YEARS){
    diff <- dim( attr(dt, 'specfp')$entrantprev)[2] - attr(dt, 'specfp')$SIM_YEARS
    while(diff != 0){
      attr(dt, 'specfp')$entrantprev <-  abind::abind( attr(dt, 'specfp')$entrantprev,  attr(dt, 'specfp')$entrantprev[,ncol( attr(dt, 'specfp')$entrantprev) - 1])
      diff <- dim( attr(dt, 'specfp')$entrantprev)[2] - attr(dt, 'specfp')$SIM_YEARS
      
    }
  }
  
  if(dim( attr(dt, 'specfp')$entrantartcov)[2] < attr(dt, 'specfp')$SIM_YEARS){
    diff <- dim( attr(dt, 'specfp')$entrantartcov)[2] - attr(dt, 'specfp')$SIM_YEARS
    while(diff != 0){
      attr(dt, 'specfp')$entrantartcov <-  abind::abind( attr(dt, 'specfp')$entrantartcov,  attr(dt, 'specfp')$entrantartcov[,ncol( attr(dt, 'specfp')$entrantartcov) - 1])
      diff <- dim( attr(dt, 'specfp')$entrantartcov)[2] - attr(dt, 'specfp')$SIM_YEARS
      
    }
  }
  
  if(length(attr(dt, 'specfp')$artpaed_isperc) < attr(dt, 'specfp')$SIM_YEARS){
    diff <- attr(dt, 'specfp')$SIM_YEARS - length(attr(dt, 'specfp')$artpaed_isperc)
    add_names <- setdiff(seq(start.year, stop.year), as.numeric(names(attr(dt, 'specfp')$artpaed_isperc)))
    add <- rep(FALSE, length(add_names))
    names(add) <- add_names
    new <- c(attr(dt, 'specfp')$artpaed_isperc, add)
    new <- new[order(names(new))]
    attr(dt, 'specfp')$artpaed_isperc <-  new
  }
  
  if(length(attr(dt, 'specfp')$artpaed_num) < attr(dt, 'specfp')$SIM_YEARS){
    diff <- attr(dt, 'specfp')$SIM_YEARS - length(attr(dt, 'specfp')$artpaed_num)
    add_names <- setdiff(seq(start.year, stop.year), as.numeric(names(attr(dt, 'specfp')$artpaed_num)))
    add <- rep(0, length(add_names))
    names(add) <- add_names
    new <- c(attr(dt, 'specfp')$artpaed_num, add)
    new <- new[order(names(new))]
    attr(dt, 'specfp')$artpaed_num <-  new
    
    
  }
  
  if(length(attr(dt, 'specfp')$cotrim_isperc) < attr(dt, 'specfp')$SIM_YEARS){
    diff <- attr(dt, 'specfp')$SIM_YEARS - length(attr(dt, 'specfp')$cotrim_isperc)
    add_names <- setdiff(seq(start.year, stop.year), as.numeric(names(attr(dt, 'specfp')$cotrim_isperc)))
    add <- rep(FALSE, length(add_names))
    names(add) <- add_names
    new <- c(attr(dt, 'specfp')$cotrim_isperc, add)
    new <- new[order(names(new))]
    attr(dt, 'specfp')$cotrim_isperc <-  new
    
    
  }
  
  if(length(attr(dt, 'specfp')$cotrim_num) < attr(dt, 'specfp')$SIM_YEARS){
    diff <- attr(dt, 'specfp')$SIM_YEARS - length(attr(dt, 'specfp')$cotrim_num)
    add_names <- setdiff(seq(start.year, stop.year), as.numeric(names(attr(dt, 'specfp')$cotrim_num)))
    add <- rep(0, length(add_names))
    names(add) <- add_names
    new <- c(attr(dt, 'specfp')$cotrim_num, add)
    new <- new[order(names(new))]
    attr(dt, 'specfp')$cotrim_num <-  new

  }
  
###########################################################################
# Other Misc. Changes -----------------------------------------------------
###########################################################################
  if(epp.mod == 'rspline'){attr(dt, 'specfp')$equil.rprior <- TRUE}
  if(run.name %in% c("190630_fixonARTIND","190630_fixonARTIND_tightprior")){
    temp <- readRDS(paste0('/share/hiv/data/PJNZ_EPPASM_prepped_subpop/MWI.rds'))
    temp.artmxrr <- attr(temp, 'specfp')$artmx_timerr
    attr(dt, 'specfp')$artmx_timerr <- temp.artmxrr
    print('temp.artmxrr replaced')
  }
  
  attr(dt, 'eppd')$ancsitedat = unique(attr(dt, 'eppd')$ancsitedat)
  ## TODO - fix se = 0 data points in ZAF
  # attr(dt, 'eppd')$hhs <- attr(dt, 'eppd')$hhs[!attr(dt, 'eppd')$hhs$se == 0,]

  
  attr(dt, 'specfp')$relinfectART <- 0.3
  print('relinfectART changed')
  if(any(colnames(attr(dt, 'eppd')) == 'year_id')){
    x <- as.data.table(attr(dt, 'eppd')$ancsitedat)
    x <- setnames(x, 'year_id', 'year')
    attr(dt, 'eppd')$ancsitedat <-  as.data.frame(x)
    print('colname changed from year_id to year')
  }
  if(any(attr(dt, 'eppd')$ancsitedat$prev > 1)){
    print('A prevalence above 1 was removed')
    attr(dt, 'eppd')$ancsitedat <- as.data.frame(as.data.table(attr(dt, 'eppd')$ancsitedat)[prev < 1,])
  }
  if(geoadjust){
    attr(dt, 'eppd')$ancsitedat$offset <- attr(dt, 'eppd')$ancsitedat$offset %>% as.numeric()
    print('offsets changed to numeric')
    
  }
  if(!geoadjust & any(colnames(data.table(attr(dt, 'eppd')$ancsitedat)) == 'year_id')){
    temp <- data.table(attr(dt, 'eppd')$ancsitedat)
    setnames(temp, 'year_id', 'year')
    temp <- temp[,ihme_loc_id := NULL]
    temp <- temp[,high_risk := NULL]
    attr(dt, 'eppd')$ancsitedat <- data.frame(temp)
    print('high risk pops removed and year_id col changed to year')
  }
  
  dir.create(paste0('/ihme/hiv/epp_output/', gbdyear, "/", run.name, '/dt_objects/'), recursive = T)
  saveRDS(dt, file = paste0('/ihme/hiv/epp_output/', gbdyear, "/", run.name, '/dt_objects/', loc, '_dt.RDS' ))
  
  return(dt)
}
