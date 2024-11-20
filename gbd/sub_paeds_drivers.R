sub_paeds_drivers_fit <- function(fit, art_path, pmtct_path){
  # ART + Cotrim
  art <- fread(art_path)
  if('ART_Cov_pct' %in% colnames(art)){
    setnames(art, old = 'ART_Cov_pct', new = 'ART_cov_pct')
    
  }
  if(min(art$year) > start.year){
    backfill <- data.table(year = start.year:(min(art$year) - 1))
    backfill <- backfill[, names(art)[!names(art) == 'year'] := 0]
    art <- rbind(art, backfill, use.names = T)
  }
  art <- art[order(year)]
  art[is.na(art)] <- 0
  art[,art_isperc := ifelse(ART_cov_pct > 0, TRUE, FALSE)]
  art[,cotrim_isperc := ifelse(Cotrim_cov_pct > 0, TRUE, FALSE)]
  artpaed <- ifelse(art$art_isperc, art[,ART_cov_pct], art[,ART_cov_num])
  names(artpaed) <- art$year
  fit$fp$artpaed_num <- artpaed
  art_isperc <- art[,art_isperc]
  names(art_isperc) <- art$year
  fit$fp$artpaed_isperc <- art_isperc
  cotrim <-  ifelse(art$cotrim_isperc, art[,Cotrim_cov_pct], art[,Cotrim_cov_num])
  names(cotrim) <- art$year
  fit$fp$cotrim_num <- cotrim
  cotrim_isperc <- art[,cotrim_isperc]
  names(cotrim_isperc) <- art$year
  fit$fp$cotrim_isperc <- cotrim_isperc

  # PMTCT
  pmtct <- fread(pmtct_path)
  if(min(pmtct$year) > start.year){
    backfill <- data.table(year = start.year:(min(pmtct$year) - 1))
    backfill <- backfill[, names(pmtct)[!names(pmtct) == 'year'] := 0]
    pmtct <- rbind(pmtct, backfill, use.names = T)
  }
  pmtct <- pmtct[order(year)]
  years <- unique(pmtct$year)
  pmtct_num <- data.table(year = years)
  pmtct_isperc <- data.table(year = years)
  for(var in c('tripleARTdurPreg', 'tripleARTbefPreg', 'singleDoseNevir', 'prenat_optionB', 'prenat_optionA', 'postnat_optionB', 'postnat_optionA', 'dualARV')){
    pmtct.var <- pmtct[,c('year', paste0(var, '_num'), paste0(var, '_pct')), with = F]
    vector <- ifelse(pmtct.var[,get(paste0(var, '_pct'))] > 0, pmtct.var[,get(paste0(var, '_pct'))], pmtct.var[,get(paste0(var, '_num'))])
    pmtct_num[,paste0(var) := vector]
    vector <- ifelse(pmtct.var[,get(paste0(var, '_pct'))] > 0, TRUE, FALSE)
    pmtct_isperc[,paste0(var) := vector]
  }
  fit$fp$pmtct_num <- data.frame(pmtct_num)
  fit$fp$pmtct_isperc <- data.frame(pmtct_isperc)

  
  return(fit)
}

