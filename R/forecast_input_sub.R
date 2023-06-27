forecast.sub <- function(loc, start.year, stop.year, j, dt, run.name, c.scenario, gbd.run.name, sub.art.forecast = T, trans.rate.pred = F){
  years = start.year:stop.year
  trans.year.idx = transition.year - 1970
  
  ## Incidence
  ## Transmission rate projection
  if(trans.rate.pred){
    pre.fit <- readRDS(paste0('/ihme/hiv/epp_output/gbd20/',gbd.run.name, '/fitmod/', loc, '_', j, '.RDS'))
    fit <- readRDS(paste0('/ihme/hiv/epp_output/gbd20/', gbd.run.name, '/fit/', loc, '/', j, '.RDS'))
    param = fnCreateParam(attr(fit, 'theta'), pre.fit$fp)
    attr(dt, 'specfp')[names(param)[!grepl('frr', names(param)) & !grepl('incrr', names(param))]] <- param[!grepl('frr', names(param)) & !grepl('incrr', names(param))]
    # holding rvec constant into the future
    len.rvec = length(attr(dt, 'specfp')$rvec)
    len.ext.rvec = ((stop.year - 1970) * 10) - len.rvec + 2
    attr(dt, 'specfp')$rvec = c(attr(dt, 'specfp')$rvec, rep(attr(dt, 'specfp')$rvec[len.rvec], len.ext.rvec))
    inc.age = attr(dt, 'specfp')$incrr_age
    inc.age.last = attr(dt, 'specfp')$incrr_age[,,dim(inc.age)[3]]
    inc.age.ext = array(0, c(66, 2, length(start.year:stop.year)))
    for(i in 1:length(start.year:stop.year)){
      if(i <= length(dim(inc.age)[3])){
        inc.age.ext[,,i] = inc.age[,,i]
      }else{
        inc.age.ext[,,i] = inc.age.last
      }
    }
    attr(dt, 'specfp')$incrr_age = inc.age.ext
    attr(dt, 'specfp')$eppmod = 'rhybrid'
    attr(dt, 'specfp')$tsEpidemicStart = pre.fit$fp$tsEpidemicStart
  }else{
    ## Direct incidence input
    attr(dt, 'specfp')$eppmod = "directincid"
    inc.input = fread(paste0('/mnt/share/hiv/spectrum_input/', run.name, '_', c.scenario, '/incidence/', loc, '.csv'))
    inc.input = melt(inc.input, id.vars = 'year')
    if(paste0('draw', j) %in% unique(inc.input$variable)){
      inc.input = inc.input[variable == paste0('draw', j)]
    }else{
      draws.exist = unique(as.character(inc.input$variable))
      draws.exist = as.numeric(gsub('draw', '', draws.exist))
      rand.draw = sample(draws.exist, 1)
      inc.input = inc.input[variable == paste0('draw', rand.draw)]
    }
    inc.input = inc.input[year %in% 1970:stop.year]
    if(min(inc.input$year) >1970){
      copy.dt = inc.input[year == min(inc.input$year)]
      copy.dt[,value := 0]
      append.dt = data.table()
      for(y in 1970:(min(inc.input$year) - 1)){
        y.dt = copy.dt[,year := y]
        append.dt = rbind(append.dt, y.dt)
      }
      inc.input = rbind(append.dt, inc.input)
    }
    attr(dt, 'specfp')$incidinput = (inc.input$value)/100
    names(attr(dt, 'specfp')$incidinput) = inc.input$year
    attr(dt, 'specfp')$incidpopage = 0L
  }
  
  ## temp - it seems some locations had child art = 0 after year 2020
  ## for now just holding these locations constant - needs to be updated
  if(attr(dt, 'specfp')$artpaed_num[trans.year.idx + 1] == 0){
    paed.art = attr(dt, 'specfp')$artpaed_num
    paed.art = paed.art[1:(trans.year.idx)]
    paed.art = c(paed.art, rep(paed.art[trans.year.idx], (stop.year - transition.year) + 1))
    names(paed.art) = years
    attr(dt, 'specfp')$artpaed_num = paed.art
    attr(dt, 'specfp')$artpaed_isperc[trans.year.idx:length(years)] = attr(dt, 'specfp')$artpaed_isperc[trans.year.idx]
    attr(dt, 'specfp')$artpaed_isperc = attr(dt, 'specfp')$artpaed_isperc[1:length(years)]
    
    paed.cot = attr(dt, 'specfp')$cotrim_num
    paed.cot = paed.cot[1:(trans.year.idx)]
    paed.cot = c(paed.cot, rep(paed.cot[trans.year.idx], (stop.year - transition.year) + 1))
    names(paed.cot) = years
    attr(dt, 'specfp')$cotrim_num = paed.cot
    attr(dt, 'specfp')$cotrim_isperc[trans.year.idx:length(years)] = attr(dt, 'specfp')$cotrim_isperc[trans.year.idx]
    attr(dt, 'specfp')$cotrim_isperc = attr(dt, 'specfp')$cotrim_isperc[1:length(years)]
    
    temp.pmtct = attr(dt, 'specfp')$pmtct_dropout
    temp.pmtct = extend.years(temp.pmtct, years)
    attr(dt, 'specfp')$pmtct_dropout = temp.pmtct
    
    pmtct = fread(paste0('/mnt/share/hiv/spectrum_input/', run.name, '_', c.scenario, '/PMTCT/', loc, '.csv'))
    pmtct <- pmtct[year %in% years]
    #pmtct <- extend.years(pmtct, years)
    if(min(pmtct$year) > start.year){
      backfill <- data.table(year = start.year:(min(pmtct$year) - 1))
      backfill <- backfill[, names(pmtct)[!names(pmtct) == 'year'] := 0]
      pmtct <- rbind(pmtct, backfill, use.names = T)
    }
    pmtct <- pmtct[order(year)]
    pmtct_num <- data.table(year = years)
    pmtct_isperc <- data.table(year = years)
    for(var in c('tripleARTdurPreg', 'tripleARTbefPreg', 'singleDoseNevir', 'prenat_optionB', 'prenat_optionA', 'postnat_optionB', 'postnat_optionA', 'dualARV')){
      pmtct.var <- pmtct[,c('year', paste0(var, '_num'), paste0(var, '_pct')), with = F]
      vector <- ifelse(pmtct.var[,get(paste0(var, '_pct'))] > 0, pmtct.var[,get(paste0(var, '_pct'))], pmtct.var[,get(paste0(var, '_num'))])
      pmtct_num[,paste0(var) := vector]
      vector <- ifelse(pmtct.var[,get(paste0(var, '_pct'))] > 0, TRUE, FALSE)
      pmtct_isperc[,paste0(var) := vector]
    }
    attr(dt, 'specfp')$pmtct_num <- data.frame(pmtct_num)
    attr(dt, 'specfp')$pmtct_isperc <- data.frame(pmtct_isperc)
  }
  if(sub.art.forecast){
    if(file.exists(paste0('/share/hiv/spectrum_draws/20220621_', c.scenario, '/forecast_art_coverage_draws/', loc, '.csv'))){
      art.forecast = fread(paste0('/share/hiv/spectrum_draws/20220621_', c.scenario, '/forecast_art_coverage_draws/', loc, '.csv'))
    }else{
      parent.id = loc.table[ihme_loc_id == loc, parent_id]
      parent.loc = loc.table[location_id == parent.id, ihme_loc_id]
      art.forecast = fread(paste0('/share/hiv/spectrum_draws/20220621_', c.scenario, '/forecast_art_coverage_draws/', parent.loc, '.csv'))
    }
  art.for = art.forecast[,c('age', 'sex', 'CD4', 'year', paste0('art_coverage_', j-1)), with = F]
  setnames(art.for,paste0('art_coverage_', j-1), 'art_coverage' )
  art.for <- art.for[CD4 == 500, cat := 1]
  art.for <- art.for[CD4 == 350, cat := 2]
  art.for <- art.for[CD4 == 250, cat := 3]
  art.for <- art.for[CD4 == 200, cat := 4]
  art.for <- art.for[CD4 == 100, cat := 5]
  art.for <- art.for[CD4 == 50, cat := 6]
  art.for<- art.for[CD4 == 0, cat := 7]
  art.for[sex == 'male', sex := 'Male']
  art.for[sex == 'female', sex := 'Female']
  art.for = art.for[year >= transition.year - 1]
  ## Dimensions = CD4, age, sex, years
  art.arr = array(0, c(7, 66, 2, length((transition.year - 1):(stop.year + 1))))
  dimnames(art.arr) <- list(cd4stage = paste0(1:7), age = paste0(15:80), sex = c('Male', 'Female'), year = paste0((transition.year - 1):(stop.year + 1)))
  for(c.cd4 in paste0(1:7)){
    for(c.sex in c('Male', 'Female')){
      for(c.age in paste0(15:80)){
        for(c.year in paste0((transition.year - 1):(stop.year + 1))){
          art.replace = art.for[cat == as.numeric(c.cd4) & age == as.numeric(c.age) & sex == c.sex & year == as.numeric(c.year)]
          art.arr[c.cd4, c.age, c.sex, c.year] = as.numeric(art.replace[, art_coverage])
        }
      }
    }
  }
  attr(dt, 'specfp')$art_pred = art.arr
  }else{
      # ART initiation prediction
      pred.init = fread(paste0('/ihme/hiv/epp_output/', gbdyear, '/221223_bittern/artinit/pred_init_tenth_year.csv'))
      if(loc %in% pred.init$ihme_loc_id){
        pred.init = pred.init[ihme_loc_id == loc]
      } else{
        # placeholder 0.05 for new locations for dove run
        pred.init = pred.init[ihme_loc_id == 'AGO']
      }
      init.mat = matrix(data = c(pred.init[sex == 'male', art_init], pred.init[sex == 'female', art_init]))
      attr(dt, 'specfp')$pred_art_init = init.mat
      
  }
  ## UNAIDS adult ART input should cut off at transition year
  attr(dt, 'specfp')$art15plus_num <- attr(dt, 'specfp')$art15plus_num[,1:(transition.year - 1970)]
  attr(dt, 'specfp')$art15plus_isperc <- attr(dt, 'specfp')$art15plus_isperc[,1:(transition.year - 1970)]
  
  # Extend sex incidence rate ratio
  inc.sex = attr(dt, 'specfp')$incrr_sex
  inc.sex.ext = c(inc.sex, rep(inc.sex[length(inc.sex)], ((stop.year - 1970) - length(inc.sex) + 1)))
  attr(dt, 'specfp')$incrr_sex <- inc.sex.ext
  
  return(dt)
}
