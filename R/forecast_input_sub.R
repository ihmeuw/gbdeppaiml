forecast.sub <- function(loc, start.year, stop.year, j, dt, sub.art.forecast = T){
  years = start.year:stop.year
  
  ## incidence
  attr(dt, 'specfp')$eppmod = "directincid"
  inc.input = fread(paste0('/mnt/share/hiv/spectrum_input/20220621_reference/incidence/', loc, '.csv'))
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
  
  ## temp - it seems most locations had child art = 0 after year 2020
  ## for now just holding constant - needs to be updated
  if(attr(dt, 'specfp')$artpaed_num[52] == 0){
    paed.art = attr(dt, 'specfp')$artpaed_num
    paed.art = paed.art[1:51]
    paed.art = c(paed.art, rep(paed.art[51], 30))
    names(paed.art) = years
    attr(dt, 'specfp')$artpaed_num = paed.art
    attr(dt, 'specfp')$artpaed_isperc[51:81] = TRUE
    attr(dt, 'specfp')$artpaed_isperc = attr(dt, 'specfp')$artpaed_isperc[1:length(years)]
    
    paed.cot = attr(dt, 'specfp')$cotrim_num
    paed.cot = paed.cot[1:51]
    paed.cot = c(paed.cot, rep(paed.cot[51], 30))
    names(paed.cot) = years
    attr(dt, 'specfp')$cotrim_num = paed.cot
    attr(dt, 'specfp')$cotrim_isperc[51:81] = TRUE
    attr(dt, 'specfp')$cotrim_isperc = attr(dt, 'specfp')$cotrim_isperc[1:length(years)]
    
    temp.pmtct = attr(dt, 'specfp')$pmtct_dropout
    temp.pmtct = extend.years(temp.pmtct, years)
    attr(dt, 'specfp')$pmtct_dropout = temp.pmtct
    
    pmtct = fread(paste0('/mnt/share/hiv/spectrum_input/20220418_reference/PMTCT/', loc, '.csv'))
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
  art.forecast = fread(paste0('/share/hiv/spectrum_draws/20220621_reference/forecast_art_coverage_draws/', loc, '.csv'))
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
  art.for = art.for[year >= 2018]
  ## Dimensions = CD4, age, sex, years
  art.arr = array(0, c(7, 66, 2, length(2018:stop.year)))
  dimnames(art.arr) <- list(cd4stage = paste0(1:7), age = paste0(15:80), sex = c('Male', 'Female'), year = paste0(2018:stop.year))
  for(c.cd4 in paste0(1:7)){
    for(c.sex in c('Male', 'Female')){
      for(c.age in paste0(15:80)){
        for(c.year in paste0(2018:stop.year)){
          art.replace = art.for[cat == as.numeric(c.cd4) & age == as.numeric(c.age) & sex == c.sex & year == as.numeric(c.year)]
          art.arr[c.cd4, c.age, c.sex, c.year] = as.numeric(art.replace[, art_coverage])
        }
      }
    }
  }
  attr(dt, 'specfp')$art_pred = art.arr
}
  return(dt)
}
