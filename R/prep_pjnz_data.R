## Collapses PJNZ files to create eppd (collapse_epp())
## Reads in demp and projp to create specfp
## Saves object as .rds file (in order to avoid reading and collapsing PJNZ files every run)

prepare_spec_object <- function(loc, popadjust = TRUE, popupdate=TRUE, use_ep5=FALSE){
  
  pjnz <- find_pjnz(loc)[[1]]
  ##TODO: Make this work for ZAF
  if(grepl ('ZAF', loc)){
    eppd <- epp::read_epp_data(pjnz)
    zaf.dict <- list("MP" = "ZAF_487", "GP" = "ZAF_484", "KZN" = "ZAF_485", 
                     "WC" = "ZAF_490", "EC" = "ZAF_482", "LP" = "ZAF_486", 
                     "FS" = "ZAF_483", "NW" = "ZAF_488", "NC" = "ZAF_489")
    eppd.new <- eppd[[names(which(zaf.dict == loc))]]  
    attr(eppd.new, 'country') <- attr(eppd, 'country')
    attr(eppd.new, 'country_code') <- attr(eppd, 'country_code')
    attr(eppd.new, 'class') <- attr(eppd, 'class')
    eppd.new$ancrtcens <- data.frame(year=integer(), prev=integer(), n=integer())
    eppd <- list()
    eppd[[loc]] <- eppd.new
    ## Just need this for epidemic start
    epp.totals <- list()
    epp.totals$epp.input.tot <- epp::read_epp_input(pjnz)
  } else{
    epp.totals <- collapse_epp(loc)
    eppd <- epp.totals$eppd
  }
  
  country <- attr(eppd, "country")
  cc <- attr(eppd, "country_code")
  
  ## melt site-level data - move to within collapse_epp
  # eppd <- Map("[[<-", eppd, "ancsitedat", lapply(eppd, melt_ancsite_data))
  # ## tidy HHS data
  # eppd <- Map("[[<-", eppd, "hhs", lapply(eppd, tidy_hhs_data))
  
  attr(eppd, "country") <- country
  attr(eppd, "country_code") <- cc
  eppd <- eppd[[loc]]
  
  ## spectrum
  ## TODO: Do we want to implement a collapse function for demog param? (probably not, but it's worth noting
  ## that for locations we collapse (like Benin, Cote d'Ivoire, etc), the demp object will only hold the population for 1 subnational)
  
  if(loc %in% "STP"){ ##Can generalize to any 2015 PJNZ file
    demp <- create_spectrum_demog_param(loc, start.year, stop.year)
    projp <- create_hivproj_param(loc, start.year, stop.year)
  } else {
    demp <- read_specdp_demog_param(pjnz, use_ep5=use_ep5)
    projp <- read_hivproj_param(pjnz, use_ep5=use_ep5)
  }

  epp_t0 <- epp.totals$epp.input.tot$epidemic.start

  ## If popadjust = NULL, look for subp if more than 1 EPP region
  if(is.null(popadjust)){
    popadjust <- length(eppd) > 1
  }


  specfp <- create_spectrum_fixpar(projp, demp, popadjust=popadjust, time_epi_start=epp_t0)
  
  specfp$ss$time_epi_start <- epp_t0
  
  temp.anc <- data.table(eppd$ancsitedat)
  temp.anc <- temp.anc[!grepl('Pseudo', site)]
  eppd$ancsitedat <- data.frame(temp.anc)
  temp.anc <- eppd$anc.prev
  temp.anc <- temp.anc[!grepl('Pseudo', rownames(temp.anc)),]
  eppd$anc.prev <- temp.anc
  temp.anc <- eppd$anc.n
  temp.anc <- temp.anc[!grepl('Pseudo', rownames(temp.anc)),]
  eppd$anc.n <- temp.anc
  eppd$anc.used <- eppd$anc.used[!grepl('Pseudo', names(eppd$anc.used))]
  
  ## output
  val <- list()
  attr(val, 'eppd') <- eppd
  attr(val, 'specfp') <- specfp
  attr(val, 'country') <- read_country(pjnz)
  attr(val, 'region') <- loc
  return(val)
}

## For group 2 countries that have a PJNZ file
prepare_spec_object_group2 <- function(loc){
  pjnz <- find_pjnz(loc)[[1]]
  fp <- prepare_directincid(pjnz)
  
  dt <- list()
  attr(dt, 'specfp') <- fp
  ## create eppd object in append.deaths() function later
  return(dt)
}

prepare_spec_object_ind <- function(loc, start.year = 1970, stop.year = 2019, popadjust = TRUE){
  eppd <- prepare_eppd_ind(loc)
  demp <- create_spectrum_demog_param(loc, start.year, stop.year)
  projp <- create_hivproj_param(loc, start.year, stop.year)
  ## Hard coded based on previous India data prep code
  epp_t0 <- 1985
  
  specfp <- create_spectrum_fixpar(projp, demp, popadjust=popadjust, time_epi_start=epp_t0)
  
  specfp$ss$time_epi_start <- epp_t0
  val <- list()
  attr(val, 'eppd') <- eppd
  attr(val, 'specfp') <- specfp
  attr(val, 'country') <- eppd$country
  attr(val, 'region') <- loc
  saveRDS(val, paste0('/share/hiv/data/PJNZ_EPPASM_prepped_subpop/', loc, '.rds'))
  
  return(val)
  
}

## Generates a demp object
## NOTE that .DP files *do* exist for India states, but they are formatted differently from more recent (Spectrum 2016 onward) files
## If we are interested in running India states using UNAIDS parameters, we could write the code to extract these.
## Given that these files are super out of date, we aren't going to extract them for now.
create_spectrum_demog_param <- function(loc, start.year = 1970, stop.year = 2019){

  proj.years <- start.year:stop.year
  version <- "Spectrum 5.63"
  
  ## Create all structures in demp object
  ## Fill with 0s, with the exception of srb
  ## All variables will be subbed using sub.pop.params.demp()
  basepop <- array(0, c(81, 2, length(proj.years)))
  dimnames(basepop) <- list(paste0(0:80), c('Male', 'Female'), paste0(proj.years))
  Sx <- array(0, c(81, 2, length(proj.years)))
  dimnames(Sx) <- list(age=0:80, sex=c("Male", "Female"), year=proj.years)  
  mx <- -log(Sx)
  tfr <- rep(0, length(proj.years))
  names(tfr) <- proj.years
  asfr <- array(0, c(35, length(proj.years)))
  dimnames(asfr) <- list(age = paste0(15:49), year = paste0(start.year:stop.year))
  births <- rep(0, length(proj.years))
  names(births) <- proj.years
  srb <- rep(102, length(proj.years))
  names(srb) <- proj.years
  netmigr <- array(0, c(81, 2, length(proj.years)))
  dimnames(netmigr) <- list(age=0:80, sex=c("Male", "Female"), year=proj.years)  
  
  demp <- list("basepop"=basepop, "mx"=mx, "Sx"=Sx, "asfr"=asfr, "tfr"=tfr, "srb"=srb, "netmigr"=netmigr,
               "births"=births)
  class(demp) <- "demp"
  attr(demp, "version") <- version
  
  return(demp)
}

## Creates projp object
create_spectrum_demog_param <- function(loc, start.year = 1970, stop.year = 2019){
  
  proj.years <- start.year:stop.year
  version <- "Spectrum 5.63"
  
  ## Create all structures in demp object
  ## Fill with 0s, with the exception of srb
  ## All variables will be subbed using sub.pop.params.demp()
  basepop <- array(0, c(81, 2, length(proj.years)))
  dimnames(basepop) <- list(paste0(0:80), c('Male', 'Female'), paste0(proj.years))
  Sx <- array(0, c(81, 2, length(proj.years)))
  dimnames(Sx) <- list(age=0:80, sex=c("Male", "Female"), year=proj.years)  
  mx <- -log(Sx)
  tfr <- rep(0, length(proj.years))
  names(tfr) <- proj.years
  asfr <- array(0, c(35, length(proj.years)))
  dimnames(asfr) <- list(age = paste0(15:49), year = paste0(start.year:stop.year))
  births <- rep(0, length(proj.years))
  names(births) <- proj.years
  srb <- rep(102, length(proj.years))
  names(srb) <- proj.years
  netmigr <- array(0, c(81, 2, length(proj.years)))
  dimnames(netmigr) <- list(age=0:80, sex=c("Male", "Female"), year=proj.years)  
  
  demp <- list("basepop"=basepop, "mx"=mx, "Sx"=Sx, "asfr"=asfr, "tfr"=tfr, "srb"=srb, "netmigr"=netmigr,
               "births"=births)
  class(demp) <- "demp"
  attr(demp, "version") <- version
  
  return(demp)
}

## Creates projp object
create_hivproj_param <- function(loc, start.year = 1970, stop.year = 2019){
  yr_start <- start.year
  yr_end <- stop.year
  proj.years = yr_start:yr_end
  
  ## TODO: Get these fertility and incrr adjustments by country from Jeff
  print('Warning: Still reading in placeholders from other location for some parameters')
  pjnz <- find_pjnz(loc="MWI")[[1]]
  temp.projp <- read_hivproj_param(pjnz, use_ep5=FALSE)
  relinfectART <- 0.15
  if(grepl('IND', loc)){
    temp.loc <- loc.table[parent_id == loc.table[ihme_loc_id == loc, location_id], ihme_loc_id][1]
  }else{
    temp.loc <- loc
  }
  fert_rat <- fread(paste0('/share/hiv/spectrum_input/190630_rhino/TFRreduction/', temp.loc, '.csv'))
  fert_rat <- fert_rat[, tfr_ratio]
  fert_rat_years <- array(0, c(7, length(proj.years)))
  for(j in 1:length(proj.years)){
    fert_rat_years[,j] <- fert_rat
  }
  dimnames(fert_rat_years) <- list(agegr = seq(15, 45, 5), proj.years)
  cd4fert_rat <- rep(1.0, 7)
  frr_art6mos <- rep(1.0, 7)
  names(frr_art6mos) <- seq(15, 45, 5)
  frr_scalar <- 1.0
  
  ## Currently subbing in GBD sex incrr parameters, if they exist
  incrr_sex <- fread(paste0('/ihme/hiv/spectrum_input/FtoM_inc_ratio/', temp.loc, '.csv'))
  incrr_sex <- incrr_sex[,.(FtoM_inc_ratio = mean(FtoM_inc_ratio)), by = 'year']
  pre.fill <- incrr_sex[year == 1, FtoM_inc_ratio]
  length.pre.fill <- 1985 - start.year - 1
  incrr_sex <- c(rep(pre.fill, length.pre.fill), incrr_sex[, FtoM_inc_ratio])
  if(length(incrr_sex) < length(proj.years)){
    post.fill <- incrr_sex[length(incrr_sex)]
    incrr_sex <- c(incrr_sex, rep(post.fill, length(proj.years) - length(incrr_sex)))
  } else if(length(incrr_sex) > length(proj.years)){
    incrr_sex <- incrr_sex[1:length(proj.years)]
  }
  names(incrr_sex) <- proj.years
  
  ## Subbing in previous (GBD 2017 and before) spec inputs for age incrr
  incrr_age <- fread(paste0(aim.dir, 'sex_age_pattern/age_IRRs/Feb17/GEN_IRR.csv'))
  incrr_age[, value := runif(n=nrow(incrr_age),min=lower,max=upper)]
  incrr_age[sex == 1, sex_name := 'Male']
  incrr_age[sex == 2, sex_name := 'Female']
  incrr_age[,c('lower', 'upper', 'sex') := NULL]
  incrr_age <- dcast.data.table(incrr_age, age~sex_name, value.var = 'value')
  incrr_age <- rbind(data.table(age = c(0, 5, 10), Female = c(0, 0, 0), Male = c(0, 0, 0)), incrr_age)
  incrr_age <- rbind(incrr_age, data.table(age = c(80), Female = c(0), Male = c(0)))
  incrr_age[, age := NULL]
  incrr_age <- as.matrix(incrr_age)
  incrr_age_years <- array(0, c(17, 2, length(proj.years)))
  for(j in 1:length(proj.years)){
    incrr_age_years[,,j] <- incrr_age
  }
  dimnames(incrr_age_years) <- list(seq(0, 80, 5), c('Male', 'Female'), paste0(proj.years))
  
  ## Using CD4 initial distribution and median CD4 placeholder
  ## TODO: How are these estimated? How should we fill these?
  artmx_timerr <- temp.projp$artmx_timerr
  cd4_initdist <- temp.projp$cd4_initdist
  art_alloc_method <- as.integer(1)
  scale_cd4_mort <- as.integer(0)
  art15plus_eligthresh <- fread(paste0('/share/hiv/spectrum_input/190630_rhino/adultARTeligibility/', temp.loc, '.csv'))
  art15plus_eligthresh[year >= 2016, cd4_threshold := 999]
  art15plus_eligthresh <- extend.years(art15plus_eligthresh, proj.years)
  art15plus_eligthresh <- art15plus_eligthresh[,.(year, cd4_threshold)]
  if(min(art15plus_eligthresh$year) > start.year){
    art15plus_eligthresh <- rbind(data.table(year = c(start.year:(min(art15plus_eligthresh$year) -1) ), cd4_threshold = rep(200, length(start.year:(min(art15plus_eligthresh$year) - 1) ))), art15plus_eligthresh)
  }
  art15plus_eligthresh <- art15plus_eligthresh[,cd4_threshold]
  names(art15plus_eligthresh) <- proj.years
  artelig_specpop <- temp.projp$artelig_specpop
  
  ## GBD progression parameters are subbed in later
  cd4_prog <- array(0, c(6, 4, 2))
  cd4_mort <- array(0, c(7, 4, 2))
  art_mort <- array(0, c(3, 7, 4, 2))
  
  median_cd4init <- rep(0, length(proj.years))
  names(median_cd4init) <- proj.years
  
  ## ART variables
  ## For now, using last year's extrapolated numbers
  ## art15plus_numperc - 0 if count, else 1
  if(grepl("IND",loc)){
    sub.art.cov.path <- paste0("/home/j/WORK/04_epi/01_database/02_data/hiv/04_models/gbd2015/02_inputs/AIM_assumptions/program_stats/
                               ART_adults/170322_IND_split_extrapolated/", loc, "_Adult_ART_cov.csv")
  } else {
    sub.art.cov.path <- paste0("/share/hiv/spectrum_input/190630_rhino/adultARTcoverage/",loc,".csv")
  }
  art <- fread(sub.art.cov.path)
  art[, value := ifelse(ART_cov_pct > 0, ART_cov_pct, ART_cov_num)]
  art[, numper := ifelse(ART_cov_pct > 0, 1, 0)]
  art <- art[, .(year, sex, value, numper)]
  art <- extend.years(art, start.year:stop.year)
  art15plus_num <- art[,.(year, sex, value)]
  art15plus_num <- dcast.data.table(art15plus_num, sex~year, value.var = 'value')
  art15plus_num[, sex := NULL]
  art15plus_num <- as.matrix(art15plus_num)
  dimnames(art15plus_num) <- list(sex=c("Male", "Female"), year=proj.years)
  
  art15plus_numperc <- art[,.(year, sex, numper)]
  art15plus_numperc <- dcast.data.table(art15plus_numperc, sex~year, value.var = 'numper')
  art15plus_numperc[, sex := NULL]
  art15plus_numperc <- as.matrix(art15plus_numperc)
  dimnames(art15plus_numperc) <- list(sex=c("Male", "Female"), year=proj.years)
  
  
  
  ##TODO: Not sure where to get art dropout and vert trans from
  ## For now, using previous vertical transmission results
  art_dropout <- rep(0, length(proj.years))
  names(art_dropout) <- proj.years
  spec <- fread(paste0('/share/hiv/spectrum_draws/190630_rhino_combined/compiled/stage_2/', loc, '_ART_data.csv'))
  verttrans <- spec[,.(birth_prev = mean(birth_prev), hiv_births = mean(hiv_births)), by = c('year', 'sex', 'age')]
  verttrans <- verttrans[, .(birth_prev = sum(birth_prev), hiv_births = sum(hiv_births)), by = 'year']
  verttrans[, value := ifelse(hiv_births == 0, 0, birth_prev/hiv_births)]
  verttrans <- verttrans[,.(year, value)]
  verttrans <- extend.years(verttrans, proj.years)
  if(min(verttrans$year) > start.year){
    verttrans <- rbind(data.table(year = c(start.year:(min(verttrans$year) -1) ), value = rep(0, length(start.year:(min(verttrans$year) - 1) ))), verttrans)
  }
  verttrans <- verttrans[,value]
  names(verttrans) <- proj.years
  
  hivpop <- spec[,.(pop_lt200 = mean(pop_lt200), pop_200to350 = mean(pop_200to350), pop_gt350 = mean(pop_gt350), pop_art = mean(pop_art)), by = c('sex', 'age', 'year')]
  hivpop <- hivpop[,.(sex, age, year, value = (pop_lt200 + pop_200to350 + pop_gt350 + pop_art))]
  hivpop <- extend.years(hivpop, proj.years)
  single <- data.table(expand.grid(year = start.year:stop.year, sex = c('male', 'female'), single.age = 0:80))
  single[, age := single.age - single.age%%5]
  hivpop <- merge(hivpop, single, by = c('sex', 'year', 'age'), all.y = TRUE)
  hivpop[, age := NULL]
  setnames(hivpop, 'single.age', 'age')
  hivpop[is.na(value), value := 0]
  hivpop[, value := value/5]
  hivpop <- dcast.data.table(hivpop, age + year ~ sex, value.var = 'value')
  temp <- array(0, c(81, 2, length(proj.years)))
  for(j in 1:length(proj.years)){
    hivpop.year <- hivpop[year == start.year + j - 1,.(male, female)]
    temp[,,j] <- as.matrix(hivpop.year)
  }
  dimnames(temp) <- list(0:80, c('Male', 'Female'), paste0(proj.years))
  hivpop <- temp
  
  hivdeaths <- spec[,.(value = mean(hiv_deaths)), by = c('sex', 'age', 'year')]
  hivdeaths <- extend.years(hivdeaths, proj.years)
  hivdeaths <- merge(hivdeaths, single, by = c('sex', 'year', 'age'), all.y = TRUE)
  hivdeaths[, age := NULL]
  setnames(hivdeaths, 'single.age', 'age')
  hivdeaths[is.na(value), value := 0]
  hivdeaths[, value := value/5]
  hivdeaths <- dcast.data.table(hivdeaths, age + year ~ sex, value.var = 'value')
  temp <- array(0, c(81, 2, length(proj.years)))
  for(j in 1:length(proj.years)){
    hivdeaths.year <- hivdeaths[year == start.year + j - 1,.(male, female)]
    temp[,,j] <- as.matrix(hivdeaths.year)
  }
  dimnames(temp) <- list(0:80, c('Male', 'Female'), paste0(proj.years))
  hivdeaths <- temp
  
  ## age 14 population
  ## Pulling age 14 HIV+ population from previous estimates (until we build paediatric model)
  ## This is a super rough estimate
  age14hivpop <- spec[age == 10,.(pop_lt200 = mean(pop_lt200), pop_200to350 = mean(pop_200to350),
                                  pop_gt350 = mean(pop_gt350), pop_art = mean(pop_art)), by = c('sex', 'age', 'year') ]
  age14hivpop <- age14hivpop[, .(sex, year, pop_noart = (pop_lt200 + pop_200to350 + pop_gt350)/5, pop_art = pop_art/5)]
  age14hivpop <- extend.years(age14hivpop, proj.years)
  art.cd4 <- expand.grid(sex = c('male', 'female'), ARTstage = c("PERINAT", "BF0MOS",  "BF6MOS",  "BF1YR",  "ART0MOS", "ART6MOS", "ART1YR"),
                         CD4cat = c("CD4_1000", "CD4_750",  "CD4_500",  "CD4_350",  "CD4_200",  "CD4_0"), year = proj.years)
  age14hivpop <- merge(age14hivpop, art.cd4, by = c('sex', 'year'))
  ## Super super rough estimates for now
  age14hivpop[ARTstage %in% c('ART0MOS', 'ART6MOS', 'ART1YR') & CD4cat == 'CD4_1000', value := pop_art/3]
  age14hivpop[ARTstage %in% c('PERINAT', 'BF0MOS', 'BF6MOS', 'BF1YR') & CD4cat == 'CD4_1000', value := pop_noart/4]
  age14hivpop[is.na(value), value := 0]
  age14hivpop[,c('pop_noart', 'pop_art') := NULL]
  age14hivpop <- dcast.data.table(age14hivpop, sex + year + ARTstage ~ CD4cat, value.var = 'value')
  temp <- array(0, c(7, 6, 2, length(proj.years)))
  for(j in 1:length(proj.years)){
    for(c.sex in c('male', 'female')){
      age14.year <- age14hivpop[year == start.year + j - 1 & c.sex == sex, c("CD4_1000", "CD4_750",  "CD4_500",  "CD4_350",  "CD4_200",  "CD4_0"), with = F]
      if(nrow(age14.year) == 0){
        age14.year <- array(0, c(7,6))
      }
      sex.index <- ifelse(c.sex == 'male', 1, 2)
      temp[,,sex.index,j] <- as.matrix(age14.year)
    }
  }
  age14hivpop <- temp
  dimnames(age14hivpop) <- list(ARTstage = c("PERINAT", "BF0MOS",  "BF6MOS",  "BF1YR",  "ART0MOS", "ART6MOS", "ART1YR" ),
                                CD4cat = c("CD4_1000", "CD4_750",  "CD4_500",  "CD4_350",  "CD4_200",  "CD4_0"),
                                Sex = c('Male', 'Female'),
                                Year = paste0(proj.years))
  
  dir <- paste0('/share/hiv/epp_input/gbd20/', run.name)
  pop <- fread(paste0(dir, '/population_single_age/', loc, '.csv'))
  pop <- pop[age_group_id == 14 + 48]
  pop <- pop[,.(year = year_id, sex_id, population)]
  while(max(pop$year) < stop.year){
    pop.ext <- pop[year == max(pop$year)]
    pop.ext[,year := year + 1]
    pop <- rbind(pop, pop.ext)
  }
  pop <- dcast.data.table(pop, sex_id~year, value.var = 'population')
  pop[, sex_id := NULL]
  age14totpop <- as.matrix(pop)
  dimnames(age14totpop) <- list(c("Male", "Female"), proj.years)
  
  projp <- list("yr_start" = yr_start,
                "yr_end" = yr_end,
                "relinfectART" = relinfectART,
                "fert_rat" = fert_rat_years,
                "cd4fert_rat" = cd4fert_rat,
                "frr_art6mos" = frr_art6mos,
                "frr_scalar" = frr_scalar,
                "incrr_sex" = incrr_sex,
                "incrr_age" = incrr_age_years,
                "cd4_initdist" = cd4_initdist,
                "cd4_prog" = cd4_prog,
                "cd4_mort" = cd4_mort,
                "art_mort" = art_mort,
                "artmx_timerr" = artmx_timerr,
                "art15plus_numperc" = art15plus_numperc,
                "art15plus_num" = art15plus_num,
                "art15plus_eligthresh" = art15plus_eligthresh,
                "artelig_specpop" = artelig_specpop,
                "median_cd4init" = median_cd4init,
                art_alloc_method = art_alloc_method,
                scale_cd4_mort = scale_cd4_mort,
                "art_dropout" = art_dropout,
                "verttrans" = verttrans,
                "hivpop" = hivpop,
                "hivdeaths" = hivdeaths,
                "age14hivpop" = age14hivpop,
                "age14totpop" = age14totpop)
  class(projp) <- "projp"
  attr(projp, "version") <- 5.63
  attr(projp, "validdate") <- Sys.Date()
  attr(projp, "validversion") <- "5.63"
  return(projp)
}
