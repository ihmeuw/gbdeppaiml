## Reads in prepped .rds object and subs in indicated GBD parameters
## Output object is read to run through fitmod()
read_spec_object <- function(loc, i, start.year = 1970, stop.year = 2019, trans.params.sub = TRUE, 
                             pop.sub = TRUE, anc.sub = TRUE, anc.backcast = TRUE, prev.sub = TRUE, art.sub = TRUE, sexincrr.sub = TRUE, 
                             popadjust = TRUE, age.prev = FALSE, paediatric = FALSE, anc.rt = FALSE){
                             
 if(file.exists(paste0('/share/hiv/data/PJNZ_EPPASM_prepped/', loc, '.rds'))){
  dt <- readRDS(paste0('/share/hiv/data/PJNZ_EPPASM_prepped/', loc, '.rds')) 
    
  } else {
    dt <- readRDS(paste0('/share/hiv/data/PJNZ_EPPASM_prepped_2a/', loc, '.rds')) 
  }
  
  ## Substitute IHME data
  ## Population parameters
  if(pop.sub){
    ## TODO fix this workflow
    print('Substituting demographic parameters')
    if(grepl('IND', loc)){
      demp <- create_spectrum_demog_param(loc, start.year, stop.year)
      projp <- create_hivproj_param(loc, start.year, stop.year)
      attr(dt, 'specfp') <- create_spectrum_fixpar(projp, demp, proj_start = start.year, proj_end = stop.year, popadjust=popadjust)
    }
      specfp <- sub.pop.params.specfp(attr(dt, 'specfp'), loc, i)
      specfp <- update_spectrum_fixpar(specfp, proj_start = start.year, proj_end = stop.year,time_epi_start = specfp$ss$time_epi_start, popadjust=popadjust)
      attr(dt, 'specfp') <- specfp
    }
  ## Pediatric inputs
  if(paediatric){
    print('Preparing paediatric module inputs')
    dt <- sub.paeds(dt, loc, i)
  }
  ## Transition parameters
  if(trans.params.sub) {
    print('Substituting transition parameters')
    dt <- sub.off.art(dt, loc, i)
    dt <- sub.on.art(dt, loc, i)
    dt <- sub.cd4.prog(dt, loc, i)
  }
  ## Extrapolated ART
  if(art.sub){
    print('Substituting ART data')
    dt <- sub.art(dt,loc, use.recent.unaids = FALSE)
  }
  ## Group 1 inputs
  if(grepl('1', loc.table[ihme_loc_id == loc, group])){
    ## Prevalence surveys
    if(prev.sub) {
      print("Substituting prevalence surveys")
      if(age.prev){
        dt <- sub.prev.granular(dt, loc)
        attr(dt, 'specfp')$fitincrr <- 'linincrr'
      } else{
        dt <- sub.prev(loc, dt)	
        attr(dt, 'specfp')$fitincrr <- FALSE
      }
    }
    
    ## ANC data
    if(anc.sub){
      print("ANC substitution")
      high.risk.list <- loc.table[epp == 1 & collapse_subpop == 0 & !grepl("ZAF", ihme_loc_id) & !grepl("KEN", ihme_loc_id), ihme_loc_id]
      ken.anc.path <- paste0(root, "WORK/04_epi/01_database/02_data/hiv/data/prepped/kenya_anc_map.csv")
      ken.anc <- fread(ken.anc.path)
      no.anc.ken <- setdiff(loc.table[epp == 1 & grepl("KEN", ihme_loc_id), ihme_loc_id], ken.anc$ihme_loc_id)
      if(loc %in% c(high.risk.list, "PNG", no.anc.ken)) {
        anc.backcast <- F
      }
      
      if(anc.backcast) {
        print("Substituting ANC backcast output")
        dt <- sub.anc(loc, dt, i)
      }
      
    } 
    if(sexincrr.sub){
      print('Substiting sex incrr')
      dt <- sub.sexincrr(dt, loc, i)
    }
    
    ## Subsetting KEN counties from province
    if(grepl('KEN', loc)){
      ken.anc.path <- paste0('/share/hiv/epp_input/gbd19/kenya_anc_map.csv')
      ken.anc <- fread(ken.anc.path)
      county.sites <- ken.anc[ihme_loc_id == loc, site]
      prov.sites <- row.names(attr(dt, "eppd")$anc.prev)
      keep.index <- which(prov.sites %in% county.sites  | grepl(loc.table[ihme_loc_id == loc, location_name], prov.sites))
      attr(dt, "eppd")$anc.used[] <- FALSE
      if(length(keep.index) > 0) {
        attr(dt, "eppd")$anc.used[keep.index] <- TRUE
      }
      ##TODO - need to update mapping, take out grepl on location name
      attr(dt, 'eppd')$ancsitedat$used[!(attr(dt, 'eppd')$ancsitedat$site %in% county.sites | grepl(loc.table[ihme_loc_id == loc, location_name], attr(dt, 'eppd')$ancsitedat$site))] <- FALSE
      # ART
      prop.path <- paste0("/share/hiv/epp_input/gbd19/KEN_ART_props.csv")
      prop.dt <- fread(prop.path)
      prop <- prop.dt[ihme_loc_id == loc, prop_pepfar]
      attr(dt,"specfp")$art15plus_num <- attr(dt,"specfp")$art15plus_num * prop
    }
      
  
    if(!anc.rt){
      attr(dt, 'eppd')$ancrtsite.prev <- NULL
      attr(dt, 'eppd')$ancrtsite.n <- NULL
      attr(dt, 'eppd')$ancrtcens <- NULL
    }
    attr(dt, 'specfp')$prior_args <- list(logiota.unif.prior = c(log(1e-14), log(0.000025)))
  }else{
    ## Group 2 inputs
    print('Appending vital registration death data')
    dt <- append.vr(dt, loc, run.name)
    attr(dt, 'specfp')$group <- '2'
    attr(dt, 'specfp')$mortadjust = 'simple'
    print('Appending case notification data')
    dt <- append.diagn(dt, loc, run.name)
    attr(dt, 'specfp')$incid_func <- NULL
    attr(dt, 'specfp')$incidinput <- NULL
    attr(dt, 'specfp')$eppmod <- 'rlogistic'
    attr(dt, 'specfp')$ss$time_epi_start <- 1970
    
    print('Appending CIBA age/sex incrr priors')
    dt <- append.ciba.incrr(dt, loc, run.name)
    
  }
  
    return(dt)
    
}

## A modified version of create_spectrum_fixpar in spectrum.R in EPPASM, which is used to create a specfp object from projp and demp
## In order to bypass reading in pjnz files, this function can be used to format the specfp object so it's ready to run through simmod
## NOTE that when major changes are made to create_spectrum_fixpar(), they may need to be added to this function
update_spectrum_fixpar <- function(specfp, hiv_steps_per_year = 10L, proj_start = start.year, proj_end = stop.year,time_epi_start = 1970,
                                   AGE_START = 15L, popadjust=TRUE, artelig200adj=TRUE, who34percelig=0){
  
  ## ########################## ##
  ##  Define model state space  ##
  ## ########################## ##
  
  ## Parameters defining the model projection period and state-space
  ss <- list(proj_start = proj_start,
             PROJ_YEARS = as.integer(proj_end - proj_start + 1L),
             AGE_START  = as.integer(AGE_START),
             hiv_steps_per_year = as.integer(hiv_steps_per_year),
             time_epi_start=time_epi_start)
  
  ## populuation projection state-space
  ss$NG <- 2
  ss$pDS <- 2               # Disease stratification for population projection (HIV-, and HIV+)
  
  ## macros
  ss$m.idx <- 1
  ss$f.idx <- 2
  
  ss$hivn.idx <- 1
  ss$hivp.idx <- 2
  
  ss$pAG <- 81 - AGE_START
  ss$ag.rate <- 1
  ss$p.fert.idx <- 16:50 - AGE_START
  ss$p.age15to49.idx <- 16:50 - AGE_START
  ss$p.age15plus.idx <- (16-AGE_START):ss$pAG
  
  
  ## HIV model state-space
  ss$h.ag.span <- as.integer(c(2,3, rep(5, 6), 31))   # Number of population age groups spanned by each HIV age group [sum(h.ag.span) = pAG]
  ss$hAG <- length(ss$h.ag.span)          # Number of age groups
  ss$hDS <- 7                             # Number of CD4 stages (Disease Stages)
  ss$hTS <- 3                             # number of treatment stages (including untreated)
  
  ss$ag.idx <- rep(1:ss$hAG, ss$h.ag.span)
  ss$agfirst.idx <- which(!duplicated(ss$ag.idx))
  ss$aglast.idx <- which(!duplicated(ss$ag.idx, fromLast=TRUE))
  
  
  ss$h.fert.idx <- which((AGE_START-1 + cumsum(ss$h.ag.span)) %in% 15:49)
  ss$h.age15to49.idx <- which((AGE_START-1 + cumsum(ss$h.ag.span)) %in% 15:49)
  ss$h.age15plus.idx <- which((AGE_START-1 + cumsum(ss$h.ag.span)) >= 15)
  
  ## Paediatric state space
  ss$pAGu5 <- 5 ## under 5 ages
  ss$hDSu5 <- 7 ## cd4 percent
  ss$hMT <- 4 ## perinatal, bf0, bf6, bf12
  ss$pAGu15 <- 10 ## 5-15 ages
  ss$hDSu15 <- 6 ##under 15 cd4 count categories
  ss$u5.elig.groups <- list('30' = 1, '25' = 2, '20' = 3, '15' = 4, '10' = 5, '5' = 6, '0' = 7)
  ss$u15.elig.groups <- list('1000' = 1, '750' = 2, '500' = 3, '350' = 4, '200' = 5, '0' = 6)  
  ss$prenat.opt <- c('tripleARTdurPreg', 'tripleARTbefPreg', 'singleDoseNevir', 'prenat_optionB', 'prenat_optionA', 'dualARV')
  
  
  invisible(list2env(ss, environment())) # put ss variables in environment for convenience
  
  specfp$ss <- ss
  specfp$SIM_YEARS <- ss$PROJ_YEARS
  specfp$proj.steps <- proj_start + 0.5 + 0:(ss$hiv_steps_per_year * (specfp$SIM_YEARS-1)) / ss$hiv_steps_per_year
  
  ## ######################## ##
  ##  Demographic parameters  ##
  ## ######################## ##
  
  ## Calcuate the net-migration and survival up to AGE_START for each birth cohort.
  ## For cohorts born before projection start, this will be the partial
  ## survival since the projection start to AGE_START, and the corresponding lagged "births"
  ## represent the number in the basepop who will survive to the corresponding age.
  
  cumnetmigr <- array(0, dim=c(NG, PROJ_YEARS))
  cumsurv <- array(1, dim=c(NG, PROJ_YEARS))
  if(AGE_START > 0){
    for(i in 2:PROJ_YEARS){  # start at 2 because year 1 inputs are not used
      for(s in 1:2){
        for(j in max(1, AGE_START-(i-2)):AGE_START){
          ii <- i+j-AGE_START
          cumsurv[s,i] <- cumsurv[s,i] * specfp$Sx[j,s,ii]
          if(j==1)
            cumnetmigr[s,i] <- specfp$netmigr[j,s,ii] * (1+2*specfp$Sx[j,s,ii])/3
          else
            cumnetmigr[s,i] <- cumnetmigr[s,i]*specfp$Sx[j,s,ii] + specfp$netmigr[j,s,ii] * (1+specfp$Sx[j,s,ii])/2
        }
      }
    }
  }
  
  ## initial values for births
  birthslag <- array(0, dim=c(NG, PROJ_YEARS))             # birthslag(i,s) = number of births of sex s, i-AGE_START years ago
  birthslag[,1:AGE_START] <- t(specfp$basepop[AGE_START:1,])  # initial pop values (NOTE REVERSE ORDER). Rest will be completed by fertility during projection
  
  specfp$birthslag <- birthslag
  specfp$cumsurv <- cumsurv
  specfp$cumnetmigr <- cumnetmigr
  
  
  ## set population adjustment
  specfp$popadjust <- popadjust
  if(!length(setdiff(proj_start:proj_end, dimnames(specfp$targetpop)[[3]]))){
    specfp$entrantpop <- specfp$targetpop[1,,as.character(proj_start:proj_end)]
  }
  if(popadjust & is.null(specfp$targetpop))
    stop("targetpop does not span proj_start:proj_end")

  ## ###################### ##
  ##  HIV model parameters  ##
  ## ###################### ##
  
  if(is.null(specfp$relinfectART)){
    specfp$relinfectART <- 0.15
  }
  
  ## Update eligibility threshold from CD4 <200 to <250 to account for additional
  ## proportion eligible with WHO Stage 3/4.
  if(artelig200adj){
    specfp$artcd4elig_idx <- replace(specfp$artcd4elig_idx, specfp$artcd4elig_idx==5L, 4L)
  }
  ## percentage of those with CD4 <350 who are based on WHO Stage III/IV infection
  specfp$who34percelig <- who34percelig
  
  ##TODO could add GBD 2017 age 15 hiv prevalence, add ART coverage at age 15
  
  specfp$netmig_hivprob <- 0.4*0.22
  specfp$netmighivsurv <- 0.25/0.22
  
  attr(dt, 'specfp')$incid_func <- 'id'
  
  ## Circumcision parameters (default no effect)
  specfp$circ_incid_rr <- 0.0  # no reduction
  specfp$circ_prop <- array(0.0, c(ss$pAG, ss$PROJ_YEARS),
                        list(age = ss$AGE_START + 1:ss$pAG - 1L,
                             year = ss$proj_start + 1:ss$PROJ_YEARS - 1L))
  
  return(specfp)
}


  