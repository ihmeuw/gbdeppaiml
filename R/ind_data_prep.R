
## Function to read .ep1 and .ep3 files to create eppd object
prepare_eppd_ind <- function(loc, proj.end=2019, anc.sub = FALSE){
  unaids.year <- loc.table[ihme_loc_id == loc, unaids_recent]
  if(unaids.year %in% 2016:2019) {
    dir <- paste0("/home/j/DATA/UNAIDS_ESTIMATES/", unaids.year, "/", loc, '/')
  } else {
    dir <- paste0("/ihme/limited_use/LIMITED_USE/PROJECT_FOLDERS/UNAIDS_ESTIMATES/", unaids.year, "/IND/")        
  }
  filepath <- dir
  eppd <- ind.prepare.epp.fit(filepath, proj.end = proj.end, anc.sub = T)
  gen.pop.dict <- c("General Population", "General population", "GP", "GENERAL POPULATION", "GEN. POPL.", "General population(Low Risk)", "Remaining Pop")
  gen.pop.i <- which(names(eppd) %in% gen.pop.dict)
  temp.eppd <- list()
  temp.eppd[['General Population']] <- eppd[[gen.pop.i]]
  eppd <- temp.eppd
  ## melt site-level data
  eppd <- Map("[[<-", eppd, "ancsitedat", lapply(eppd, melt_ancsite_data))
  
  ## tidy HHS data
  eppd <- Map("[[<-", eppd, "hhs", lapply(eppd, tidy_hhs_data))
  
  gen.pop.dict <- c("General Population", "General population", "GP", "GENERAL POPULATION", "GEN. POPL.", "General population(Low Risk)", "Remaining Pop")
  gen.pop.i <- which(names(eppd) %in% gen.pop.dict)
  eppd <- eppd[[gen.pop.i]]
  
  ## Align with other eppd objects
  eppd$region <- loc
  eppd$projset_id <- 0
  eppd$ancrtcens <- data.frame(year=integer(), prev=integer(), n=integer())
  
  return(eppd)
}

ind.prepare.epp.fit <- function(filepath, proj.end=2016.5, anc.sub = T, sub.art.cov = F){
  loc_id <- loc.table[ihme_loc_id == loc, location_id]
  
  ## Paths
  sub.art.cov.path <- paste0("/home/j/WORK/04_epi/01_database/02_data/hiv/04_models/gbd2015/02_inputs/AIM_assumptions/program_stats/ART_adults/170322_IND_split_extrapolated/", loc, "_Adult_ART_cov.csv")
  sub.anc.path <- paste0(root, "WORK/04_epi/01_database/02_data/hiv/data/prepped/NACO_linked_ANC.csv")
  sub.anc.ibbs.path <- paste0(root, "WORK/04_epi/01_database/02_data/hiv/data/prepped/ibbs_anc_2014.csv")
  
  # old loc id map
  input.loc.map <- fread(paste0(root, "WORK/04_epi/01_database/02_data/hiv/04_models/gbd2015/02_inputs/location_map.csv"))[, .(ihme_loc_id, gbd15_spectrum_loc)]
  input.loc.map[ ihme_loc_id=="IND_4871", gbd15_spectrum_loc:= "IND_TL"]  
  iso3_init <- input.loc.map[ihme_loc_id == loc, gbd15_spectrum_loc]
  
  possible_files.which <- grepl(loc ,list.files('/ihme/limited_use/LIMITED_USE/PROJECT_FOLDERS/UNAIDS_ESTIMATES/2013/IND'))
  possible_files <- list.files('/ihme/limited_use/LIMITED_USE/PROJECT_FOLDERS/UNAIDS_ESTIMATES/2013/IND')[possible_files.which]
  ep1 <- paste0(filepath, possible_files[grepl('.EP1', possible_files)])
  ep1 <- scan(ep1, "character", sep="\n")
  ep1 <<- ep1[3:length(ep1)]
  ep4 <- paste0(filepath, possible_files[grepl('.EP3', possible_files)])
  ep4 <- scan(ep4, "character", sep="\n")
  ep4 <<- ep4[3:length(ep4)]
  
  ## epp
  xml <- paste0(filepath, possible_files[grepl('.XML', possible_files)])
  eppd <- ind.read.epp.data(xml)
  ##TODO - fix ANC subbing - there is currently an issue when merging together anc.prev, anc.n, and anc.used the melt_ancsite_data function
  if(anc.sub) {
    ## Prevalence
    # Prep ANC prevalence data for insertion
    gen.pop.dict <- c("General Population", "General population", "GP", "GENERAL POPULATION", "GEN. POPL.", "General population(Low Risk)", "Remaining Pop")
    sub.anc.dt <- fread(sub.anc.path, header = T)[gbd15_spectrum_loc == iso3_init]
    gen.pop <- names(eppd)[names(eppd) %in% gen.pop.dict]
    year.range <- colnames(eppd[[gen.pop]]$anc.prev)
    # Add years where needed
    for(add.year in setdiff(year.range, names(sub.anc.dt))) {
      sub.anc.dt[, (add.year) := NA]
    }
    # Convert to matrix
    sub.anc.matrix <- as.matrix(sub.anc.dt[, year.range, with=F]) / 100  # Convert from per 100 to per 1
    rownames(sub.anc.matrix) <- sub.anc.dt[["Site Name"]]
    # Sub in prevalence
    eppd[[gen.pop]]$anc.prev <- sub.anc.matrix
    
    ## Sample size
    # Prep sample size for insertion
    sub.anc.matrix[!is.na(sub.anc.matrix)] <- 400
    eppd[[gen.pop]]$anc.n <- sub.anc.matrix
    
    ## Used True/False
    eppd[[gen.pop]]$anc.used <- rep(TRUE, nrow(sub.anc.matrix))
    
    ##### Substitue the subpop anc with ibbs prev rate and sample size
    sub.anc.ibbs.dt <- fread(sub.anc.ibbs.path, header = T)[ihme_loc_id == loc_id]
    for (sub.pop in unique(sub.anc.ibbs.dt$subpop)){
      subpop.anc.ibbs.dt <- sub.anc.ibbs.dt[subpop==sub.pop]
      for (year.id in unique(subpop.anc.ibbs.dt$year)){
        year.range <- colnames(eppd[[sub.pop]]$anc.prev)
        ## add prev rate
        add.matrix <- t(rep(NA, length(year.range)))
        colnames(add.matrix) <- year.range
        rownames(add.matrix) <- "IBBS"
        add.matrix[, as.character(year.id)] <- subpop.anc.ibbs.dt[subpop==sub.pop & year==year.id]$prev/100
        eppd[[sub.pop]]$anc.prev <- rbind(eppd[[sub.pop]]$anc.prev, add.matrix)
        ## add sample size n
        n.add.matrix <- t(rep(NA, length(year.range)))
        colnames(n.add.matrix) <- year.range
        rownames(n.add.matrix) <- "IBBS"
        n.add.matrix[, as.character(year.id)] <- subpop.anc.ibbs.dt[subpop==sub.pop & year==year.id]$n
        eppd[[sub.pop]]$anc.n <- rbind(eppd[[sub.pop]]$anc.n, n.add.matrix)
        ## add used index
        eppd[[sub.pop]]$anc.used <- rep(TRUE, nrow(eppd[[sub.pop]]$anc.n))
      }
    }
  }
  for(subp in names(eppd)){
    if(length(eppd[[subp]]$anc.used)!= nrow(eppd[[subp]]$anc.prev)){
      eppd[[subp]]$anc.used <- rep(TRUE, nrow(eppd[[subp]]$anc.prev))
    }
  }
  
  return(eppd)
}


############################################################################
####  Function to read prevalence data used in EPP fitting (from .xml)  ####
############################################################################


ind.read.epp.data <- function(epp.xml){
  obj <- xmlTreeParse(epp.xml)
  
  r <- xmlRoot(obj)[[1]]
  eppSetChildren.idx <- which(xmlSApply(r, xmlAttrs) == "eppSetChildren")
  country <- xmlToList(r[[which(xmlSApply(r, xmlAttrs) == "worksetCountry")]][[1]])
  print(country)
  epp.data <- list() # declare list to store output
  attr(epp.data, "country") <- country
  used.indices <- NULL
  for(eppSet.idx in 1:xmlSize(r[[eppSetChildren.idx]])){
    
    tmp.eppSet <- r[[eppSetChildren.idx]][[eppSet.idx]][[1]]
    n.iter <- 1
    if (xmlSize(xmlSApply(tmp.eppSet, xmlAttrs)) > 0 & xmlSize(which(xmlSApply(tmp.eppSet, xmlAttrs) == "siteNames")) == 0) {
      newSetChildren.idx <- which(xmlSApply(tmp.eppSet, xmlAttrs) == "eppSetChildren")
      n.iter <- xmlSize(tmp.eppSet[[newSetChildren.idx]])
    }
    for (eppSet.idx.2 in 1:n.iter) {
      eppSet <- tmp.eppSet
      if (xmlSize(xmlSApply(tmp.eppSet, xmlAttrs)) > 0 & xmlSize(which(xmlSApply(tmp.eppSet, xmlAttrs) == "siteNames")) == 0)
        eppSet <- tmp.eppSet[[newSetChildren.idx]][[eppSet.idx.2]][[1]]
      
      if (xmlSize(eppSet) == 0) {
        
        tmp_len <- 0
        tmp_index <- 0
        obj_size <- 0
        while(obj_size == 0) {
          tmp_index <- tmp_index + 1
          if (tmp_index != eppSet.idx & !(tmp_index %in% used.indices)) {
            print(paste('tmp_index',tmp_index))
            eppSet <- r[[eppSetChildren.idx]][[tmp_index]][[1]]
            tmp_len <- length(which(xmlSApply(eppSet, xmlAttrs) == "groupToAssignTo"))
            if (tmp_len > 0) {
              tmp_eppSet <- eppSet[[which(xmlSApply(eppSet, xmlAttrs) == "groupToAssignTo")]]
              set_location <- ifelse(xmlSize(tmp_eppSet) == 1, 1, which(xmlSApply(tmp_eppSet, xmlAttrs) == "epp2011.core.sets.ProjectionSet"))
              tmp_eppSet <- tmp_eppSet[[set_location]]
              obj_size <- xmlSize(tmp_eppSet)
            }
          }
        }
        used.indices <- c(used.indices, tmp_index)
        eppSet <- eppSet[[which(xmlSApply(eppSet, xmlAttrs) == "groupToAssignTo")]]
        set_location <- ifelse(xmlSize(eppSet) == 1, 1, which(xmlSApply(eppSet, xmlAttrs) == "epp2011.core.sets.ProjectionSet"))
        eppSet <- eppSet[[set_location]]
      }
      eppName <- xmlToList(eppSet[[which(xmlSApply(eppSet, xmlAttrs) == "name")]][["string"]])
      print(eppName)
      ##  ANC data  ##
      
      siteNames.idx <- which(xmlSApply(eppSet, xmlAttrs) == "siteNames")
      siteSelected.idx <- which(xmlSApply(eppSet, xmlAttrs) == "siteSelected")
      survData.idx <- which(xmlSApply(eppSet, xmlAttrs) == "survData")
      survSampleSizes.idx <- which(xmlSApply(eppSet, xmlAttrs) == "survSampleSizes")
      
      siteNames <- xmlSApply(eppSet[[siteNames.idx]][[1]], xmlSApply, xmlToList, FALSE)
      siteIdx <- as.numeric(xmlSApply(eppSet[[siteNames.idx]][[1]], xmlAttrs)) ## 0 based
      anc.used <- as.logical(xmlSApply(eppSet[[siteSelected.idx]][[1]], xmlSApply, xmlToList, FALSE))
      
      nsites <- length(siteNames)
      nANCyears <- max(as.integer(xmlSApply(eppSet[[survData.idx]][["array"]][[1]][[1]], xmlAttrs))) + 1
      
      ## ANC prevalence
      anc.prev <- matrix(NA, nsites, nANCyears)
      rownames(anc.prev) <- siteNames
      colnames(anc.prev) <- 1985+0:(nANCyears-1)
      for(clinic.idx in 1:nsites){
        clinic <- eppSet[[survData.idx]][["array"]][[clinic.idx]][[1]]
        prev <- as.numeric(xmlSApply(clinic, xmlSApply, xmlToList, FALSE))
        idx <- as.integer(xmlSApply(clinic, xmlAttrs)) + 1
        anc.prev[clinic.idx, idx] <- prev
      }
      anc.prev[is.na(anc.prev)] <- 0.0 ## NOTE: appears that if value is 0.0, the array index is omitted from XML file, might apply elsewhere.
      anc.prev[anc.prev == -1] <- NA
      anc.prev <- anc.prev/100
      
      ## ANC sample sizes
      anc.n <- matrix(NA, nsites, nANCyears)
      rownames(anc.n) <- siteNames
      colnames(anc.n) <- 1985+0:(nANCyears-1)
      for(clinic.idx in 1:nsites){
        clinic <- eppSet[[survSampleSizes.idx]][["array"]][[clinic.idx]][[1]]
        n <- as.numeric(xmlSApply(clinic, xmlSApply, xmlToList, FALSE))
        idx <- as.integer(xmlSApply(clinic, xmlAttrs)) + 1
        anc.n[clinic.idx, idx] <- n
      }
      anc.n[anc.n == -1] <- NA
      
      # anc.prev[,colnames(anc.prev) <= 2000] <- NA
      # anc.n[,colnames(anc.n) <= 2000] <- NA
      ##  HH surveys  ##
      
      hhsUsed.idx <- which(xmlSApply(eppSet, xmlAttrs) == "surveyIsUsed")
      hhsHIV.idx <- which(xmlSApply(eppSet, xmlAttrs) == "surveyHIV")
      hhsSampleSize.idx <- which(xmlSApply(eppSet, xmlAttrs) == "surveySampleSize")
      hhsSE.idx <- which(xmlSApply(eppSet, xmlAttrs) == "surveyStandardError")
      hhsYear.idx <- which(xmlSApply(eppSet, xmlAttrs) == "surveyYears")
      
      nhhs <- max(xmlSize(eppSet[[hhsYear.idx]][[1]]),
                  xmlSize(eppSet[[hhsHIV.idx]][[1]]),
                  xmlSize(eppSet[[hhsSE.idx]][[1]]),
                  xmlSize(eppSet[[hhsSampleSize.idx]][[1]]),
                  xmlSize(eppSet[[hhsUsed.idx]][[1]]))
      
      hhs <- data.frame(year = rep(NA, nhhs), prev = rep(NA, nhhs), se = rep(NA, nhhs), n = rep(NA, nhhs), used = rep(NA, nhhs))
      
      hhs$year[as.integer(xmlSApply(eppSet[[hhsYear.idx]][[1]], xmlAttrs))+1] <- as.numeric(xmlSApply(eppSet[[hhsYear.idx]][[1]], xmlSApply, xmlToList, FALSE))
      hhs$prev[as.integer(xmlSApply(eppSet[[hhsHIV.idx]][[1]], xmlAttrs))+1] <- as.numeric(xmlSApply(eppSet[[hhsHIV.idx]][[1]], xmlSApply, xmlToList, FALSE))/100
      hhs$se[as.integer(xmlSApply(eppSet[[hhsSE.idx]][[1]], xmlAttrs))+1] <- as.numeric(xmlSApply(eppSet[[hhsSE.idx]][[1]], xmlSApply, xmlToList, FALSE))/100
      hhs$n[as.integer(xmlSApply(eppSet[[hhsSampleSize.idx]][[1]], xmlAttrs))+1] <- as.numeric(xmlSApply(eppSet[[hhsSampleSize.idx]][[1]], xmlSApply, xmlToList, FALSE))
      hhs$used[as.integer(xmlSApply(eppSet[[hhsUsed.idx]][[1]], xmlAttrs))+1] <- as.logical(xmlSApply(eppSet[[hhsUsed.idx]][[1]], xmlSApply, xmlToList, FALSE))
      
      hhs <- subset(hhs, !is.na(prev))
      
      epp.data[[eppName]] <- list(country=country,
                                  region=eppName,
                                  anc.used=anc.used,
                                  anc.prev=anc.prev,
                                  anc.n=anc.n,
                                  hhs=hhs)
    }
  }
  
  
  class(epp.data) <- "eppd"
  
  return(epp.data)
}
