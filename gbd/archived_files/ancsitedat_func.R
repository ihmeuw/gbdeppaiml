find_pjnz <- function(loc, year){
  if(grepl("KEN", loc) & loc.table[ihme_loc_id == loc, level] == 5) {
    temp.loc <- loc.table[location_id == loc.table[ihme_loc_id == loc, parent_id], ihme_loc_id]
  } else if(grepl('ZAF', loc)){
    temp.loc <- 'ZAF'
  } else {
    temp.loc <- loc
  } 
  
  
  loc.name <- loc.table[ihme_loc_id == temp.loc, location_name]
  ####CHANGING THIS LINE BECAUSE WE NEED TO GET 2016 DATA TO MATCH LBD
  unaids.year <- year
  if(temp.loc == 'NAM'){unaids.year = 2017}
  ## TODO: What is wrong with the 2018 ZAF file?
  if(grepl('ZAF', loc)){unaids.year = 2017}
  ##make exception for india
  if(grepl('IND', loc)){dir <-paste0("/ihme/limited_use/LIMITED_USE/PROJECT_FOLDERS/UNAIDS_ESTIMATES/2013/IND")}else{
    
    if(unaids.year %in% 2016:2019) {
      dir <- paste0("/home/j/DATA/UNAIDS_ESTIMATES/", unaids.year, "/", temp.loc, '/')
    } else {
      dir <- paste0("/ihme/limited_use/LIMITED_USE/PROJECT_FOLDERS/UNAIDS_ESTIMATES/", unaids.year, "/", temp.loc, "/")        
    }
  }
  if(file.exists(dir)) {
    pjnz.list <- list.files(dir, pattern = "PJNZ", full.names = T)
    pjn.list <- list.files(dir, pattern = "PJN", full.names = T)
    
    file.list <- grep(temp.loc, pjnz.list, value = T)
    if(length(file.list) == 0){file.list <- grep(temp.loc, pjn.list, value = T)}
    if(loc == "NGA") file.list <- c()
  } else {
    one.up <- paste(head(unlist(tstrsplit(dir, "/")), -1), collapse = "/")
    dir.list <- list.files(one.up, pattern = loc, full.names = T)
    file.list <- unlist(lapply(dir.list, function(dir) {
      list.files(dir, pattern = "PJNZ", full.names = T)
    }))
  }
  
  if(loc.name=="Central"){
    file.list <-  pjnz.list[which(grepl(paste0("Kenya-", loc.name), pjnz.list))]
  }
  if(loc.name=="Plateau"){
    file.list <-  pjnz.list[which(grepl(paste0("Nigeria_", loc.name), pjnz.list))]
  }
  
  ##This may be requried for the 2018 files, or just rename
  # if(loc.name=="Niger" | loc.name=="Guinea"| 
  #    loc.name=="Congo" | loc.name=="Sudan"){
  #   file.list <-  pjnz.list[which(grepl(paste0("/", loc.name,"_"), pjnz.list))]
  # }
  
  if(loc.name=="Niger" ){
    file.list <-  pjnz.list[which(grepl(paste0("/", loc.name,"_"), pjnz.list))]
  }
  
  if(temp.loc =="NGA_25344"){
    loc.name <- 'Nigeria_Niger'
    file.list <- grep(loc.name, pjnz.list, value = T)    
  }
  
  if(temp.loc =="NGA_25332"){
    loc.name <- 'FCT_Abuja'
    file.list <- grep(loc.name, pjnz.list, value = T)    
  }
  
  
  if(length(file.list) == 0) {
    loc.name <- loc.table[ihme_loc_id == temp.loc, location_name]
    file.list <-  pjnz.list[which(grepl(paste0(loc.name,"_"), pjnz.list))]
    if(length(file.list == 0)){
      loc.name <- paste(unlist(strsplit(toupper(loc.name), split = ' ')), collapse = '_')
      file.list <- pjnz.list[which(grepl((loc.name), pjnz.list))]
    }
    if(loc.name == 'Sudan'){
      file.list <- file.list[!grepl('South', file.list)]
    }
    
    if(length(file.list) == 0) {
      file.list <- grep(loc.name, pjnz.list, value = T)
    }
    
    if(length(file.list) == 0){
      loc.name <- loc.table[ihme_loc_id == temp.loc, location_name]
      loc.name <- gsub(" ", "", gsub("[^[:alnum:] ]", "", loc.name))
      file.list <- grep(loc.name, pjnz.list, value = T)     
    }
    
  }
  
  
  
  print(file.list)
  return(file.list)
}


prepare_spec_object <- function(loc, popadjust = TRUE, popupdate=TRUE, use_ep5=FALSE, year){
  
  pjnz <- find_pjnz(loc, year)[[1]]
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
    epp.totals <- collapse_epp(loc, year)
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
  demp <- read_specdp_demog_param(pjnz, use_ep5=use_ep5)
  
  projp <- read_hivproj_param(pjnz, use_ep5=use_ep5)
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

collapse_epp <- function(loc, year){
  file.list <- find_pjnz(loc, year)
  eppd.list <- lapply(file.list, function(file) {
    pjnz <- file
    eppd <- epp::read_epp_data(pjnz)
  })
  
  cc <- attr(eppd.list[[1]], 'country_code')
  
  subpop.tot <- loc
  
  eppd.tot <- eppd.list
  names(eppd.tot) <- names(eppd.list)
  
  if(length(file.list) > 1){
    add_index <<- TRUE
    for(kk in 1:length(eppd.list)){
      attr(eppd.tot[[kk]],"subpop") <-   names(eppd.list[[kk]])[1]
    }
  } else {
    add_index <<- FALSE
    eppd.tot <- eppd.list[[1]]
    names(eppd.tot) <- names(eppd.list[[1]])
    
    for(kk in 1:length(names(eppd.tot))){
      attr(eppd.tot[[kk]],"subpop") <- names(eppd.tot)[[kk]]
    }
    
  }
  
  ancsitedat <- melt_ancsite_data(eppd.tot, add_index = add_index)
  hhsdat <-  tidy_hhs_data(eppd.tot, add_index = add_index)
  
  eppd.list <- unlist(eppd.list,recursive = FALSE)
  eppd.tot <- eppd.list[1]
  subpop.tot <- loc
  names(eppd.tot) <- subpop.tot
  eppd.tot[[subpop.tot]]$ancsitedat <- ancsitedat
  eppd.tot[[subpop.tot]]$hhs <- hhsdat
  
  
  ##########REMOVE START HERE################
  # region
  eppd.tot[[subpop.tot]]$region <- subpop.tot
  eppd.tot[[subpop.tot]]$region <- subpop.tot
  
  #country
  attr(eppd.tot,"country") <- eppd.tot[[1]]$country
  attr(eppd.tot,"country_code") <- cc
  
  #anc.used (append)
  eppd.tot[[subpop.tot]]$anc.used <- unlist(lapply(eppd.list, function(eppd) {
    anc.used <- eppd$anc.used
    names(anc.used) <- rownames(eppd$anc.prev)
    return(anc.used)
  }))
  
  
  
  # anc.prev (append)
  eppd.tot[[subpop.tot]]$anc.prev <- do.call(rbind, lapply(eppd.list, function(eppd) {
    subpop <- names(eppd)
    anc.prev <- eppd$anc.prev
  }))
  
  # anc.n (append)
  eppd.tot[[subpop.tot]]$anc.n <- do.call(rbind, lapply(eppd.list, function(eppd) {
    subpop <- names(eppd)
    anc.n <- eppd$anc.n
  }))
  
  # ancrtsite.prev (collapse)
  eppd.tot[[subpop.tot]]$ancrtsite.prev <- do.call(rbind, lapply(eppd.list, function(eppd) {
    subpop <- names(eppd)
    ancrtsite.prev <- eppd$ancrtsite.prev
  }))
  
  # ancrtsite.prev (append)
  eppd.tot[[subpop.tot]]$ancrtsite.n <- do.call(rbind, lapply(eppd.list, function(eppd) {
    subpop <- names(eppd)
    ancrtsite.n <- eppd$ancrtsite.n
  }))
  
  # TODO: Pull all of this out, vet, sub in
  # For now, just collapsing
  artcens.temp <- data.table(do.call(rbind, lapply(eppd.list, function(eppd) {
    subpop <- names(eppd)
    ancrtcens <- eppd$ancrtcens
  })))
  # library(data.table)
  if(dim(artcens.temp)[1]!=0){
    artcens.temp <- artcens.temp[,.(prev = weighted.mean(prev, n), n = sum(n)), by = 'year']
    eppd.tot[[subpop.tot]]$ancrtcens <- as.data.frame(artcens.temp)
    # eppd.tot[[subpop.tot]]$ancrtcens <- NULL
  } else {
    eppd.tot[[subpop.tot]]$ancrtcens <- data.frame(year=integer(), prev=integer(), n=integer())
  }
  
  # hhs (append) ** be careful "not used TRUE"
  # hhs.temp <- data.table(do.call(rbind, lapply(eppd.list, function(eppd) {
  #   subpop <- names(eppd)
  #   hhs <- eppd$hhs
  # })))
  #hhs.temp <- hhs.temp[used == TRUE]
  # 
  # ##########remove end###############
  # # TODO: what to do with hhs? we're subbing in our own prev surveys, so probably doesn't matter
  # if(any(!is.na(hhs.temp$n))){
  #   hhs.temp[, pos := n * prev]
  #   hhs.sum <- hhs.temp[, lapply(.SD, sum), by = .(year)]
  #   hhs.sum[, prev := pos / n]
  #   hhs.sum[, se := ((prev * (1 - prev)) / n)**0.5]
  #   hhs.sum[, used := NULL]
  #   hhs.sum[, pos := NULL]
  #   hhs.sum[, used := TRUE]
  # }
  
  #eppd.tot[[subpop.tot]]$hhs <- as.data.frame(hhs.temp)
  # eppd.tot[[subpop.tot]]$hhs <- as.data.frame(hhs.temp)
  
  ## epp.subp
  epp.subp.list <- lapply(file.list, function(file) {
    pjnz <- file
    epp.subp <- epp::read_epp_subpops(pjnz)
  })
  
  #this depends on first one having no subpops so I think better to make an empty list
  epp.subp.tot <- list()
  
  
  # total
  total.temp <- data.table(do.call(rbind, lapply(epp.subp.list, function(epp.subp) {
    anc.n <- epp.subp$total
  })))
  total.sum <- total.temp[, lapply(.SD, sum), by = .(year)]
  epp.subp.tot$total <- as.data.frame(total.sum)
  epp.subp.tot$subpops[[subpop.tot]] <- as.data.frame(total.sum)
  
  ## epp.input
  if(length(file.list) > 1) {
    epp.input.list <- lapply(file.list, function(file) {
      pjnz <- file
      epp.input<- epp::read_epp_input(pjnz)
    })
    epp.input.tot <- epp.input.list[[1]]
    attr(epp.input.tot,"country") <- subpop.tot
    
    # start.year (check for difference)
    start.years <- unlist(lapply(epp.input.list, function(epp.input) {
      start.year <- epp.input$start.year
    }))
    length(unique(start.years)) == 1
    
    # stop.year (check for difference)
    stop.years <- unlist(lapply(epp.input.list, function(epp.input) {
      stop.year <- epp.input$stop.year
    }))
    length(unique(stop.years)) == 1
    
    # epidemic.start (check for difference)
    epidemic.starts <- unlist(lapply(epp.input.list, function(epp.input) {
      epidemic.start <- epp.input$epidemic.start
    }))
    epp.input.tot$epidemic.start <- min(epidemic.starts)
    
    # epp.pop (sum and mean)
    epp.pop.temp <- data.table(do.call(rbind, lapply(epp.input.list, function(epp.input) {
      epp.pop <- epp.input$epp.pop
    })))
    epp.pop.sum <- epp.pop.temp[, lapply(.SD, sum), by = .(year)]
    epp.pop.mean <- epp.pop.temp[, lapply(.SD, mean), by = .(year)]
    epp.pop.comb <- cbind(epp.pop.sum[, .(year, pop15to49, pop15, pop50, netmigr)], epp.pop.mean[, .(cd4median, hivp15yr)])
    epp.input.tot$epp.pop <- epp.pop.comb
    
    # cd4lowlim (check for difference)
    cd4lowlim.temp <- data.table(do.call(rbind, lapply(epp.input.list, function(epp.input) {
      cd4lowlim <- epp.input$cd4lowlim
    })))
    
    # cd4initperc (check for difference)
    cd4initperc.temp <- data.table(do.call(rbind, lapply(epp.input.list, function(epp.input) {
      cd4initperc <- epp.input$cd4initperc
    })))
    
    # cd4stage.dur (check for difference)
    cd4stage.dur.temp <- data.table(do.call(rbind, lapply(epp.input.list, function(epp.input) {
      cd4stage.dur <- epp.input$cd4stage.dur
    })))
    
    # cd4mort, artmort.less6mos, artmort.6to12mos, artmort.after1yr (leave the same)
    
    # infectreduc (check for difference)
    infectreducs <- unlist(lapply(epp.input.list, function(epp.input) {
      infectreduc <- epp.input$infectreduc
    }))
    length(unique(infectreducs)) == 1
    
    # epp.art (sum and mean) ** beware of percentages!!! also not sure whether 1stto2ndline is count or percent
    epp.art.temp <- rbindlist(lapply(epp.input.list, function(epp.input) {
      epp.art <- epp.input$epp.art
    }), fill = T)
    epp.art.temp[is.na(m.isperc), m.isperc := "N"]
    epp.art.temp[is.na(f.isperc), f.isperc := "N"]
    if("P" %in% unique(c(epp.art.temp$m.isperc, epp.art.temp$f.isperc))) {
      # Add prevalence
      epp.prev <- unlist(lapply(file.list, function(pjnz) {
        spu <- epp::read_spu(pjnz)$prev
        mean.spu <- rowMeans(spu)
      }))
      epp.prev.subset <- epp.prev[names(epp.prev) %in% paste0(unique(epp.art.temp$year))]
      
      if(nrow(epp.art.temp) != length(epp.prev.subset)) {
        stop("ART collapse problem")
      }
      epp.art.temp[, prev := epp.prev.subset]
      
      # Add population
      pop <- epp.pop.temp[year %in% unique(epp.art.temp$year)]
      epp.art.temp <- cbind(epp.art.temp, pop[, .(pop15to49)])
      epp.art.temp[, c("m.val", "f.val") := .(as.numeric(m.val), as.numeric(f.val))]
      epp.art.temp[m.isperc == "P", m.val := (weighted.mean(m.val, w = pop15to49 * prev) / .N), by = .(year)]
      epp.art.temp[f.isperc == "P", f.val := (weighted.mean(f.val, w = pop15to49 * prev) / .N), by = .(year)]
      epp.art.temp[, c("pop15to49", "prev") := NULL]
    }
    
    epp.art.hold <- epp.art.temp[1:length(min(epp.art.temp$year):max(epp.art.temp$year)), .(m.isperc, f.isperc)]
    epp.art.temp[, m.isperc := NULL]
    epp.art.temp[, f.isperc := NULL]
    epp.art.sum <- epp.art.temp[, lapply(.SD, sum), by = .(year)]
    epp.art.mean <- epp.art.temp[, lapply(.SD, mean), by = .(year)]
    epp.art.mode <- epp.art.temp[, lapply(.SD, Mode), by = .(year)]
    epp.art.comb <- cbind(epp.art.sum[, .(year, m.val, f.val, artdropout)],
                          epp.art.mode[, .(cd4thresh)],
                          epp.art.mean[, c("m.perc50plus", "f.perc50plus", "perc50plus", "1stto2ndline", "art15yr"), with = F],
                          epp.art.hold)
    epp.art.order <- epp.art.comb[, c("year", "m.isperc", "m.val", "f.isperc", "f.val", "cd4thresh", "m.perc50plus", "f.perc50plus", "perc50plus", "1stto2ndline", "art15yr"), with = F]
    epp.input.tot$epp.art <- as.data.frame(epp.art.order)
    
    # art.specpop (check for difference)
    art.specpop.temp <- data.table(do.call(rbind, lapply(epp.input.list, function(epp.input) {
      art.specpop <- epp.input$art.specpop
    })))
    
    # hivp15yr.cd4dist (check for difference)
    hivp15yr.cd4dist.temp <- data.table(do.call(rbind, lapply(epp.input.list, function(epp.input) {
      hivp15yr.cd4dist <- epp.input$hivp15yr.cd4dist
    })))
    
    # art15yr.cd4dist (check for difference)
    art15yr.cd4dist.temp <- data.table(do.call(rbind, lapply(epp.input.list, function(epp.input) {
      art15yr.cd4dist <- epp.input$art15yr.cd4dist
    })))
  } else {
    epp.input.tot <- epp::read_epp_input(file.list[1])
  }
  
  
  # epidemic.type (check for difference)
  # epidemic.types <- unlist(lapply(epp.input.list, function(epp.input) {
  #   epidemic.type <- epp.input$epidemic.type
  # }))
  # length(unique(epidemic.types)) == 1
  
  # ## Save
  # dir.create(paste0(dir, loc), showWarnings = F)
  # save(eppd.tot, file = paste0(dir, loc, "/eppd.Rdata"))
  # save(epp.subp.tot, file = paste0(dir, loc, "/epp_subp.Rdata"))
  # save(epp.input.tot, file = paste0(dir, loc, "/epp_input.Rdata"))
  
  
  epp_totals <- list(eppd = eppd.tot, epp.subp.tot = epp.subp.tot, epp.input.tot = epp.input.tot )
  return(epp_totals)
  
}

melt_ancsite_data <- function(eppd, add_index=FALSE){
  
  eppd2 <- eppd
  
  
  ancsitedat <- do.call(rbind,lapply(eppd2,function(eppd){
    eppd_x <- eppd2
    if(add_index){
      eppd_x <- eppd[[1]] 
    }
    print(typeof(eppd_x))
    anc.used <- data.frame(site=rownames(eppd_x$anc.prev), used= eppd_x$anc.used)
    anc.prev <- subset(reshape2::melt( eppd_x$anc.prev, varnames=c("site", "year"), value.name="prev"), !is.na(prev))
    ##breaking here
    anc.n <- subset(reshape2::melt( eppd_x$anc.n, varnames=c("site", "year"), value.name="n"), !is.na(n))
    ancsitedat <- merge(anc.used, anc.prev)
    ancsitedat <- merge(ancsitedat, anc.n)
    ancsitedat$subpop <- attr(eppd,"subpop")
    ancsitedat$type <- "ancss"
    
    
    if(exists("ancrtsite.prev", eppd) && !is.null(eppd$ancrtsite.prev)){
      ancrtsite.prev <- subset(reshape2::melt(eppd$ancrtsite.prev, varnames=c("site", "year"), value.name="prev"), !is.na(prev))
      ancrtsite.n <- subset(reshape2::melt(eppd$ancrtsite.n, varnames=c("site", "year"), value.name="n"), !is.na(n))
      
      ancrtsite <- merge(anc.used, ancrtsite.prev)
      ancrtsite <- merge(ancrtsite, ancrtsite.n)
      ancrtsite$type <- rep("ancrt", nrow(ancrtsite))
      ancrtsite$subpop <- rep(attr(eppd,"subpop"), nrow(ancrtsite)) 
      
      ancsitedat <- rbind(ancsitedat, ancrtsite)
    }
    
    return(ancsitedat)
  }))
  
  # ancsitedat <- merge(anc.used, anc.prev)
  # ancsitedat <- merge(ancsitedat, anc.n)
  # ancsitedat$type <- "ancss"
  
  
  ancsitedat <- subset(ancsitedat, used)
  ancsitedat$agegr <- rep("15-49", nrow(ancsitedat))
  ancsitedat$age <- rep(15, nrow(ancsitedat))
  ancsitedat$agspan <- rep(35, nrow(ancsitedat))
  
  rownames(ancsitedat) <- NULL
  ancsitedat
}
