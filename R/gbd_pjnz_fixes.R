
library(xml2)

get_eppxml_workset <- function(pjnz){
  
  xmlfile <- grep(".xml", unzip(pjnz, list=TRUE)$Name, value=TRUE)
  xmlfile <- grep(".xml", unzip(pjnz, list=TRUE)$Name, value=TRUE)
  
  if(!length(xmlfile)){
    warning(paste0("No EPP .xml file found for ", basename(pjnz)))
    return(NULL)
  }
  
  con <- unz(pjnz, xmlfile)
  epp.xml <- read_xml(con)
  
  r <- xml_children(xml_child(epp.xml))
  names(r) <- xml_attr(r, "property")
  
  return(r)
}


read_epp_input_fixes = function (pjnz) 
{
  ep1file <- grep(".ep1", unzip(pjnz, list = TRUE)$Name, value = TRUE)
  con <- unz(pjnz, ep1file)
  ep1 <- scan(con, "character", sep = "\n")
  close(con)
  country.idx <- grep("COUNTRY", ep1)
  firstprojyr.idx <- which(sapply(ep1, substr, 1, 11) == "FIRSTPROJYR")
  lastprojyr.idx <- which(sapply(ep1, substr, 1, 10) == "LASTPROJYR")
  popstart.idx <- grep("POPSTART", ep1) + 1
  popend.idx <- grep("POPEND", ep1) - 1
  country <- as.character(read.csv(text = ep1[country.idx], 
                                   header = FALSE, as.is = TRUE)[2])
  country.code <- as.integer(read.csv(text = ep1[country.idx], 
                                      header = FALSE)[3])
  start.year <- as.integer(read.csv(text = ep1[firstprojyr.idx], 
                                    header = FALSE)[2])
  stop.year <- as.integer(read.csv(text = ep1[lastprojyr.idx], 
                                   header = FALSE)[2])
  epp.pop <- setNames(read.csv(text = ep1[popstart.idx:popend.idx], 
                               header = FALSE, as.is = TRUE), c("year", "pop15to49", 
                                                                "pop15", "pop50", "netmigr"))
  ep4file <- grep(".ep4", unzip(pjnz, list = TRUE)$Name, value = TRUE)
  con <- unz(pjnz, ep4file)
  ep4 <- scan(con, "character", sep = "\n")
  close(con)
  cd4lim.idx <- which(sapply(ep4, substr, 1, 12) == "CD4LOWLIMITS")
  lambda.idx <- which(sapply(ep4, substr, 1, 6) == "LAMBDA")
  cd4init.idx <- which(sapply(ep4, substr, 1, 13) == "NEWINFECTSCD4")
  mu.idx <- which(sapply(ep4, substr, 1, 3) == "MU_")
  alpha1.idx <- which(sapply(ep4, substr, 1, 6) == "ALPHA1")
  alpha2.idx <- which(sapply(ep4, substr, 1, 6) == "ALPHA2")
  alpha3.idx <- which(sapply(ep4, substr, 1, 6) == "ALPHA3")
  infectreduc.idx <- which(sapply(ep4, substr, 1, 11) == "INFECTREDUC")
  artstart.idx <- grep("ARTSTART", ep4) + 1
  artend.idx <- grep("ARTEND", ep4) - 1
  DS <- 7
  cd4lim <- as.integer(read.csv(text = ep4[cd4lim.idx], header = FALSE)[-1][1:DS])
  cd4init <- as.matrix(read.csv(text = ep4[cd4init.idx], header = FALSE, 
                                row.names = 1)[, 1:DS])
  lambda <- as.matrix(read.csv(text = ep4[lambda.idx], header = FALSE, 
                               row.names = 1)[, 1:(DS - 1)])
  mu <- as.matrix(read.csv(text = ep4[mu.idx], header = FALSE, 
                           row.names = 1)[, 1:DS])
  alpha1 <- as.matrix(read.csv(text = ep4[alpha1.idx], header = FALSE, 
                               row.names = 1)[, 1:DS])
  alpha2 <- as.matrix(read.csv(text = ep4[alpha2.idx], header = FALSE, 
                               row.names = 1)[, 1:DS])
  alpha3 <- as.matrix(read.csv(text = ep4[alpha3.idx], header = FALSE, 
                               row.names = 1)[, 1:DS])
  infectreduc <- as.numeric(read.csv(text = ep4[infectreduc.idx], 
                                     header = FALSE)[2])
  epp.art <- setNames(read.csv(text = ep4[artstart.idx:artend.idx], 
                      header = FALSE, as.is = TRUE),
                  c("V1","V3","V5","V6","V7","V8","V9","V10"))
  epp.art$V2 <- rep("N",nrow(epp.art))
  epp.art$V4 <- rep("N",nrow(epp.art))
  epp.art <- epp.art[,c(paste0("V",1:10))]
  
  epp.art <- setNames(epp.art, c("year", "m.isperc",  "m.val", "f.isperc", "f.val", "cd4thresh", "m.perc50plus", 
                                                                "f.perc50plus", "perc50plus", "1stto2ndline"))
  specpop.idx <- grep("SPECPOP", ep4)
  if (length(specpop.idx)) {
    art.specpop <- setNames(read.csv(text = ep4[specpop.idx], 
                                     header = FALSE, colClasses = c("NULL", "character", 
                                                                    "numeric", "integer"))[, 1:3], c("specpop", 
                                                                                                     "percelig", "yearelig"))
    art.specpop$percelig <- art.specpop$percelig/100
  }  
    #else {art.specpop <- data.frame(specpop = character(), percelig = numeric(),  
#                                      yearelig = integer())
# }
  cd4median.start.idx <- which(ep4 == "CD4MEDIAN_START") + 
    1
  cd4median.end.idx <- which(ep4 == "CD4MEDIAN_END") - 1
  if (length(cd4median.start.idx) > 0) 
    epp.pop$cd4median <- read.csv(text = ep4[cd4median.start.idx:cd4median.end.idx], 
                                  header = FALSE, colClasses = c("NULL", "numeric"))[[1]]
  else epp.pop$cd4median <- 0
  hivp15yr.start.idx <- which(ep4 == "HIVPOS_15YEAROLDS") + 
    1
  hivp15yr.end.idx <- which(ep4 == "HIVPOS_15YEAROLDS_END") - 
    1
  if (length(hivp15yr.start.idx) > 0) 
    epp.pop$hivp15yr <- read.csv(text = ep4[hivp15yr.start.idx:hivp15yr.end.idx], 
                                 header = FALSE, colClasses = c("NULL", "numeric"))[[1]]
  else epp.pop$hivp15yr <- 0
  art15yr.start.idx <- which(ep4 == "HIVPOS_15YEAROLDSART") + 
    1
  art15yr.end.idx <- which(ep4 == "HIVPOS_15YEAROLDSART_END") - 
    1
  if (length(art15yr.start.idx) > 0) 
    epp.art$art15yr <- read.csv(text = ep4[art15yr.start.idx:art15yr.end.idx], 
                                header = FALSE, colClasses = c("NULL", "numeric"))[[1]]
  else epp.art$art15yr <- 0
  artdropout.start.idx <- which(ep4 == "ARTDROPOUTRATE") + 
    1
  artdropout.end.idx <- which(ep4 == "ARTDROPOUTRATE_END") - 
    1
  if (length(artdropout.start.idx) > 0) 
    epp.art$artdropout <- read.csv(text = ep4[artdropout.start.idx:artdropout.end.idx], 
                                   header = FALSE, colClasses = c("NULL", "numeric"))[[1]]
  else epp.art$artdropout <- 0
  hivp15yr.cd4dist.idx <- which(ep4 == "HIVPOS15_CD4") + 1
  if (length(hivp15yr.cd4dist.idx) > 0) 
    hivp15yr.cd4dist <- as.numeric(read.csv(text = ep4[hivp15yr.cd4dist.idx], 
                                            header = FALSE))
  else hivp15yr.cd4dist <- rep(0, length(cd4lim))
  art15yr.cd4dist.idx <- which(ep4 == "HIVPOS15ART_CD4") + 
    1
  if (length(art15yr.cd4dist.idx) > 0) 
    art15yr.cd4dist <- as.numeric(read.csv(text = ep4[art15yr.cd4dist.idx], 
                                           header = FALSE))
  else art15yr.cd4dist <- rep(0, length(cd4lim))
  if (mean(lambda[, 1]) > 1) 
    lambda <- lambda
  else lambda <- 1/lambda
  r <- get_eppxml_workset(pjnz)
  projsets <- xml_find_all(r, ".//object")
  projsets <- projsets[which(xml_attr(projsets, "class") == 
                               "epp2011.core.sets.ProjectionSet")]
  eppSet <- xml_children(projsets[1])
  epidemic.start <- as.integer(xml_double(eppSet[which(xml_attr(eppSet, 
                                                                "property") == "priorT0vr")]))
  eppin <- list(start.year = start.year, stop.year = stop.year, 
                epidemic.start = epidemic.start, epp.pop = epp.pop, 
                cd4lowlim = cd4lim, cd4initperc = cd4init, cd4stage.dur = lambda, 
                cd4mort = mu, artmort.less6mos = alpha1, artmort.6to12mos = alpha2, 
                artmort.after1yr = alpha3, infectreduc = infectreduc, 
                epp.art = epp.art, art.specpop = art.specpop, hivp15yr.cd4dist = hivp15yr.cd4dist, 
                art15yr.cd4dist = art15yr.cd4dist)
  class(eppin) <- "eppin"
  attr(eppin, "country") <- country
  attr(eppin, "country.code") <- country.code
  return(eppin)
}


read_spu_fixes = function (pjnz) 
{
  spufile <- grep("\\.SPU$", unzip(pjnz, list = TRUE)$Name,  value = TRUE)
  spu <- read.csv(unz(pjnz, spufile), header = FALSE)
  n.resamp <- as.numeric(strsplit(as.character(spu[1, 1]), 
                                  " ")[[1]][3])
  break.rows <- which(spu[, 1] == "==")
  n.years <- break.rows[2] - break.rows[1] - 2
  count <- sapply(strsplit(as.character(spu[break.rows[-1]-(n.years+1),1]), " "), function(x) as.numeric(x[2]))
  years <- as.numeric(as.character(spu[break.rows[1] - n.years:1, 1]))
  
  incid <- sapply(break.rows[-1], function(idx) {
    spu[which(spu[,3] == '?'),3] <- NA
    spu[idx - n.years:1,3]
    return(as.numeric(as.character(spu[idx-n.years:1,3])))
  })[, rep(1:length(count), count)]/100
  
  
  prev <- sapply(break.rows[-1], function(idx) {
    spu[which(spu[,2] == '?'),2] <- NA
    return(as.numeric(as.character(spu[idx-n.years:1,2])))
  })[,rep(1:length(count), count)]/100
  
  rownames(incid) <- years
  rownames(prev) <- years
  return(list(incid = incid, prev = prev))
}


  