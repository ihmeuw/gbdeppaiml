dt <- readRDS('/ihme/hiv/epp_output/gbd20/201012_ancrt/dt_objects/BDI_dt.RDS')
anc <- attr(dt, 'eppd')$ancsitedat
ancrtcens <- attr(dt,'eppd')$ancrtcens
ancrtcens <- data.table(ancrtcens)
anc <- rbind(anc, ancrtcens[,type := 'cens'], fill = T)
gg <- ggplot(anc, aes(year, prev, col = factor(type))) + geom_point()
gg


p <- ggplot(anc[type == 'ancrt'], aes(x=prev, col = factor(year))) + 
  geom_density() + ggtitle('ANCRT dist by year')


p <- ggplot(anc, aes(x=prev, col = factor(type))) + 
  geom_density() 
  
ancss <- anc[type == 'ancss']
setnames(ancss, 'prev', 'ancss')
ancrt <- anc[type == 'ancrt']
setnames(ancrt, 'prev', 'ancrt')
ancss[,year2 := year**2]
ancss[,year3 := year**3]

lm_ancss <- lm(ancss ~ year + year2 + year3, data = ancss)

ancrt[,year2 := year**2]
lm_ancrt <- lm(ancrt ~ year + year2, data = ancrt)

ancss_predict <- predict(lm_ancss, list(year = seq(1999,2022), year2 = seq(1999,2022)**2, year3 = seq(1999,2022)**3), interval = 'confidence')
ancss_predict <- as.data.table(ancss_predict)
ancss_predict[,year := seq(1999,2022)]
setnames(ancss_predict, 'fit', 'prev')
ancss_predict[,type := 'estimate']
ancss_predict <- rbind(ancss_predict, anc[,.(year, prev, type)], fill = TRUE)
ancss_predict[,type := as.factor(type)]
ancss_predict[,year := as.integer(year)]

gg <- ggplot() + 
  geom_line(data = ancss_predict[type == 'estimate'],aes(year, prev)) + 
  geom_ribbon(data = ancss_predict[type == 'estimate'], aes(x = year,ymin = lwr, ymax = upr, alpha = 0.5))+
  geom_point(data = ancss_predict[type != 'estimate'], aes(year, prev, col = factor(type)))







loc.table <- get_locations(hiv_metadata = T)
loc.list <- loc.table[epp == 1, ihme_loc_id]
loc.list <- loc.list[!grepl('IND', loc.list)]
anc_data <- c()
for(loc in loc.list){
  dt <- readRDS(paste0('/ihme/hiv/epp_output/gbd20/191224_trumpet//dt_objects/', loc, '_dt.RDS'))
  anc <- attr(dt, 'eppd')$ancsitedat
  anc <- as.data.table(anc)
  if(ncol(anc) == 10){
    anc[,offset := NA]
  }
  if(any(anc[,type] == 'ancrt')){
    anc_data <- rbind(anc, anc_data, fill = T)
  }else{
    next
  }
  print(loc)
}


anc_data <- anc_data[,.(year, site, prev, type, n)]


anc_data[type == 'ancss',ancss_prev := prev, by = c('year', 'site')]
anc_data[type == 'ancss',ancss_n := n, by = c('year', 'site')]
anc_data[type == 'ancrt',ancrt_prev := prev, by = c('year', 'site')]
anc_data[type == 'ancrt',ancrt_n := n, by = c('year', 'site')]

ancss <- unique(anc_data[,.(year, site, ancss_prev, ancss_n)])
ancrt <- unique(anc_data[,.(year, site, ancrt_prev, ancrt_n)])
anc_data <- merge(ancss, ancrt)




