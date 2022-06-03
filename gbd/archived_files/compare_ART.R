### Code
epp.list <- sort(loc.table[epp == 1 & grepl('1', group), ihme_loc_id])
loc.list <- epp.list
##standard loc list
loc.list <- c(loc.list, 'MRT', 'STP', 'COM')
loc.list <- c('BWA', 'SWZ', 'LSO', 'ZMB', 'GNQ', loc.list[grepl('ZAF', loc.list)])

pdf('/ihme/homes/mwalte10/new_ART.pdf')
for(loc in loc.list){
  hist <- fread(paste0('/ihme/hiv/spectrum_prepped/aggregates/200713_yuka/', loc, '.csv'))
  hist <- hist[age_group_id %in% c(8:20, 30,31,32,235),]
  hist <- hist[,.(pop_art = sum(pop_art), prev = sum(pop_hiv)), by = c('run_num', 'year_id', 'sex_id')]
  hist[,cov := pop_art/ prev]
  hist <- hist[,.(cov = mean(cov)), by = c('year_id', 'sex_id')]
  hist <- hist[!is.nan(cov)]
  hist[,run := 'old']
  
  new <- fread(paste0('/mnt/share/hiv/data/adultART_extraction/', loc, '_adult_ART_cov.csv'))
  new <- new[,.(sex_id = sex, year_id = year, cov = ART_cov_pct, run = 'new')]
  
  dt <- rbind(hist, new)
  
  gg = ggplot(dt[year_id > 1994], aes(year_id, cov, col = as.factor(run))) + facet_wrap(~sex_id) + geom_line(lwd = 1.5) + ggtitle(loc.table[ihme_loc_id == loc, plot_name]) + theme_bw() +
    labs(x = 'Year', y = 'ART coverage', col = 'ART version') + theme(text= element_text(size =15), legend.position = 'bottom')
  print(gg)
}
dev.off()