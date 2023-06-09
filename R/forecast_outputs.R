get_artinit <- function(mod){
  artinit = attr(mod, 'artinit')
  dimnames(artinit) = list(sex = c('male', 'female'), year = start.year:stop.year, ii = 1:10)
  artinit = data.table(as.data.frame.table(artinit))
  setnames(artinit, 'Freq', 'art_init')
  # March 2023 - outputting both counts of ART initiations and on-ART population at 1/10 year timestep
  # In order to subdivide spend into maintaining current ART pop + new initiations
  onart = attr(mod, 'onart_timestep')
  dimnames(onart) = list(sex = c('male', 'female'), year = start.year:stop.year, ii = 1:10)
  onart = data.table(as.data.frame.table(onart))
  setnames(onart, 'Freq', 'on_art')
  artinit = merge(artinit, onart, by = c('sex', 'year', 'ii'))
  # for finding proportion of off-ART population initiating ART at 1/10 yr timestep and averaging over the year
  # artinit = artinit[,.(art_init = mean(art_init)), by = c('sex', 'year')]
  
  artinit[,year := as.numeric(as.character(year))]
  artinit[,ii := as.numeric(ii)]
  artinit[,sex := as.character(sex)]
  
  return(artinit)
}
