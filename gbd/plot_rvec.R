loc.list <- c('AGO', 'BFA', 'BWA', 
              'CAF', 'CPV', 'GHA',
              'GNQ', 'LSO', 'ZMB', 
              'SSD')

load_rvec <- function(loc){
  rvec_mod <- readRDS(paste0('/ihme/homes/mwalte10/hiv_gbd2020/200923_socialdets/', loc, '.RDS'))
  rvec <- data.table(rvec = rvec_mod$rvec, loc = loc, ts = c(1:522), run = '200923_socialdets')
  
  rvec_reg<- readRDS(paste0('/ihme/homes/mwalte10/hiv_gbd2020/200924_socialdets/', loc, '.RDS'))
  rvec_reg <- data.table(rvec = rvec_reg$rvec, loc = loc, ts = c(1:522), run = '200924_socialdets')
  
  rvec <- rbind(rvec, rvec_reg)
  return(rvec)
}
rvec <- lapply(loc.list, load_rvec)
rvec <- rbindlist(rvec)
dt <- rvec
gg <- ggplot(dt, aes(ts, rvec, col = factor(loc))) + geom_line() + facet_wrap(~run)
print(gg)
# pdf('/ihme/homes/mwalte10/hiv_gbd2020/200922_socialdets/rvec.pdf')
# gg
# graphics.off()
