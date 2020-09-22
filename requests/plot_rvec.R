loc = 'BEN'
x <- list.files(paste0('/ihme/homes/mwalte10/hiv_gbd2020/', loc, '/'))

rvec <- list()
for (file in x){
  dt <- fread(paste0('/ihme/homes/mwalte10/hiv_gbd2020/', loc, '/', file))
  dt[,sim := which(x == file)]
  rvec <- rbind(rvec, dt)
}

rvec <- rvec[,x := rep(c(1:472), 1168 * 2)]
rvec[,original := as.character(original)]
rvec[original == 1, original := 'old']
rvec[original == 0, original := 'modified']

library("viridis")  
gg <- ggplot(rvec, aes(x = sim,y = exp(rvec_sim1), col = factor(x))) + 
  geom_line(show.legend = F) + facet_wrap(~factor(original)) + 
  scale_color_viridis(discrete = T)
  gg

dir.create(paste0('/ihme/homes/mwalte10/hiv_gbd2020/rvec_plots/'))
pdf(paste0('/ihme/homes/mwalte10/hiv_gbd2020/rvec_plots/', loc, '.pdf'))
gg
dev.off
