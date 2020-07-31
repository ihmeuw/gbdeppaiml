loc = 'IND_4842'
run.name = '2020_ind_test_agg10'
theta.files <- list.files(paste0('/ihme/hiv/epp_output/gbd20/', run.name, "/", loc))
theta.files <- theta.files[grepl('theta', theta.files)]
theta.dt <- rbindlist(lapply(theta.files, function(ff){
  draw.dt <- fread(paste0('/ihme/hiv/epp_output/gbd20/', run.name, "/", loc, '/', ff))
  draw.dt[,i := .I]
  draw.dt[, draw := paste0('draw_', gsub('.csv', '', gsub('theta_', '', ff)))]
  return(draw.dt)
}))
theta.dt <- theta.dt[,.(theta = mean(theta), upper = quantile(theta, 0.975), lower = quantile(theta, 0.025)), by = 'i']
theta.dt.10 <- theta.dt

loc = 'IND_4842'
run.name = '2020_ind_test_agg9'
theta.files <- list.files(paste0('/ihme/hiv/epp_output/gbd20/', run.name, "/", loc))
theta.files <- theta.files[grepl('theta', theta.files)]
theta.dt <- rbindlist(lapply(theta.files, function(ff){
  draw.dt <- fread(paste0('/ihme/hiv/epp_output/gbd20/', run.name, "/", loc, '/', ff))
  draw.dt[,i := .I]
  draw.dt[, draw := paste0('draw_', gsub('.csv', '', gsub('theta_', '', ff)))]
  return(draw.dt)
}))
theta.dt.9 <- theta.dt[,.(theta = mean(theta), upper = quantile(theta, 0.975), lower = quantile(theta, 0.025)), by = 'i']

x <- rbind(theta.dt.9[,agg := 9], theta.dt.10[,agg := 10])
x <- melt(x, id.vars = c('i', 'agg'))

ggplot(x[i!=4,], aes(i, (value), col = factor(agg))) + geom_point() + facet_wrap(~variable)
