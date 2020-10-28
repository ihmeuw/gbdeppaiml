plot_rvec_epp_v_eppasm <- function(loc = 'IND_4841', epp_run = '200316_windchime_ind',
                                   eppasm_vec = c('2020_ind_test_agg2',  '2020_ind_test_agg3', '2020_ind_test_agg4')){
  
  #######
  #LOAD EPP DATA
  #not that currently this is just taking one draw (I think), but eventually should be expanded so that
  #it is averaging across draws
  #######
  path = paste0("/ihme/hiv/epp_output/gbd20/", epp_run, '/',loc,"/results1.RData")
  load(path)
  rvec <- result[[1]]$rvec
  proj.steps <- result[[1]]$fp$proj.steps
  idx <- which(proj.steps %% 1 == 0.5)
  year <- floor(proj.steps[idx])
  subset.rvec <- rvec[idx,]
  mean <- rowMeans(subset.rvec)
  rt.data <- as.data.table(cbind(year,mean))
  epp <- cbind(rt.data, model = 'EPP')
  epp <- as.data.table(epp)
  
  #######
  #LOAD EPPASM RUNS
  #######
  eppasm_runs <- c()
  for(run.name in eppasm_vec){
    r_vers <- run_table[run_name == run.name, epp_mod]
    if(r_vers == 'rhybrid'){
      theta <- read.csv(paste0("/ihme/hiv/epp_output/gbd20/", run.name, '/',loc,"/theta_1.csv"),header=TRUE)[,1]
      rw_dk <- 5
      rlogistic_steps <- seq(1970.5,2003,by=0.1)
      n_rw <- ceiling((2019.5 - 2003) / rw_dk)
      rw_steps <- seq(2003.1,2022.5,by=0.1)
      rlogistic <- function(t,p=par){
        p[1] - (p[1] - p[2]) / (1 + exp(-p[3] * (t - p[4])))
      }
      #LOGISTIC
      par <- theta[1:4]
      par[3] <- exp(par[3])
      rvec_rlog <-rlogistic(rlogistic_steps, par)
      #TRANSITION PERIOD uses linear interpolation
      rw_transition <- approx(c(2003, 2008.001), c(0, 1),rw_steps[-1] , rule = 2)$y
      #RW EXTENSION
      n_rw <- 4 
      rw_knots <- seq(2003, 2003 + n_rw*5, by = 5)
      rw_idx <- findInterval(rw_steps[-1], rw_knots)
      th_rw <- theta[4+1:n_rw]
      diff_rlog <- diff(rlogistic(rw_steps, par))
      diff_rw <- 0.1 * th_rw[rw_idx] / sqrt(rw_dk)
      diff_rvec <- (1 - rw_transition) * diff_rlog + rw_transition * diff_rw
      rvec_rw <- cumsum(c(rvec_rlog[length(rvec_rlog)], diff_rvec))
      rvec <- c(rvec_rlog, rvec_rw)
      final_rt <- exp(rvec)
      
      eppasm <- cbind(as.numeric(final_rt), 'EPPASM')
      eppasm <- cbind(eppasm, seq(1970.5,2022.5, by = 0.1))
      eppasm <- as.data.table(eppasm)
      colnames(eppasm) <- c('value', 'model', 'ts')
      eppasm[,value := as.numeric(value)]
      eppasm[,ts := as.numeric(ts)]
      new_eppasm <- c()
      for (year in seq(1970,2022)) {
        new_eppasm[year - 1969] <- mean(as.numeric(eppasm[grep(year, eppasm[,ts]),value]))
      }
      eppasm <- cbind('mean' = new_eppasm, 'year' = seq(1970,2022), model = r_vers)
      eppasm <- as.data.table(eppasm)
      
    }
    
    if(r_vers == 'rlogistic'){
      theta <- read.csv(paste0("/ihme/hiv/epp_output/gbd20/", run.name, '/',loc,"/theta_1.csv"),header=TRUE)[,1]
      rlogistic_steps <- seq(1970.5,2022.5,by=0.1)
      rlogistic <- function(t,p=par){
        p[1] - (p[1] - p[2]) / (1 + exp(-p[3] * (t - p[4])))
      }
      par <- theta[1:4]
      par[3] <- exp(par[3])
      rvec_rlog <-rlogistic(rlogistic_steps, par)
      final_rt <- exp(rvec_rlog)
      eppasm <- cbind(as.numeric(final_rt), 'EPPASM')
      eppasm <- cbind(eppasm, seq(1970.5,2022.5, by = 0.1))
      eppasm <- as.data.table(eppasm)
      colnames(eppasm) <- c('value', 'model', 'ts')
      eppasm[,value := as.numeric(value)]
      eppasm[,ts := as.numeric(ts)]
      new_eppasm <- c()
      for (year in seq(1970,2022)) {
        new_eppasm[year - 1969] <- mean(as.numeric(eppasm[grep(year, eppasm[,ts]),value]))
      }
      eppasm <- cbind('mean' = new_eppasm, 'year' = seq(1970,2022), model = r_vers)
      eppasm <- as.data.table(eppasm)
      
    }
    
    if(r_vers == 'rspline'){
      theta <- read.csv(paste0("/ihme/hiv/epp_output/gbd20/", run.name, '/',loc,"/theta_1.csv"),header=TRUE)[,1]
      numKnots <- 7
      u <- theta[1:numKnots]
      proj.steps = seq(1970.5,2022.5,by=0.1)
      tsEpidemicStart1 = 1985
      tsEpidemicStart <- proj.steps[which.min(abs(proj.steps - tsEpidemicStart1))]
      epi_steps <- proj.steps[proj.steps >= tsEpidemicStart]
      proj.dur <- diff(range(epi_steps))
      rvec.knots <- seq(min(epi_steps) - 3*proj.dur/(numKnots-3), max(epi_steps) + 3*proj.dur/(numKnots-3), proj.dur/(numKnots-3))
      rvec.spldes <- rbind(matrix(0, length(proj.steps) - length(epi_steps), numKnots),
                           splines::splineDesign(rvec.knots, epi_steps))
      beta <- numeric(numKnots)
      beta[1] <- u[1]
      beta[2] <- u[1]+u[2]
      for(i in 3:numKnots){
        beta[i] <- -beta[i-2] + 2*beta[i-1] + u[i]
      } 
      
      eppasm = rvec.spldes %*% beta
      eppasm <- cbind(eppasm, seq(1970.5,2022.5, by = 0.1))
      eppasm <- as.data.table(eppasm)
      colnames(eppasm) <- c('value', 'ts')
      eppasm[,value := as.numeric(value)]
      eppasm[,ts := as.numeric(ts)]
      new_eppasm <- c()
      for (year in seq(1970,2022)) {
        new_eppasm[year - 1969] <- mean(as.numeric(eppasm[grep(year, eppasm[,ts]),value]))
      }
      eppasm <- cbind('mean' = new_eppasm, 'year' = seq(1970,2022), model = r_vers)
      eppasm <- as.data.table(eppasm)
      
      
    }
    eppasm_runs <- rbind(eppasm_runs, eppasm)
  }
  
  dt <- rbind(epp, eppasm_runs)
  dt <- as.data.table(dt)
  dt[,model := as.factor(model)]
  
  if(!dir.exists(out.dir)){
    dir.create(out.dir, recursive = T)
  }
  pdf(paste0(out.dir, loc, '.pdf'), height = 8, width = 11)
  gg <- ggplot(dt[year > 1985], aes(y = as.numeric(mean),x = as.numeric(year), col = model)) + geom_line() + 
    ggtitle(paste0('Rvec, ', loc)) + theme_bw()
  print(gg)
  graphics.off()
  

}

run_table <- fread('/ihme/hiv/epp_input/gbd20/eppasm_run_table.csv')
out.dir <- '/ihme/homes/mwalte10/hiv_gbd2020/vetting/india/'
ind_locs <- loc.table[epp == 1,ihme_loc_id]
ind_locs <- ind_locs[grep('IND', ind_locs)]
lapply(ind_locs, plot_rvec_epp_v_eppasm, epp_run = '200316_windchime_ind', eppasm_vec = c('2020_ind_test_agg2',  '2020_ind_test_agg3', '2020_ind_test_agg4'))
setwd(out.dir)
system(paste0("/usr/bin/ghostscript -dBATCH -dSAFER -DNOPAUSE -q -sDEVICE=pdfwrite -sOutputFile=", "plots.pdf -f $(ls | sort -n | xargs)"))

