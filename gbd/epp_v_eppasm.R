epp_v_eppasm <- function(loc = 'IND_4842', epp_name = '191224_trumpet', eppasm_name = '2020_ind_test_agg2', gbd_new = T){
  epp_inc <- paste0(root,"/WORK/04_epi/01_database/02_data/hiv/04_models/gbd2015/02_inputs/incidence_draws/",epp_name,"/", loc, '_SPU_inc_draws.csv')
  epp_prev <- paste0(root,"/WORK/04_epi/01_database/02_data/hiv/04_models/gbd2015/02_inputs/prevalence_draws/",epp_name,"/", loc, '_SPU_prev_draws.csv')
  epp_inc <- fread(epp_inc)
  epp_prev <- fread(epp_prev)
  ##Average incidence and prevalences across draws
  epp_inc[,inc := rowSums(epp_inc[,2:101]) / 100 / 100]
  epp_prev[,prev := rowSums(epp_prev[,2:101]) / 100 / 100]
  epp_inc <- epp_inc[,.(year, inc)]
  epp_prev <- epp_prev[,.(year, prev)]
  epp_inc[,metric := 'inc']
  epp_prev[,metric := 'prev']
  
  if(gbd_new){
    epp.new_inc <- paste0(root,"/WORK/04_epi/01_database/02_data/hiv/04_models/gbd2015/02_inputs/incidence_draws/",'200316_windchime_ind',"/", loc, '_SPU_inc_draws.csv')
    epp.new_prev <- paste0(root,"/WORK/04_epi/01_database/02_data/hiv/04_models/gbd2015/02_inputs/prevalence_draws/",'200316_windchime_ind',"/", loc, '_SPU_prev_draws.csv')
    epp.new_inc <- fread(epp.new_inc)
    epp.new_prev <- fread(epp.new_prev)
    ##Average incidence and prevalences across draws
    epp.new_inc[,inc := rowSums(epp.new_inc[,2:101]) / 100 / 100]
    epp.new_prev[,prev := rowSums(epp.new_prev[,2:101]) / 100 / 100]
    epp.new_inc <- epp.new_inc[,.(year, inc)]
    epp.new_prev <- epp.new_prev[,.(year, prev)]
    epp.new_inc[,metric := 'inc']
    epp.new_prev[,metric := 'prev']
  }
  
  
  eppasm_fp <- paste0('/ihme/hiv/epp_output/gbd20/', eppasm_name, '/compiled/', loc, '.csv')
  eppasm <- fread(eppasm_fp)
  eppasm_inc <- eppasm[,.(sex, age, year, pop, new_hiv, run_num)]
  ##only keep 15-49
  eppasm_inc <- eppasm_inc[age %in% seq(15,49),]
  ##sum by sex and age for new infections and population
  eppasm_inc[,new_hiv := sum(new_hiv), by = c('year', 'run_num')]
  eppasm_inc[,pop := sum(pop), by = c('year', 'run_num')]
  eppasm_inc[,age := NULL]
  ##average across runs
  eppasm_inc[,new_hiv := mean(new_hiv), by = c('year')]
  eppasm_inc[,pop := mean(pop), by = c('year')]
  ##convert to rate per one to match epp
  eppasm_inc[,new_hiv_rate := new_hiv / pop]
  eppasm_inc <- eppasm_inc[,.(year, new_hiv, new_hiv_rate)]
  eppasm_inc <- unique(eppasm_inc)
  eppasm_inc <- eppasm_inc[,.(year, new_hiv_rate)]
  eppasm_inc[,metric := 'inc']
  setnames(eppasm_inc, 'new_hiv_rate', 'inc')
  
  eppasm_prev <- eppasm[,.(sex, age, year, pop, run_num, pop_gt350, pop_200to350, pop_lt200)]
  ##sum across all infection stages
  eppasm_prev[,prev := pop_gt350 + pop_200to350 + pop_lt200]
  ##only keep 15-49 year olds
  eppasm_prev <- eppasm_prev[age %in% seq(15,49),]
  eppasm_prev <- eppasm_prev[,.(sex, year, pop, run_num, prev)]
  ##sum across age and sexes
  eppasm_prev[, prev := sum(prev), by = c('year', 'run_num')]
  eppasm_prev[, pop := sum(pop), by = c('year', 'run_num')]
  ##average across runs
  eppasm_prev[, prev := mean(prev), by = c('year')]
  eppasm_prev[, pop := mean(pop), by = c('year')]
  eppasm_prev <- eppasm_prev[,.(year, prev, pop)]
  ##convert to rate per one to match epp
  eppasm_prev[,prev_rate := prev / pop]
  eppasm_prev <- unique(eppasm_prev)
  eppasm_prev <- eppasm_prev[,.(year, prev_rate)]
  eppasm_prev[,metric := 'prev']
  setnames(eppasm_prev, 'prev_rate', 'prev')
  
  ######plot epp vs eppasm
  
  ##put into data table format for ggplot
  epp_inc[,model := 'epp']
  epp_prev[,model := 'epp']
  epp.new_inc[,model := 'gbd20,epp']
  epp.new_prev[,model := 'gbd20,epp']
  eppasm_inc[,model := 'eppasm']
  eppasm_prev[,model := 'eppasm']
  
  setnames(epp_inc, 'inc', 'value')
  setnames(eppasm_inc, 'inc', 'value')
  setnames(epp.new_inc, 'inc', 'value')
  setnames(epp.new_prev, 'prev', 'value')
  setnames(epp_prev, 'prev', 'value')
  setnames(eppasm_prev, 'prev', 'value')
  dt <- rbind(epp_inc, epp_prev, eppasm_inc, eppasm_prev, epp.new_inc, epp.new_prev)
  
  lines <- c('solid', 'dashed')
  names(lines) <- c('prev', 'inc')
  color_x <- c('red', 'blue', 'green')
  names(color_x) <- c('epp', 'eppasm', 'gbd20,epp')
  
  ##create eppasm_vs_epp plot if necessary in the vetting folder
  if(!dir.exists(paste0('/ihme/hiv/epp_output/gbd20/', eppasm_name, '/vetting/eppasm_vs_epp/'))){
    dir.create(paste0('/ihme/hiv/epp_output/gbd20/', eppasm_name, '/vetting/eppasm_vs_epp/'), recursive = T)
  }
  pdf(paste0('/ihme/hiv/epp_output/gbd20/', eppasm_name, '/vetting/eppasm_vs_epp/', loc, '.pdf'), width = 11, height = 8)
  gg <- ggplot(data = dt, aes(x = year, y = value, col = model, linetype = metric)) +
    geom_line() + ggtitle(paste0(loc, ' EPPASM vs. EPP'))  
  print(gg)
  graphics.off()
}

library(mortdb, lib = "/share/mortality/shared/r/")
loc.table <- get_locations(hiv_metadata = TRUE)
ind_locs <- loc.table[grepl('IND',ihme_loc_id) & epp == 1, ihme_loc_id]
lapply(ind_locs, epp_v_eppasm, epp_name = '190630_rhino_ind', eppasm_name = '2020_ind_test_agg3')
setwd('/ihme/hiv/epp_output/gbd20/2020_ind_test_agg3/vetting/eppasm_vs_epp/')
system(paste0("/usr/bin/ghostscript -dBATCH -dSAFER -DNOPAUSE -q -sDEVICE=pdfwrite -sOutputFile=compiled_plots.pdf -f *"))

















