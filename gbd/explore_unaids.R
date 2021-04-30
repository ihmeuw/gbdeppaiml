dt <- fread('/ihme/homes/mwalte10/unaids_monthly_data.csv')
dt = dt[TIME_PERIOD != '-',]
dt[,TIME_PERIOD := as.character(TIME_PERIOD)]
dt[,month := lapply(strsplit(TIME_PERIOD,'-'), function(x) x[2])]
dt[,month := as.character(month)]
dt[,month := as.numeric(month)]
legend <- fread('/ihme/homes/mwalte10/legend.csv')
legend <- legend[meaning != '']
legend <- legend[1:7,]
keep_cols <- c('COUNTRY_CODE', 'month', legend$code)
dt <- dt[,keep_cols, with = FALSE]
dt <- melt(dt, id.vars = c('COUNTRY_CODE', 'month'))
dt <- dt[!is.na(value)]

has_pre_pan <- dt[month < 3,.(COUNTRY_CODE,variable)]
has_pre_pan[,pre_pan := 1]
dt <- merge(dt, unique(has_pre_pan), by = c('COUNTRY_CODE', 'variable'), all.x = T)
dt[,pre_pan := ifelse(is.na(pre_pan), 0, 1)]
dt[,number_months := length(unique(month)), by = c('COUNTRY_CODE', 'variable')]
dt <- dcast(dt, month + COUNTRY_CODE + pre_pan + number_months ~ variable, value.var = 'value') 
setnames(dt, c('COUNTRY_CODE', 'NUM_ART-ALL', 'NUM_ART-AGE_LT_15_IF', 'NUM_ART-MALE_GTE_15','NUM_ART-FEMALE_GTE_15', 'NUM_INIT-ALL', 'NUM_VL_SUPPRESSED-ALL', 'NUM_VL_TESTED-ALL'),
         c('ihme_loc_id', 'on_art', 'paed_on_art', 'on_art_male', 'on_art_female', 'init_art', 'v_sup', 'v_test'))
dt <- melt(dt, id.vars = c('month', 'ihme_loc_id', 'pre_pan', 'number_months'))
dt[variable == 'on_art_male', sex_id := 1]
dt[variable == 'on_art_female', sex_id := 2]
dt[is.na(sex_id), sex_id := 3]
dt[variable == 'on_art_female', variable := 'on_art']
dt[variable == 'on_art_male', variable := 'on_art']
dt <- dcast(dt, month + ihme_loc_id + sex_id +pre_pan + number_months ~ variable, value.var = 'value') 

dt_melt <- melt(dt, id.vars = c('month', 'ihme_loc_id', 'pre_pan', 'number_months', 'sex_id'))
dt_melt <- dt_melt[!is.na(value),]
dt_melt <- merge(dt_melt, loc.table[,.(ihme_loc_id, super_region_name)])
dt_melt[,value_super_region := sum(value), by = c('month', 'sex_id', 'variable')]


ggplot(unique(dt_melt[sex_id == 3 & super_region_name == 'Sub-Saharan Africa' & pre_pan == 1,]), aes(month, value, col = as.factor(ihme_loc_id))) + 
  geom_line() + facet_wrap(~variable, scales = 'free') + ggtitle('Sub-Saharan Africa') + xlim(c(1,12))
ggplot(unique(dt_melt[sex_id == 3 & super_region_name == 'North Africa and Middle East' & pre_pan == 1,]), aes(month, value, col = as.factor(ihme_loc_id))) + 
  geom_line() + facet_wrap(~variable, scales = 'free') + ggtitle('North Africa and Middle East') + xlim(c(1,12))
ggplot(unique(dt_melt[sex_id == 3 & super_region_name == "Central Europe, Eastern Europe, and Central Asia" & pre_pan == 1,]), aes(month, value, col = as.factor(ihme_loc_id))) + 
  geom_line() + facet_wrap(~variable, scales = 'free') + ggtitle("Central Europe, Eastern Europe, and Central Asia") + xlim(c(1,12))
ggplot(unique(dt_melt[sex_id == 3 & super_region_name == "Latin America and Caribbean" & pre_pan == 1,]), aes(month, value, col = as.factor(ihme_loc_id))) + 
  geom_line() + facet_wrap(~variable, scales = 'free') + ggtitle("Latin America and Caribbean") + xlim(c(1,12))
ggplot(unique(dt_melt[sex_id == 3 & super_region_name ==  "South Asia" & pre_pan == 1,]), aes(month, value, col = as.factor(ihme_loc_id))) + 
  geom_line() + facet_wrap(~variable, scales = 'free') + ggtitle("South Asia") + xlim(c(1,12))
ggplot(unique(dt_melt[sex_id == 3 & super_region_name ==  "Southeast Asia, East Asia, and Oceania"    & pre_pan == 1,]), aes(month, value, col = as.factor(ihme_loc_id))) + 
  geom_line() + facet_wrap(~variable, scales = 'free') + ggtitle("Southeast Asia, East Asia, and Oceania"    ) + xlim(c(1,12))



dt_by_month <- dcast(dt_melt[sex_id == 3,.(ihme_loc_id, month, variable, value, super_region_name)], ihme_loc_id + variable + super_region_name ~ month, value.var = 'value')
dt_by_month_ssa_init <- dt_by_month[variable == 'init_art' & super_region_name == 'Sub-Saharan Africa']
dt_by_month_ssa_init <- dt_by_month_ssa_init[!is.na(`1`) | !is.na(`2`) | !is.na(`3`),]
dt_by_month_ssa_init <- melt(dt_by_month_ssa_init, id.vars = c('ihme_loc_id', 'variable', 'super_region_name'))

ggplot(dt_by_month_ssa_init, aes(as.numeric(variable.1), value)) + geom_line() + geom_point() + facet_wrap(~ihme_loc_id, scales = 'free') + ggtitle('Initiated ART')


dt_by_month <- dcast(dt_melt[sex_id == 3,.(ihme_loc_id, month, variable, value, super_region_name)], ihme_loc_id + variable + super_region_name ~ month, value.var = 'value')
dt_by_month_ssa_init <- dt_by_month[variable == 'init_art' ]
dt_by_month_ssa_init <- dt_by_month_ssa_init[!is.na(`1`) | !is.na(`2`) | !is.na(`3`),]
dt_by_month_ssa_init <- melt(dt_by_month_ssa_init, id.vars = c('ihme_loc_id', 'variable', 'super_region_name'))
ggplot(dt_by_month_ssa_init, aes(as.numeric(variable.1), value)) + geom_line() + geom_point() + facet_wrap(~ihme_loc_id, scales = 'free') + ggtitle('Initiated ART')


dt_by_month_ssa_init <- dt_by_month[variable == 'paed_on_art' ]
dt_by_month_ssa_init <- dt_by_month_ssa_init[!is.na(`1`) | !is.na(`2`) | !is.na(`3`),]
dt_by_month_ssa_init <- melt(dt_by_month_ssa_init, id.vars = c('ihme_loc_id', 'variable', 'super_region_name'))
ggplot(dt_by_month_ssa_init, aes(as.numeric(variable.1), value)) + geom_line() + geom_point() + facet_wrap(~ihme_loc_id, scales = 'free') + ggtitle('Pediatric On ART')



