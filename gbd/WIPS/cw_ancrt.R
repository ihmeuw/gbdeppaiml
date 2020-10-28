## ---------------------------
## Script name: 
## Purpose of script:
##
## Author: Maggie Walters
## Date Created: 2018-04-11
## Email: mwalte10@uw.edu
## ---------------------------
##
## Notes:
##   
##
## ---------------------------

## Used in basically every script
Sys.umask(mode = "0002")
windows <- Sys.info()[1][["sysname"]]=="Windows"
root <- ifelse(windows,"J:/","/home/j/")
user <- ifelse(windows, Sys.getenv("USERNAME"), Sys.getenv("USER"))

source(paste0(ifelse(windows, "H:", paste0("/ihme/homes/", user)), "/gbdeppaiml/gbd/00_req_packages.R"))

h_root = '/homes/mwalte10/'
lib.loc <- paste0(h_root,"R/",R.Version(),"/",R.Version(),".",R.Version())
dir.create(lib.loc,recursive=T, showWarnings = F)
.libPaths(c(lib.loc,.libPaths()))
packages <- c("reticulate")
for(p in packages){
  if(p %in% rownames(installed.packages())==FALSE){
    install.packages(p)
  }
  library(p, character.only = T)
}

library(crosswalk, lib.loc = "/ihme/code/mscm/R/packages/")
loc.table <- get_locations(hiv_metadata = T)
loc.list <- loc.table[epp == 1, ihme_loc_id]
loc.list <- loc.list[!grepl('IND', loc.list)]

# Attempt #1 ---------------------------------------
#### using year and site to crosswalk ancrt to ancss
####
####
df <- c()
for(loc in loc.list){
  dt <- readRDS(paste0('/ihme/hiv/epp_output/gbd20/191224_trumpet//dt_objects/', loc, '_dt.RDS'))
  anc <- attr(dt, 'eppd')$ancsitedat
  anc <- as.data.table(anc)
  if(ncol(anc) == 10){
    anc[,offset := NA]
  }
  if(any(anc[,type] == 'ancrt')){
    df <- rbind(anc, df, fill = T)
  }else{
    next
  }
  print(loc)
}
df <- df[,.(year, site, prev, type, n)]
df_orig <- copy(df)


anc_data <- df
anc_data[type == 'ancss',ancss_prev := prev, by = c('year', 'site')]
anc_data[type == 'ancss',ancss_n := n, by = c('year', 'site')]
anc_data[type == 'ancrt',ancrt_prev := prev, by = c('year', 'site')]
anc_data[type == 'ancrt',ancrt_n := n, by = c('year', 'site')]
ancss <- unique(anc_data[,.(year, site, ancss_prev, ancss_n)])
ancrt <- unique(anc_data[,.(year, site, ancrt_prev, ancrt_n)])
# setnames(ancrt, 'year', 'year_alt')
# setnames(ancss, 'year', 'year_ref')
# anc_data <- merge(ancss, ancrt, by = 'site', allow.cartesian = T, all.y = T)

anc_data <- merge(ancss, ancrt)
anc_data_matched <- anc_data[!is.na(ancss_prev) & !is.na(ancrt_prev)]
matched_prev <- anc_data_matched
matched_prev[,refvar := 'ancss']
matched_prev[,altvar := 'ancrt']
setnames(matched_prev, 'ancss_prev', 'prev_ref')
setnames(matched_prev, 'ancrt_prev', 'prev_alt')
# matched_prev[,year := (year_alt + year_ref) / 2, by = 'site']
df_matched <- matched_prev[,.(site, prev_ref, prev_alt, refvar, altvar, year, ancrt_n, ancss_n)]
df_matched[prev_ref == 0, prev_ref := 0.005]
df_matched[prev_alt == 0, prev_alt := 0.005]
df_matched[,prev_se_alt := ((prev_alt * (1 - prev_alt)) / ancrt_n)^0.5]
df_matched[,prev_se_ref := ((prev_ref * (1 - prev_ref)) / ancss_n)^0.5]
df_matched <- df_matched[,.(site, prev_ref, prev_alt, prev_se_alt, prev_se_ref, refvar, altvar, year)]
df_matched[,refvar := as.factor(refvar)]
df_matched[,altvar := as.factor(altvar)]




dat_diff <- as.data.frame(cbind(
  delta_transform(
    mean = df_matched$prev_alt,
    sd = df_matched$prev_se_alt,
    transformation = "linear_to_logit" ),
  delta_transform(
    mean = df_matched$prev_ref,
    sd = df_matched$prev_se_ref,
    transformation = "linear_to_logit")
))
names(dat_diff) <- c("mean_alt", "mean_se_alt", "mean_ref", "mean_se_ref")
df_matched[, c("logit_diff", "logit_diff_se")] <- calculate_diff(
  df = dat_diff,
  alt_mean = "mean_alt", alt_sd = "mean_se_alt",
  ref_mean = "mean_ref", ref_sd = "mean_se_ref" )
df_matched[,year := as.integer(year)]
df_matched$id2 <- as.integer(as.factor(df_matched$site)) # ...housekeeping

df_matched[,dorm_ref := 'ancss']
df_matched[,dorm_alt := 'ancrt']
df_matched[,year := as.factor(year)]

dat1 <- CWData(
  df = df_matched,
  obs = "logit_diff",       # matched differences in logit space
  obs_se = "logit_diff_se", # SE of matched differences in logit space
  alt_dorms = "dorm_alt",   # var for the alternative def/method
  ref_dorms = "dorm_ref",   # var for the reference def/method
  covs = list("year", 'site'),       # list of (potential) covariate columns
  study_id = 'id2'        # var for random intercepts; i.e. (1|study_id)
)

fit1 <- CWModel(
  cwdata = dat1,           # result of CWData() function call
  obs_type = "diff_logit", # must be "diff_logit" or "diff_log"
  cov_models = list( # specify covariate details
    CovModel("intercept"), CovModel("year"), CovModel("site")),
  gold_dorm = "ancss"  # level of 'ref_dorms' that's the gold standard
)

df_orig[prev == 0, prev := 0.005]
df_orig[,prev_se := ((prev * (1 - prev)) / n)^0.5]
df_orig[,type := as.factor(type)]
df_orig[, c("prev2", "prev2_se", "diff", "diff_se")] <- adjust_orig_vals(
  fit_object = fit1,       # result of CWModel()
  df = df_orig,            # original data with obs to be adjusted
  orig_dorms = "type", # name of column with (all) def/method levels
  orig_vals_mean = "prev",  # original mean
  orig_vals_se = "prev_se"  # standard error of original mean
)



# Attempt #2 ---------------------------------------
###### matching on ihme_loc_id and year
######
######
df <- c()
for(loc in loc.list){
  dt <- readRDS(paste0('/ihme/hiv/epp_output/gbd20/191224_trumpet//dt_objects/', loc, '_dt.RDS'))
  anc <- attr(dt, 'eppd')$ancsitedat
  anc <- as.data.table(anc)
  if(ncol(anc) == 10){
    anc[,offset := NA]
  }
  anc[,ihme_loc_id := loc]
  if(any(anc[,type] == 'ancrt')){
    df <- rbind(anc, df, fill = T)
  }else{
    next
  }
  print(loc)
}
df <- df[,.(year, site, prev, type, n, ihme_loc_id)]
df_orig <- copy(df)
anc_data <- df
anc_data[,prev_se := ((prev * (1 - prev)) / n)^0.5]



anc_data[type == 'ancss',prev_alt := prev, by = c('year', 'site', 'ihme_loc_id')]
anc_data[type == 'ancss',prev_se_alt := prev_se, by = c('year', 'site', 'ihme_loc_id')]
anc_data[type == 'ancrt',prev_ref := prev, by = c('year', 'site', 'ihme_loc_id')]
anc_data[type == 'ancrt',prev_se_ref := prev_se, by = c('year', 'site', 'ihme_loc_id')]

ancss <- unique(anc_data[type == 'ancss',.(year, site, prev_alt, prev_se_alt, ihme_loc_id)])
ancrt <- unique(anc_data[type == 'ancrt',.(year, site, prev_ref, prev_se_ref, ihme_loc_id)])
setnames(ancss, 'site', 'alt_site')
setnames(ancrt, 'site', 'ref_site')
anc_data_matched <- merge(ancss, ancrt, by = c('year', 'ihme_loc_id'), allow.cartesian = T)

matched_prev <- anc_data_matched
matched_prev[,altvar := 'ancss']
matched_prev[,refvar := 'ancrt']

df_matched <- matched_prev[,.(prev_ref, prev_alt, refvar, altvar, year, prev_se_ref, prev_se_alt, ihme_loc_id)]
df_matched[prev_ref == 0, prev_ref := 0.005]
df_matched[prev_alt == 0, prev_alt := 0.005]

df_matched[,refvar := as.factor(refvar)]
df_matched[,altvar := as.factor(altvar)]
df_matched[prev_se_alt == 0 ,prev_se_alt := 1e-4]
df_matched[prev_se_ref == 0 ,prev_se_ref := 1e-4]



dat_diff <- as.data.frame(cbind(
  delta_transform(
    mean = df_matched$prev_alt,
    sd = df_matched$prev_se_alt,
    transformation = "linear_to_logit" ),
  delta_transform(
    mean = df_matched$prev_ref,
    sd = df_matched$prev_se_ref,
    transformation = "linear_to_logit")
))
names(dat_diff) <- c("mean_alt", "mean_se_alt", "mean_ref", "mean_se_ref")

df_matched[, c("logit_diff", "logit_diff_se")] <- calculate_diff(
  df = dat_diff,
  alt_mean = "mean_alt", alt_sd = "mean_se_alt",
  ref_mean = "mean_ref", ref_sd = "mean_se_ref" )
df_matched[,year := as.integer(year)]
df_matched$id2 <- as.integer(as.factor(df_matched$ihme_loc_id)) # ...housekeeping

df_matched[,dorm_alt := 'ancss']
df_matched[,dorm_ref := 'ancrt']
df_matched[,year := as.factor(year)]
df_matched[,ihme_loc_id := as.factor(ihme_loc_id)]


dat1 <- CWData(
  df = df_matched,
  obs = "logit_diff",       # matched differences in logit space
  obs_se = "logit_diff_se", # SE of matched differences in logit space
  alt_dorms = "dorm_alt",   # var for the alternative def/method
  ref_dorms = "dorm_ref",   # var for the reference def/method
  covs = list("year", "ihme_loc_id"),       # list of (potential) covariate columns
  study_id = 'id2'        # var for random intercepts; i.e. (1|study_id)
)

fit1 <- CWModel(
  cwdata = dat1,           # result of CWData() function call
  obs_type = "diff_logit", # must be "diff_logit" or "diff_log"
  cov_models = list( # specify covariate details
    CovModel("intercept"), CovModel("year"), CovModel("ihme_loc_id")),
  gold_dorm = "ancrt"  # level of 'ref_dorms' that's the gold standard
)

df_orig[prev == 0, prev := 0.005]
df_orig[,prev_se := ((prev * (1 - prev)) / n)^0.5]
df_orig[,type := as.factor(type)]
df_orig[,prev := as.numeric(prev)]
df_orig[,prev_se := as.numeric(prev)]
df_orig[,site:=NULL]
df_orig[,n := NULL]
df_orig[,year := as.factor(year)]
df_orig[,ihme_loc_id := as.factor(ihme_loc_id)]
df_orig[, c("prev2", "prev2_se", "diff", "diff_se")] <- adjust_orig_vals(
  fit_object = fit1,       # result of CWModel()
  df = df_orig,            # original data with obs to be adjusted
  orig_dorms = "type", # name of column with (all) def/method levels
  orig_vals_mean = "prev",  # original mean
  orig_vals_se = "prev_se"  # standard error of original mean
)


ggplot(df_orig, aes(prev, prev2, col = as.factor(type))) + geom_point() + ggtitle('XWalk ANCSS to ANCRT by ihme_loc_id and year')
plot(df_matched[,prev_ref], df_matched[,prev_alt], main = 'Covariance of obs by ihme_loc_id and year')

# Attempt #3 ---------------------------------------
###### matching on site and year
######
######
df <- c()
for(loc in loc.list){
  dt <- readRDS(paste0('/ihme/hiv/epp_output/gbd20/191224_trumpet//dt_objects/', loc, '_dt.RDS'))
  anc <- attr(dt, 'eppd')$ancsitedat
  anc <- as.data.table(anc)
  if(ncol(anc) == 10){
    anc[,offset := NA]
  }
  anc[,ihme_loc_id := loc]
  if(any(anc[,type] == 'ancrt')){
    df <- rbind(anc, df, fill = T)
  }else{
    next
  }
  print(loc)
}
df <- df[,.(year, site, prev, type, n)]
df_orig <- copy(df)
anc_data <- df
anc_data[,prev_se := ((prev * (1 - prev)) / n)^0.5]



anc_data[type == 'ancss',prev_alt := prev, by = c('year', 'site')]
anc_data[type == 'ancss',prev_se_alt := prev_se, by = c('year', 'site')]
anc_data[type == 'ancrt',prev_ref := prev, by = c('year', 'site')]
anc_data[type == 'ancrt',prev_se_ref := prev_se, by = c('year', 'site')]

ancss <- unique(anc_data[type == 'ancss',.(year, site, prev_alt, prev_se_alt)])
ancrt <- unique(anc_data[type == 'ancrt',.(year, site, prev_ref, prev_se_ref)])
# setnames(ancss, 'site', 'alt_site')
# setnames(ancrt, 'site', 'ref_site')
anc_data_matched <- merge(ancss, ancrt, by = c('year', 'site'), allow.cartesian = T)

matched_prev <- anc_data_matched
matched_prev[,altvar := 'ancss']
matched_prev[,refvar := 'ancrt']

df_matched <- matched_prev[,.(prev_ref, prev_alt, refvar, altvar, year, prev_se_ref, prev_se_alt, site)]
df_matched[prev_ref == 0, prev_ref := 0.005]
df_matched[prev_alt == 0, prev_alt := 0.005]

df_matched[,refvar := as.factor(refvar)]
df_matched[,altvar := as.factor(altvar)]
df_matched[prev_se_alt == 0 ,prev_se_alt := 1e-4]
df_matched[prev_se_ref == 0 ,prev_se_ref := 1e-4]



dat_diff <- as.data.frame(cbind(
  delta_transform(
    mean = df_matched$prev_alt,
    sd = df_matched$prev_se_alt,
    transformation = "linear_to_logit" ),
  delta_transform(
    mean = df_matched$prev_ref,
    sd = df_matched$prev_se_ref,
    transformation = "linear_to_logit")
))
names(dat_diff) <- c("mean_alt", "mean_se_alt", "mean_ref", "mean_se_ref")

df_matched[, c("logit_diff", "logit_diff_se")] <- calculate_diff(
  df = dat_diff,
  alt_mean = "mean_alt", alt_sd = "mean_se_alt",
  ref_mean = "mean_ref", ref_sd = "mean_se_ref" )
df_matched[,year := as.integer(year)]
df_matched$id2 <- as.integer(as.factor(df_matched$site)) # ...housekeeping

df_matched[,dorm_alt := 'ancss']
df_matched[,dorm_ref := 'ancrt']
df_matched[,year := as.factor(year)]
df_matched[,site := as.factor(site)]


dat1 <- CWData(
  df = df_matched,
  obs = "logit_diff",       # matched differences in logit space
  obs_se = "logit_diff_se", # SE of matched differences in logit space
  alt_dorms = "dorm_alt",   # var for the alternative def/method
  ref_dorms = "dorm_ref",   # var for the reference def/method
  covs = list("year", "site"),       # list of (potential) covariate columns
  study_id = 'id2'        # var for random intercepts; i.e. (1|study_id)
)

fit1 <- CWModel(
  cwdata = dat1,           # result of CWData() function call
  obs_type = "diff_logit", # must be "diff_logit" or "diff_log"
  cov_models = list( # specify covariate details
    CovModel("intercept"), CovModel("year"), CovModel("site")),
  gold_dorm = "ancrt"  # level of 'ref_dorms' that's the gold standard
)

df_orig[prev == 0, prev := 0.005]
df_orig[,prev_se := ((prev * (1 - prev)) / n)^0.5]
df_orig[,type := as.factor(type)]
df_orig[,prev := as.numeric(prev)]
df_orig[,prev_se := as.numeric(prev)]
df_orig[,n := NULL]
df_orig[,year := as.factor(year)]
df_orig[,site := as.factor(site)]
df_orig[, c("prev2", "prev2_se", "diff", "diff_se")] <- adjust_orig_vals(
  fit_object = fit1,       # result of CWModel()
  df = df_orig,            # original data with obs to be adjusted
  orig_dorms = "type", # name of column with (all) def/method levels
  orig_vals_mean = "prev",  # original mean
  orig_vals_se = "prev_se"  # standard error of original mean
)


ggplot(df_orig, aes(prev, prev2, col = as.factor(type))) + geom_point() + ggtitle('XWalk ANCSS to ANCRT by site and year')
plot(df_matched[,prev_ref], df_matched[,prev_alt], main = 'Covariance of obs by site and year')

# Attempt #4 ---------------------------------------
###### matching on site and year ancrt to ancss
######
######
df <- c()
for(loc in loc.list){
  dt <- readRDS(paste0('/ihme/hiv/epp_output/gbd20/191224_trumpet//dt_objects/', loc, '_dt.RDS'))
  anc <- attr(dt, 'eppd')$ancsitedat
  anc <- as.data.table(anc)
  if(ncol(anc) == 10){
    anc[,offset := NA]
  }
  anc[,ihme_loc_id := loc]
  if(any(anc[,type] == 'ancrt')){
    df <- rbind(anc, df, fill = T)
  }else{
    next
  }
  print(loc)
}
df <- df[,.(year, site, prev, type, n)]
df_orig <- copy(df)
anc_data <- df
anc_data[,prev_se := ((prev * (1 - prev)) / n)^0.5]



anc_data[type == 'ancrt',prev_alt := prev, by = c('year', 'site')]
anc_data[type == 'ancrt',prev_se_alt := prev_se, by = c('year', 'site')]
anc_data[type == 'ancss',prev_ref := prev, by = c('year', 'site')]
anc_data[type == 'ancss',prev_se_ref := prev_se, by = c('year', 'site')]

ancss <- unique(anc_data[type == 'ancss',.(year, site, prev_alt, prev_se_alt)])
ancrt <- unique(anc_data[type == 'ancrt',.(year, site, prev_ref, prev_se_ref)])
anc_data_matched <- merge(ancss, ancrt, by = c('year', 'site'), allow.cartesian = T)

matched_prev <- anc_data_matched
matched_prev[,refvar := 'ancss']
matched_prev[,altvar := 'ancrt']

df_matched <- matched_prev[,.(prev_ref, prev_alt, refvar, altvar, year, prev_se_ref, prev_se_alt, site)]
df_matched[prev_ref == 0, prev_ref := 0.005]
df_matched[prev_alt == 0, prev_alt := 0.005]

df_matched[,refvar := as.factor(refvar)]
df_matched[,altvar := as.factor(altvar)]
df_matched[prev_se_alt == 0 ,prev_se_alt := 1e-4]
df_matched[prev_se_ref == 0 ,prev_se_ref := 1e-4]



dat_diff <- as.data.frame(cbind(
  delta_transform(
    mean = df_matched$prev_alt,
    sd = df_matched$prev_se_alt,
    transformation = "linear_to_logit" ),
  delta_transform(
    mean = df_matched$prev_ref,
    sd = df_matched$prev_se_ref,
    transformation = "linear_to_logit")
))
names(dat_diff) <- c("mean_alt", "mean_se_alt", "mean_ref", "mean_se_ref")

df_matched[, c("logit_diff", "logit_diff_se")] <- calculate_diff(
  df = dat_diff,
  alt_mean = "mean_alt", alt_sd = "mean_se_alt",
  ref_mean = "mean_ref", ref_sd = "mean_se_ref" )
df_matched[,year := as.integer(year)]
df_matched$id2 <- as.integer(as.factor(df_matched$site)) # ...housekeeping

df_matched[,dorm_alt := 'ancrt']
df_matched[,dorm_ref := 'ancss']
df_matched[,year := as.factor(year)]
df_matched[,site := as.factor(site)]


dat1 <- CWData(
  df = df_matched,
  obs = "logit_diff",       # matched differences in logit space
  obs_se = "logit_diff_se", # SE of matched differences in logit space
  alt_dorms = "dorm_alt",   # var for the alternative def/method
  ref_dorms = "dorm_ref",   # var for the reference def/method
  covs = list("year", "site"),       # list of (potential) covariate columns
  study_id = 'id2'        # var for random intercepts; i.e. (1|study_id)
)

fit1 <- CWModel(
  cwdata = dat1,           # result of CWData() function call
  obs_type = "diff_logit", # must be "diff_logit" or "diff_log"
  cov_models = list( # specify covariate details
    CovModel("intercept"), CovModel("year"), CovModel("site")),
  gold_dorm = "ancss"  # level of 'ref_dorms' that's the gold standard
)

df_orig[prev == 0, prev := 0.005]
df_orig[,prev_se := ((prev * (1 - prev)) / n)^0.5]
df_orig[,type := as.factor(type)]
df_orig[,prev := as.numeric(prev)]
df_orig[,prev_se := as.numeric(prev)]
df_orig[,n := NULL]
df_orig[,year := as.factor(year)]
df_orig[,site := as.factor(site)]
df_orig[, c("prev2", "prev2_se", "diff", "diff_se")] <- adjust_orig_vals(
  fit_object = fit1,       # result of CWModel()
  df = df_orig,            # original data with obs to be adjusted
  orig_dorms = "type", # name of column with (all) def/method levels
  orig_vals_mean = "prev",  # original mean
  orig_vals_se = "prev_se"  # standard error of original mean
)


ggplot(df_orig, aes(prev, prev2, col = as.factor(type))) + geom_point() + ggtitle('XWalk ANCRT to ANCSS by site and year')
plot(df_matched[!is.na(prev_ref) & !is.na(prev_alt),prev_ref], df_matched$prev_alt, main = 'Covariance of obs by site and year')

# Attempt #5 ---------------------------------------
###### matching on ihme_loc_id and year ancrt to ancss
######
######
df <- c()
for(loc in loc.list){
  dt <- readRDS(paste0('/ihme/hiv/epp_output/gbd20/191224_trumpet//dt_objects/', loc, '_dt.RDS'))
  anc <- attr(dt, 'eppd')$ancsitedat
  anc <- as.data.table(anc)
  if(ncol(anc) == 10){
    anc[,offset := NA]
  }
  anc[,ihme_loc_id := loc]
  if(any(anc[,type] == 'ancrt')){
    df <- rbind(anc, df, fill = T)
  }else{
    next
  }
  print(loc)
}
df <- df[,.(year, site, prev, type, n, ihme_loc_id)]
df_orig <- copy(df)
anc_data <- df
anc_data[,prev_se := ((prev * (1 - prev)) / n)^0.5]



anc_data[type == 'ancrt',prev_alt := prev, by = c('year', 'site', 'ihme_loc_id')]
anc_data[type == 'ancrt',prev_se_alt := prev_se, by = c('year', 'site', 'ihme_loc_id')]
anc_data[type == 'ancss',prev_ref := prev, by = c('year', 'site', 'ihme_loc_id')]
anc_data[type == 'ancss',prev_se_ref := prev_se, by = c('year', 'site', 'ihme_loc_id')]

ancss <- unique(anc_data[type == 'ancss',.(year, site, prev_alt, prev_se_alt, ihme_loc_id)])
ancrt <- unique(anc_data[type == 'ancrt',.(year, site, prev_ref, prev_se_ref, ihme_loc_id)])
setnames(ancss, 'site', 'alt_site')
setnames(ancrt, 'site', 'ref_site')
anc_data_matched <- merge(ancss, ancrt, by = c('year', 'ihme_loc_id'), allow.cartesian = T)

matched_prev <- anc_data_matched
matched_prev[,altvar := 'ancrt']
matched_prev[,refvar := 'ancss']

df_matched <- matched_prev[,.(prev_ref, prev_alt, refvar, altvar, year, prev_se_ref, prev_se_alt, ihme_loc_id)]
df_matched[prev_ref == 0, prev_ref := 0.005]
df_matched[prev_alt == 0, prev_alt := 0.005]

df_matched[,refvar := as.factor(refvar)]
df_matched[,altvar := as.factor(altvar)]
df_matched[prev_se_alt == 0 ,prev_se_alt := 1e-4]
df_matched[prev_se_ref == 0 ,prev_se_ref := 1e-4]



dat_diff <- as.data.frame(cbind(
  delta_transform(
    mean = df_matched$prev_alt,
    sd = df_matched$prev_se_alt,
    transformation = "linear_to_logit" ),
  delta_transform(
    mean = df_matched$prev_ref,
    sd = df_matched$prev_se_ref,
    transformation = "linear_to_logit")
))
names(dat_diff) <- c("mean_alt", "mean_se_alt", "mean_ref", "mean_se_ref")

df_matched[, c("logit_diff", "logit_diff_se")] <- calculate_diff(
  df = dat_diff,
  alt_mean = "mean_alt", alt_sd = "mean_se_alt",
  ref_mean = "mean_ref", ref_sd = "mean_se_ref" )
df_matched[,year := as.integer(year)]
df_matched$id2 <- as.integer(as.factor(df_matched$ihme_loc_id)) # ...housekeeping

df_matched[,dorm_alt := 'ancrt']
df_matched[,dorm_ref := 'ancss']
df_matched[,year := as.factor(year)]
df_matched[,ihme_loc_id := as.factor(ihme_loc_id)]


dat1 <- CWData(
  df = df_matched,
  obs = "logit_diff",       # matched differences in logit space
  obs_se = "logit_diff_se", # SE of matched differences in logit space
  alt_dorms = "dorm_alt",   # var for the alternative def/method
  ref_dorms = "dorm_ref",   # var for the reference def/method
  covs = list("year", "ihme_loc_id"),       # list of (potential) covariate columns
  study_id = 'id2'        # var for random intercepts; i.e. (1|study_id)
)

fit1 <- CWModel(
  cwdata = dat1,           # result of CWData() function call
  obs_type = "diff_logit", # must be "diff_logit" or "diff_log"
  cov_models = list( # specify covariate details
    CovModel("intercept"), CovModel("year"), CovModel("ihme_loc_id")),
  gold_dorm = "ancss"  # level of 'ref_dorms' that's the gold standard
)

df_orig[prev == 0, prev := 0.005]
df_orig[,prev_se := ((prev * (1 - prev)) / n)^0.5]
df_orig[,type := as.factor(type)]
df_orig[,prev := as.numeric(prev)]
df_orig[,prev_se := as.numeric(prev)]
df_orig[,site:=NULL]
df_orig[,n := NULL]
df_orig[,year := as.factor(year)]
df_orig[,ihme_loc_id := as.factor(ihme_loc_id)]
df_orig[, c("prev2", "prev2_se", "diff", "diff_se")] <- adjust_orig_vals(
  fit_object = fit1,       # result of CWModel()
  df = df_orig,            # original data with obs to be adjusted
  orig_dorms = "type", # name of column with (all) def/method levels
  orig_vals_mean = "prev",  # original mean
  orig_vals_se = "prev_se"  # standard error of original mean
)


ggplot(df_orig, aes(prev, prev2, col = as.factor(type))) + geom_point() + ggtitle('XWalk ANCRT to ANCSS by ihme_loc_id and year')

