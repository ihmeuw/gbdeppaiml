## ---------------------------
## Script name: crosswalk.R
## Purpose of script: Attempt to crosswalk ancrt to ancss
##
## Author: Maggie Walters
## Date Created: 2020-10-21
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

# CROSSWALK DATA ----------------------------------------------------------
## load IHME crosswalk package
library(crosswE0E61085|alk, lib.loc = "/ihme/code/mscm/R/packages/")
loc.table <- get_locations(hiv_metadata = T)
loc.list <- loc.table[epp == 1, ihme_loc_id]
loc.list <- loc.list[!grepl('IND', loc.list)]
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
# 
# 
# df[type == 'ancss',ancss_prev := prev, by = c('year', 'site')]
# df[type == 'ancss',ancss_n := n, by = c('year', 'site')]
# df[type == 'ancrt',ancrt_prev := prev, by = c('year', 'site')]
# df[type == 'ancrt',ancrt_n := n, by = c('year', 'site')]
# 
# ancss <- unique(df[,.(year, site, ancss_prev, ancss_n)])
# ancrt <- unique(df[,.(year, site, ancrt_prev, ancrt_n)])
# df <- merge(ancss, ancrt)
setnames(df, 'prev', 'val')
## offset values of 0 and 1, impute standard error where missing 
##df is all data including the gold standard and df2 is the gold standard
df[val==0,val:=5e-3]
df[,standard_error:=sqrt(val*(1-val)/n)]

## transform val and standard_error into logit space for adjustment later on
df[, c("logit_val", "logit_standard_error")] <- as.data.table(crosswalk::delta_transform(mean=df$val,sd=df$standard_error,transformation='linear_to_logit'))

## set-up
# for married women we do not crosswalk (nor preform counterfactual extractions) for fecund_desire_no_preg_ppa
# because these surveys essentially provide no information on need for married women, however, for unmarried 
# women they still may provide valuable information on sexual activity 
id.vars <- c("site", "year")
topics <- c("ancrt")

## prevent confusing colname overlap from merging on counterfactuals
all_coeffs <- data.table()
small_se_nids <- list()
for (topic in topics) {
  topic_coeffs <- data.table()
  ## load counterfactual data
  df2 <- df[type == topic]
  
  ## format counterfactual extractions 
  df2 <- df2[,c(id.vars,"val","standard_error"),with=F]
  setnames(df2,c("val","standard_error"),c("prev_alt","prev_se_alt"))
  df2[,dorm_alt:=topic]
  ## subset data to gold standard extractions which have corresponding counterfactual extractions
  #training <- training[missing_fecund==0&missing_desire==0&missing_desire_later==0&missing_no_pregppa==0]
  gold <- df[match(paste0(df2$site, df2$year), 
                   paste0(df2$site, df2$year))]
  ## format gold standard extractions
  gold <- gold[type != topic]
  gold <- gold[,c(id.vars,"val","standard_error"),with=F]
  setnames(gold,c("val","standard_error"),c("prev_ref","prev_se_ref"))
  gold[,dorm_ref:="gold_standard"]
  ## combine gold standard and counterfactual extractions
  df_matched <- merge(gold,df2,by=id.vars, allow.cartesian = TRUE)
  ## prepare data for crosswalking
  dat_diff <- as.data.frame(cbind(
    crosswalk::delta_transform(
      mean = df_matched$prev_alt, 
      sd = df_matched$prev_se_alt,
      transformation = "linear_to_logit" ),
    crosswalk::delta_transform(
      mean = df_matched$prev_ref, 
      sd = df_matched$prev_se_ref,
      transformation = "linear_to_logit")
  ))
  names(dat_diff) <- c("mean_alt", "mean_se_alt", "mean_ref", "mean_se_ref")
  ## get table of matched reference and gold standard data pairs 
  df_matched[, c("logit_diff", "logit_diff_se")] <- calculate_diff(
    df = dat_diff, 
    alt_mean = "mean_alt", alt_sd = "mean_se_alt",
    ref_mean = "mean_ref", ref_sd = "mean_se_ref" )
  ## if logit_diff==0 is that indicative of a counterfactual mistake? or there were just no women in the topic of interest 
  df_matched <- df_matched[logit_diff!=0]
  ## remove any entries with abnormally small standard errors
  ## need to return to ubcov and fix weighting scheme, take note of nid's 
  ## crosswalk time
  ## do separately for each age group
  for (ag in unique(df_matched$age_group)) {
    # subset to all matched pairs for a particular age group
   # df_xwalk <- df_matched[age_group==ag]
    df_xwalk <- df_matched
    
    df_xwalk$id2 <- as.integer(as.factor(df_xwalk$site)) ## housekeeping
    # format data for meta-regression; pass in data.frame and variable names
    dat1 <- CWData(
      df = df_xwalk,
      obs = "logit_diff",       # matched differences in logit space
      obs_se = "logit_diff_se", # SE of matched differences in logit space
      alt_dorms = "dorm_alt",   # var for the alternative def/method
      ref_dorms = "dorm_ref",   # var for the reference def/method
      covs = list(),       # list of (potential) covariate columns
      study_id = "id2", # var for random intercepts; i.e. (1|study_id)
      add_intercept = T
    )
    # create crosswalk object called fit1
    fit1 <- CWModel(
      cwdata = dat1,           # result of CWData() function call
      obs_type = "diff_logit", # must be "diff_logit" or "diff_log"
      cov_models = list( # specify covariate details
        CovModel("intercept")),
      gold_dorm = "gold_standard"  # level of 'ref_dorms' that's the gold standard
    )
    # pull coefficients and other variables from crosswalk object, save to all_coeffs
    df_result <- data.table(fit1$create_result_df())
    df_result$age_group <- ag
    df_result <- df_result[dorms==topic,c("age_group","dorms","cov_names","beta","beta_sd","gamma")]
    df_result[is.na(gamma),gamma:=0]
    topic_coeffs <- rbind(topic_coeffs,df_result)
    all_coeffs <- rbind(all_coeffs,df_result)
  }
  df <- merge(df,topic_coeffs,by="age_group",all.x=T)
  # adjust val and standard error in logit space
  df[,(topic):=logit_val-beta]
  df[,(paste0(topic,'_se')):=sqrt(logit_standard_error^2+beta_sd^2+sqrt(gamma)^2)]  
  df <- df[,-c("dorms","cov_names","beta","beta_sd","gamma")]
  # transform the adjusted val and standard error back into linear space 
  df[, c((topic),(paste0(topic,'_se')))] <- as.data.table(crosswalk::delta_transform(mean=df[[(topic)]],
                                                                                     sd=df[[(paste0(topic,'_se'))]],
                                                                                     transformation='logit_to_linear'))
}
## adjust non-gold standard data 
df[,raw_val := val]
df[,raw_se := standard_error]
df[missing_fecund == 0 & missing_desire == 1 & missing_desire_later == 0 & missing_no_preg == 0 & missing_no_ppa == 0,c('val','standard_error') := list(desire,desire_se)]
df[missing_fecund == 0 & missing_desire == 0 & missing_desire_later == 1 & missing_no_preg == 0 & missing_no_ppa == 0,c('val','standard_error') := list(desire_later,desire_later_se)]
df[missing_fecund == 0 & missing_desire == 0 & missing_desire_later == 0 & missing_no_preg == 0 & missing_no_ppa == 1,c('val','standard_error') := list(no_ppa,no_ppa_se)]
df[missing_fecund == 0 & missing_desire == 0 & missing_desire_later == 0 & missing_no_preg == 1 & missing_no_ppa == 1,c('val','standard_error') := list(no_preg_ppa,no_preg_ppa_se)]
df[missing_fecund == 1 & missing_desire == 0 & missing_desire_later == 0 & missing_no_preg == 0 & missing_no_ppa == 1,c('val','standard_error') := list(fecund_no_ppa,fecund_no_ppa_se)]
df[missing_fecund == 1 & missing_desire == 0 & missing_desire_later == 0 & missing_no_preg == 1 & missing_no_ppa == 1,c('val','standard_error') := list(fecund_no_preg_ppa,fecund_no_preg_ppa_se)]
df[missing_fecund == 1 & missing_desire == 1 & missing_desire_later == 0 & missing_no_preg == 0 & missing_no_ppa == 1,c('val','standard_error') := list(fecund_desire_no_ppa,fecund_desire_no_ppa_se)]
#df[missing_fecund == 1 & missing_desire == 1 & missing_desire_later == 0 & missing_no_preg == 1 & missing_no_ppa == 1,c('val','standard_error') := list(fecund_desire_no_preg_ppa,fecund_desire_no_preg_ppa_se)]
df[missing_fecund == 1 & missing_desire == 0 & missing_desire_later == 1 & missing_no_preg == 0 & missing_no_ppa == 1,c('val','standard_error') := list(fecund_desire_later_no_ppa,fecund_desire_later_no_ppa_se)]
df[missing_fecund == 1 & missing_desire == 0 & missing_desire_later == 1 & missing_no_preg == 1 & missing_no_ppa == 1,c('val','standard_error') := list(fecund_desire_later_no_preg_ppa,fecund_desire_later_no_preg_ppa_se)]
df[missing_fecund == 0 & missing_desire == 1 & missing_desire_later == 0 & missing_no_preg == 0 & missing_no_ppa == 1,c('val','standard_error') := list(desire_no_ppa,desire_no_ppa_se)]
df[missing_fecund == 0 & missing_desire == 1 & missing_desire_later == 0 & missing_no_preg == 1 & missing_no_ppa == 1,c('val','standard_error') := list(desire_no_preg_ppa,desire_no_preg_ppa_se)]
df[missing_fecund == 0 & missing_desire == 0 & missing_desire_later == 1 & missing_no_preg == 0 & missing_no_ppa == 1,c('val','standard_error') := list(desire_later_no_ppa,desire_later_no_ppa_se)]
df[missing_fecund == 0 & missing_desire == 0 & missing_desire_later == 1 & missing_no_preg == 1 & missing_no_ppa == 1,c('val','standard_error') := list(desire_later_no_preg_ppa,desire_later_no_preg_ppa_se)]
## remove aggregate age microdata
df <- df[!is.na(age_group_id) | cv_report == 1]