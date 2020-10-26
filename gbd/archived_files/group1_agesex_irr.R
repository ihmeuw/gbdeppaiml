### Setup
rm(list=ls())
windows <- Sys.info()[1][["sysname"]]=="Windows"
root <- ifelse(windows,"J:/","/home/j/")
user <- ifelse(windows, Sys.getenv("USERNAME"), Sys.getenv("USER"))
code.dir <- paste0(ifelse(windows, "H:", paste0("/homes/", user)), "/gbdeppaiml/")

## Packages
library(data.table); library(ggplot2); library(lme4)

loc.table <- fread(paste0('/share/hiv/epp_input/gbd19/190503_all/location_table.csv'))

## 15-24:25+ RR
one.dt <- unique(fread('/homes/tahvif/age_sex_prev_natl_1524.csv'))
one.dt[age_year == 15, age := 'young']
one.dt[age_year == 25, age := 'old']
one.dt <- one.dt[,.(iso3, year, prev, sex_id, age)]
one.dt <- dcast.data.table(one.dt, iso3 + year + sex_id ~ age, value.var = 'prev')
one.dt[, rr := young/old]
setnames(one.dt, 'iso3', 'ihme_loc_id')
one.dt <- merge(one.dt, loc.table[,.(ihme_loc_id, region_name)], by = 'ihme_loc_id')
one.dt = one.dt[!is.na(sex_id)]
one.dt[, sex := ifelse(sex_id == 1, 'male', 'female')]
fit.f <- lm(rr ~ year, data= one.dt[sex == 'female'])
pred.f <- predict(fit.f, expand.grid(year = 2000:2019, sex = c('female')), allow.new.levels = T)
out.f <- expand.grid(year = 2000:2019, sex = c('female'))
out.f$pred <- pred.f

fit.m <- lm(rr ~ year, data= one.dt[sex == 'male'])
pred.m <- predict(fit.m, expand.grid(year = 2000:2019, sex = c('male')), allow.new.levels = T)
out.m <- expand.grid(year = 2000:2019, sex = c('male'))
out.m$pred <- pred.m

pred.dt <- data.table(rbind(out.f, out.m))
pdf('/homes/tahvif/eppasm_age_irr.pdf', width = 10, height = 6)

gg <- ggplot() + 
  geom_point(data = one.dt, aes(x = year, y = rr, color = region_name)) + 
  geom_line(data = pred.dt, aes(x = year, y = pred)) +
  facet_wrap(~sex) + 
  ggtitle('15-24:25+ Ratio from Prevalence Surveys') +
  theme_bw()
print(gg)


## 15-24:25-34, 35+:25-34 RR
two.dt <- unique(fread('/homes/tahvif/age_sex_prev_natl_152535.csv'))
two.dt[age_year == 15, age := 'young']
two.dt[age_year == 25, age := 'ref']
two.dt[age_year == 35, age := 'old']
two.dt <- two.dt[,.(iso3, year, prev, sex_id, age)]
two.dt <- dcast.data.table(two.dt, iso3 + year + sex_id ~ age, value.var = 'prev')
two.dt[, rr_young := young/ref]
two.dt[, rr_old := old/ref]
setnames(two.dt, 'iso3', 'ihme_loc_id')
two.dt <- merge(two.dt, loc.table[,.(ihme_loc_id, region_name)], by = 'ihme_loc_id')
two.dt = two.dt[!is.na(sex_id)]
two.dt[, sex := ifelse(sex_id == 1, 'male', 'female')]
fit.f.young <- lm(rr_young ~ year, data= two.dt[sex == 'female'])
pred.f.young <- predict(fit.f.young, expand.grid(year = 2000:2019, sex = 'female', age = '15-24'), allow.new.levels = T)
out.f.young <- expand.grid(year = 2000:2019, sex = c('female'), age = '15-24')
out.f.young$pred <- pred.f.young

fit.f.old <- lm(rr_old ~ year, data= two.dt[sex == 'female'])
pred.f.old <- predict(fit.f.old, expand.grid(year = 2000:2019, sex = 'female', age = '35+'), allow.new.levels = T)
out.f.old <- expand.grid(year = 2000:2019, sex = c('female'), age = '35+')
out.f.old$pred <- pred.f.old

fit.m.young <- lm(rr_young ~ year, data= two.dt[sex == 'male'])
pred.m.young <- predict(fit.m.young, expand.grid(year = 2000:2019, sex = 'male', age = '15-24'), allow.new.levels = T)
out.m.young <- expand.grid(year = 2000:2019, sex = c('male'), age = '15-24')
out.m.young$pred <- pred.m.young

fit.m.old <- lm(rr_old ~ year, data= two.dt[sex == 'male'])
pred.m.old <- predict(fit.m.old, expand.grid(year = 2000:2019, sex = 'male', age = '35+'), allow.new.levels = T)
out.m.old <- expand.grid(year = 2000:2019, sex = c('male'), age = '35+')
out.m.old$pred <- pred.m.old

pred.dt <- data.table(rbindlist(list(out.f.young, out.f.old, out.m.young, out.m.old)))
plot.dt <- two.dt[,.(ihme_loc_id, year, rr_old, rr_young, sex, region_name)]
plot.dt <- melt(plot.dt, id.vars = c('ihme_loc_id', 'year', 'sex', 'region_name'), value.name = 'rr')
plot.dt[, age := ifelse(variable == 'rr_young', '15-24', '35+')]
gg <- ggplot() + 
  geom_point(data = plot.dt, aes(x = year, y = rr, color = region_name)) + 
  geom_line(data = pred.dt, aes(x = year, y = pred)) +
  facet_wrap(~sex + age) + 
  ggtitle('15-24 and 35+ Ratio (reference = 25-34) from Prevalence Surveys') +
  theme_bw()
print(gg)


## sex ratio
three.dt <- fread('/homes/tahvif/age_sex_prev_natl_1549.csv')
three.dt[,sex := ifelse(sex_id == 1, 'male', 'female')]
three.dt <- three.dt[!is.na(sex)]
three.dt <- three.dt[,.(iso3, year, prev, sex)]
three.dt <- dcast.data.table(three.dt, iso3 + year ~ sex, value.var = 'prev')
three.dt[, rr := female/male]
setnames(three.dt, 'iso3', 'ihme_loc_id')
three.dt <- merge(three.dt, loc.table[,.(ihme_loc_id, region_name)], by = 'ihme_loc_id')

fit <- lm(rr ~ year, data= three.dt)
pred <- predict(fit, expand.grid(year = 1980:2019), allow.new.levels = T)
out.dt <- expand.grid(year = 1980:2019)
out.dt$pred <- pred

gg <- ggplot() + 
  geom_point(data = three.dt, aes(x = year, y = rr, color = region_name)) + 
  geom_line(data = out.dt, aes(x = year, y = pred)) +
  ggtitle('Female:male ratio from Prevalence Surveys') +
  theme_bw()
print(gg)

dev.off()