locs <- unlist(strsplit(list.files('/ihme/hiv/data/PJNZ_EPPASM_prepped_subpop/'), split = '.rds'))
locs <- locs[-which(locs == 'lbd_anc')]

gbd_files <- list()
gbd_amounts <- c()
gbd_years <- list()
gbd_sites <- list()
years <- seq(1980, 2020)
count_me <- function(obj){
  counts <- c()
  for(i in 1:length(years)){
    counts[i] <- length(which(obj == years[i]))
    
  }
  return(counts)
}
for(i in 1:length(locs)){
  x <- readRDS(paste0('/ihme/hiv/data/PJNZ_EPPASM_prepped_subpop/', locs[i], '.rds'))
  gbd_files[[i]] <- subset(attr(x, 'eppd')$ancsitedat, type = 'ancss')
  gbd_amounts[i] <- nrow(attr(x, 'eppd')$ancsitedat)
  x <- as.data.table(attr(x, 'eppd')$ancsitedat) 
  gbd_years[[i]] <- count_me(x)
  gbd_sites[[i]] <- length(unique(x$site))
}
gbd_years_condensed <- as.data.table(do.call(rbind, gbd_years))
gbd_sites_condensed <- as.data.table(do.call(rbind, gbd_sites)) 

sites <- unique(gbd_sites_condensed$site)
gbd.year.count <- colSums(gbd_years_condensed)


zaf_files <- list.files('/ihme/hiv/data/PJNZ_EPPASM_prepped/')[which(grepl('ZAF', list.files('/ihme/hiv/data/PJNZ_EPPASM_prepped/')) == TRUE)]
gbd_amounts.zaf <- c()
gbd_years.zaf <- list()
gbd_sites.zaf <- c()
for (file in zaf_files) {
  x <- readRDS(paste0('/ihme/hiv/data/PJNZ_EPPASM_prepped/', file))
  gbd_amounts.zaf[file] <- nrow(attr(x, 'eppd')$ancsitedat)
  x <- attr(x, 'eppd')$ancsitedat
  gbd_years.zaf[[file]] <- count_me(x)
  gbd_sites.zaf[[file]] <- length(unique(x$site))
}
gbd_years_condensed.zaf <- as.data.table(do.call(rbind, gbd_years.zaf))
gbd.year.count.zaf <- colSums(gbd_years_condensed.zaf)


gbd_amounts <- c(gbd_sites_condensed, gbd_amounts.zaf)
years <- sort(unique(gbd_years_condensed.zaf$year))
names(gbd.year.count.zaf) <- years
gbd.year.count <- c(gbd.year.count + gbd.year.count.zaf)
names(gbd.year.count) <- years



perc_diff <- (lbd_sites - gbd_sites) / ((lbd_sites + gbd_sites) /2)
perc_diff.dt <- cbind(perc_diff * 100, locs)
perc_diff.dt <- perc_diff.dt %>% as.data.table()
colnames(perc_diff.dt) <- c('pdiff', 'loc')
ggplot(data = perc_diff.dt, aes(x = year ,y = as.numeric(pdiff))) + geom_bar(stat = 'identity') + ggtitle('Percent Difference Between GBD and LBD Data by Year') +
  xlab('Year') + ylab('Percent Difference')+ 
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        title = element_text(size = 20),
        axis.text.x = element_text(angle = 45, size = 14))


gbd_sites <- c()
lbd_sites <- c()
locs <- unlist(strsplit(list.files('/ihme/hiv/data/PJNZ_EPPASM_prepped_subpop/'), split = '.rds'))
locs <- locs[-which(locs == 'lbd_anc')]
locs <- locs[!grepl('_', locs)]
locs <- c(locs, 'NGA',  'KEN', 'ETH')
locs <- locs[-which(locs == 'DOM')]
locs <- locs[-which(locs == 'HTI')]
locs <- locs[-which(locs == 'PNG')]


for(loc in locs){
  number <- grepl(loc, list.files('/share/hiv/data/PJNZ_EPPASM_prepped_subpop/'))
  if(length(which(number == TRUE)) > 1){
    files <- list.files('/share/hiv/data/PJNZ_EPPASM_prepped_subpop/')[which(number == TRUE)]
    temp.dt <- list()
    for(file_sub in files){
      x <- readRDS(paste0('/share/hiv/data/PJNZ_EPPASM_prepped_subpop/',file_sub))
      x <- attr(x, 'eppd')$ancsitedat
      temp.dt <- rbind(temp.dt, x)
    }
    gbd_sites[loc] <- length(unique(temp.dt$site))
  }else{
    x <- readRDS(paste0('/share/hiv/data/PJNZ_EPPASM_prepped_subpop/', loc, '.rds'))
    x <- attr(x, 'eppd')$ancsitedat
    gbd_sites[loc] <- length(unique(x$site))
  }
}

for(loc in locs){
  number <- grepl(loc, list.files('/share/hiv/data/PJNZ_EPPASM_prepped_subpop/lbd_anc/'))
  if(length(which(number == TRUE)) > 1){
    files <- list.files('/share/hiv/data/PJNZ_EPPASM_prepped_subpop/lbd_anc/')[which(number == TRUE)]
    temp.dt <- list()
    for(file_sub in files){
      x <- readRDS(paste0('/share/hiv/data/PJNZ_EPPASM_prepped_subpop/lbd_anc/',file_sub))
      temp.dt <- rbind(temp.dt, x)
    }
    lbd_sites[loc] <- length(unique(temp.dt$site))
  }else{
    x <- readRDS(paste0('/share/hiv/data/PJNZ_EPPASM_prepped_subpop/lbd_anc/', loc, '.rds'))
    lbd_sites[loc] <- length(unique(x$site))
  }
}

zaf_gbd_sites <- 0
for(zaf.site in zaf){
  x <- readRDS(paste0('/share/hiv/data/PJNZ_EPPASM_prepped/', zaf.site, '.rds'))
  i <- attr(x, 'eppd')$ancsitedat
  zaf_gbd_sites <-  length(unique(i$site)) + zaf_gbd_sites
}

zaf_lbd_sites <- 0
for(zaf.site in zaf){
  if(!file.exists(paste0('/share/hiv/data/PJNZ_EPPASM_prepped_subpop/lbd_anc/', zaf.site, '.rds'))){next}
  x <- readRDS(paste0('/share/hiv/data/PJNZ_EPPASM_prepped_subpop/lbd_anc/', zaf.site, '.rds'))
  zaf_lbd_sites <-  length(unique(x$site)) + zaf_lbd_sites
}

lbd_sites <- c(lbd_sites, zaf_lbd_sites)
gbd_sites <- c(gbd_sites, zaf_gbd_sites)
names(lbd_sites)[length(lbd_sites)] <-  'ZAF'
names(gbd_sites)[length(gbd_sites)] <-  'ZAF'


perc_diff <- (lbd_sites - gbd_sites) / ((lbd_sites + gbd_sites) /2)
perc_diff.dt <- cbind(perc_diff * 100, names(gbd_sites))
perc_diff.dt <- as.data.table(perc_diff.dt )
colnames(perc_diff.dt) <- c('pdiff', 'loc')
ggplot(data = perc_diff.dt, aes(x = loc ,y = as.numeric(pdiff))) + geom_bar(stat = 'identity') + ggtitle('Percent Difference Between GBD and LBD Data by Location') +
  xlab('Location') + ylab('Percent Difference')+ 
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        title = element_text(size = 20),
        axis.text.x = element_text(angle = 45, size = 14))


loc <- 'GIN'
gbd_old <- readRDS(paste0('/share/hiv/data/PJNZ_EPPASM_prepped_subpop/', loc, '.rds')) 
gbd_old <- attr(gbd_old, 'eppd')$ancsitedat
gbd_old <- subset(gbd_old, type = 'ancss')
gbd_new <- as.data.frame(readRDS(paste0('/share/hiv/data/PJNZ_EPPASM_prepped_subpop/lbd_anc/offset/', loc, '.rds')))
plot(x = gbd_old$year, y = gbd_old$prev, pch = 16, col = alpha('blue', 0.4), xlab = 'Year', ylab = 'Prevalence', 
     main = 'Guinea Prevalence by Year', cex = 2, cex.axis = 1.5, cex.main = 2, cex.lab = 1.5)
points(x = gbd_new$year, y = gbd_new$prev, pch = 16, col = alpha('orange', 0.4), cex = 2)
legend('topleft', col = c('blue', 'orange'), legend = c('GBD Data', 'LBD Data'), pch = rep(16, 2), bty = 'n', cex = 1.5)

plot(x = gbd_old$year, y = gbd_old$n, pch = 16, col = alpha('blue', 0.4), xlab = 'Year', ylab = 'Sample Size', 
     main = 'Guinea Sample Sizes by Year', cex = 2, cex.axis = 1.5, cex.main = 2, cex.lab = 1.5)
points(x = gbd_new$year, y = gbd_new$n, pch = 16, col = alpha('orange', 0.4), cex = 2)
legend('topleft', col = c('blue', 'orange'), legend = c('GBD Data', 'LBD Data'), pch = rep(16, 2), bty = 'n', cex = 1.5)





locs <- unlist(strsplit(list.files('/ihme/hiv/data/PJNZ_EPPASM_prepped_subpop/'), split = '.rds'))
locs <- locs[-which(locs == 'lbd_anc')]

######plot one data prep
{years <- seq(1980, 2020)
  count_me <- function(obj){
    counts <- c()
    for(i in 1:length(years)){
      counts[i] <- length(which(obj == years[i]))
      
    }
    return(counts)
  }
  
  gbd_amounts <- c()
  gbd_years <- list()
  
  for(i in 1:length(locs)){
    x <- readRDS(paste0('/ihme/hiv/data/PJNZ_EPPASM_prepped_subpop/', locs[i], '.rds'))
    gbd_amounts[i] <- nrow(attr(x, 'eppd')$ancsitedat)
    x <- as.data.table(attr(x, 'eppd')$ancsitedat) 
    gbd_years[[i]] <- count_me(x)
  }
  names(gbd_amounts) <- locs
  subnats <- c(sum(gbd_amounts[grep('KEN', names(gbd_amounts))]),
               sum(gbd_amounts[grep('NGA', names(gbd_amounts))]),
               sum(gbd_amounts[grep('ETH', names(gbd_amounts))]))
  remove <- grep('_', names(gbd_amounts))
  locs <- names(gbd_amounts)[-remove]
  gbd_amounts <- c(gbd_amounts[-remove], subnats)
  locs <- c(locs, 'KEN', 'NGA', 'ETH')
  names(gbd_amounts) <- locs
  
  anc_dat <- as.data.table(readRDS('/home/j/WORK/11_geospatial/10_mbg/hiv/unaids_anc/anc_data_2019_10_22.rds')) 
  lbd_amounts_og <- c()
  for(i in 1:length(gbd_amounts)){
    lbd_amounts_og[i] <- length(which(anc_dat$country == names(gbd_amounts)[i]))
  }}

######plot one graph code
{first_data <- data.table(cbind(gbd_amounts, lbd_amounts_og, names(gbd_amounts)))
  colnames(first_data) <- c('GBD', 'LBD', 'Country')
  first_data[which(as.integer(first_data$LBD )/ as.integer(first_data$GBD) > 1.2),change := 'Gained Site Years']
  first_data[which(as.integer(first_data$LBD )/ as.integer(first_data$GBD) < 0.8),change := 'Lost Site Years']
  first_data[which(is.na(change)), change := 'No Significant Change']
  
  diag_plot <- ggplot(data = first_data, aes(x = as.integer(GBD), y = as.integer(LBD))) +
    geom_text(aes(label = Country, color = factor(change))) + xlim(1,1000) + ylim(1,1000) +   labs(color = 'Site Year Change') +
    geom_abline(slope = 1, intercept = 0, aes(color = 'red')) +
    ylab('LBD Site Years') + ggtitle('Site Year Discrepancies, Pre-Processing') +
    scale_color_manual(values = c('blue', 'orange', 'black')) + xlab('GBD Site Years') + ylab('LBD Site Years') + ggtitle('Site Year Discrepancies') +
    labs(fill ="Site Year Changes") + 
    theme(axis.text = element_text(size = 14),
          axis.title = element_text(size = 16),
          title = element_text(size = 20),
          legend.text = element_text(size = 14))
  print(diag_plot) }

######plot two data prep
{
  locs <- unlist(strsplit(list.files('/ihme/hiv/data/PJNZ_EPPASM_prepped_subpop/'), split = '.rds'))
  locs <- locs[-which(locs == 'lbd_anc')]
  years <- seq(1980, 2020)
  count_me <- function(obj){
    counts <- c()
    for(i in 1:length(years)){
      counts[i] <- length(which(obj == years[i]))
      
    }
    return(counts)
  }
  
  gbd_amounts <- c()
  gbd_years <- list()
  
  zaf <- list.files('/ihme/hiv/data/PJNZ_EPPASM_prepped_subpop/lbd_anc/')[grep( 'ZAF_', list.files('/ihme/hiv/data/PJNZ_EPPASM_prepped_subpop/lbd_anc/'))]
  zaf <- unlist(strsplit(zaf, '.rds'))
  zaf_amounts <- c()
  for(i in 1:length(zaf)){
    x <- readRDS(paste0('/ihme/hiv/data/PJNZ_EPPASM_prepped_subpop/lbd_anc/', zaf[i], '.rds'))
    zaf_amounts[i] <-   nrow(x)
  }
  names(zaf_amounts) <- zaf
  for(i in 1:length(locs)){
    if(!file.exists(paste0('/ihme/hiv/data/PJNZ_EPPASM_prepped_subpop/lbd_anc/', locs[i], '.rds'))){
      gbd_amounts[i] <- 0
      next
    }
    x <- readRDS(paste0('/ihme/hiv/data/PJNZ_EPPASM_prepped_subpop/lbd_anc/', locs[i], '.rds'))
    gbd_amounts[i] <- nrow(x)
    gbd_years[[i]] <- count_me(x)
  }
  names(gbd_amounts) <- locs
  gbd_amounts_new <- c(gbd_amounts, zaf_amounts)
  
  gbd_amounts <- c()
  gbd_years <- list()
  
  for(i in 1:length(names(gbd_amounts_new))){
    loc <- names(gbd_amounts_new)[i]
    if(grepl('ZAF', loc)){
      x <- readRDS(paste0('/ihme/hiv/data/PJNZ_EPPASM_prepped/', loc, '.rds'))
      
    }else{
      x <- readRDS(paste0('/ihme/hiv/data/PJNZ_EPPASM_prepped_subpop/', loc, '.rds'))
      
    }
    gbd_amounts[i] <- nrow(attr(x, 'eppd')$ancsitedat)
    x <- as.data.table(attr(x, 'eppd')$ancsitedat) 
    gbd_years[[i]] <- count_me(x)
  }
  names(gbd_amounts) <- names(gbd_amounts_new)
  
}

######plot two graph code
{
  first_data <- data.table(cbind(gbd_amounts, gbd_amounts_new, names(gbd_amounts)))
  colnames(first_data) <- c('GBD', 'LBD', 'Country')
  first_data[which(as.integer(first_data$LBD )/ as.integer(first_data$GBD) > 1.2),change := 'Gained Site Years']
  first_data[which(as.integer(first_data$LBD )/ as.integer(first_data$GBD) < 0.8),change := 'Lost Site Years']
  first_data[which(is.na(change)), change := 'No Significant Change']
  first_data <- first_data[-which(grepl('ZAF', first_data$Country) == TRUE),]
  
  diag_plot <- ggplot(data = first_data, aes(x = as.integer(GBD), y = as.integer(LBD))) +
    geom_text(aes(label = Country, color = factor(change))) + xlim(1,1000) + ylim(1,1000) +   labs(color = 'Site Year Change') +
    geom_abline(slope = 1, intercept = 0, aes(color = 'red')) +
    ylab('LBD Site Years') + ggtitle('Site Year Discrepancies, Pre-Processing') +
    scale_color_manual(values = c('blue', 'orange', 'black')) + xlab('GBD Site Years') + ylab('LBD Site Years') + ggtitle('Site Year Discrepancies') +
    labs(fill ="Site Year Changes") + 
    theme(axis.text = element_text(size = 14),
          axis.title = element_text(size = 16),
          title = element_text(size = 20),
          legend.text = element_text(size = 14))
  print(diag_plot) 
}



