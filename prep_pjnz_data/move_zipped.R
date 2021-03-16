spec_prepped <- fread('/ihme/hiv/spectrum_prepped/art_draws/200713_yuka/NGA_25322_ART_data.csv')
summary <- fread('/ihme/hiv/epp_output/gbd20/200713_yuka/summary_files/NGA_25322.csv')
upload <- get_draws(gbd_id_type = 'cause_id', gbd_id = '298', location_id = 25322, gbd_round_id = 7, decomp_step = 'iterative', source=  'como')
upload <- melt(upload, id.vars = c('location_id', 'measure_id', 'sex_id', 'year_id', 'metric_id', 'age_group_id', 'cause_id'))


spec_prepped[ age_group_id == 10 & sex_id == 1 & year_id == 2010, new_hiv]
upload[ age_group_id == 10 & sex_id == 1 & year_id == 2010 & measure_id == 6, value]


key <- loc.table[level == 3,.(location_name, ihme_loc_id)]
add <- data.table(location_name = c('Moz', 'Bolivia', 'burkina_faso', 'cameroon', 'CAR','Comor', 'CotedI', 'DR', 'DRC', 'Guine_B', 'Guinee', 'haiti', 'Iran', 'Kyrgyz', 'Lao',
                                    'Mauritan', 'Mold', 'NewZ', 'Sierra', 'Syria', 'tanzan', 'Timor', 'Venezu', 'ZW', 'Phil', 'PNG'), 
                  ihme_loc_id = c('MOZ', 'BOL', 'BFA', 'CMR', 'CAR', 'COM', 'CIV', 'DOM', 'DRC', 'GNB', 'GIN', 'HTI', 'IRN', 'KGZ', 'LAO', 'MRT', 'MDV', 'NZL', 'SLE', 'SYR', 'TNZ',
                                  'TLS', 'VEN', 'ZWE', 'PHL', 'PNG'))
key <- rbind(key ,add)
pjnz_2020 <- list.files('/snfs1/DATA/UNAIDS_ESTIMATES/2020/2020_ALL_COUNTRIES/')

return <- list()
for(file in pjnz_2020){
  for(loc in key$location_name){
    if(grepl(loc, file)){
      x = key[location_name == loc, ihme_loc_id]
      break
    }else{
      x = NA
      next
    }
  }
  x <- cbind(file[1], x)
  return <- rbind(return, x)
}
return <- data.table(return)
return[,row := c(1:nrow(return))]
return[,from := as.character(paste0('/snfs1/DATA/UNAIDS_ESTIMATES/2020/2020_ALL_COUNTRIES/', V1)), by = 'row']
return[,to := as.character(paste0('/snfs1/DATA/UNAIDS_ESTIMATES/2020/', x, '/')), by = 'row']
for(row in 1:nrow(return)){
  file.copy(from = return[row,from], to = return[row, to])
}
