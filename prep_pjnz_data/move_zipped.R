key <- loc.table[level == 3,.(location_name, ihme_loc_id)]
add <- data.table(location_name = c('Nigeria', 'Moz', 'Bolivia', 'burkina_faso', 'cameroon', 'CAR','Comor', 'CotedI', 'DRC', 'Guine_B', 'Guinee', 'haiti', 'Iran', 'Kyrgyz', 'Lao',
                                    'Mauritan', 'Mold', 'NewZ', 'Sierra', 'Syria', 'tanzan', 'Timor', 'Venezu', 'ZW', 'Phil', 'PNG', 'angola', 'burndi', 'DomRep',
                                    'rwanda', 'Tanzania', 'zambia', 'lesotho', 'eswatini', 'Cabo_Verde', 'chad', 'cotedivoire', 'gambia', 'ghana', 'guinee',
                                    'guine_bissau', 'liberia', 'senegal', 'sierra', 'togo', 'Nigeria', 'PHILIPPINES', 'Moldova', 'JORDAN', 'SYRIA', 'Kenya'), 
                  ihme_loc_id = c('NGA', 'MOZ', 'BOL', 'BFA', 'CMR', 'CAR', 'COM', 'CIV', 'DOM',  'GNB', 'GIN', 'HTI', 'IRN', 'KGZ', 'LAO', 'MRT', 'MDV', 'NZL', 'SLE', 'SYR', 'TNZ',
                                  'TLS', 'VEN', 'ZWE', 'PHL', 'PNG', 'AGO', 'BDI', 'DOM', 'RWA', 'TZA', 'ZMB', 'LSO', 'SWZ', 'CPV', 'TCD', 'CIV', 'GMB', 'GHA',
                                  'GIN', 'GNB', 'LBR', 'SEN', 'SLE', 'TGO', 'NGA', 'PHL', 'MDA', 'JOR', 'SYR', 'KEN'))

key <- rbind(key ,add)
key <- key[ihme_loc_id != 'NER',]
pjnz_2021 <- list.files('/snfs1/DATA/UNAIDS_ESTIMATES/2021/2021_ALL_COUNTRIES/')

return <- list()
for(file in pjnz_2021){
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
return[,from := as.character(paste0('/snfs1/DATA/UNAIDS_ESTIMATES/2021/2021_ALL_COUNTRIES/', V1)), by = 'row']
return[,to := as.character(paste0('/snfs1/DATA/UNAIDS_ESTIMATES/2021/', x, '/')), by = 'row']
for(row in 1:nrow(return)){
  file.copy(from = return[row,from], to = return[row, to])
}
