new_list = mclapply(loc.table[epp == 1 & !grepl("IND",ihme_loc_id) & most_detailed,ihme_loc_id],function(loc){
  print(loc)
  compiled = fread(paste0("/share/hiv/epp_output/gbd20/200713_yuka/compiled/",loc,".csv"))
  compiled = compiled[year == 2019 & age %in% 15:49]
  compiled[,plhhiv := pop_art + pop_gt350 + pop_200to350 + pop_lt200]
  compiled = compiled[,list(plhiv = sum(plhhiv)), by = .(year,run_num)]
  compiled = compiled[,list(plhiv = mean(plhiv)), by = .(year)]
  store1 = compiled
  store1[,file := "epp"]
  
  
  compiled = fread(paste0("/share/hiv/spectrum_prepped/aggregates/200713_yuka/",loc,".csv"))
  compiled = compiled[year_id == 2019 & age_group_id %in% c(8:14)]
  compiled = compiled[,list(plhiv = sum(pop_hiv)), by = .(year_id,run_num)]
  compiled = compiled[,list(plhiv = mean(plhiv)), by = .(year_id)]
  compiled[,file := "spec_agg"]
  setnames(compiled,"year_id","year")
  
  
  
  out = rbind(store1,compiled)
  out[,iso := loc]
  return(out)
  
},mc.cores = 5)

new_list = rbindlist(new_list)
new_list1 = dcast(new_list, year + iso ~ file, value.var = "plhiv")
new_list1[ ,diff := spec_agg/epp]
new_list1[order(diff)]

####
#reckoning needs to be rerun for group one 
#pull out the summary files to make sure that it matches the email
#prevalence discrepancy: see what happened by looking at what would be attributed to the populatoin 