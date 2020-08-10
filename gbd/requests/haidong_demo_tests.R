################################################################################
## Purpose: Modify population and migration before inputting in EPP-ASM to run test for Haidong's grant
## Date created: August 7, 2020
## Date modified:
## Author: Deepa Jahagirdar
## Run instructions: 
## Notes: Likely just a 1-off
################################################################################

#' @import data.table
#' @param loc string, ihme_loc_id (example: MWI)
#' @param gbd_year string, GBD year (example: "gbd20")
#' @param run.name string, EPP-ASM run name (example: "190630_rhino2)


base_run = "200807_demo_tests"
run.name = "200807_demo_tests1"
ken_locs = loc.table[grepl("KEN",ihme_loc_id) & most_detailed]

if(run.name == "200807_demo_tests1"){
  ##LSO
  loc = "LSO"
  pop1 = fread(paste0("/share/hiv/epp_input/gbd20/",base_run ,"/population_single_age/",loc,".csv"))
  pop2 = fread(paste0("/share/hiv/epp_input/gbd20/",base_run ,"/population/",loc,".csv"))
  pop1[,population := population * 1.15]
  pop2[,population := population * 1.15]
  fwrite(pop1,paste0("/share/hiv/epp_input/gbd20/",run.name,"/population_single_age/",loc,".csv"))
  fwrite(pop2,paste0("/share/hiv/epp_input/gbd20/",run.name,"/population/",loc,".csv"))
  
  ##KEN
  for(loc in ken_locs$ihme_loc_id){
    print(loc)
    mig = fread(paste0("/share/hiv/epp_input/gbd20/",base_run,"/migration/",loc,".csv"))
    mig[,value := value/2] 
    fwrite(mig,paste0("/share/hiv/epp_input/gbd20/",base_run,"/migration/",loc,".csv"),row.names = FALSE)
  
  }
  
}

run.name = "200807_demo_tests2"
if(run.name == "200807_demo_tests2"){
  ##Sub in SWZ pop structure for LSO
  
  loc1 = "SWZ"; loc2 = "LSO"
  
  pop1a = fread(paste0("/share/hiv/epp_input/gbd20/",base_run ,"/population_single_age/",loc1,".csv"))
  pop1b = fread(paste0("/share/hiv/epp_input/gbd20/",base_run ,"/population_single_age/",loc2,".csv"))
  pop2a = fread(paste0("/share/hiv/epp_input/gbd20/",base_run ,"/population/",loc1,".csv"))
  pop2b = fread(paste0("/share/hiv/epp_input/gbd20/",base_run ,"/population/",loc2,".csv"))
  
  pop1a[,sum_pop := sum(population)]
  pop1b[,sum_pop := sum(population)]
  pop2a[,sum_pop := sum(population)]
  pop2b[,sum_pop := sum(population)]
  
  pop1a[,prop := population/sum_pop]
  pop2a[,prop := population/sum_pop]
  
  pop1b = merge(pop1b,pop1a[,.(age_group_id,year_id,sex_id,prop)],all.x=TRUE)
  pop2b = merge(pop2b,pop2a[,.(age_group_id,year_id,sex_id,prop)],all.x=TRUE)
  
  pop1b[,population1 := sum_pop * prop]
  pop2b[,population1 := sum_pop * prop]
  
  sum(pop2b$population1) == sum(sum(pop2b$population))

  
  ##Compare
  ggplot(pop2b,aes(linetype = factor(sex_id))) + 
    geom_line(aes(year_id,population)) + 
    geom_line(aes(year_id,population1),col="red") +
  facet_wrap(~age_group_id, scales = "free_y")
  
  
  pop1b[,population := population1]
  pop2b[,population := population1]
  pop1b[,population1 := NULL]
  pop2b[,population1 := NULL]
  
  fwrite(pop1b,paste0("/share/hiv/epp_input/gbd20/",run.name,"/population_single_age/",loc2,".csv"))
  fwrite(pop2b,paste0("/share/hiv/epp_input/gbd20/",run.name,"/population/",loc2,".csv"))
  
} 









