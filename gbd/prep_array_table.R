################################################
#Prepare array job for eppasm
################################################

###load in all ihme functions
invisible(sapply(list.files("/share/cc_resources/libraries/current/r/", full.names = T), source))


gbd_round = 7
decomp_step = 'iterative'
draws = 1000
jobs = 200
rep_each = draws / jobs

loc.table <- get_locations(hiv_metadata = T)

epp.locs <- c(loc.table[epp == 1,ihme_loc_id], 'MRT', 'COM', 'STP')

draw_table <- matrix(data = c(1:draws), ncol = rep_each, byrow = T)
array_table <- lapply(epp.locs, function(x) dt <- data.table(copy(draw_table))[,loc := x])
array_table <- rbindlist(array_table)

colnames(array_table) <- c(paste0('draw_', c(1:rep_each)), 'loc')

write.csv(array_table, file = '/ihme/homes/mwalte10/eppasm_run_tab.csv', row.names = F)
