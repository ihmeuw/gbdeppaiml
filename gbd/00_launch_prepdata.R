### Setup
rm(list=ls(all.names = T))

# windows <- Sys.info()[1][["sysname"]]=="Windows"
# root <- ifelse(windows,"J:/","/home/j/")
# user <- ifelse(windows, Sys.getenv("USERNAME"), Sys.getenv("USER"))
# code.dir <- paste0(ifelse(windows, "H:", paste0("/homes/", user)), "/gbdeppaiml/prep_pjnz_data/")
# date <- substr(gsub("-","",Sys.Date()),3,8)

# Roots -------------------------------------------------
root <- "/home/j/"
user <- Sys.getenv("USER")
code_root <- file.path("/mnt/share/homes", user, "gbdeppaiml")
code_dir <-file.path("/mnt/share/homes", user, "gbdeppaiml/prep_pjnz_data/")
# FIXME - setting to scratch output for now
.output_root <- file.path("/mnt/share/homes", user, "scratch/hiv/data/PJNZ_prepped")

# Reconnect -----------------------------------------------------
# Reconnect to an existing output folder?
RECONNECT <- T
.reconnect_version <- "2023_03_21.01" # output directory version to reconnect to

# Define output folder structure
# to assist with versioning
loadNamespace('ihme.covid', "/ihme/covid-19/.r-packages/current")
if(RECONNECT) {
  .output_path <- file.path(.output_root, .reconnect_version)
} else {
  .output_path <- ihme.covid::get_output_dir(.output_root, date = "today")
}

# Packages --------------------------------------
library(data.table)
library(glue)

# for metadata
library("SamsElves", lib.loc = "/mnt/share/code/hiv_tb_selectid/r_packages")
### Functions
library(mortdb, lib = "/mnt/team/mortality/pub/shared/r/4") # 2023 Mar 21 11:49:52 - I think this is the right one - unsure - who owns this?
# library(mortdb, lib = "/home/j/WORK/02_mortality/shared/r") # 2023 Mar 21 11:14:49 - wrong version/folder

# library(mortdb, lib = "/home/j/WORK/02_mortality/shared/r") # 2023 Mar 21 11:14:49 - another copy with wrong version/folder
# setwd(file.path("/ihme/homes", user, "eppasm"))
# devtools::load_all(path = file.path("/ihme/homes", user, "eppasm"))
library(epp, lib.loc = "/snfs1/Project/GBD_HIV/packages_r") # 2023 Mar 23 10:57:13 - replaced per Meixin
library(eppasm, lib.loc = "/snfs1/Project/GBD_HIV/packages_r") # 2023 Mar 23 10:57:13 - replaced per Meixin
# setwd(code_dir) # 2023 Mar 21 11:52:31 - fails - folder does not exist
# devtools::load_all()
source(file.path(code_root, "R/prep_pjnz_data.R")) # 2023 Mar 23 15:01:38 - pjnz loader funcs
source(file.path(code_root, "R/gbd_prep_data.R")) # 2023 Mar 23 15:01:38 - pjnz loader funcs
source(file.path(code_root, "R/ind_data_prep.R")) # 2023 Mar 23 15:01:38 - pjnz loader funcs 
# 2023 Mar 23 15:46:21 - China failure - may be issue with one file, resolve by wrapping with tryCatch()?

## Arguments
# cluster_project <- "proj_hiv" # 2023 Mar 21 11:13:57 - unused
unaids_year <- 2020 
gbdyear <- 2020 # 2023 Mar 23 15:08:03 - required by loader functions, somehow

# Metadata 1 -----------------------------------------------------------------
# Build shell now, append actual run parameters later
metadata_shell <- build_metadata_shell(code_root = code_root)
.start_time <- proc.time()

# Location table ----------------------------------------------
loc.table <- data.table(get_locations(hiv_metadata = T))
loc_vec <- unique(loc.table$ihme_loc_id)

### Functions
# loc.table <- data.table(get_locations(hiv_metadata = T)) # 2023 Mar 21 11:17:32 - defined multiple times

#Alternate metadata until 2019 becomes available
# loc.table <- "/ihme/mortality/shared/hiv_model_strategy_2020.csv" # 2023 Mar 21 11:17:32 - defined multiple times
# all_locs = loc.table[unaids_2019==1 & grepl("1",group) & epp==1,ihme_loc_id] # 2023 Mar 21 11:18:04 - unused

### Tables
# loc.table <- data.table(get_locations(hiv_metadata = T)) # 2023 Mar 21 11:20:14 - repeated below

### Code
# epp_list <- sort(loc.table[epp == 1, ihme_loc_id]) # 2023 Mar 21 11:20:27 - repeated below
# loc_vec <- epp_list
# dir.create(paste0('/ihme/hiv/data/PJNZ_prepped/', unaids_year, '/')) # 2023 Mar 21 11:18:29 - now handled with .output_path above

## Launch prepare locations file

# unaids_2020 <- strsplit(list.dirs('/snfs1/DATA/UNAIDS_ESTIMATES/2020/'), split = '//') # 2023 Mar 21 11:40:29 - this is never used
# unaids_2020 <- sapply(unaids_2020[2:length(unaids_2020)], '[[', 2) # FIXME - never use `sapply` in a pipeline - inconsistent output by definition

# Loader & prepare -------------------------

for(loc in loc_vec){
  # (loc <- loc_vec[7])
  unaids_year <- loc.table[ihme_loc_id==loc, unaids_recent] # 2023 Mar 23 15:20:12 - some cells are NA - where do files go then?
  .output_path_year <- file.path(.output_path, unaids_year)
  
  if(!dir.exists(.output_path_year)){
    dir.create(.output_path_year, recursive = TRUE, showWarnings = FALSE)
  }
  
  # Handle Group 1 locations
  if(grepl('1', loc.table[ihme_loc_id == loc, group])){
    
    # Handle non-India Group 1
    if(!grepl('IND', loc)){
      val <- prepare_spec_object(loc)
      print(attr(val,"country"))
      
    } else {
      # Handle India
      val <- prepare_spec_object_ind(loc)
    }
    
  } else {
    # Handle Group 2 locations
    val <- prepare_spec_object_group2(loc)
    #> 2023 Mar 23 15:05:34 - this relies on these objects, which are not passed in as arguments - dangerous
    #> `loc.table`
    #> gbdyear - no created here, nor in `main` branch - creating and dummy object to keep moving
  }
  
  file_name <- paste0(loc, '.rds')
  saveRDS(val, file.path(.output_path_year, file_name))
  
}

# Metadata 2 -----------------------------------------------------------------

.stop_time <- proc.time()
.run_time <- .stop_time - .start_time
.run_time <- paste(round(.run_time[["elapsed"]] / 60, 2), "min")
metadata_additions <- list(
  RECONNECT = RECONNECT,
  output_path    = .output_path,
  stop_time      = as.character(Sys.time()),
  run_time       = .run_time,
  unaids_year = unaids_year,
  loc_vec = loc_vec
)

# save to disk
metadata <- list()
metadata[["gbdeppaiml_launch_prepdata.R"]] <- append(x = metadata_shell, values = metadata_additions)
yaml::write_yaml(metadata, file.path(.output_path, "metadata.yaml"))
msg_prt(glue("ART data snapshot output to {.output_path}"))

# 2023 Mar 21 11:56:19 - is this a check step?
# cal = readRDS(paste0("/share/hiv/data/PJNZ_EPPASM_prepped/", loc, '.rds'))
# nrow(unique(attr(cal,"eppd")$ancsitedat))


### End

