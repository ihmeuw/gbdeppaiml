setwd(paste0(ifelse(windows, "H:", paste0("/ihme/homes/", user)), "/eppasm/"))
devtools::load_all()
code.dir <- paste0(ifelse(windows, "H:", paste0("/ihme/homes/", user)), "/gbdeppaiml/")
setwd(code.dir)
devtools::load_all()
input_table <- fread(paste0(ifelse(windows, "H:", paste0("/ihme/homes/", user)), '/gbdeppaiml/lbd_anc_align/inputs.csv'))
code.dir <- paste0(ifelse(windows, "H:", paste0("/ihme/homes/", user)), "/gbdeppaiml/")
cluster.project <- 'proj.hiv'
user <- ifelse(windows, Sys.getenv("USERNAME"), Sys.getenv("USER"))

args <- commandArgs(trailingOnly = TRUE)
print(args)
if(length(args) > 0) {
  run.name <- args[1]
  run.name.old <- args[2]
} else {
  run.name <- '191224_trumpet'
  run.name.old <- '191224_trumpet'
}




rerun_conditions <- c(input_table[run.name == run.name.old, lbd_anc_data] != input_table[run.name == run.name.old, lbd_anc_data],
                      input_table[run.name == run.name.old, geo_codebook] != input_table[run.name == run.name.old, geo_codebook],
                      input_table[run.name == run.name.old, sf_dir] != input_table[run.name == run.name.old, sf_dir],
                      input_table[run.name == run.name.old, rd] != input_table[run.name == run.name.old, rd],
                      input_table[run.name == run.name.old, lbd_anc_mean_est] != input_table[run.name == run.name.old, lbd_anc_mean_est])
if(any(rerun_conditions == TRUE)){
  lat_long.string <- paste0("qsub -l m_mem_free=30G -l fthread=1 -l h_rt=01:00:00 -q all.q -P ", cluster.project, " ",
                            "-e /share/homes/", user,"/errors ",
                            "-o /share/temp/sgeoutput/", user, "/output ",
                            "-N ",  "lat_long ",
                            code.dir, "gbd/singR_shell.sh ",
                            code.dir, "lbd_anc_align/lat_long.R ")
  print(lat_long.string)
  system(lat_long.string)
  
  recreate.string <- paste0("qsub -l m_mem_free=30G -l fthread=1 -l h_rt=01:00:00 -q all.q -P ", cluster.project, " ",
                        "-e /share/homes/", user,"/errors ",
                        "-o /share/temp/sgeoutput/", user, "/output ",
                        "-N ",  "lbd_processing ",
                        "-hold_jid ", "lat_long ",
                        code.dir, "gbd/singR_shell.sh ",
                        code.dir, "lbd_anc_align/lbd_anc_recreate.R ")
  print(recreate.string)
  system(recreate.string)
  
  offset.string <- paste0("qsub -l m_mem_free=30G -l fthread=1 -l h_rt=01:00:00 -q all.q -P ", cluster.project, " ",
                        "-e /share/homes/", user,"/errors ",
                        "-o /share/temp/sgeoutput/", user, "/output ",
                        "-N ", "create_offsets ",
                        "-hold_jid ", "lbd_processing ",
                        code.dir, "gbd/singR_shell.sh ",
                        code.dir, "lbd_anc_align/offsets/create_offsets.R ")
  print(offset.string)
  system(offset.string)
}

