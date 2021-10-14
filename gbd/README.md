# HIV Vetting Shiny

This repository formats the outputs of the forecasting (whether FHS or GK) and GBD pipelines into a format that can be uploaded to a shiny. The shiny can be used to compare the current and former runs, allowing you to zoom in on any portion of the time series. 

## Requirements

### Repositories

The code is formatted to pull from repositories that have been cloned to your homes folder. 

To prepare forecasting inputs, you must first clone the [FHS repo](https://stash.ihme.washington.edu/projects/FOR/repos/hiv_forecasting_inputs/browse?at=refs%2Fheads%2Fforecasting_2020). If you haven't done this, you will be remined to in [00_prep_shiny.R](https://stash.ihme.washington.edu/projects/HIVTBID/repos/hiv_shiny_vetting/browse/00_prep_shiny.R#24-26). 

### Shell scripts and images
[00_prep_shiny.R](https://stash.ihme.washington.edu/projects/HIVTBID/repos/hiv_shiny_vetting/browse/00_prep_shiny.R) requires the [HIV image](/ihme/singularity-images/hiv/hiv_11.img) and the general R [shell script](/ihme/singularity-images/rstudio/shells/execR.sh) used at IHME. To deploy the shiny, you need to use the [general R image](/ihme/singularity-images/rstudioihme_rstudio_4055.img). To balance this, I generally run [00_prep_shiny.R](https://stash.ihme.washington.edu/projects/HIVTBID/repos/hiv_shiny_vetting/browse/00_prep_shiny.R) interactively and then open a screen to run [01_run_shiny.R](https://stash.ihme.washington.edu/projects/HIVTBID/repos/hiv_shiny_vetting/browse/01_run_shiny.R). Code to do this:

```bash
screen
qlogm_fair(){
     MEM=${1:-100G}
     TIME=${2:-24:00:00}
     THREADS=${3:-50}
     qlogin -P proj_hiv -l m_mem_free=$MEM -l fthread=$THREADS -l h_rt=$TIME -q all.q -l archive -now no 
}

function myR_gen(){
    /ihme/singularity-images/rstudio/shells/interactiveR.sh -i /ihme/singularity-images/rstudioihme_rstudio_4055.img
}
qlogm_fair
myR_gen
```
Once the R session has opened, 

```R
source('/ihme/homes/[USERNAME]/hiv_shiny_vetting/01_run_shiny.R', echo = T)
```

