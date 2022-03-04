#Group 1 HIV Parent Repository

This repository contains the steps to prep a new GBD year run, run group 1 countries through EPP-ASM and prepare these draw-level files for post-processing steps which occur in the [HIV parent repository](https://stash.ihme.washington.edu/projects/HIVTBID/repos/hiv_gbd/browse/04_prep_all_results).

...

## Run patterns
This repository is divided into ordinal scripts that complete the aforementioned modelling processes. These are as follows:

[01_prep_inputs.R](https://github.com/ihmeuw/gbdeppaiml/blob/main/gbd/01_prep_inputs.R): Prepares the appropriate inputs for a new EPPASM run. Can be done for a subset of countries if a partial run is being done, and can just copy inputs from the most recent EPPASM run if new inputs don't need to be pulled. If new inputs are being pulled. This will launch a series of scripts outlined below:
	[gbd_prep_inputs.R](https://github.com/ihmeuw/gbdeppaiml/blob/main/gbd/gbd_prep_inputs.R) and [gbd_prep_ind_inputs.R](https://github.com/ihmeuw/gbdeppaiml/blob/main/gbd/gbd_prep_ind_inputs.R): Pulls in new demographics inputs (ASFR, population, migration, MLT, and births) and outputs them in /ihme/hiv/epp_input/[GBDYEAR]/[RUNNAME]/. Relies on /ihme/hiv/epp_input/[GBDYEAR]/input_ids.csv to know which version of demographics to pull. This table needs to be updated manually with information on the most up to date run_ids from the demographics team. Generally, its best to ask in the demographics slack channel to ensure that these are correct. 
	[cache_prev_surveys_age_sex.R](https://github.com/ihmeuw/gbdeppaiml/blob/main/gbd/cache_prev_surveys_age_sex.R): Prepares prevalence data that is required to inform the transmission rate in EPPASM. Pulls in the extraction sheet from /ihme/hiv/data/prevalence_surveys/. 
	[prep_art_props.R](https://github.com/ihmeuw/gbdeppaiml/blob/main/gbd/prep_art_props.R): Splits ART for the urban/rural divides in India and Kenya. Relies on /ihme/hiv/epp_input/gbd19/KEN_ART_props.csv
	[00_launch_lbd_process.R](https://github.com/ihmeuw/gbdeppaiml/blob/main/lbd_anc_align/00_launch_lbd_process.R): Only needs to be run once per round. Launch script to create offsets for ANC prevalence data. Relies on something changing in the [rerun conditions](https://github.com/ihmeuw/gbdeppaiml/blob/main/lbd_anc_align/inputs.csv), which needs to be manually updated.

[02_launch_eppasm.R](https://github.com/ihmeuw/gbdeppaiml/blob/main/gbd/02_launch_eppasm.R): Launch script for the EPP-ASM model process, compilation, summary, and plotting scripts. 
	[main.R](https://github.com/ihmeuw/gbdeppaiml/blob/main/gbd/main.R): Central script that launches EPPASM. Prepares the dt object needed to run the simulation by subbing in IHME's internal estimates for some inputs (read_spec_object). Generates a transmission rate that matches the input prevalence data (fitmod), and then uses that transmission rate to complete the disease simulation (gbd_sim_mod).
	[compile_draws.R](https://github.com/ihmeuw/gbdeppaiml/blob/main/gbd/compile_draws.R): Combines all draw-level files into one .csv per location. Fills in any missing draws, and replaces negative values with zero. 
	[get_summary_files.R](https://github.com/ihmeuw/gbdeppaiml/blob/main/gbd/get_summary_files.R): Summarizes compiled files for plotting.
	[main_plot_output.R](https://github.com/ihmeuw/gbdeppaiml/blob/main/gbd/main_plot_output.R): Generates 15-49 plots, age and sex specific plots, and pediatric plots showing maternal to child transmission. 

[02a_india_steps.R](https://github.com/ihmeuw/gbdeppaiml/blob/main/gbd/02a_india_steps.R): Prepares EPPASM India output to go through spectrum. Lauches [split_ind_states.R](https://github.com/ihmeuw/gbdeppaiml/blob/main/gbd/split_ind_states.R).


[03_reckoning_prep.R](https://github.com/ihmeuw/gbdeppaiml/blob/main/gbd/03_reckoning_prep.R): Calls [aggregate.R](https://github.com/ihmeuw/gbdeppaiml/blob/main/gbd/aggregate.R) and [apply_age_splits.R](https://github.com/ihmeuw/gbdeppaiml/blob/main/gbd/apply_age_splits.R). Aggregates sub-nationals up to the national level and splits the 5 year age groups produced in EPPASM into the standard GBD age groups. apply_age_splits.R outputs to several directories important for upload, shown below:
	-/ihme/hiv/spectrum_prepped/art_draws/
	-/ihme/hiv/spectrum_prepped/death_draws/
	-/ihme/hiv/spectrum_prepped/birth_prev/

[04_vetting.R](https://github.com/ihmeuw/gbdeppaiml/blob/main/gbd/04_vetting.R): Useful in deciphering changes between runs. Allows you to compare inputs (whether they be UNAIDS inputs, demographics, of the DT objects themselves) and results (age-specific results and covariates generated from our results). 
	* Note: currently this is mostly just a script with some useful code more than a necessary component of the pipeline. 

...

## Requirements

### Repositories
The code is formatted to pull from repositories that have been cloned to your homes folder. 

Running prep and processing requires the [HIV image](/ihme/singularity-images/hiv/hiv_11.img) and the [central IHME R shell script](/ihme/singularity-images/rstudio/shells/execR.sh)

### Run instructions
If running over ~20 locations it is easier to launch from the terminal. To open an RStudio session use the following. 
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
source('[PATH TO FILE]', echo = T)
```
