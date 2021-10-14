## 01_create_data.R
Pulls in data and saves it to /ihme/hiv/papers/hiv_redistribution/data/. **NOTE: This file pulls from mwcunnin's temp folder on the J drive. Need to reconsider this data's permanent location.**

Creates: 
* hiv_corrected_cause_data.csv
* clean_hiv_redis_corr_data.csv
* pop_data.csv

## 02_summarise_data.R
Requires clean_hiv_redis_corr_data.csv, hiv_corrected_cause_data, hiv_redistributed_cause_data, and cod_data_2020_edited.csv. Utilizes two functions (global_summary and global_rd) to format the redistributed and corrected data into a format useful for the rest of the analysis. 

Creates:
* corr_deaths_summary.RDS
* rd_deaths_summary.RDS
* final_deaths_summary.RDS

## 02_tables_and_figures.R
Requires corr_deaths_summary.RDS, rd_deaths_summary.RDS, and final_deaths_summary.RDS and two .csvs. Creates a ggplot theme for all figures, and saves results to /ihme/hiv/papers/hiv_redistribution/results/. 

corr_deaths_summary.RDS is used to create:
* Figure 1
* Figure 2

rd_deaths_summary.RDS is used to create:
* Figure 1
* Figure 2
* Appendix figure 3

final_deaths_summary.RDS is used to create:
* Table 1
* Figure 4
* Figure 5
* Figure 6

clean_hiv_redis_corr_data.csv is used to create:
* Figure 3
* Appendix table 1

raw_redistributed_and_hiv_corr_deaths_scaled_to_completeness.csv is used to create:
* Appendix table 2

## 03_results.Rmd
Requires clean_hiv_redis_corr_data.csv, hiv_corrected_cause_data.csv, hiv_redistribution_cause_data.csv, and cod_data_2020_edited.csv as well as corr_deaths_summary, rd_deaths_summary, and final_deaths_summary.RDS. Number plugs the results section. 
