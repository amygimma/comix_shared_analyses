# comix_shared_analyses
analysis code for CoMix and other social contact surveys


# Process for Cleaning UK Code:

The `r/run_all_uk.R` script runs all cleaning code for survey versions 1 (panels A, B, C, D), and version 2 (panel E, F, splitting out child versions (EC, FC).

## Data Cleaning - Survey Version 1
1. V1_data_cleaning/dm01_rename_spss.R
2. V1_data_cleaning/dm02_data_clean.R 
  a. calls V2_data_cleaning/dm02a_child_data_clean.R for children's panels
3. V1_data_cleaning/dm03_data_checks.R (run manually)
4. V1_data_cleaning/dm04_combine_survey_data.R
  
  

## Data Cleaning - Survey Version 2
1. V2_data_cleaning/dm01_rename_spss.R
  a. calls V2_data_cleaning/dm_split_data.R to separate adult and children's data for processing
2. V2_data_cleaning/dm02_data_clean.R 
  a. calls V2_data_cleaning/dm02a_child_data_clean.R for children's panels
3. V2_data_cleaning/dm03_data_checks.R (run manually)
4. V2_data_cleaning/dm04_combine_survey_data.R


# Process data function

## Version 1

## Version 2

# Data cleaning notes

## Version 1

## Version 2
