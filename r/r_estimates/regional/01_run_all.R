library(here)
library(data.table)
here::here()
source("./r/r_estimates/manual_contact_matrix_functions.R")
source("./r/r_estimates/regional/00_setup_regions.R")
source("./r/functions/utility_functions.R")

library(doParallel)
registerDoParallel()
##########################################################
#
#                 User defined options
#
##########################################################

## all_regions for creating contacts matrices per region
panel_name <- c("all_regions", "all_regions_impute")[1]
# Pick the current week to get this week and last week's data
# This is for the folder name

if(week_name == 2){
  wave_ids <- c("A 1", "B 1")
 }else if(week_name == 8){
  wave_ids <- c("A 4", "B 4", "C 1", "D 1")
}else if(week_name == 9){
  wave_ids <- c("A 5", "B 4", "C 2", "D 1")
}else if(week_name == 10){
  wave_ids <- c("A 5", "B 5", "C 2", "D 2")
}else if(week_name == 11){
  wave_ids <- c("A 6", "B 5", "C 3", "D 2")
}else if(week_name == 12){
  wave_ids <- c("A 6", "B 6", "C 3", "D 3")
}else if(week_name == 13){
  wave_ids <- c("A 7", "B 6", "C 4", "D 3")
}else if(week_name == 14){
  wave_ids <- c("A 7", "B 7", "C 4", "D 4")
}else if(week_name == 15){
  wave_ids <- c("A 8", "B 7", "C 5", "D 4")
}else if(week_name == 16){
  wave_ids <- c("A 8", "B 8", "C 5", "D 5")
} else if(week_name == 17){
  wave_ids <- c("A 9", "B 8", "C 5", "D 5")
} else if(week_name == 19){
  wave_ids <- c("A 10", "B 9", "C 6", "D 5")
} else if(week_name == 20){
  wave_ids <- c("E 1", "EC 1")
}

print(wave_ids)

## Cap at 100 contacts.
panel_details <- c(NA, "cap_100", "trim_100", "ind_reported")[c(3)]
nickname_flags <- c(NA, "rm_non_contacts", "rm_suspected_multiple_contacts")[c(2)]
panel_details <- c(panel_details, nickname_flags)

## Only for UK currently
country_code_path <- "uk"
# Population data used for the UK
popyear <- 2020

## Select regions
regions <- c("South East", "North West", "Midlands", "South West", "London",
             "Wales", "North East and Yorkshire", "Scotland", "East of England",
             "Northern Ireland")

TEST <- FALSE
# Usually set to 200, or 1000
bootstrap_samples <- 200
# bootstrap_samples <- 10



settings <- c(panel_name, NA, panel_details, bootstrap_samples)
settings <- settings[!is.na(settings)]
# Saves to filter specific folders so we don't overwrite data
panel_name <- paste(settings, collapse = "_")
panel_name_ <-  panel_name

#age groups for which to create contact matrix
age_groups <- data.table(
  age_low = c(0, 5, 13, 18, seq(30, 70, 10)),
  age_high = c(4, 12, 17, seq(29, 69, 10), 100)
)
age_groups[, "name"] <- paste0(age_groups[, age_low], "-", age_groups[, age_high])

# Contact ages are not exact, they are grouped and some are missing
## Mean will take the mean between the ages
## Sample uniform will pick any number between the age bound with equal chance
## Sample_popdist will sample between the age bounds weighted by the population data

# Method for non-exact age groups of contacts
contact_age_process <- c("mean", "sample_uniform", "sample_popdist")[3]
# Method for unknown ages for contacts
contact_age_unknown_process <- c("remove", "sample_partdist", "sample_popdist")[3]

#what contact matrix subsets to create?
contact_filters <- list(
  "all" = list(),
  "home" = list("cnt_home" = "Yes"),
  "work" = list("cnt_work" = "Yes"),
  "school" = list("cnt_school" = "Yes"),
  "other" = list("cnt_home" = "No", "cnt_work" = "No", "cnt_school" = "No")
)


source("./r/r_estimates/regional/00_add_regional_directories.R")
comix_matrices_path <- matrices_path
comix_outputs_path <- outputs_path


source('./r/r_estimates/regional/02_manual_contact_matrix_comix_byregion.R')

by(part$n_cnt_all, part$nhs_regions, table)
summ <- part[, .(mean_cnt_all = mean(n_cnt_all), N = .N), by = nhs_regions]
cnt100 <- part[n_cnt_all == 100, .(over_100 = .N), by = nhs_regions]
summ <- merge(summ, cnt100, by = "nhs_regions", all = T)
summ
#regions <- regions[-10]

## Replace NA with zero
### Only use this if you know what you're doing and have checked the values
replace_na <- TRUE

source('./r/r_estimates/regional/03b_compare_with_bbc.R')


out_values_rounded[variable== "r_oth"][order(median)]
