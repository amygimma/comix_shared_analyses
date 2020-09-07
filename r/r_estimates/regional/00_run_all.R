library(here)
library(data.table)
here::here()
source("./r/r_estimates/manual_contact_matrix_functions.R")
##########################################################
#
#                 User defined options
#
##########################################################

## all_regions for creating contacts matrices per region
panel_name <- "all_regions"
# Pick the current week to get this week and last week's data
week_name <- 20
## Only for UK currently
country_code_path <- "uk"
# Population data used for the UK
popyear <- 2020


TEST <- FALSE


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


## Create boostrap matrices
