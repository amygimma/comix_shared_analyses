rm(list=ls())
library(here)
library(data.table)
here::here()
source("./r/r_estimates/manual_contact_matrix_functions.R")
source("r/functions/utility_functions.R")

##########################################################
#
#                 User defined options
#
##########################################################


country_code_path <- c("uk", "nl", "be")[1]
panel_ <- c("A", "B", "E", "AB", "AC", "BD", "AD", "EEC")[8]
            #1    2    3    4     5     6     7      8
filter_region <- c(NA, "ENGLAND")[1]



# Panel details - choose only one
panel_details <- c(NA, "cap_100", "trim_100", "trim_50", "ind_reported")[3]
if (length(panel_details) > 1) stop("Choose one option for panel_details")
# Remove suspicious contacts
nickname_flags <- c(NA, "rm_non_contacts", "rm_suspected_multiple_contacts")[c(2,3)]

panel_details <- c(panel_details, nickname_flags)

# Default of NA is 2000
nboots <- c(NA, "boots_5", "boots_100", "boots_500", "boots_1000")[2]
TEST <- FALSE

settings <- c(panel_, filter_region, panel_details, nboots)
settings <- settings[!is.na(settings)]
# Saves to filter specific folders so we don't overwrite data
panel_name <- paste(settings, collapse = "_")
panel_name_ <-  panel_name

filter_type <- c("wave_id", "week", "wave_IDS")[3]
# Folder name AND used for filtering by wave_id, when set above


# Used only when filter_type set to wave_IDS (capitalized to make distinct)
wave_ids <- c("E 1", "EC 1")

# Week number 1 - 16 (only used if filtering by week)
# week_name <- 13

popyear <- 2020
week_name <- FALSE

if(filter_type == "wave_id") wave_name <- paste0("wave_", wave_id)
if(filter_type == "week") wave_name <- paste0("wave_", week)
if(filter_type == "wave_IDS") wave_name <- gsub(" ", "_", paste0(wave_ids, collapse = "_"))

#age groups for which to create contact matrix

age_groups <- data.table(
  age_low = c(0, 5, 13, 18, seq(30, 70, 10)),
  age_high = c(4, 12, 17, seq(29, 69, 10), 100)
)
age_groups[, "name"] <- paste0(age_groups[, age_low], "-", age_groups[, age_high])


#how to handle contact ages in age-grouo?
contact_age_process <- c("mean", "sample_uniform", "sample_popdist")[3]
#how to handle contact ages that are unknown?
contact_age_unknown_process <- c("remove", "sample_partdist", "sample_popdist")[3]
#how are participants bootstrapped?
bootstrap_type <- c("sample_participants_contacts", "no_sample", "bootstrap_all")[2]

#how many bootstrap samples are needed?
#set to 0 to disable
if ("boots_5" %in% nboots) {
  bootstrap_samples <- 5
} else if ("boots_1000" %in% nboots) {
  bootstrap_samples <- 1000
} else if ("boots_100" %in% nboots) {
  bootstrap_samples <- 100
} else if ("boots_500" %in% nboots) {
  bootstrap_samples <- 500
} else {
  bootstrap_samples <- 2000
}

#what contact matrix subsets to create?
contact_filters <- list(
  "all" = list(),
  "home" = list("cnt_home" = "Yes"),
  "work" = list("cnt_work" = "Yes"),
  "school" = list("cnt_school" = "Yes"),
  "other" = list("cnt_home" = "No", "cnt_work" = "No", "cnt_school" = "No")
)


#Read correct COMIX data
scripts_path <- here("r/r_estimates/")
source(file.path(scripts_path, "add_analysis_directories.R"))
comix_matrices_path <- matrices_path
panel_name <- panel_

part <- readRDS(file.path(data_path, "clean_participants.rds"))
contacts <- readRDS(file.path(data_path, "clean_contacts.rds"))
part <- as.data.table(part)
contacts <- as.data.table(contacts)

table(contacts$wave, contacts$panel, contacts$country_code)

### FILTER ENGLAND VS NOT
if ("ENGLAND" %in% filter_region) {
  not_england <- c("Scotland", "Northern Ireland", "Wales")
  part <- part[!(regions %in% not_england)]
}

if (filter_type == "wave_id") {
  ## FILTER BY WAVE_ID
  wave_id_ <- wave_id
  part <- part[wave_id == wave_id_]
}
if (filter_type == "week") {
  ### FILTER BY  WEEK
  part <- part[week %in% weeks_]
}
if (filter_type == "wave_IDS"){
  part <- part[wave_id %in% wave_ids]
}
contacts <- contacts[part_id %in% part$part_id & wave_id %in% unique(part$wave_id)]

if ("trim_100" %in% panel_details) {
  ## Filter Individually reported only
  contacts <- trim_contacts(contacts, n = 100)
  ## (then update n_cnt_* cols in case they're used later)
}

if ("trim_50" %in% panel_details) {
  ## Filter Individually reported only
  contacts <- trim_contacts(contacts, n = 50)
  ## (then update n_cnt_* cols in case they're used later)
}

if ("cap_100" %in% panel_details) {
  ## Filter Individually reported only
  part <- part[n_cnt_all <= 100]
  ## (then update n_cnt_* cols in case they're used later)
}


if ("ind_reported" %in% panel_details) {
  ## Filter Individually reported only
  contacts <- contacts[individually_reported == 1]
  ## (then update n_cnt_* cols in case they're used later)
}
if ("rm_non_contacts" %in% panel_details) {
  ## Filter suspected non contacts (suspected == 1)
  contacts <- contacts[suspected_non_contact == 0]
  ## (then update n_cnt_* cols in case they're used later)
}
if ("rm_suspected_multiple_contacts" %in% panel_details) {
  ## Filter suspected multiple contacts (suspected == 1)
  contacts <- contacts[suspected_multiple_contact == 0]
}

part <- add_n_cnts_location_cols(part, contacts, replace_existing_cols = TRUE)

## Check Filters
table(part$wave, part$panel)
table(part$regions)
table(contacts$individually_reported, contacts$wave_id)
summary(part$n_cnt_all)

nrow(part)
nrow(contacts)

part[, .(mean_cnt_all = mean(n_cnt_all),
         mean_cnt_not_household = mean(n_cnt_not_household),
         mean_cnt_not_home = mean(n_cnt_not_home)), by = "panel"]
part[, part_id := cp_number]
contacts[, part_id := cp_number]

# Save data description to file for reporting
fwrite_details(part, contacts, settings, panel_name_, panel_details,
               filter_region, filter_type, nboots, comix_matrices_path)
##########################################################
#
#                 END user defined options
#
##########################################################

# need to get population data for current year
popdata <- getPopdata(country_code_path, year=popyear)
#age groups in comix
comix_age_groups <- data.table(
  age_low = c(NA,  NA, NA, NA, 0,  65,  85,  0, 18, 1, 5,  5, 10, 12, 12, 15, 16, 18, 20, 25, 35, 45, 55, 65, 70, 75, 80, 85),
  age_high = c(NA, NA, NA, NA, 0, 100, 100, 17, 64, 4, 9, 11, 14, 17, 15, 19, 17, 19, 24, 34, 44, 54, 64, 69, 74, 79, 84, 100)
)
comix_age_groups[, "name"] <- c(
  "Don't know", "Don’t know", "This person is me", "Prefer not to answer", "Under 1", "65+", "85+",
  paste0(
    comix_age_groups[8:(nrow(comix_age_groups-1)), age_low],
    "-",
    comix_age_groups[8:(nrow(comix_age_groups-1)), age_high]
  )
)

if(bootstrap_samples == 0){
  sampled_part <- sample_age_child_participants(part, popdata)
  participants_bootstrapped_sets <- list(sampled_part)
} else {
  participants_bootstrapped_sets <- lapply(
    1:bootstrap_samples,
    function(x, part){
      good_sample <- FALSE
      while(!good_sample){
        sampled_part <- sample_age_child_participants(part, popdata)
        part2 <- sampled_part[sample(x=1:nrow(part), size=nrow(part),replace=T)]
        if(sum(!unique(part2[, part_age_group]) %in% unique(part[, part_age_group])) == 0){
          good_sample <- TRUE
        }
      }
      return(part2)
    },
    part=part
  )
}

# can be parallelized this as well
contacts_bootstrapped <- processContacts(
  contacts, comix_age_groups, popdata, contact_age_process, contact_age_unknown_process,
  bootstrap_samples, participants_bootstrapped_sets, bootstrap_type=bootstrap_type
)

#different contact sets to calculate matrices for
contacts_bootstrapped_sets <- lapply(
  contacts_bootstrapped,
  function(contacts){
    lapply(
      setNames(c(1:length(contact_filters)), names(contact_filters)),
      function(x, contact_filters, contacts){
        if(length(contact_filters[[x]]) == 0){
          return(contacts)
        } else {
          #for loop will combine akin to & statement
          for(i in 1:length(contact_filters[[x]])){
            varname_ <- names(contact_filters[[x]])[[i]]
            varval_ <- contact_filters[[x]][[i]]
            contacts <- contacts[get(varname_) == varval_]
          }
          if(nrow(contacts) == 0){
            warning(sprintf("filter %s returned an empty contact dataset", names(contact_filters)[[x]]))
          }
          return(contacts)
        }
      },
      contact_filters=contact_filters,
      contacts=contacts
    )
  }
)

#use bootstrapped datasets to create matrices
contacts_bootstrapped_matrices <- lapply(
  1:length(contacts_bootstrapped_sets),
  function(i, contacts_bootstrapped_sets, participants_bootstrapped_sets){
    message(i)
    lapply(
      contacts_bootstrapped_sets[[i]],
      calculate_matrix,
      participants = participants_bootstrapped_sets[[i]],
      population_data = popdata,
      age_groups = age_groups,
      weight_dayofweek = TRUE,
      use_reciprocal_for_missing = TRUE,
      symmetric_matrix = TRUE
    )
  },
  contacts_bootstrapped_sets,
  participants_bootstrapped_sets
)

#matrix may be empty, as there may be no contacts for the selected participants in some settings
#for those matrices, use the assigned null_matrix
null_matrix <- matrix(0, nrow = nrow(age_groups), ncol = nrow(age_groups), dimnames = list(age_groups$name,age_groups$name))
null_matrix[c(1:3),c(1:3)] <- NA

contacts_bootstrapped_matrices <- lapply(
  contacts_bootstrapped_matrices,
  function(x){
    lapply(
      x,
      function(y){
        if(is.null(y)){
          return(null_matrix)
        } else {
          return(y)
        }
      }
    )
  }
)

#need to check how this saves the matrices
#seems to save in multiple locations
saveRDS(
  contacts_bootstrapped_matrices,
  file.path(comix_matrices_path, paste0("bootstrap_samples.rds"))
)
print(paste("saved_to", file.path(comix_matrices_path, paste0("bootstrap_samples.rds"))))

# Create or call polymod matrices code
source("r/r_estimates/national/02_manual_contact_matrix_polymod.R")
# Estimate R0
source("r/r_estimates/national/03_manual_contact_matrix_impute_process.R")

