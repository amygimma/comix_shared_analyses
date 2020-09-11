library(here)
library(data.table)
library(snakecase)
here::here()
source("./r/r_estimates/manual_contact_matrix_functions.R")
source("r/functions/utility_functions.R")

##########################################################
#
#                 User defined options
#
##########################################################


country_code_path <- c("sc")[1]
panel_ <- c("A", "B")[1]

# Panel details - choose only one
panel_details <- c(NA, "cap_100", "trim_100", "trim_50", "ind_reported")[1]
if (length(panel_details) > 1) stop("Choose one option for panel_details")

# Default of NA is 2000
nboots <- c("boots_10", "boots_100", "boots_250", "boots_500")[1]
TEST <- FALSE

settings <- c(panel_, panel_details, nboots)
settings <- settings[!is.na(settings)]
# Saves to filter specific folders so we don't overwrite data
panel_name <- paste(settings, collapse = "_")
panel_name_ <-  panel_name

filter_type <- c("wave_id", "week", "wave_IDS")[3]
# Folder name AND used for filtering by wave_id, when set above


# Used only when filter_type set to wave_IDS (capitalized to make distinct)
wave_ids <- c("A 2")

# Week number 1 - 16 (only used if filtering by week)
# weeks_ <- c(12)

popyear <- 2020
# week_name <- 13
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
if ("boots_10" %in% nboots) {
  bootstrap_samples <- 10
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
  "home" = list("cnt_home" = 1),
  "work" = list("cnt_work" = 1),
  "school" = list("cnt_school" = 1),
  "other" = list("cnt_home" = 0, "cnt_work" = 0, "cnt_school" = 0)
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
# contacts[, individually_reported := 1]
contacts[, date := (as.Date(as.character(date)) - 1)]
contacts[, weekday := weekdays(date)]


cdates <- unique(contacts[, list(part_id, date, weekday)])
part <- merge(part, cdates, by = "part_id", all.x = T)
part[n_cnt_all == 0, date := as.Date("2020-08-06")]
part[n_cnt_all == 0, weekday := weekdays(date)]

table(part$date, useNA = "always")
table(part$weekday, useNA = "always")
table(part$wave_id)
table(contacts$wave, contacts$panel)

### FILTER ENGLAND VS NOT
# if ("ENGLAND" %in% filter_region) {
#   not_england <- c("Scotland", "Northern Ireland", "Wales")
#   part <- part[!(regions %in% not_england)]
# }

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


part <- add_n_cnts_location_cols_scotland(part, contacts, replace_existing_cols = TRUE)

## Check Filters
table(part$wave, part$panel)
table(part$regions)
table(contacts$individually_reported, contacts$wave_id)
summary(part$n_cnt_all)

nrow(part)
nrow(contacts)

part[, .(mean_cnt_all = mean(n_cnt_all),
         # mean_cnt_not_household = mean(n_cnt_not_household),
         mean_cnt_not_home = mean(n_cnt_not_home)), by = "panel"]

# Save data description to file for reporting
fwrite_details(part, contacts, settings, panel_name_, panel_details,
               filter_region = "NA", filter_type, nboots, comix_matrices_path)
##########################################################
#
#                 END user defined options
#
##########################################################

# load population data
popdata <- as.data.table(read_xlsx("data/raw_data/sc/pop_breakdown_scot.xlsx"))
names(popdata) <- snakecase::to_snake_case(names( popdata))
popdata[, gender := ifelse(gender == "M", "male", "female")]
popdata <- data.table::dcast(age ~ gender, data = popdata, value.var = "population_est")

popdata <- popdata[, total := male + female]
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
mean(contacts_bootstrapped[[1]][, .(n = .N), by = "part_id"]$n)

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
# source("r/r_estimates/national/02_manual_contact_matrix_polymod.R")
# Estimate R0
source("r/r_estimates/scotland/03_manual_contact_matrix_impute_process.R")

