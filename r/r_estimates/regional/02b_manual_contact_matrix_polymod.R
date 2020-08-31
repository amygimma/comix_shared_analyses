library(here)
library(data.table)
here::here()
source("./r/change_R2/manual_contact_matrix_functions.R")
##########################################################
#
#                 User defined options
#
##########################################################

panel_name <- "polymod"
wave_name <- "1"
country_code_path <- c("uk", "nl", "be")[1]
popyear <- 2005
TEST <- FALSE

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

#how many bootstrap samples are needed?
#set to 0 to disable
bootstrap_samples <- 0
#how are participants bootstrapped?
bootstrap_type <- c("sample_participants_contacts", "no_sample", "bootstrap_all")[2]

#what contact matrix subsets to create?
contact_filters <- list(
  "all" = list(),
  "home" = list("cnt_home" = 1),
  "work" = list("cnt_work" = 1),
  "school" = list("cnt_school" = 1),
  "other" = list("cnt_home" = 0, "cnt_work" = 0, "cnt_school" = 0)
)

##########################################################
#
#                 END user defined options
#
##########################################################

#Read correct COMIX data
scripts_path <- here("r/change_in_R/")
source(file.path(scripts_path, "add_analysis_directories.R"))
polymod_matrices_path <- matrices_path

wave_id_ <- paste(panel_name, wave_name)

part <- readRDS(file.path(data_path, "polymod_participants.rds"))
#part <- part[wave_id == wave_id_]
polymod_dayofweek <- data.table(
  day_id = c(0:6, NA),
  weekday = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Monday")
)
part <- merge(part, polymod_dayofweek, by.x="dayofweek", by.y="day_id")


contacts <- readRDS(file.path(data_path, "polymod_contacts.rds"))
contacts <- merge(contacts, part[, c("part_id", "weekday")])

#need to get population data for current year
popdata <- getPopdata(country_code_path, year=popyear)

#age groups in comix
comix_age_groups <- data.table(
  age_low = c(NA, NA, 0, 1, 5, 10, 15, 20, 25, 35, 45, 55, 65, 70, 75, 80, 85),
  age_high = c(NA, NA, 0, 4, 9, 14, 19, 24, 34, 44, 54, 64, 69, 74, 79, 84, 100)
)
comix_age_groups[, "name"] <- c(
  "Don't know", "Prefer not to answer", "Under 1",
  paste0(
    comix_age_groups[4:(nrow(comix_age_groups-1)), age_low],
    "-",
    comix_age_groups[4:(nrow(comix_age_groups-1)), age_high]
  )
)
polymod_age_groups <- NULL

if(bootstrap_samples == 0){
  participants_bootstrapped_sets <- list(part)
} else {
  participants_bootstrapped_sets <- lapply(
    1:bootstrap_samples,
    function(x, part){
      good_sample <- FALSE
      while(!good_sample){
        part2 <- part[sample(x=1:nrow(part), size=nrow(part),replace=T)]
        #part2[, part_id_old := part_id]
        #part2[, "part_id"] <- c(1:nrow(part))
        if(unique(part[, part_age_group]) %in% sum(!unique(part2[, part_age_group])) == 0){
          good_sample <- TRUE
        }
      }
      return(part2)
    },
    part=part
  ) 
}

#can be parallelized this as well
contacts_bootstrapped <- processContacts(
  contacts, polymod_age_groups, popdata, contact_age_process, contact_age_unknown_process,
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
      population_data = popdata, age_groups = age_groups,
      weight_dayofweek = TRUE, use_reciprocal_for_missing = TRUE, symmetric_matrix = TRUE
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
  file.path(polymod_matrices_path, "bootstrap_samples.rds")
)
