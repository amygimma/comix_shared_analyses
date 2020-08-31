## Go to 01_run_all to define inputs.

#Read correct COMIX data
scripts_path <- here("r/change_in_R/")

part <- readRDS(file.path(data_path, "clean_participants.rds"))

# Select relevant wave_ids
part <- part[wave_id %in% wave_ids]


contacts <- readRDS(file.path(data_path, "clean_contacts.rds"))
contacts <- contacts[wave_id %in% wave_ids]

popdata <- getPopdata(country_code_path, year=popyear)

#age groups in comix
## Give all categories 
## names(table(contacts$cnt_age))
## 0-17, 18-64, 65-100 mass contacts
## 
comix_age_groups <- data.table(
  age_low = c(NA,  NA, NA, NA, 0,  0, 1, 10, 12, 15, 18, 18, 18, 20, 25, 30, 35, 40, 45,  5, 5, 50, 55, 60, 65, 65, 70, 70, 75, 80, 85),
  age_high = c(NA, NA, NA, NA, 0, 17, 4, 14, 17, 19, 19, 30, 64, 24, 34, 40, 44, 50, 54, 11, 9, 60, 64, 70, 100, 69, 120, 74, 79, 84, 100)
)
comix_age_groups[, "name"] <- c(
  "Don't know", "Prefer not to answer", "Under 1",
  paste0(
    comix_age_groups[4:(nrow(comix_age_groups-1)), age_low],
    "-",
    comix_age_groups[4:(nrow(comix_age_groups-1)), age_high]
  )
)

part_main <- part
contacts <- contacts[part_id %in% part$part_id & wave_id %in% unique(part$wave_id)]


if ("cap_100" %in% panel_details) {
  ## Filter Individually reported only 
  part <- part[n_cnt_all <= 100]
  ## (then update n_cnt_* cols in case they're used later)
}

if ("trim_100" %in% panel_details) {
  ## Filter Individually reported only 
  contacts <- trim_contacts(contacts, n = 100)
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

# Save data description to file for reporting
fwrite_details(part, contacts, settings, panel_name_, panel_details,
               wave_ids, NA, bootstrap_samples, comix_matrices_path) 



foreach(region_ = regions, .packages = c("data.table")) %dopar%{
  print(sprintf("region %s", region_))
  
  part <- part_main[nhs_regions == region_]
  print("Starting part bootstrap")
  participants_bootstrapped_sets <- lapply(
    1:bootstrap_samples,
    function(x, part){
      good_sample <- FALSE
      while(!good_sample){
        sampled_part <- sample_age_child_participants(part, popdata)
        part2 <- sampled_part[sample(x=1:nrow(part), size=nrow(part),replace=T)]
        if(sum(!unique(part2[, part_age_group]) %in% unique(part[, part_age_group])) == 0){
          good_sample <- TRUE
          print("Good sample")
        }
      }
      return(part2)
    },
    part=part
  )
  
  contacts_bootstrapped <- processContacts(
    contacts, comix_age_groups, popdata, contact_age_process, contact_age_unknown_process,
    bootstrap_samples, participants_bootstrapped_sets, bootstrap_type=c("sample_participants_contacts", "no_sample", "bootstrap_all")[2]
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
  
  null_matrix <- matrix(0, nrow = nrow(age_groups), ncol = nrow(age_groups), dimnames = list(age_groups$name,age_groups$name))
  null_matrix[c(1:3),c(1:3)] <- 0
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
  
  saveRDS(
    contacts_bootstrapped_matrices,
    file.path(matrices_path, paste0("bootstrap_samples_", region_,".rds"))
  )
}
