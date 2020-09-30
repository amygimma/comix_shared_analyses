library(data.table)

if(file.exists("r/user_setup.R")) source("r/user_setup.R")
data_path <- "data"
if (!is.null(USER_DATA_PATH)) data_path <- USER_DATA_PATH


waves_list <- list(
  # list(panel_ = "panel_e", wave_ = "wave_1", spss_ref = "PEW1"),
  # list(panel_ = "panel_e", wave_ = "wave_2", spss_ref = "PEW2"),
  # list(panel_ = "panel_e", wave_ = "wave_3", spss_ref = "PEW3"),
  list(panel_ = "panel_e", wave_ = "wave_4", spss_ref_ = "PEW4")#,

  # list(panel_ = "panel_f", wave_ = "wave_1", spss_ref = "PFW1"),
  # list(panel_ = "panel_f", wave_ = "wave_2", spss_ref = "PFW2"),
  # list(panel_ = "panel_f", wave_ = "wave_3", spss_ref = "PFW3")
)

scripts_path <- file.path("r", "V2_data_cleaning")
country_code_ <- "uk"
CLEANING_SCRIPT <- TRUE
for (wave_list in waves_list) {
  # wave_list <- waves_list[[1]]
  panel_ <- wave_list$panel_
  wave_ <- wave_list$wave_
  spss_ref_ <- wave_list$spss_ref_
  source(file.path(scripts_path, "dm01_rename_spss.R"))
  source(file.path(scripts_path, "dm_split_survey.R"))

  #Clean adult data
  panel_ <- wave_list$panel_
  source(file.path(scripts_path, "dm02_data_clean.R"))

  # Clean child data
  panel_ <- paste0(panel_, "c")
  source(file.path(scripts_path, "dm02_data_clean.R"))
}


# source(file.path(clean_path, "dm04_combine_survey_files.R"))
CLEANING_SCRIPT <- FALSE


## Add POLYMOD

# Align Polymod with UK data and census

library(socialmixr)
data("polymod")

# age_bins <- c(0, 5, 13, 18, 30, 40, 50, 60, 70, 120)
# age_bins_census <- c(seq(0,70, 10), 120)

pm_part <- polymod$participants[country == "United Kingdom"]
pm_contacts <- polymod$contacts[part_id %in% pm_part$part_id]

pm_part$part_age_group <- cut(pm_part$part_age,
                              breaks = age_bins,
                              right = FALSE)

pm_part$part_age_group_census <-  cut(pm_part$part_age,
                                      breaks = age_bins_census,
                                      #labels = age_groups,
                                      ordered_result = TRUE,
                                      right = FALSE)


pm_contacts_part <- merge(pm_contacts, pm_part, by = "part_id")

data_path <- "data/uk"

saveRDS(pm_part, file = file.path(data_path, "polymod_participants.rds"))
saveRDS(pm_contacts, file = file.path(data_path, "polymod_contacts.rds"))
saveRDS(pm_contacts_part, file = file.path(data_path, "polymod_contacts_part.rds"))



# ==============================+

pm_part <- polymod$participants[country == "Belgium"]
pm_contacts <- polymod$contacts


# pm_contacts <- pm_contacts$contacts

pm_part$part_age_group <- cut(pm_part$part_age,
                              breaks = age_bins,
                              right = FALSE)

pm_part$part_age_group_census <-  cut(pm_part$part_age,
                                      breaks = age_bins_census,
                                      #labels = age_groups,
                                      ordered_result = TRUE,
                                      right = FALSE)


pm_contacts_part <- merge(pm_contacts, pm_part, by = "part_id")

data_path <- "data/be"

saveRDS(pm_part, file = file.path(data_path, "polymod_participants.rds"))
saveRDS(pm_contacts, file = file.path(data_path, "polymod_contacts.rds"))
saveRDS(pm_contacts_part, file = file.path(data_path, "polymod_contacts_part.rds"))

# ==========================+
#
#
#
pm_part <- polymod$participants[country == "Netherlands"]

pm_contacts <- polymod$contacts

pm_part$part_age_group <- cut(pm_part$part_age,
                              breaks = age_bins,
                              right = FALSE)

pm_part$part_age_group_census <-  cut(pm_part$part_age,
                                      breaks = age_bins_census,
                                      #labels = age_groups,
                                      ordered_result = TRUE,
                                      right = FALSE)


pm_contacts_part <- merge(pm_contacts, pm_part, by = "part_id")

data_path <- "data/nl"

saveRDS(pm_part, file = file.path(data_path, "polymod_participants.rds"))
saveRDS(pm_contacts, file = file.path(data_path, "polymod_contacts.rds"))
saveRDS(pm_contacts_part, file = file.path(data_path, "polymod_contacts_part.rds"))

