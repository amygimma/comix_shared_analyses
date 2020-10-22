### Run data cleaning
library(here)

clean_path <- here("r", "V1_data_cleaning")

country_code_ <- "uk"
panel_ <- "panel_a"
wave_ <- "wave_1"
source(file.path(clean_path, "dm02_data_clean.R"))


panel_ <- "panel_a"
wave_ <- "wave_2"
source(file.path(clean_path, "dm02_data_clean.R"))

panel_ <- "panel_a"
wave_ <- "wave_3"
source(file.path(clean_path, "dm02_data_clean.R"))

panel_ <- "panel_a"
wave_ <- "wave_4"
source(file.path(clean_path, "dm02_data_clean.R"))

panel_ <- "panel_a"
wave_ <- "wave_5"
source(file.path(clean_path, "dm02_data_clean.R"))

panel_ <- "panel_a"
wave_ <- "wave_6"
source(file.path(clean_path, "dm02_data_clean.R"))

panel_ <- "panel_a"
wave_ <- "wave_7"
source(file.path(clean_path, "dm02_data_clean.R"))

panel_ <- "panel_a"
wave_ <- "wave_8"
source(file.path(clean_path, "dm02_data_clean.R"))

panel_ <- "panel_a"
wave_ <- "wave_9"
source(file.path(clean_path, "dm02_data_clean.R"))

panel_ <- "panel_a"
wave_ <- "wave_10"
source(file.path(clean_path, "dm02_data_clean.R"))

panel_ <- "panel_b"
wave_ <- "wave_1"
source(file.path(clean_path, "dm02_data_clean.R"))

panel_ <- "panel_b"
wave_ <- "wave_2"
source(file.path(clean_path, "dm02_data_clean.R"))

panel_ <- "panel_b"
wave_ <- "wave_3"
source(file.path(clean_path, "dm02_data_clean.R"))

panel_ <- "panel_b"
wave_ <- "wave_4"
source(file.path(clean_path, "dm02_data_clean.R"))

panel_ <- "panel_b"
wave_ <- "wave_5"
source(file.path(clean_path, "dm02_data_clean.R"))

panel_ <- "panel_b"
wave_ <- "wave_6"
source(file.path(clean_path, "dm02_data_clean.R"))

panel_ <- "panel_b"
wave_ <- "wave_7"
source(file.path(clean_path, "dm02_data_clean.R"))

panel_ <- "panel_b"
wave_ <- "wave_8"
source(file.path(clean_path, "dm02_data_clean.R"))


# Children's panels
panel_ <- "panel_c"
wave_ <- "wave_1"
source(file.path(clean_path, "dm02_data_clean.R"))

panel_ <- "panel_c"
wave_ <- "wave_2"
source(file.path(clean_path, "dm02_data_clean.R"))

panel_ <- "panel_c"
wave_ <- "wave_3"
source(file.path(clean_path, "dm02_data_clean.R"))

panel_ <- "panel_c"
wave_ <- "wave_4"
source(file.path(clean_path, "dm02_data_clean.R"))

panel_ <- "panel_c"
wave_ <- "wave_5"
source(file.path(clean_path, "dm02_data_clean.R"))

panel_ <- "panel_c"
wave_ <- "wave_6"
source(file.path(clean_path, "dm02_data_clean.R"))

panel_ <- "panel_d"
wave_ <- "wave_1"
source(file.path(clean_path, "dm02_data_clean.R"))

panel_ <- "panel_d"
wave_ <- "wave_2"
source(file.path(clean_path, "dm02_data_clean.R"))

panel_ <- "panel_d"
wave_ <- "wave_3"
source(file.path(clean_path, "dm02_data_clean.R"))

panel_ <- "panel_d"
wave_ <- "wave_4"
source(file.path(clean_path, "dm02_data_clean.R"))

panel_ <- "panel_d"
wave_ <- "wave_5"
source(file.path(clean_path, "dm02_data_clean.R"))

source(file.path(clean_path, "dm04_combine_survey_files.R"))



## Might want to add in data checks after each one?



# BE and NL (same schedule)
# =================
# country_codes <- c("nl", "be")
# for (country_code in country_codes) {
# # First panel first wave BE
#   country_code_ <- country_code
#   panel_ <- "panel_a"
#   wave_ <- "wave_1"
#   source(file.path(clean_path, "dm02_data_clean.R"))
#
#   country_code_ <- country_code
#   panel_ <- "panel_a"
#   wave_ <- "wave_2"
#   source(file.path(clean_path, "dm02_data_clean.R"))
#
#   country_code_ <- country_code
#   panel_ <- "panel_a"
#   wave_ <- "wave_3"
#   source(file.path(clean_path, "dm02_data_clean.R"))
#
#   country_code_ <- country_code
#   panel_ <- "panel_a"
#   wave_ <- "wave_4"
#   source(file.path(clean_path, "dm02_data_clean.R"))
#
#   country_code_ <- country_code
#   panel_ <- "panel_a"
#   wave_ <- "wave_5"
#   source(file.path(clean_path, "dm02_data_clean.R"))
#
#   country_code_ <- country_code
#   panel_ <- "panel_a"
#   wave_ <- "wave_6"
#   source(file.path(clean_path, "dm02_data_clean.R"))
#
#   country_code_ <- country_code
#   panel_ <- "panel_a"
#   wave_ <- "wave_7"
#   source(file.path(clean_path, "dm02_data_clean.R"))
# }
#
# source(file.path(clean_path, "dm04_combine_survey_files.R"))
#
#
# ## Add POLYMOD
#
# # Align Polymod with UK data and census
#
# library(socialmixr)
# data("polymod")
#
# # age_bins <- c(0, 5, 13, 18, 30, 40, 50, 60, 70, 120)
# # age_bins_census <- c(seq(0,70, 10), 120)
#
# pm_part <- polymod$participants[country == "United Kingdom"]
# pm_contacts <- polymod$contacts[part_id %in% pm_part$part_id]
#
# pm_part$part_age_group <- cut(pm_part$part_age,
#                               breaks = age_bins,
#                               right = FALSE)
#
# pm_part$part_age_group_census <-  cut(pm_part$part_age,
#                                       breaks = age_bins_census,
#                                       #labels = age_groups,
#                                       ordered_result = TRUE,
#                                       right = FALSE)
#
#
# pm_contacts_part <- merge(pm_contacts, pm_part, by = "part_id")
#
# data_path <- "data/uk"
#
# saveRDS(pm_part, file = file.path(data_path, "polymod_participants.rds"))
# saveRDS(pm_contacts, file = file.path(data_path, "polymod_contacts.rds"))
# saveRDS(pm_contacts_part, file = file.path(data_path, "polymod_contacts_part.rds"))
#
#
#
# # ==============================+
#
# pm_part <- polymod$participants[country == "Belgium"]
# pm_contacts <- polymod$contacts
#
#
# # pm_contacts <- pm_contacts$contacts
#
# pm_part$part_age_group <- cut(pm_part$part_age,
#                               breaks = age_bins,
#                               right = FALSE)
#
# pm_part$part_age_group_census <-  cut(pm_part$part_age,
#                                       breaks = age_bins_census,
#                                       #labels = age_groups,
#                                       ordered_result = TRUE,
#                                       right = FALSE)
#
#
# pm_contacts_part <- merge(pm_contacts, pm_part, by = "part_id")
#
# data_path <- "data/be"
#
# saveRDS(pm_part, file = file.path(data_path, "polymod_participants.rds"))
# saveRDS(pm_contacts, file = file.path(data_path, "polymod_contacts.rds"))
# saveRDS(pm_contacts_part, file = file.path(data_path, "polymod_contacts_part.rds"))
#
# # ==========================+
# #
# #
# #
# pm_part <- polymod$participants[country == "Netherlands"]
#
# pm_contacts <- polymod$contacts
#
# pm_part$part_age_group <- cut(pm_part$part_age,
#                               breaks = age_bins,
#                               right = FALSE)
#
# pm_part$part_age_group_census <-  cut(pm_part$part_age,
#                                       breaks = age_bins_census,
#                                       #labels = age_groups,
#                                       ordered_result = TRUE,
#                                       right = FALSE)
#
#
# pm_contacts_part <- merge(pm_contacts, pm_part, by = "part_id")
#
# data_path <- "data/nl"
#
# saveRDS(pm_part, file = file.path(data_path, "polymod_participants.rds"))
# saveRDS(pm_contacts, file = file.path(data_path, "polymod_contacts.rds"))
# saveRDS(pm_contacts_part, file = file.path(data_path, "polymod_contacts_part.rds"))
#
