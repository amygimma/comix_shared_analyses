library(data.table)

if(file.exists("r/user_setup.R")) source("r/user_setup.R")
data_path <- "data"
if (!is.null(USER_DATA_PATH)) data_path <- USER_DATA_PATH

# =================
# =================
#   CLEAN & SAVE
# =================
# =================


SAVE_LOCAL <- TRUE

waves_list <- list(
  list(panel_ = "panel_e", wave_ = "wave_1", spss_ref_ = "PEW1"),
  list(panel_ = "panel_e", wave_ = "wave_2", spss_ref_ = "PEW2"),
  list(panel_ = "panel_e", wave_ = "wave_3", spss_ref_ = "PEW3"),
  list(panel_ = "panel_e", wave_ = "wave_4", spss_ref_ = "PEW4"),

  list(panel_ = "panel_f", wave_ = "wave_1", spss_ref_ = "PFW1"),
  list(panel_ = "panel_f", wave_ = "wave_2", spss_ref_ = "PFW2"),
  list(panel_ = "panel_f", wave_ = "wave_3", spss_ref_ = "PFW3")
)

scripts_path <- file.path("r", "V2_data_cleaning")
country_code_ <- "uk"
for (wave_list in waves_list) {
  # wave_list <- waves_list[[1]]
  panel_ <- wave_list$panel_
  wave_ <- wave_list$wave_
  spss_ref_ <- wave_list$spss_ref_
  message(spss_ref_)
  source(file.path(scripts_path, "dm01_rename_spss.R"))

  #Clean adult data
  message("Cleaning adult data")
  panel_ <- wave_list$panel_
  source(file.path(scripts_path, "dm02_data_clean.R"))

  # Clean child data
  message("Cleaning child data")
  panel_ <- paste0(panel_, "c")
  source(file.path(scripts_path, "dm02_data_clean.R"))
}

source(file.path("r", "V2_data_cleaning", "dm04_combine_survey_files.R"))
SAVE_LOCAL <- FALSE


