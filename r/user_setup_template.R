# 1. Customize this file to the file paths for your workflow,
# 2. Save as `r/user_setup.R` (this file will be ignored by github)

REMOVE_LS <- TRUE

# Base paths
USER_BASE_PATH <- getwd()
USER_BASE_FILR_PATH <- file.path("~", "..", "..", "Users", "amygimma", "Filr",
                                 "Net Folders", "EPH Shared", "Comix_survey")

# Folder for clean data files (eg, `clean_participants.rds`), do not include country code
USER_DATA_PATH <- file.path(USER_BASE_FILR_PATH, "data")

# Folder for saving analysis outputs
USER_OUTPUTS_PATH <- file.path(USER_BASE_FILR_PATH, "outputs")