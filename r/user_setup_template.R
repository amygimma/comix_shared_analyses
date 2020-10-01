# 1. Customize this file to the file paths for your workflow,
# 2. Save as `r/user_setup.R` (this file will be ignored by github)

# CAUTION: Only set to true when using RSTUDIO Projects or setting wd each session
REMOVE_LS <- TRUE

# RStudio project path
USER_PROJECT_PATH <- getwd()

# Laptop user path
USER_LOCAL_PATH <- file.path("~", "..","amygimma")

# Filr folder path
USER_BASE_FILR_PATH <- file.path(USER_LOCAL_PATH, "Filr", "Net Folders",
                                 "EPH Shared", "Comix_survey")

# Base path do use for read/write as default
USER_BASE_PATH <- USER_BASE_FILR_PATH

# Folder for raw spss data files
USER_SPSS_PATH <- file.path(USER_BASE_FILR_PATH, "spss_files")

# Folder for clean data files (eg, `clean_participants.rds`), do not include country code
USER_DATA_PATH <- file.path(USER_BASE_FILR_PATH, "data")

# Folder for saving analysis outputs
USER_OUTPUTS_PATH <- file.path(USER_BASE_FILR_PATH, "outputs")

