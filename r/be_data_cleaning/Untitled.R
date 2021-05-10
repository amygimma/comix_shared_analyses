## dm_data_clean
library(data.table)

data_path <- "data"
# if (!is.null(USER_DATA_PATH) & !SAVE_LOCAL) data_path <- USER_DATA_PATH
# data_path <- USER_DATA_PATH

## Change object here for manual cleaning
if(!exists("country_code_")){
  country_code_ <- "be"
  panel_ <- "panel_b"
  wave_ <- "wave_1"
}
source('r/functions/V2_process_data.R')
source('r/functions/utility_functions.R')

survey <-
  readRDS(file.path(data_path, country_code_, panel_, wave_, "survey_data.rds"))

table(survey$Panel, survey$Wave, survey$Qcountry)
table(survey$Sampletype)


dt_ <- process_data(survey)

table(dt_$table_row)
