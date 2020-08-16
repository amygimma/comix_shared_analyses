# Run to add contact nickname and flag columns when needed (week 1-7)

library(foreign)
library(data.table)
library(here)

## Update to the latest data and then savem
## 
spss_country_path <- c("nl_be", "no", "uk")[1]
path <- file.path("data", "raw_data", "spss_files", spss_country_path)
spss_files <- list.files(path)
spss_files

# Change index here
spss_file_name <- spss_files[2]
spss_file_name
spss_file <- here(path, spss_file_name)
# spss_file <- here(path, "20-037762_PCW1_interim_v1_130520_ICUO_sav.sav")

df <- read.spss(spss_file)
dt <- as.data.table(df)
ncol(dt)
nrow(dt)


# get panel and wave
split_name <- strsplit(spss_file_name, "_")[[1]]

if (grepl(spss_file_name, "uk")) {
  dt[, Qcountry := split_name[2]]
  dt[, Panel := gsub("([a-z])([A-D])", "\\1 \\2", split_name[1])]
  # Needed when the wave is recorded as "Wave3" instead of "Wave 3"
  dt[, Wave := gsub("([a-z])([0-9])", "\\1 \\2", split_name[3])]
  table(dt$Wave, dt$Qcountry)
}
if (grepl("NLBE", spss_file_name)) {
  dt[, Qcountry := "NLBE"]
  dt[, Panel := "Panel A"]
  dt[, Wave := gsub("([a-z])([0-9])", "\\1 \\2", split_name[3])]
  table(dt$Wave, dt$Panel, dt$Qcountry)
}

data_path <- "data"

country_codes <- c("be", "nl", "no", "uk")[1]

# Create survey paths
for (country_code in country_codes) {
  panel_name <- tolower(gsub(" ", "_", as.character(dt$Panel[1])))
  wave_name <- tolower(gsub(" ", "_", as.character(dt$Wave[1])))
  survey_path <- file.path(data_path, country_code, panel_name, wave_name)
  
  # If re-running, use original_survey_data.rds
  original_data_filename <- c("original_survey_data.rds", "survey_data.rds")[1]
  
  survey <- readRDS(file.path(survey_path, original_data_filename))
  # if file does not exist, save to original survey data
  if (!("original_survey_data.rds" %in% list.files(survey_path))) {
    saveRDS(survey, file.path(survey_path, "original_survey_data.rds"))
  }
  
  country_dt <- dt[Respondent_ID %in% survey$Respondent_ID]
  raw_survey_path <- file.path(survey_path, "survey_data_new_cols.rds")
  
  # Save spss with new cols
  saveRDS(country_dt, raw_survey_path)
  message(paste("Saved to:", survey_path))
  
  
  # respondent_id column name has not been stable
  if (is.null(survey$Respondent_ID)) setnames(survey, "Respondent_id", "Respondent_ID")
  
  # CHECK DATA - check that the data has the correct number of rows
  sum(survey$Panel == country_dt$Panel) == nrow(survey)
  # CHECK DATA - check that the resp ids match
  sum(sort(as.numeric(survey$Respondent_ID)) == as.numeric(sort(country_dt$Respondent_ID))) == nrow(survey)
  
  survey[, Respondent_ID := as.numeric(Respondent_ID)]
  country_dt[, Respondent_ID := as.numeric(Respondent_ID)]
  country_dt[, Qcountry := toupper(country_code)]
  # CHECK DATA
  nl <- length(names(survey))
  cols <- names(survey)[grep("Q63_", names(survey))]
  
  # remove data from original survey before merging
  survey[, (cols) := NULL]
  
  # CHECK DATA
  length(names(survey)) == nl - length(cols)
  grep("Q63_", names(survey), value = T)
  
  contactflagcols <- paste0("contact", 1:100)
  survey[, (contactflagcols) := NULL]
  table(country_dt$Wave,country_dt$Panel, country_dt$Qcountry)
  table(survey$Wave,survey$Panel, survey$Qcountry)

  if (grepl(spss_file_name, "uk")){
  survey_merged <- merge(survey, country_dt,
                         by = c("Respondent_ID", "Qcountry", "Panel", "Wave", 
                                "resp_age", "resp_gender"))
  } else {
  survey_merged <- merge(survey, country_dt,
                         by = c("Respondent_ID", "Qcountry", "Panel", "Wave"))
  }
  
  grep("Q63_", names(survey_merged), value = T)
  grep("contact", names(survey_merged), value = T)
  
  saveRDS(survey_merged, file.path(survey_path, "survey_data.rds"))
}
