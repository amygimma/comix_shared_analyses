library(data.table)

## Change object here for manual cleaning
if(!exists("country_code_")){
  country_code_ <- "uk"
  panel_ <- "panel_e"
  wave_ <- "wave_"
}
source('r/functions/utility_functions.R')

if(file.exists("r/user_setup.R")) source("r/user_setup.R")

data_path <- "data"
if (!is.null(USER_DATA_PATH) & !SAVE_LOCAL) data_path <- USER_DATA_PATH

survey <-
  readRDS(file.path(data_path, country_code_, panel_, wave_, "full_survey_data.rds"))
table(survey$Panel, survey$Wave, survey$Qcountry)

full_survey <- survey

child_qs <- grep("QP", names(survey), value = T)
child_cs <- grep("Pcontact", names(survey), value = T)
child_cols <- c(child_qs, child_cs)

adult_qs <- grep("^Q[0-9]+|NewQ", names(survey), value = T)
adult_qs <- grep("^Q23|Q20|NewQP", adult_qs, value = T, invert = T)
adult_cs <- grep("^contact[0-9]+", names(survey), value = T)
adult_cols <- c(adult_qs, adult_cs)


adult_survey <- survey[as.character(Sampletype) == "Sampletype=1 Main sample"]
adult_survey <- adult_survey[, -child_cols, with = F]

child_survey <- survey[as.character(Sampletype) == "Sampletype=2 Parent sample"]
child_survey <- child_survey[, -adult_cols, with = F]
child_survey[, Panel := paste0(Panel, "C")]
table(child_survey$Panel)
grep("QP54", names(child_survey), value = T) # 1

new_names_ec <- gsub("QP", "Q", names(child_survey))
new_names_ec <- gsub("Pcontact", "contact", new_names_ec)
new_names_ec[duplicated(new_names_ec)] # 0

names(child_survey) <- new_names_ec
setnames(child_survey, old = names(child_survey), new = new_names_ec)
names(child_survey)[duplicated(names(child_survey))] # 0
child_survey[, survey_type := "child"]

# SAVE RDS FILES
panel_ <- gsub(" ", "_", tolower(adult_survey$Panel[1]))
wave_ <- gsub(" ", "_", tolower(adult_survey$Wave[1]))

dir.create(file.path(data_path, country_code_, panel_, wave_), showWarnings = F)
saveRDS(full_survey,
        file.path(data_path, country_code_, panel_, wave_, "original_survey_data.rds"))
saveRDS(adult_survey,
        file.path(data_path, country_code_, panel_, wave_, "survey_data.rds"))

panel_ <- paste0(panel_, "c")
dir.create(file.path(data_path, country_code_, panel_, wave_), recursive = T)
dir.create(path = file.path(data_path, country_code_, panel_, wave_), showWarnings = F)
saveRDS(child_survey,
        file.path(data_path, country_code_, panel_, wave_, "survey_data.rds"))


# # Check for errors [Keep for reference]

# grep("QP54", adult_cols, value = T) # 0
# grep("Q54", adult_cols, value = T) # 1
# grep("Q23", adult_cols, value = T) # 0

# grep("QP66_LOOP_94_QP71_9", names(child_survey), value = T) # 0
# grep("Q66_LOOP_94_Q71_9", new_names_ec , value = T) # 1
# grep("contact900", names(child_survey), value = T) # 1
# grep("contact900", new_names_ec , value = T) # 1
# grep("Q66_LOOP_94_Q71_9", names(child_survey), value = T) # 1
# grep("contact900", adult_cols , value = T) # 1
# #
# grep("Q23_LOOP_75_Q26", names(adult_survey), value = T) # 5
# grep("Q23_LOOP_75_Q26", names(child_survey), value = T) # 5


