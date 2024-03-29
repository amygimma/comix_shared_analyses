library(data.table)
source("r/user_setup.R")
data_path <- "data"
user_data_path <- USER_DATA_PATH

country_codes <- "uk"
source("r/V1_data_cleaning/00_run_all_data_cleaning.R")
source("r/V1_data_cleaning/dm04_combine_survey_files.R")

pad <- readRDS(file.path(data_path, "uk", "panels_a_d", "clean_participants.rds"))
table(pad$wave, pad$panel)

cad <- readRDS(file.path(data_path, "uk", "panels_a_d", "clean_contacts.rds"))
table(cad$wave, cad$panel)

had <- readRDS(file.path(data_path, "uk", "panels_a_d", "clean_households.rds"))
table(had$wave, had$panel)
table(cad$cnt_nickname_flag, cad$wave_id)

source("r/V2_data_cleaning/00_run_all_data_cleaning.R")
source("r/V2_data_cleaning/dm04_combine_survey_files.R")

pef <- readRDS(file.path(data_path, "uk", "panels_e_f", "clean_participants.rds"))
table(pef$wave, pef$panel)

cef <- readRDS(file.path(data_path, "uk", "panels_e_f", "clean_contacts.rds"))
table(cef$wave, cef$panel)

hef <- readRDS(file.path(data_path, "uk", "panels_e_f", "clean_households.rds"))
table(hef$wave, hef$panel)




pall <- rbind(pad, pef, fill = T)
table(pall$wave, pall$panel)

cntall <- rbind(cad, cef, fill = T)
table(cntall$wave, cntall$panel)

hhall <- rbind(cad, cef, fill = T)
table(hhall$wave, hhall$panel)

saveRDS(pall, "data/uk/clean_participants.rds")
saveRDS(pall, file.path(user_data_path, "uk/clean_participants.rds"))

saveRDS(cntall, "data/uk/clean_contacts.rds")
saveRDS(cntall, file.path(user_data_path, "uk/clean_contacts.rds"))

saveRDS(hhall, "data/uk/clean_households.rds")
saveRDS(hhall, file.path(user_data_path, "uk/clean_households.rds"))

