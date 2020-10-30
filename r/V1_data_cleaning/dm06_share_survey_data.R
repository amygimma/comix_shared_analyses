# Create password protected files
#

library(data.table)
# NOTES:
# To zip files into a password protected file, open the terminal and
# move into the directory that contains the files (
# `cd path/contact_survey_2020/data/sharing/nl`) and enter
# `7z a ../CoMix_NL_W1_W3.7z * -pIEw82u3**jbs3  - replacing the characters after -p with
# a secure password (no space after -p)
#


nl_data <- file.path("data", "nl")
nl_files <- c("clean_participants.rds", "clean_contacts.rds", "clean_households.rds")
share_path_nl <- "data/sharing/nl"
#
for( file_name in nl_files) {
  dt <- readRDS(file.path(nl_data, file_name))
  print(file_name)
  table(dt$wave, dt$panel, dt$country)

  saveRDS(dt, file.path(share_path_nl, file_name))
  write.csv(dt, file.path(share_path_nl, gsub("rds", "csv", file_name)),
            row.names = FALSE)
}
#
#

be_data <- file.path("data", "be")

be_files <- c("clean_participants.rds", "clean_contacts.rds", "clean_households.rds")
share_path_be <- "data/sharing/be"
#
for( file_name in be_files) {
  dt <- readRDS(file.path(be_data, file_name))
  print(file_name)
  table(dt$wave, dt$panel, dt$country)

  saveRDS(dt, file.path(share_path_be, file_name))
  write.csv(dt, file.path(share_path_be, gsub("rds", "csv", file_name)),
            row.names = FALSE)
}
#
#

no_data <- file.path("data", "no")

no_files <- c("clean_participants.rds", "clean_contacts.rds", "clean_households.rds")
share_path_no <- "data/sharing/no"
#
for( file_name in no_files) {
  dt <- readRDS(file.path(no_data, file_name))
  print(file_name)
  # browser()
  table(dt$wave, dt$panel, dt$country)

  saveRDS(dt, file.path(share_path_no, file_name))
  write.csv(dt, file.path(share_path_no, gsub("rds", "csv", file_name)),
            row.names = FALSE)
  message(file.path(share_path_no, file_name))
}

