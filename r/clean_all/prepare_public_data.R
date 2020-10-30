library(data.table)
source("r/functions/utility_functions.R")

partall <- readRDS("data/uk/clean_participants.rds")
# Remove weeks 6 and 7 due to change in survey which has apparently influenced
# participant responses

partall[,
        nhs_regions := fcase(
          regions == "Scotland", "Scotland",
          regions == "Northern Ireland", "Northern Ireland",
          regions == "Wales", "Wales",
          regions == "Greater London", "London",
          regions == "South East",  "South East",
          regions == "South West", "South West",
          regions == "North West", "North West",
          regions == "West Midlands", "Midlands",
          regions == "East Midlands", "Midlands",
          regions == "North East", "North East and Yorkshire",
          regions == "Yorkshire and The Humber", "North East and Yorkshire",
          regions == "East of England", "East of England"

        )
]
table(partall$nhs_regions, useNA = "always")
# Remove responses for weeks 6 (D 1) and 7 (C 2) due to changes in survey design
ppub <- partall[!week %in% c(6,7)]
# Remove parent sample child selection which are obvious errors in age group entry
ppub <- ppub[!is.na(part_age_group)]

ppub <- ppub[, list(part_id, panel, wave, wave_id, week, survey_date, survey_weekday,
                    date, weekday, part_age_group, part_gender_nb, hh_size, hh_type,
                    area_rural_urban_label, nhs_regions, part_social_group,
                    part_employstatus, part_occupation, part_income, country_code)]
ppub[, part_social_group := gsub("  ", " ", part_social_group)]


cntall <- readRDS("data/uk/clean_contacts.rds")
cpub <- cntall[!week %in% c(6,7)]

# Remove contacts flagged as suspected non-contact or suspected multiple contact
# Assumption: When multiple contact fields are available, group contacts will be
# recorded there


# TODO:
# make cnt time group
# update hhm_contact_hn to cnt_household
cpub <- cpub[, cnt_other := ifelse(
  cnt_home == "No" & cnt_work == "No" & cnt_school == "No",
  "Yes", "No")]
cpub <- cpub[suspected_multiple_contact == 0 & suspected_non_contact == 0]
cpub <- cpub[, list(part_id, country_code, panel, wave, wave_id, week, cnt_age,
                    cnt_gender, cnt_type, cnt_frequency, cnt_home, cnt_work,
                    cnt_school, cnt_other, hhm_contact_yn, phys_contact,
                    individually_reported)]

saveRDS(ppub, "data/uk/public_participants.rds")
saveRDS(cpub, "data/uk/public_contacts.rds")

if (file.exists("r/user_setup.R")) source("r/user_setup.R")
if (exists("USER_DATA_PATH")) {
  saveRDS(ppub, file.path(USER_DATA_PATH, "uk", "public_participants.rds"))
  saveRDS(cpub, file.path(USER_DATA_PATH, "uk", "public_contacts.rds"))
}
