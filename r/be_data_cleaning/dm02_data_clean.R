## dm_data_clean
library(data.table)

data_path <- "data"
# if (!is.null(USER_DATA_PATH) & !SAVE_LOCAL) data_path <- USER_DATA_PATH
# data_path <- USER_DATA_PATH
## Change object here for manual cleaning
if(!exists("country_code_")){
  country_code_ <- "be"
  panel_ <- "panel_b"
  wave_ <- "wave_3"
}
source('r/functions/V2_process_data.R')
source('r/functions/utility_functions.R')

survey <-
  readRDS(file.path(data_path, country_code_, panel_, wave_, "survey_data.rds"))
qp_names <- grep("qP", names(survey), value = T)
q_names <- gsub("qP", "q", qp_names)

setnames(survey, old = qp_names, new = q_names)


table(survey$Panel, survey$Wave, survey$Qcountry)
table(survey$Sampletype)


dt_ <- process_data(survey)

table(dt_$table_row, useNA = "always")

# Re-calculate the hhm added by particpants in the children's surveys to assign
# hhm_ids to over 1000  (new houshold members are originally assigned variables
# of 150 - 200) to group with the adult particpant which is recorded as 999
dt_[, table_row :=
      ifelse(table_row >= 150 & table_row <= 200, table_row - 150 + 1000, table_row)]

# Check number of participants is correct
n_participants_check <- length(unique(dt_$respondent_id))
# use hhcomp_remove column to set difference household contacts for adults vs children
if (is.null(dt_$hhcomp_remove)) {
  # Adult household check and first round children's household check

  n_households_check <- sum((dt_$table_row <= 19 |
                               dt_$table_row >= 999) & dt_$table_row != 0,
                            na.rm = T)
} else {
  # household check - adult surveys:  hhm <= 19
  #  children's surveys: account for the adult participant (999) and household
  #  members added after the first round (1000 or over) AND remove household
  #  members which have been removed by the participant
  #

  n_households_check <- sum(
    ((dt_$table_row <= 19 | dt_$table_row >= 999 & dt_$table_row != 0)) &
      (is.na(dt_$hhcomp_remove)  | ( dt_$hhcomp_remove == "No")), na.rm = T)
}

# on children's survey, table_row 999 is the responder, and would be counted twice
n_contacts_check <- sum((dt_$table_row < 999 & dt_$table_row >= 19) |
                          dt_$q62 == "Yes", na.rm = TRUE)

print(paste("Participants:", n_participants_check))
print(paste("Households:", n_households_check))
print(paste("Contacts:", n_contacts_check))

## Check variables names
varnames <- as.data.table(read.csv("codebook/be_var_names_v2.csv"))


change_names <- function(df, varnames, c_code) {
  loops <- grep(
    "loop|contact[1-100]|contact[900-998]|hhcompremove_[0-9]|hhcompadd_[0-9]|_i$",
    names(df), value = T)
  df <- df[, -loops, with = F]

  c_codes <- c(be = "be", uk = "uk", nl = "nl")
  remove <- paste(setdiff(c_codes, c_code), collapse = "|")
  keep <- grep(remove, varnames$var, invert = T, value = T)
  varnames <- varnames[var %in% keep]
  # browser()
  index_name <- match(names(df), varnames$var)
  index_name <- index_name[!is.na(index_name)]
  df <- df[, varnames$var[index_name], with = FALSE]
  index_name <- match(names(df), varnames$var)
  index_name <- index_name[!is.na(index_name)]
  new_names <- as.character(varnames$new_name[index_name])
  old_names <- names(df)

  matched <- grep("[0-9]|[a-z]", new_names)
  new_names <- new_names[matched]
  old_names <- old_names[matched]
  df <- df[, old_names, with = F]

  names(df) <- new_names

  # setDT(df)
  # setcolorder(df, new_names)
  df
}

if (is.null(dt_$q20)) dt_$q20 <- dt_$q20_new

dt <- change_names(dt_, varnames, tolower(dt_$qcountry))


dt[, part_id := fcase(
  panel == "Panel A", 10000 + part_id,
  panel == "Panel B", 20000 + part_id,
  panel == "Panel BC", 30000 + part_id

)]

dt[, part_age := as.numeric(part_age)]


dt[,   country := fcase(
  as.character(country_code) == "UK", "United Kingdom",
  as.character(country_code) == "BE", "Belgium",
  as.character(country_code) == "NL", "Netherlands",
  as.character(country_code) == "NO", "Norway"
)]


## Format dates
dt[row_id == 0, survey_date := as.Date(paste0(year,"-", month,"-", day ))]

#  Date is contact date to match POLYMOD for use in socialmixr
dt[, date := survey_date - 1]

part_dates <- dt[row_id == 0, list(part_id, date)]
dt[, date := NULL]
dt <- merge(dt, part_dates, by = "part_id")
dt[row_id != 0, survey_date := date + 1]
dt[, survey_weekday := weekdays(survey_date)]
dt[, weekday := weekdays(date)]
# add week number
# dt <- add_week_number(dt)
dt[ , wave_id := toupper(paste(gsub("panel ", "", tolower(as.character(panel))),
                               gsub("wave ", "", tolower(as.character(wave)))))]
dt[!is.na(survey_date), week := week(min(survey_date))]

table(dt$survey_weekday, useNA = "always")
table(dt$weekday, useNA = "always")
table(dt$week, useNA = "always")

# HH Sizes
table(dt$hh_size, useNA = "always")
dt[!is.na(hh_size), hh_size := fcase(
  hh_size == "11 or more", 12,
  hh_size == "None", 1,
  as.numeric(hh_size) %in% seq(1,10), as.numeric(hh_size))
]
table(dt$hh_size, useNA = "always")

# Save as factor to be clear that 12 is "12 or more"
dt[!is.na(hh_size), hh_size := factor(hh_size,
                                      levels = seq(1,12,1),
                                      labels = c("1","2", "3", "4", "5", "6", "7", "8", "9",
                                                 "10", "11", "12 or more"))]
table(dt$hh_size)


# cnt/hhm age groups
dt[is.na(cnt_age), cnt_age := hhm_age_group]
dt[is.na(cnt_gender), cnt_gender := hhm_gender]


# Part age groups
### Do not change these age bands as they are fixed in the survey.
## Add in min and max age group
age_groups <- c("Under 1", "1-4", "5-9", "5-11", "10-14", "12-15", "15-19", "16-17", "18-19", "20-24",
                "25-34", "35-44", "45-54", "55-64", "65-69", "70-74",
                "75-79", "80-84", "85+")
age_min <- c(0, 1,  5,  5, 10, 12, 15, 16, 18, 20, 25, 35, 45, 55, 65, 70, 75, 80, 85)
age_max <- c(1, 4,  9, 11, 14, 15, 19, 17, 19, 24, 34, 44, 54, 64, 69, 74, 79, 84, 100)


for(i in 1:length(age_groups)){
  dt[cnt_age == age_groups[i], "cnt_age_est_min"] <- age_min[i]
  dt[cnt_age == age_groups[i], "cnt_age_est_max"] <- age_max[i]

}


age_bins <- c(0, 5, 13, 18, 30, 40, 50, 60, 70, 120)
dt[ ,part_age_group := cut(part_age,
                           breaks = age_bins,
                           right = FALSE)]
table(dt$part_age_group, useNA = "always")

## Create ID for contacts and households members

if (dt$sample_type[1] == "Sampletype=2 Parent sample") {
  dt[row_id != 0 | hhm_contact_yn == "Yes", cont_id := paste0(part_id ,"-", row_id, "-", week)]

}

dt[row_id != 0, cont_id := paste0(part_id ,"-", row_id, "-", week)]

if (dt$panel[1] %in% c("Panel EC", "Panel FC")) {
  dt[row_id == 0 & hhm_contact_yn == "Yes", cont_id := paste0(part_id ,"-", row_id, "-", week)]
}

if (!is.null(dt$hhcomp_remove)) {
  # Children's data cleaning: hh_size can be up to 18, remove household contacts
  # which have been removed by the participant (indicated by hhcomp_remove).
  # Adult particpant and new hhm have a row id of >= 999
  # check
  dt[hhcomp_remove == "Yes", cont_id := NA]
  dt[(row_id <= 19 | row_id >= 999) & (is.na(hhcomp_remove) |
                                         hhcomp_remove != "Yes"), hhm_id := row_id]
} else {
  # Adult survey asked for hhm each time, so limit is 11 and none are removed
  dt[row_id <= 19 | row_id >= 999, hhm_id := row_id]
}

if (survey$Panel[1] %in% c("Panel C", "Panel D",  "Panel EC", "Panel FC")) {
  # Participant is recorded as both hhm_id = 0 and hhm_id = 999 depending on question
  # Assign part age and gender to contact 999 (hhm_id of participant)
  dt[hhm_id == 999, hhm_age_group := gsub(",", "-", gsub("\\[|\\)", "", part_age_group))]
  dt[hhm_id == 999, cnt_age := gsub(",", "-", gsub("\\[|\\)", "", part_age_group))]
  dt[hhm_id == 999, hhm_gender := gsub(",", "-", gsub("\\[|\\)", "", part_gender))]
  dt[hhm_id == 999, cnt_gender := gsub(",", "-", gsub("\\[|\\)", "", part_gender))]
  # Assign hhm_id 0 attributes to 999
  hhm_cols <- grep("hhm_", names(dt), value = T)
  remove <- c("hhm_id", "hhm_age_group", "hhm_gender", "child_hhm_select_raw",
              "hhm_contact_yn")
  hhm_cols <- setdiff(hhm_cols, remove)
  for (colname in hhm_cols) {
    dt[hhm_id == 999, eval(colname) := dt[part_id == part_id & hhm_id == 0][[colname]]]
  }
}


# Physical contact
dt[,   phys_contact := fcase(
  cnt_phys == "Yes", 1,
  cnt_phys == "No", 0,
  default = NA_real_)]

## Split the data into participants, households, and contacts
part <- dt[hhm_id == 0 & !is.na(part_age)]
nrow(part)
vars_remove <- "cnt|phys|cont_id|hhm_gender|hhm_employstatus|hhm_id|hhm_age_group"
hhm_vars_remove <- c("hhm_student", "contact_yn")
vars_remove <- paste(c(vars_remove, hhm_vars_remove), collapse = "|")
# This keeps the mult_contacts_*_phys columns
keep <- names(dt)[grep("work_cnt_|other_cnt_|ecdc_", names(part))]

part_vars <- names(part)[grep(vars_remove, names(part), invert = TRUE)]
part_vars <- union(part_vars, keep)

part <- part[, part_vars, with = FALSE]

names(part) <- gsub("hhm_", "part_", names(part))

nrow(part)


# Remove participants and household member who were not contacts
contacts <- dt[!is.na(cont_id)]
contacts <- contacts[!(is.na(hhm_contact_yn) & !is.na(hhm_id))]
contacts[is.na(hhm_contact_yn), hhm_contact_yn := "Contact"]
contacts <- contacts[hhm_contact_yn %in% c("Yes", "Contact") ]

contacts_names <- names(contacts)[grep("cnt|phys_contact", names(contacts))]
contacts_names <- c(contacts_names, "part_id", "date", "panel", "wave",
                    "wave_id", "country",  "cont_id", "country_code", "hhm_id",
                    "week", "weekday", "hhm_contact_yn")


contacts <- contacts[, contacts_names, with = FALSE]
contacts[, individually_reported := 1]


### Clean, Reshape, and Merge Multiple Contacts
######################################################


mult_contacts_cols <- grep("multiple_work_cnt_belgium", names(dt), value = TRUE)
# mult_contacts_cols <- grep("precautions", mult_contacts_cols, value = TRUE, invert = T)
total_mult_contacts <- 0
if (length(mult_contacts_cols) > 0) {
  dt[, (mult_contacts_cols) :=  lapply(.SD, as.numeric), .SDcols = mult_contacts_cols]
  part[, (mult_contacts_cols) :=  lapply(.SD, as.numeric), .SDcols = mult_contacts_cols]

  mult_contact_detail_cols <-  c("work_cnt_under_12", "work_cnt_12_to_17",
                                 "work_cnt_18_to_64", "work_cnt_65_plus", "work_cnt_dont_know",
                                 "work_cnt_duration", "work_cnt_distanced", "work_cnt_barrier",
                                 "work_cnt_wash_hands", "work_cnt_high_risk")

  # mult_contacts_cols <- c(mult_contacts_cols, mult_contact_detail_cols)

  id_cols <- c("part_id", "date", "panel", "wave", "wave_id", "country",
               "country_code", "week", "weekday", "hhm_contact_yn",
               "part_work_cnt_over_20")
  id_cols <- c(id_cols, mult_contact_detail_cols)

  mult_contacts <- dt[part_work_cnt_over_20 == "Yes" & !is.na(multiple_work_cnt_belgium),
                      c(id_cols, mult_contacts_cols), with = FALSE]

  mult_contacts <- melt(mult_contacts,
                        measure.vars = mult_contacts_cols,
                        id_cols = id_cols)
  mult_contacts[is.na(value), value := 0]
  total_mult_contacts <- sum(as.numeric(mult_contacts$value))

  if (length(mult_contacts_cols) == 0) {
    total_mult_contacts <- 0
  } else {
    # Add contacts columns
    # mult_contacts <- mult_contacts[, cont_id :=
    #                                  paste(part_id, (2000 + 1:.N), week, sep = "-"), by = c("part_id")]
    mult_contacts[, cnt_work := ifelse(grepl("work", variable), "Yes", "No")]
    mult_contacts[, cnt_school := ifelse(grepl("school", variable), "Yes", "No")]
    mult_contacts[, cnt_other := ifelse(grepl("other", variable), "Yes", "No")]
    mult_contacts[, phys_contact := NA_real_]
    # Assuming contacts at work are not home contacts (or they would hopefully be listed individually)
    mult_contacts[, cnt_home := "No"]
    # Assuming contacts at work are not household contacts (or they would hopefully be listed individually)
    mult_contacts[, hhm_contact_yn := "No"]
    mult_contacts[, individually_reported := 0]

    # Create new contact rows as reported
    mult_contact_rows <- mult_contacts[, list(variable = rep(variable, each = value)),by = names(mult_contacts)]
    mult_contact_rows[, individually_reported := 0]

    if(nrow(mult_contact_rows) != total_mult_contacts) {
      stop("Check total number of multiple contact rows")
    }

    bind_cols <- intersect(names(contacts), names(mult_contact_rows))

    contacts <- rbind(contacts, mult_contact_rows[, c(bind_cols), with = FALSE],
                      fill = T)
    contacts[, phys_contact := as.numeric(phys_contact)]
    nrow(contacts)
  }
}

### Households
###################################

# hh members are indicated by table_row id of 18 or less or 999 (parent in child) (see above)
households <- dt[!is.na(hhm_id) ]
mult_contacts_cols <- grep("multiple_contacts_", names(part), value = TRUE)

### Child surveys
###################################
if (as.character(part$panel[1]) %in% c("Panel C", "Panel D", "Panel EC", "Panel FC")) {
  source("r/V2_data_cleaning/dm02a_child_data_clean.R")

  hh_id_cols <- c("part_id", "survey_date", "date", "panel", "wave", "wave_id",
                  "country", "country_code", "week", "survey_weekday",
                  "weekday", "cont_id", "child_participant")
} else {
  part[, survey_type := "adult"]

  hh_id_cols <- c("part_id", "survey_date", "date", "panel", "wave", "wave_id",
                  "country", "country_code", "week", "survey_weekday",
                  "weekday", "cont_id")
}


### Child surveys
###################################
if (as.character(part$panel[1]) %in% c("Panel BC")) {
  source("r/V2_data_cleaning/dm02a_child_data_clean.R")

  hh_id_cols <- c("part_id", "survey_date", "date", "panel", "wave", "wave_id",
                  "country", "country_code", "week", "survey_weekday",
                  "weekday", "cont_id", "child_participant")
} else {
  part[, survey_type := "adult"]

  hh_id_cols <- c("part_id", "survey_date", "date", "panel", "wave", "wave_id",
                  "country", "country_code", "week", "survey_weekday",
                  "weekday", "cont_id")
}

### Households continued
###################################

hhm_cols <- sort(grep("hhm_", names(households), value = T))
households <- households[, c(hh_id_cols, hhm_cols), with = F]

#Normalize contact nickname flags
###################################

contacts[, cnt_nickname_flag := ifelse(
  cnt_nickname_flag == "multiple names given", "suspected multiple contact", cnt_nickname_flag
)]

contacts[, cnt_nickname_flag := ifelse(
  cnt_nickname_flag == "no contact", "suspected non contact", cnt_nickname_flag
)]

# Add total contacts and nickname flags
###################################
contacts[, individual_identified := fcase(
  cnt_nickname_flag == "individual identified", 1, default = 0
)]
contacts[, potential_hhm := fcase(
  cnt_nickname_flag == "potential household member", 1, default = 0
)]
contacts[, suspected_multiple_contact := fcase(
  cnt_nickname_flag == "suspected multiple contact", 1, default = 0
)]
contacts[, suspected_non_contact := fcase(
  cnt_nickname_flag == "suspected non contact", 1, default = 0
)]


part <- add_n_cnts_location_cols(part, contacts)

# if (!(as.character(part$panel[1]) == "Panel C" & part$wave[1] == "Wave 1")){
#   part <- part_add_nickname_flag_count(part, contacts)
# }
contacts[, cnt_nickname_masked := as.character(cnt_nickname_masked)]

# data_path <- "data"
# if (!is.null(USER_DATA_PATH)) data_path <- USER_DATA_PATH
panel_name <- tolower(gsub(" ", "_", as.character(dt$panel[1])))
wave_name <- tolower(gsub(" ", "_", as.character(dt$wave[1])))
country_code <- tolower(dt$country_code[1])
data_path <- "data"
survey_path <- file.path(data_path, country_code, panel_name, wave_name)



if (!file.exists(survey_path)) {
  if(!file.exists(file.path(data_path, country_code))) {
    dir.create(file.path(data_path, country_code))
  }
  if(!file.exists(file.path(data_path, country_code, panel_name))) {
    dir.create(file.path(data_path, country_code, panel_name))
  }
  if(!file.exists(file.path(data_path, country_code, panel_name, wave_name))) {
    dir.create(file.path(data_path, country_code, panel_name, wave_name))
  }
}


print(paste(panel_, wave_, "cleaned"))
if (part$survey_type[1] == "adult") {
  v2p <- c("Panel E", "Panel F")
  if (part$panel[1] %in% v2p & unique(part$sample_type) != "Sampletype=1 Main sample") {
    # Only panels E and F are sent with adults and children combined
    stop("Error: Sampletype=2 detected in adult survey - remove to proceed")
  }
  if(n_participants_check != nrow(part)){
    stop("Check total number of participant rows")
  }
  if(n_households_check != nrow(households)){
    stop("Check total number of household rows")
    table(households$hhm_contact_yn, useNA  = "always")
    table(households$hhm_id, useNA  = "always")
    table(households[hhm_id != 0 &  is.na(hhm_contact_yn)]$hhm_id, useNA  = "always")
  }
  if(n_contacts_check + total_mult_contacts != nrow(contacts)){
    stop("Check total number of contact rows")
  }
  message("Adult survey checked")
}

if (part$survey_type[1] == "child") {

  v2p <- c("Panel BC")
  if (part$panel[1] %in% v2p & unique(part$sample_type) !=  "Sampletype=2 Parent sample") {
    # Only panel BE-B, UK-E,UK-F sent with adults and children combined
    stop("Error: Sampletype=1 detected in child survey - remove to proceed")
  }
  if(n_participants_check != nrow(part)){
    stop("Check total number of participant rows")
  }

  if(n_households_check != nrow(households)){
    stop("Check total number of household rows")

  }

  if(n_contacts_check + total_mult_contacts != nrow(contacts)){
    stop("Check total number of contact rows")
  }
  if(!(999 %in% unique(contacts$hhm_id) | 0 %in% unique(contacts$hhm_id))) {
    stop("Adult participant not included in contacts")
  }
  message("Child survey checked")

}

# Print the survey path to use in r/dm_data_checks.R
message(survey_path)

if(!file.exists(survey_path)) stop("Date folder not created")
saveRDS(part, file = file.path(survey_path, "clean_participants.rds"))

saveRDS(contacts, file = file.path(survey_path, "clean_contacts.rds"))

saveRDS(households, file = file.path(survey_path, "clean_households.rds"))

