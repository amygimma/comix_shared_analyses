## dm_data_clean
library(data.table)

## Change object here for manual cleaning
if(!exists("country_code_")){
  country_code_ <- "uk"
  panel_ <- "panel_e"
  wave_ <- "wave_1"
}
source('r/functions/process_data.R')
source('r/functions/utility_functions.R')

# survey <-
#   readRDS(file.path("data", country_code_, panel_, wave_, "survey_data.rds"))
# table(survey$Panel, survey$Wave, survey$Qcountry)

# survey <- child_survey
# panel_ <- "panel_ec"

survey <- adult_survey

# only for panel D 1 due to participants responding twice - new raw data coming and will remove
if (survey$Panel[1] == "Panel D" & survey$Wave[1] == "Wave 1"){
  survey[, minDay := min(.SD), .SDcols = "CurrentDay", by = "Respondent_ID"]

  survey <- survey[CurrentDay == minDay]
}

if (grepl(survey$Qcountry[1], "Scotland")) {
  stop("Scottish survey")
} else if (grepl("A|B|C|D", survey$Panel)) {
  dt_ <- process_data(survey)
} else if (grepl("E", survey$Panel)) {
  # dt_ <- process_data_v2(survey)
  dt_ <- process_data(survey)

}
# Re-calculate the hhm added by particpants in the children's surveys to assign
# hhm_ids to over 1000  (new houshold members are originally assigned variables
# of 150 - 156) to group with the adult particpant which is recorded as 999
#
# dt0 <- dt_
dt_
table(dt_$table_row)
# if (dt_$panel[1] == "Panel E") {
#   dt_[table_row >= 900 & table_row < 999 , table_row := table_row - 900]
# }

if (dt_$panel[1] %in% c("Panel E", "Panel EC")) {
  dt_[table_row >= 900 & table_row < 999 , table_row := table_row - 880]
}


table(dt_$table_row)

dt_[, table_row :=
      ifelse(table_row >= 150 & table_row <= 170, table_row - 150 + 1000, table_row)]
dt_[is.na(table_row)]$table_row


table(dt_$qcountry)


# Check number of participants is correct
n_participants_check <- length(unique(dt_$respondent_id))
# use hhcomp_remove column to set difference household contacts for adults vs children
if (is.null(dt_$hhcomp_remove)) {
  # Adult household check and first round children's household check
  n_households_check <- sum((dt_$table_row <= 11 |
                               dt_$table_row >= 999) & dt_$table_row != 0)

  n_households_check <- sum((dt_$table_row <= 11 |
                               dt_$table_row >= 999) & dt_$table_row != 0,
                            na.rm = T)
} else {
  # household check - adult surveys:  hhm <= 11
  #  children's surveys: account for the adult participant (999) and household
  #  members added after the first round (1000 or over) AND remove household
  #  members which have been removed by the participant
  #
  n_households_check <- sum(
    ((dt_$table_row <= 11 | dt_$table_row >= 999) & dt_$table_row != 0) &
      (is.na(dt_$hhcomp_remove) | is.na(dt_$hhcomp_remove) | ( dt_$hhcomp_remove == "No")), na.rm = T)
}

# on children's survey, table_row 999 is the responder, and would be counted twice
n_contacts_check <- sum((dt_$table_row < 999 & dt_$table_row > 11) |
                          dt_$q62 == "Yes", na.rm = TRUE)

print(paste("Participants:", n_participants_check))
print(paste("Households:", n_households_check))
print(paste("Contacts:", n_contacts_check))

## Check variables names
varnames <- as.data.table(read.csv("codebook/var_names_v2.csv"))
# dt_names <- data.table(varnames_v2 = unique(grep("_loop_", names(dt_) ,invert = T, value = T)))
# dt_names <- data.table(varnames_v2 = unique(gsub("_[0-9]+_","_XX_", names(dt_))))

# nrow(dt_names)
# varnames1 <- merge(varnames, dt_names, by.x = "var", by.y = "varnames_v2", all = T)
# varnames1 <- fwrite(varnames1, "codebook/var_names_new.csv")


change_names <- function(df, varnames, c_code) {
  loops <- grep("loop|contact[1-100]|contact[900-998]|hhcomp|_i$", names(df), value = T)
  df <- df[, -loops, with = F]

  c_codes <- c(be = "be", uk = "uk", nl = "nl")
  remove <- paste(setdiff(c_codes, c_code), collapse = "|")
  keep <- grep(remove, varnames$var, invert = T, value = T)
  varnames <- varnames[var %in% keep]

  index_name <- match(names(df), varnames$var)
  index_name <- index_name[!is.na(index_name)]
  df <- df[, varnames$var[index_name], with = FALSE]
  index_name <- match(names(df), varnames$var)
  index_name <- index_name[!is.na(index_name)]
  new_names <- as.character(varnames$new_name[index_name])
  old_names <- names(df)
  # browser()

  matched <- grep("[0-9]|[a-z]", new_names)
  new_names <- new_names[matched]
  old_names <- old_names[matched]
  df <- df[, old_names, with = F]

  names(df) <- new_names

  # setDT(df)
  # setcolorder(df, new_names)
  df
}

# Check for extra cols in the new column files (will clean this)
if (!is.null(dt_$resp_age.x)){
  dt_[, resp_age := resp_age.x]
}
if (!is.null(dt_$resp_gender.x)) {
  dt_[, resp_age := resp_age.x]
}


if (is.null(dt_$q20)) dt_$q20 <- dt_$q20_new
dt <- change_names(dt_, varnames, tolower(dt_$qcountry))


dt[, part_id := fcase(
  panel == "Panel A", 10000 + part_id,
  panel == "Panel B", 20000 + part_id,
  panel == "Panel C", 30000 + part_id,
  panel == "Panel D", 40000 + part_id,
  panel == "Panel E", 50000 + part_id,
  panel == "Panel EC", 60000 + part_id

)]
table(dt$part_id)
dt[, part_age := as.numeric(part_age)]


dt[,   country := fcase(
  as.character(country_code) == "UK", "United Kingdom",
  as.character(country_code) == "BE", "Belgium",
  as.character(country_code) == "NL", "Netherlands",
  as.character(country_code) == "NO", "Norway"
)]

# add week number
dt <- add_week_number(dt)

## Format dates
dt[, survey_date := as.Date(paste0(year,"-", month,"-", day ))]
dt[, date := survey_date - 1]

# cols <- names(dt)[grep("work_date", names(dt))]
#
# if(dt$country_code[1] == "UK" & dt$week[1] < 4) {
#   dt[, (cols) :=
#        lapply(.SD, as.Date, format = "%d/%m/%Y"),
#      .SDcols = cols
#      ]
# } else {
#
#   dt[, (cols) :=
#        lapply(.SD, as.Date, format = "%A, %d %B"),
#      .SDcols = cols
#      ]
# }

# table(dt$hhm_avoid_work_date1)

# if(dt$country_code[1] != "UK") {
#   dt$hhm_visit_urgent_date <- NA_real_
# }
# cols <- c("hhm_seek_gov_info_date",
#           "hhm_phone_gp_date",
#           "hhm_visit_gp_date",
#           "hhm_visit_urgent_date",
#           "hhm_visit_ae_date",
#           "hhm_visit_testing_date",
#           "hhm_cont_adm_hosp_date",
#           "hhm_quarantine_start_date",
#           "hhm_quarantine_end_date",
#           "hhm_isolation_start_date",
#           "hhm_isolation_end_date",
#           "hhm_work_closure_start_date",
#           "hhm_work_closure_end_date")
#
# cols <- intersect(cols, names(dt))


# spss_date <- function(x) as.Date(as.numeric(x)/86400, origin = "1582-10-14")
# table(dt$hhm_seek_gov_info_date)
# dt[, (cols) := lapply(.SD, spss_date), .SDcols = cols ]
# table(dt$hhm_seek_gov_info_date)

dt[, weekday := weekdays(date - 1)]
dt[, survey_weekday := weekdays(date)]



table(dt$hh_size, useNA = "always")
dt[, hh_size := fcase(
  hh_size == "11 or more", 12,
  hh_size == "None", 1,
  as.numeric(hh_size) %in% seq(1,10), as.numeric(hh_size))
  ]
table(dt$hh_size, useNA = "always")

# Save as factor to be clear that 12 is "12 or more"
dt[, hh_size := factor(hh_size,
                       levels = seq(1,12,1),
                       labels = c("1","2", "3", "4", "5", "6", "7", "8", "9",
                                  "10", "11", "12 or more"))]
table(dt$hh_size)

dt[is.na(cnt_age), cnt_age := hhm_age_group]
dt[is.na(cnt_gender), cnt_gender := hhm_gender]


### Do not change these age bands as they are fixed in the survey.
## Add in min and max age group
age_groups <- c("Under 1", "1-4", "5-9", "10-14", "12-15", "15-19", "16-17", "18-19", "20-24",
                "25-34", "35-44", "45-54", "55-64", "65-69", "70-74",
                "75-79", "80-84", "85+")
age_min <- c(0, 1,  5, 10, 12, 15, 16, 18, 20, 25, 35, 45, 55, 65, 70, 75, 80, 85)
age_max <- c(1, 4,  9, 14, 15, 19, 17, 19, 24, 34, 44, 54, 64, 69, 74, 79, 84, 100)


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

dt[row_id != 0, cont_id := paste0(part_id ,"-", row_id, "-", week)]
if (dt$panel == "Panel EC") {
  dt[row_id == 0 & hhm_contact_yn == "Yes", cont_id := paste0(part_id ,"-", row_id, "-", week)]
}

if (!is.null(dt$hhcomp_remove)) {
  # Children's data cleaning: hh_size can be up to 18, remove household contacts
  # which have been removed by the participant (indicated by hhcomp_remove).
  # Adult particpant and new hhm have a row id of >= 999
  # check
  dt[hhcomp_remove == "Yes" | is.na(hhm_contact_yn), cont_id := NA]
  dt[(row_id <= 11 | row_id >= 999) & (is.na(hhcomp_remove) |
       hhcomp_remove != "Yes"), hhm_id := row_id]
} else {
  # Adult survey asked for hhm each time, so limit is 11 and none are removed
  dt[row_id <= 11 | row_id >= 999, hhm_id := row_id]
}

if (survey$Panel[1] %in% c("Panel C", "Panel D",  "Panel EC")) {
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


# Make time vars numeric and create a total time
# cols <- names(dt)[grep("hours|mins", names(dt))]
#
# dt[, (cols) :=
#      lapply(.SD, as.numeric),
#    .SDcols = cols
#    ]

# cols <- names(dt)[grep("hours", names(dt))]
# cols

# hour_mins <- function(hours,min) min + (hours*60)



# dt[ ,part_face_mask_hours_mins := hour_mins(part_face_mask_hours, part_face_mask_mins)]
# dt[ ,part_public_transport_bus_hours_mins :=
#       hour_mins(part_public_transport_bus_hours, part_public_transport_bus_mins)]
#
# dt[ ,part_public_transport_transport_train_hours_mins :=
#       hour_mins(part_public_transport_train_hours, part_public_transport_train_mins)]
#
# dt[ ,part_public_transport_taxi_uber_hours_mins :=
#       hour_mins(part_public_transport_taxi_uber_hours, part_public_transport_taxi_uber_mins)]
#
# dt[ ,part_public_transport_plane_hours_mins :=
#       hour_mins(part_public_transport_plane_hours, part_public_transport_plane_mins)]
# #
# dt[ ,cnt_total_mins :=
#       hour_mins(cnt_hours, cnt_mins)]


# # phys_t <- unique(dt$phys)
# phys_t_yes <- phys_t[grepl("Physical contact \\(any sort", phys_t)]
# phys_t_no <- phys_t[grepl("Non-physical contact", phys_t)]

dt[,   phys_contact := fcase(
  cnt_phys == "Yes", 1,
  cnt_phys == "No", 0,
  default = NA_real_)]



# Remove changed or known empty columns

# changed_questions <- c()
# if (dt$week[1] != 1 && dt$country_code[1] == "UK") {
#   changed_questions[length(changed_questions) + 1] <- "visit_concert"
# }

# area_town_pop_code contains values, none in label
# empty_cols <- c("area_town_pop_label", "quotagerange")
# if(dt$week[1] == 1 && dt$country_code[1] == "UK") {
#   empty_cols <- c("be01edu_me", "be01inc", "be02edu", "be02occhi", "be02occhi_last",
#     "be02occr", "be02occr_last", "be02sg", "be02sg_score", "emp01",
#     "hhcmp10", "nl01edu", "nl01inc", "nl01occhi", "nl01occr", "nl01sg",
#     "nlsg_version", "uk02edu", empty_cols)
# }

# vars_remove_all <- paste(c(changed_questions, empty_cols), collapse = "|")
# dt_vars <- names(dt)[grep(vars_remove_all, names(dt), invert = TRUE)]
# dt <- dt[, dt_vars, with = FALSE]


## Create a regions variables of near to NHS regions


if(dt$country_code[1] == "UK") {
  dt <- dt[, regions := fcase(
    area_3_name == "Yorkshire and The Humber", "Yorkshire and The Humber",
    area_3_name == "Yorkshire and Humberside", "Yorkshire and The Humber",
    area_3_name == "Yorkshire and", "Yorkshire and The Humber",
    area_3_name == "East Anglia", "East of England",
    area_3_name == "East of England", "East of England",
    area_3_name == "East of Engla", "East of England",
    area_3_name == "Greater London", "Greater London",
    area_3_name == "Greater Londo", "Greater London",
    area_3_name == "North East", "North East",
    area_3_name == "North West", "North West",
    area_3_name == "South East", "South East",
    area_3_name == "South West", "South West",
    area_3_name == "West Midlands", "West Midlands",
    area_3_name == "East Midlands", "East Midlands",
    area_3_name == "Northern Ireland", "Northern Ireland",
    area_3_name == "Northern Irel", "Northern Ireland",

    area_3_name == "Scotland", "Scotland",
    area_3_name == "Wales", "Wales"
  )
  ]
}

if(!"part_reported_all_contacts" %in% names(dt)) {
  dt$part_reported_all_contacts <- NA_real_
}
individually_reported <- unique(dt$part_reported_all_contacts)
individually_reported_yes <- individually_reported[
  grepl("I individually included every person", individually_reported)]
individually_reported_no <- individually_reported[
  grepl("I did not individually include", individually_reported)]

dt <- dt[, part_reported_all := fcase(
  row_id == 0 &
    part_reported_all_contacts == individually_reported_yes, 1,
  row_id == 0 &
    part_reported_all_contacts == individually_reported_no, 0,
  row_id != 0, NA_real_
)]

## Split the data into participants, households, and contacts
part <- dt[hhm_id == 0 & !is.na(part_age)]
nrow(part)
vars_remove <- "cnt|phys|cont_id|hhm_gender|hhm_pregnant|hhm_employstatus|hhm_id|hhm_age_group"
hhm_vars_remove <- c("not_attend_childcare", "close_childcare", "hhm_student", "contact_yn")
vars_remove <- paste(c(vars_remove, hhm_vars_remove), collapse = "|")
# This keeps the mult_contacts_*_phys columns
keep <- names(part)[grep("multiple_contacts", names(part))]

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

id_cols <- c("part_id", "date", "panel", "wave", "wave_id", "country",
             "country_code", "week", "weekday", "hhm_contact_yn",
             "part_reported_all_contacts")
mult_contacts_cols <- grep("multiple_contacts_", names(part), value = TRUE)
mult_contacts_cols <- grep("precautions", mult_contacts_cols, value = TRUE, invert = T)
dt[, (mult_contacts_cols) :=  lapply(.SD, as.numeric), .SDcols = mult_contacts_cols]
part[, (mult_contacts_cols) :=  lapply(.SD, as.numeric), .SDcols = mult_contacts_cols]

mult_contacts <- dt[part_reported_all == 0,
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
  mult_contacts <- mult_contacts[, cont_id :=
                                   paste(part_id, (2000 + rowid(part_id)), week, sep = "-")]
  mult_contacts[, cnt_work := ifelse(grepl("work", variable), "Yes", "No")]
  mult_contacts[, cnt_school := ifelse(grepl("school", variable), "Yes", "No")]
  mult_contacts[, cnt_other := ifelse(grepl("other", variable), "Yes", "No")]
  mult_contacts[, phys_contact := NA_real_]
  mult_contacts[, cnt_home := "No"]
  mult_contacts[, hhm_contact_yn := "No"]
  mult_contacts[, individually_reported := 0]

  mult_cnt_age_groups <- c("0-17", "18-64", "65+")
  mult_cnt_age_min <- c(0, 18, 65)
  mult_cnt_age_max <- c(17, 64, 100)
  mult_contacts[, cnt_age := fcase(
    grepl("contacts_child", variable), mult_cnt_age_groups[1],
    grepl("contacts_adult", variable), mult_cnt_age_groups[2],
    grepl("contacts_older_adult", variable), mult_cnt_age_groups[3])
    ]

  mult_contacts[, cnt_age_est_min := fcase(
    grepl("contacts_child", variable), mult_cnt_age_min[1],
    grepl("contacts_adult", variable), mult_cnt_age_min[2],
    grepl("contacts_older_adult", variable), mult_cnt_age_min[3])
    ]

  mult_contacts[, cnt_age_est_max := fcase(
    grepl("contacts_child", variable), mult_cnt_age_max[1],
    grepl("contacts_adult", variable), mult_cnt_age_max[2],
    grepl("contacts_older_adult", variable), mult_cnt_age_max[3])
    ]

  mult_contacts[, cnt_age := factor(cnt_age, levels = mult_cnt_age_groups)]

  # Create new contact rows as reported
  mult_contacts <- mult_contacts[, list(variable = rep(variable, each = value)),by = names(mult_contacts)]
  mult_contacts[, individually_reported := 0]

  if(nrow(mult_contacts) != total_mult_contacts) {
    stop("Check total number of multiple contact rows")
  }

  bind_cols <- intersect(names(contacts), names(mult_contacts))

  contacts <- rbind(contacts, mult_contacts[, c(bind_cols), with = FALSE],
                    fill = T)
  contacts[, phys_contact := as.numeric(phys_contact)]
}




### Households
###################################

# hh members are indicated by table_row id of 18 or less or 999 (parent in child) (see above)
households <- dt[!is.na(hhm_id) ]
mult_contacts_cols <- grep("multiple_contacts_", names(part), value = TRUE)


households <- households[, -c("cnt_type", mult_contacts_cols), with = FALSE]
households[, phys_contact := as.numeric(phys_contact)]
table(households$cnt_age_est_min, useNA = "always")


### Child surveys
###################################
if (as.character(part$panel[1]) %in% c("Panel C", "Panel D", "Panel EC")) {
  source("r/00_data_cleaning/dm02a_child_data_clean.R")
} else {
  part[, survey_type := "adult"]
}



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

if (!(as.character(part$panel[1]) == "Panel C" & part$wave[1] == "Wave 1")){
  part <- part_add_nickname_flag_count(part, contacts)
}
contacts[, cnt_nickname_masked := as.character(cnt_nickname_masked)]



### Save the data files
data_path <- "data"
panel_name <- tolower(gsub(" ", "_", as.character(dt$panel[1])))
wave_name <- tolower(gsub(" ", "_", as.character(dt$wave[1])))
country_code <- tolower(dt$country_code[1])
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
  if(n_participants_check != nrow(part)){
    stop("Check total number of participant rows")
  }
  if(n_households_check + n_participants_check != nrow(households)){
    stop("Check total number of household rows")

  }
  if(n_contacts_check + total_mult_contacts != nrow(contacts)){
    stop("Check total number of contact rows")
  }
}

if (part$survey_type[1] == "child") {
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
}


saveRDS(part, file = file.path(survey_path, "clean_participants.rds"))
saveRDS(contacts, file = file.path(survey_path, "clean_contacts.rds"))
saveRDS(households, file = file.path(survey_path, "clean_households.rds"))

# Print the survey path to use in r/dm_data_checks.R
message(survey_path)

