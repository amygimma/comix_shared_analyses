# library(data.table)

message("Cleaning children's data")

part[, survey_type := "child"]
# Move particpant data to responder columns
setnames(part, old = c("part_age", "part_age_group", "part_gender", "part_gender_nb"),
         new = c("resp_age", "resp_age_group", "resp_gender", "resp_gender_nb"))
# Remove exact age from participant data ()
part[ , part_age := NA]

# Identify child by hhm_id
#############################
hhm_id_pattern <- "^.*\\{_\\s*|\\s*\\}.*$"
# Example response: "{#Q21_replace[{_5}]._scale.response.value}", need to isolate id "5"
part[ , child_hhm_id := as.numeric(gsub(hhm_id_pattern, "", child_part_select_raw))]
part[child_hhm_id > 19 , child_hhm_id := child_hhm_id + 999]
# Use hhm_id and child_hhm_idabove to add child's data to the selected child in
# the household data
households[, survey_type := NULL]
households <- merge(households, part[, list(part_id, child_hhm_id, survey_type)],
                    by.x = c("part_id", "hhm_id"),
                    by.y = c("part_id", "child_hhm_id"),
                    all.x = TRUE)
table(households$survey_type, useNA = "always")
table(households$hhm_id, useNA = "always")


# Add flag for child participant (from merged part data)
households[, child_participant := fcase(
  !is.na(survey_type), TRUE,
  is.na(survey_type), FALSE)]
# Add survey type to all rows
households[, survey_type := "child"]
table(households[child_participant == T]$cnt_age_est_max, useNA = "always")

# Add flag for child participant (from merged part data)


# Add child demographics to participant columns
# ################################################
demographic_cols <- households[child_participant == TRUE,
                               list(part_id, hhm_id, cnt_age, cnt_age_est_max,
                                    cnt_age_est_min, cnt_gender)]

part <- merge(part, demographic_cols,
              by.x = c("part_id", "child_hhm_id"), by.y = c("part_id", "hhm_id"))

table(part$cnt_age)

if (dt$country_code[1] == "BE") {
  child_age_groups <- c("Under 1", "1-4", "5-6", "7-11", "12-15", "16-17")
  child_matrix_age_bins <- c("[0,1)", "[1,5)", "[5,7)", "[7,12)", "[12,16)", "[16,17)")
} else {
  child_age_groups <- c("Under 1", "1-4", "5-11",  "12-15", "16-17")
  child_matrix_age_bins <- c("[0,1)", "[1,5)", "[5,12)", "[12,16)", "[16,17)")
}

table(part$cnt_age, useNA = "always")
part[, cnt_age := factor(cnt_age, levels = child_age_groups)]
part[, cnt_age := fcase(
  cnt_age == child_age_groups[1], child_matrix_age_bins[1],
  cnt_age == child_age_groups[2], child_matrix_age_bins[2],
  cnt_age == child_age_groups[3], child_matrix_age_bins[3],
  cnt_age == child_age_groups[4], child_matrix_age_bins[4],
  cnt_age == child_age_groups[5], child_matrix_age_bins[5],
  cnt_age == child_age_groups[6], child_matrix_age_bins[6]


)]
table(part$cnt_age, useNA = "always")
part[, cnt_gender := cnt_gender]


# Add participant to contacts if appropriate
if (dt$country_code[1] == "BE") {
  parent_cnts <- dt[row_id %in% c(0,999)]
} else {
  parent_cnts <- dt[row_id == 0]
}
cnt_adult_age_bins <- c(18, 20, seq(25, 100, 10))
parent_cnts[ , part_age_group := cut(part_age,
                           breaks = cnt_adult_age_bins,
                           right = FALSE)]
table(parent_cnts$part_age_group, useNA = "always")
parent_cnts[, part_age_group := gsub("\\[|\\)", "", part_age_group)]
parent_cnts[part_age_group != "70,120", part_age_group := paste(
  tstrsplit(part_age_group, ",", "-")[[1]],
  as.numeric(tstrsplit(part_age_group, ",", "-")[[2]]) -1,
  sep = "-")]
parent_cnts[part_age_group == "70,120", part_age_group := "70-120"]
table(parent_cnts$part_age_group, useNA = "always")
# match_hhm_id <-
# contacts[hhm_id == 0, cnt_age := 1]
contacts[hhm_id == 0, cnt_age :=
             match_variable(.SD, part_id, parent_cnts, "part_age_group"), by = "part_id"]
table(contacts[hhm_id == 0]$cnt_age, useNA = "always")
contacts[hhm_id == 0, cnt_gender :=
             match_variable(.SD, part_id, parent_cnts, "part_gender_nb"), by = "part_id"]
contacts[hhm_id == 0, cont_id := paste0(part_id ,"-", 999, "-", week)]

# if (dt$country_code[1] == "BE") {
  contacts <- contacts[hhm_id == 0, hhm_id := 999]
  table(contacts[hhm_id == 999]$cnt_age)
# } else {
#   contacts <- contacts[hhm_id == 0, hhm_id := 999]
# }
# Add participant to contacts (assigned to hhm_id 999 to follow original ipsos structure)

if(nrow(contacts[hhm_id == 999]) > 0){
  if (any(is.na(contacts[hhm_id == 999]$cnt_age))) stop("Parent ages not included in contacts")
  if (any(is.na(contacts[hhm_id == 999]$cnt_gender))) stop("Parent genders not included in contacts")
} else{
  stop("Check contact table for parent contacts")
}

# Add participant to households (assigned to hhm_id 999 to follow original ipsos structure)
households[hhm_id == 0, hhm_age :=
             match_variable(.SD, part_id, parent_cnts, "part_age_group"), by = "part_id"]
households[hhm_id == 0, hhm_gender :=
             match_variable(.SD, part_id, parent_cnts, "part_gender_nb"), by = "part_id"]
table(households[hhm_id == 0]$hhm_gender, useNA = "always")
households[hhm_id == 0, cont_id := paste0(part_id ,"-", 999, "-", week)]
households <- households[hhm_id == 0, hhm_id := 999]

if(nrow(households[hhm_id == 999]) > 0){
  if (any(is.na(households[hhm_id == 999]$hhm_age))) stop("Parent ages not included in hh dt")
  if (any(is.na(households[hhm_id == 999]$hhm_gender))) stop("Parent genders not included in hh dt")
} else{
  stop("Check household table for parent demographic data")
}

# Set names of child contact demographics to participating child participant demographics
setnames(part, old = c("cnt_age", "cnt_age_est_max", "cnt_age_est_min", "cnt_gender"),
         new = c("part_age_group", "part_age_est_max", "part_age_est_min", "part_gender"))

hhm_child <- households[child_participant == T]
part[, part_student := match_variable(.DS, part_id, hhm_child, "hhm_student")]


table(part$part_age_group, useNA = "always")
table(part$part_age_est_max, useNA = "always")
table(part$part_gender, useNA = "always")
table(part$part_gender_nb, useNA = "always")
table(part$part_student, useNA = "always")

