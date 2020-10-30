# library(data.table)

message("Cleaning children's data")
part[, survey_type := "child"]

# Move particpant data to responder columns
setnames(part, old = c("part_age_group", "part_gender", "part_gender_nb"),
         new = c("resp_age_group", "resp_gender", "resp_gender_nb"))
# Remove exact age from participant data ()
part[ , part_age := NA]

# Identify child by hhm_id
#############################
hhm_id_pattern <- "^.*\\{_\\s*|\\s*\\}.*$"
# Example response: "{#Q21_replace[{_5}]._scale.response.value}", need to isolate id "5"
part[ , child_hhm_id := as.numeric(gsub(hhm_id_pattern, "", child_part_select_raw))]
# Use hhm_id and child_hhm_idabove to add child's data to the selected child in
# the household data
households <- merge(households, part[, list(part_id, child_hhm_id, survey_type)],
                    by.x = c("part_id", "hhm_id"),
                    by.y = c("part_id", "child_hhm_id"),
                    all.x = TRUE)

# Add flag for child participant (from merged part data)
households[, child_participant := fcase(
  !is.na(survey_type), TRUE,
  is.na(survey_type), FALSE)]
# Add survey type to all rows
households[, survey_type := "child"]

# Use hhm_id a
# nd child_hhm_id above to add child's data to the selected child in
# the contacts data
contacts <- merge(contacts, part[, list(part_id, child_hhm_id, survey_type)],
                  by.x = c("part_id", "hhm_id"),
                  by.y = c("part_id", "child_hhm_id"),
                  all.x = TRUE)

# Add flag for child participant (from merged part data)
contacts[, child_participant := fcase(
  !is.na(survey_type), TRUE,
  is.na(survey_type), FALSE)]
# Add survey type to all rows
contacts[, survey_type := "child"]


# Add child demographics to participant columns
# ################################################
demographic_cols <- households[child_participant == TRUE,
                               list(part_id, hhm_id, cnt_age, cnt_age_est_max,
                                    cnt_age_est_min, cnt_gender)]

part <- merge(part, demographic_cols,
              by.x = c("part_id", "child_hhm_id"), by.y = c("part_id", "hhm_id"))
child_age_groups <- c("Under 1", "1-4", "5-11",  "12-17")
child_matrix_age_bins <- c("[0,1)", "[1,5)", "[5,12)", "[12,18)")
child_age_min <- c(0, 1, 5, 12)
child_age_max <- c(1, 4, 11, 17)
part[, cnt_age_est_min := fcase(
  cnt_age == child_age_groups[1], child_age_min[1],
  cnt_age == child_age_groups[2], child_age_min[2],
  cnt_age == child_age_groups[3], child_age_min[3],
  cnt_age == child_age_groups[4], child_age_min[4]
)]

part[, cnt_age_est_max := fcase(
  cnt_age == child_age_groups[1], child_age_max[1],
  cnt_age == child_age_groups[2], child_age_max[2],
  cnt_age == child_age_groups[3], child_age_max[3],
  cnt_age == child_age_groups[4], child_age_max[4]
)]

part[, cnt_age := fcase(
  cnt_age == child_age_groups[1], child_matrix_age_bins[1],
  cnt_age == child_age_groups[2], child_matrix_age_bins[2],
  cnt_age == child_age_groups[3], child_matrix_age_bins[3],
  cnt_age == child_age_groups[4], child_matrix_age_bins[4]
)]

part[, cnt_age := factor(cnt_age, levels = child_matrix_age_bins)]
part[, cnt_gender_nb := cnt_gender]
# Remove child from contacts
contacts <- contacts[child_participant == FALSE]
#
# Remove particpant hmm_id == 0 in favor of hhm_id == 999 with contact info (see main data cleaning file)
households <- households[hhm_id != 0]

hhm_child <- households[child_participant == T]
part[, part_student := match_variable(.DS, part_id, hhm_child, "hhm_student")]

setnames(part, old = c("cnt_age", "cnt_age_est_max", "cnt_age_est_min", "cnt_gender", "cnt_gender_nb"),
         new = c("part_age_group", "part_age_est_max", "part_age_est_min", "part_gender", "part_gender_nb"))



