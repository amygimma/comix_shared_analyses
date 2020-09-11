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
households[, survey_type := NULL]
households <- merge(households, part[, list(part_id, child_hhm_id, survey_type)],
                    by.x = c("part_id", "hhm_id"),
                    by.y = c("part_id", "child_hhm_id"),
                    all.x = TRUE)
table(households$survey_type, useNA = "always")

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

child_age_groups <- c("Under 1", "1-4", "5-11",  "12-15", "16-17")
child_matrix_age_bins <- c("[0,1)", "[1,5)", "[5,12)", "[12,16)", "[16,17)")

table(part$cnt_age)
part[, cnt_age := factor(cnt_age, levels = child_age_groups)]
part[, cnt_age := fcase(
  cnt_age == child_age_groups[1], child_matrix_age_bins[1],
  cnt_age == child_age_groups[2], child_matrix_age_bins[2],
  cnt_age == child_age_groups[3], child_matrix_age_bins[3],
  cnt_age == child_age_groups[4], child_matrix_age_bins[4],
  cnt_age == child_age_groups[5], child_matrix_age_bins[5]

)]
part[, cnt_gender_nb := cnt_gender]


# Remove particpant hmm_id == 0 in favor of hhm_id == 999 with contact info (see main data cleaning file)
# households <- households[hhm_id != 0]

# Add participant to contacts if appropriate
# parent_cnts <- dt[row_id == 0 & hhm_contact_yn == "Yes"]
# parent_cnts[,cont_id := paste0(part_id ,"-", row_id, "-", week)]
# #
# contacts <- merge(contacts, parent_cnts, by = names(contacts))



setnames(part, old = c("cnt_age", "cnt_age_est_max", "cnt_age_est_min", "cnt_gender", "cnt_gender_nb"),
         new = c("part_age_group", "part_age_est_max", "part_age_est_min", "part_gender", "part_gender_nb"))

table(part$part_age_group, useNA = "always")
table(part$part_age_est_max, useNA = "always")
table(part$part_gender, useNA = "always")
table(part$part_gender_nb, useNA = "always")

