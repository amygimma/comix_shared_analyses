## Data checks
## Here will we run checks for data quality

library(data.table)
library(testthat)


# change path manually for now
survey_path <- "data/nl/panel_a/wave_3"
part <- readRDS(file.path(survey_path, "clean_participants.rds"))
contacts <- readRDS(file.path(survey_path, "clean_contacts.rds"))
households <- readRDS(file.path(survey_path, "clean_households.rds"))

table(part$panel, part$wave, part$country_code)
table(contacts$panel, contacts$wave, contacts$country)
table(households$panel, households$wave, households$country)



nrow(part)

date = Sys.Date()
issues_csv <- data.table(survey_path = survey_path,
                         table = "all",
                         column = "all",
                         issue = NA_character_,
                         date = date,
                         note = "Data check started"
                         )

add_issue <- function(issues, table, issue, column, note = NA_character_, 
                      date = Sys.Date(), survey_path = survey_path) {
  # browser()
    issue <- data.table(survey_path = survey_path,
             table = table,
             column = column,
             issue = issue,
             date = date,
             note = note
  )
  issues <- rbind(issues_csv, issue)
  return(issues)
}


## CHECK SURVEY DATA ISSUES

# check participant cols
part_cols <- part

part_check_columns <- c()
for (colname in names(part_cols)) {
  values <- part_cols[, colname, with = FALSE][[1]]
  if (is.na(unique(values)) && length(unique(values)) <= 1) {
    i <- length(part_check_columns) + 1
    part_check_columns[i] <- colname
  }
}

# These can be empty (product of air travel restrictions) or removed
concert <- grep("concert", names(part), value = TRUE)
part_check_columns <- setdiff(part_check_columns, 
                              c("part_public_transport_plane_hours", 
                                "part_public_transport_plane_mins", 
                                "part_public_transport_plane_hours_mins", 
                                "part_employ_type",
                                concert))
part_check_columns

# check household cols (ignoring known cols - remove?)
hh_check_columns <- c()
for (colname in names(households)) {
  values <- households[, colname, with = FALSE][[1]]
  if (is.na(unique(values)) && length(unique(values)) <= 1) {
    hh_check_columns[length(hh_check_columns) + 1] <- colname
  }
}
# These can be empty (product of air travel restrictions)
hh_check_columns  <- setdiff(hh_check_columns, c("part_public_transport_plane_hours", 
                              "part_public_transport_plane_mins", 
                              "part_public_transport_plane_hours_mins",
                              concert))
hh_check_columns 


# check contact cols 
cont_check_columns <- c()
for (colname in names(contacts)) {
  values <- contacts[, colname, with = FALSE][[1]]
  if (is.na(unique(values)) && length(unique(values)) <= 1) {
    cont_check_columns[length(cont_check_columns) + 1] <- colname
  }
}
cont_check_columns


## Test dates
date_colnames <- c("panel", "wave", "date", "country_code", "part_id", "hhm_id", 
  "hhm_seek_gov_info_date", "hhm_phone_gp_date", "hhm_visit_gp_date", 
  "hhm_visit_urgent_date", "hhm_visit_ae_date", "hhm_visit_testing_date", 
  "hhm_cont_adm_hosp_date", "hhm_work_closure_start_date", "hhm_work_closure_end_date", 
  "hhm_isolation_start_date", "hhm_isolation_end_date", "hhm_quarantine_start_date", 
  "hhm_quarantine_end_date")
date_colnames <- intersect(date_colnames, names(households))
date_cols <- households[, date_colnames, with = FALSE]

# rename to avoid melting survey date in matching
setnames(date_cols, "date", "DATE")
id_vars <- c("panel", "wave", "country_code", "part_id", "hhm_id", "DATE")
measure_vars <- grep(pattern = paste(id_vars, collapse = "|"), x = names(date_cols),
                     invert = TRUE, value = TRUE)
date_cols <- melt(date_cols, id.vars = id_vars, measure.vars = measure_vars)

# Check that dates are between the 2020-01-01 and survey date
check_hh_dates <- 
  date_cols[as.Date(as.numeric(value), origin = "1970-01-01") < as.Date("2020-01-01", "%Y-%m-%d") &
              as.Date(as.numeric(value), origin = "1970-01-01") >  as.Date(DATE, "%Y-%m-%d")]




# Check columns vs data dictionary
dictionary <- as.data.table(read.csv("data/raw_data/codebook/dictionary.csv", 
                                     stringsAsFactors = FALSE))

# Check for missing cols
part_missing <- setdiff(dictionary[table == "p"]$cols, names(part))
cont_missing <- setdiff(dictionary[table == "c"]$cols, names(contacts))
hh_missing <- setdiff(dictionary[table == "h"]$cols, names(households))
missing_cols <- c(part_missing, cont_missing, hh_missing)

issue_part_age_neg <- part[part_age < 18,
                           .(
                             part_id,
                             wave,
                             country,
                             var_clean = "part_age",
                             var_spss = "resp_age",
                             var_value = part_age,
                             issue = "Age is negative"
                           )]


for (col in part_check_columns) {
  issues_csv <- add_issue(issues = issues_csv, 
                          table = "particpants", 
                          column = col, 
                          issue = "empty column", 
                          note = "",  
                          date = date, 
                          survey_path = survey_path)
}
for (col in cont_check_columns) {
  issues_csv <- add_issue(issues = issues_csv, 
                          table = "contacts", 
                          column = col, 
                          issue = "empty column", 
                          note = "",  
                          date = date, 
                          survey_path = survey_path)
}
for (col in hh_check_columns) {
  issues_csv <- add_issue(issues = issues_csv, table = "households", column = col, 
                          issue = "empty column", 
                          note = "",  
                          date = date, 
                          survey_path = survey_path)
}
for (col in missing_cols) {
  issues_csv <- add_issue(issues = issues_csv, table = "", 
                          column = col, 
                          issue = "missing column", 
                          note = "",  
                          date = date, 
                          survey_path = survey_path)
}





if (nrow(issue_part_age_neg) > 0) {
  part_ids <- paste(issue_part_age_neg$part_id, collapse = ", " )
  issues_csv <- add_issue(issues = issues_csv, table = "part", column = col, 
                          issue = "Age Less than 18", 
                          note = paste("part_ids:", part_ids),  
                          date = date, 
                          survey_path = survey_path)
}

if (nrow(check_hh_dates) > 0) {
  part_ids <- paste(check_hh_dates$part_id, collapse = ", " )
  issues_csv <- add_issue(issues = issues_csv, table = "hh", column = col, 
                          issue = "Date out of range", 
                          note = paste("part_ids:", part_ids),  
                          date = date, 
                          survey_path = survey_path)
}

varnames <- fread('data/raw_data/codebook/var_names.csv')
varnames <- varnames[ , list(new_name, ipsos_varname)]
setnames(varnames, old = "new_name", new = "column")
issues_csv <- merge(issues_csv, varnames, by = "column", all = F)
write.csv(issues_csv, file.path(survey_path, "issues.csv"))

# Check for missing cols
part_missing <- setdiff(dictionary[table == "p"]$cols, names(part))
cont_missing <- setdiff(dictionary[table == "c"]$cols, names(contacts))
hh_missing <- setdiff(dictionary[table == "h"]$cols, names(households))


issues_csv
## CHECK DATA CLEANING ISSUES

# Check for duplicates
part_dups <- grep("\\.x", names(part), value = TRUE)
cont_dups <- grep("\\.x", names(contacts), value = TRUE)
hh_dups <- grep("\\.x", names(households), value = TRUE)


testthat::expect_equal(length(part_dups), 0)
testthat::expect_equal(length(part_dups), 0)
testthat::expect_equal(length(part_dups), 0)
# Check that participants can have zero contacts
testthat::expect_true(0 %in% unique(part$n_cnt_all))
# Check for required columns for grouping and merging

required_columns <- c("wave", "panel", "part_id", "country", "country_code")

testthat::expect_equal(sum(required_columns %in% names(part)), 
                       length(required_columns))
testthat::expect_equal(sum(c("cont_id", required_columns) %in% names(contacts)), 
                       length(required_columns) + 1)
testthat::expect_equal(sum(c("hhm_id", "cont_id", required_columns) %in% names(households)), 
                       length(required_columns) + 2)

testthat::expect_equal(sum(is.na(part$hh_size)), 0)
testthat::expect_equal(sum(part$hh_size == 0), 0)



testthat::expect_length(unique(part_check_columns), 0)
print("\nMissing participant data:/n")
print(unique(part_check_columns))




testthat::expect_length(unique(hh_check_columns), 0)
print("\nMissing housechold data:/n")
print(unique(hh_check_columns))


testthat::expect_length(unique(cont_check_columns), 0)
print("\nMissing contact data:/n")
print(unique(cont_check_columns))


testthat::expect_equal(nrow(check_hh_dates), 0)

testthat::expect_length(part_missing, 0)
testthat::expect_length(cont_missing, 0)
testthat::expect_length(hh_missing, 0)
missing_cols <- c(part_missing, cont_missing, hh_missing)

# Check that new rows are added to data dictionary
part_new <- setdiff(names(part), dictionary[table == "p"]$cols)
cont_new <- setdiff(names(contacts), dictionary[table == "c"]$cols)
hh_new <- setdiff(names(households), dictionary[table == "h"]$cols)
write.csv(part_new, file.path("data/raw_data/codebook", "W3_new_participant_columns.csv"), row.names = F)
write.csv(cont_new, file.path("data/raw_data/codebook", "W3_new_contact_columns.csv"), row.names = F)
              
# Add any new columns to the google spreadsheet, then uncomment the code below to 
# add to the dictionary csv
# https://docs.google.com/spreadsheets/d/1RVSc71gHSnqtNw1EWwcdudW7cQ80EXrfHStjR5diff8/edit#gid=68750415
testthat::expect_length(part_new, 0)
testthat::expect_length(cont_new, 0)
testthat::expect_length(hh_new, 0)

# # Create data dictionary table
# REMOVE COMMENTS to Add to dictionary **AFTER** new columns are added to the google document
# (Then add comments again)
# dictionary <- data.table(table = c(rep_len("p", ncol(part)),
#                                    rep_len("c", ncol(contacts)),
#                                    rep_len("h", ncol(households))),
#                          cols = c(names(part), names(contacts), names(households)))
# country_code <- tolower(part$country_code[1])
# write.csv(dictionary, paste0("data/raw_data/codebook/dictionary_", country_code, ".csv"))

