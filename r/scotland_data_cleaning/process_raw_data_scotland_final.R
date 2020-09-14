library(data.table)
library(readxl)
source("r/functions/utility_functions.R")


# ============================
# ============================
# Set variables and load data
# ============================
# ============================


# Panel labels
# ====================
panel_name <- "Panel A"
wave_name <- "Wave 2"
panel_path <- "panel_a"
wave_path <- "wave_2"
wave_id <- "A 2"
week <- 3

# Participant age bins
age_bins <- c(0, 5, 13, 18, 30, 40, 50, 60, 70, 120)

# Data file path and names
# =========================
base_data_path <- file.path("data", "raw_data", "sc")
raw_data_path <- file.path("data", "raw_data", "sc", panel_path, wave_path)
clean_data_path <- file.path("data", "sc", panel_path, wave_path)

# Demographic data
demographic_filename <- "Wave 2A registration data.xlsx"
  sheetname_dem <- "SAMPLE A"

# Contact data
data_filename <- "Wave 2A dataset.xlsx"

# Data map
datamap_filename <- "Question reference.xlsx"
  sheetname_datamap <- "Wave 2A"

# Create clean data directory
# ============================
clean_data_dir <- gsub("/wave_[0-9]", "", clean_data_path)
dir.create(clean_data_dir, showWarnings = FALSE)
dir.create(clean_data_path, showWarnings = FALSE)

# Read files
# ============================

# Demograhic data
dem <- suppressWarnings(
  read_xlsx(file.path(raw_data_path, demographic_filename),
                                  sheet = sheetname_dem))
dem <- as.data.table(dem)

# Contact data (dw = data for wave)
dw <- suppressWarnings(read_xlsx(file.path(raw_data_path, data_filename)))
dw <- as.data.table(dw)

# Data map
dmap <- suppressWarnings(read_xlsx(
  file.path(base_data_path, datamap_filename), sheet = sheetname_datamap))
# dmap <- read_xlsx("data/raw_data/sc/Question reference.xlsx", sheet = "Wave 1A")
dmap <- as.data.table(dmap)


# ============================
# ============================
# Prepare data
# ============================
# ============================

# Prep demographic and household data
# ====================================
dem_ids <- c("CP Number")
hmq <- grep("HM[0-9]", names(dem), value = T)
hm <- dem[, c(dem_ids, hmq), with = F]
hml <- melt(hm, id.vars = dem_ids)

hml[, contact_id := gsub(".*\\[\\!HM|\\!\\].*", "", variable)]
hml[, new_name := fcase(
  grepl("age groups", variable), "cnt_age",
  grepl("which of the following describes how", variable), "cnt_gender",
  grepl("normal situation", variable), "cnt_occupation",
  grepl("pupil or student", variable), "cnt_student"
)]

hml[, variable := NULL]

names(hml) <- snakecase::to_snake_case(names(hml))
hm_dt <- dcast(cp_number + contact_id ~ new_name, data = hml, value.var = "value")
hm_dt <- hm_dt[!(is.na(cnt_age) & is.na(cnt_gender) & is.na(cnt_student) & is.na(cnt_occupation))]


# Prep data map
# ===============
dmap <- dmap[!is.na(var_name)]
dmap <- dmap[!grepl("x[0-9]+", var_name)]
dmap[, var_name := gsub(" \\/.*|n_| \\/| |/","",  var_name)]

# dmap[is.na(contact) & is.na(hhm), hhm := 0]
dmap[, row_id := person_number]
dmap[grepl("salon", raw_data_col_name), var_name := "cnt_salon"]
dmap[grepl("In a healthcare setting", raw_data_col_name), var_name := "cnt_healthcare"]
dmap[grepl("Please list all other", raw_data_col_name), var_name := "contact_nickname"]
dmap[grepl("throat swab PRIOR to the past 14", raw_data_col_name), var_name := "part_hhm_covid_test_past"]

# dmap[is.na(contact) & !is.na(hhm), contact := hhm]
# dmap[!is.na(contact) & is.na(hhm), contact := contact + 19]
# dmap[, person_number := ifelse(person_number >= 16, person_number + 4, person_number)]
# dmap[, row_id := contact]
# dmap[is.na(row_id) , row_id := 0]Q
table(dmap$row_id, useNA = "always")


# ============================
# ============================
# Clean and shape
# ============================
# ============================

# Prep contact data
# ============================
id_vars <- c("CP number", "Date")
# id_vars <- c("ID", "Date")
questions <- intersect(names(dw), dmap$raw_data_col_name)
dw <- dw[, c(questions, id_vars), with = F]

# Check to see how many values there are as an early indication that something may be off
check_dw <- grep("24\\:", names(dw), value = T)[1]
check_map <- grep("24\\:", dmap$raw_data_col_name, value = T)[1]
if(check_dw != check_map) stop("map does not align with questions")

dw <- dw[!is.na(`CP number`)]
dwl <- melt(dw, id.vars = id_vars)
mname <- match(as.character(dwl$variable), dmap$raw_data_col_name)
nmes <- dmap$var_name[mname]
cids <- dmap$row_id[mname]
dwl$var_name <- nmes
dwl$row_id <- cids
dwl[,variable := NULL]

names(dwl) <- snakecase::to_snake_case(names(dwl))
# dwl[var_name == "cnt_age"]


# Process individually reported contact data
# ==========================================
tables_list <- list()
for (var in unique(dwl$var_name)) {
  print(var)

  q <- dwl[var_name == var]
  q <- q[!is.na(value)]
  if (sum(unique(q$row_id) != 0) != 0) {
    table(q$value)

    qd <- dcast(cp_number + date + row_id ~ var_name, data = q, var.value = "value")
    qd <- as.data.table(qd)
    qd <- qd[!is.na(get(var))]
    tables_list[[var]] <- qd
  }
}

match_vars <- c("cp_number", "date", "row_id")
dt <- Reduce(function(...) merge(..., by = match_vars, all = T), tables_list)

# Add household member data from registration to contacts (gender, age)

names(dem)
hm_dt <- as.data.table(hm_dt)
setnames(hm_dt,
         old = c("contact_id", "cnt_gender", "cnt_age"),
         new = c("row_id", "hhm_gender", "hhm_age"))
hm_dt[, row_id := as.integer(row_id)]
id_vars <- c("cp_number", "row_id")
# id_vars <- intersect(names(dt), names(hm_dt))
#
#
dtm <- merge(dt, hm_dt, by = id_vars, all.x = T)
table(dtm$cnt_age, useNA = "always")
dtm[is.na(cnt_age) & !is.na(hhm_age), cnt_age := hhm_age]
table(dtm$cnt_age, useNA = "always")
# dtm <- dtm[!(row_id < 20 & is.na(hhm_contact_yn))]
dtm[, wave := wave_name]
dtm[, panel := panel_name]
dtm[, week := week]
dtm[, wave_id := wave_id]
dtm[, country_code := "sc"]
dtm[, part_id := cp_number]
dtm[, individually_reported := 1]
contacts <- dtm

n_individual_contacts <- nrow(contacts)


# Process participant data
# ========================


part_names <- names(dem)[1:12][-c(2,6)]
part_new_names <- c("cp_number", "part_age", "part_gender", "part_urban_rural",
                    "part_ethic_group", "part_ethicity", "part_ethnicity_other",
                    "part_high_risk", "part_medium_risk", "hh_size")
length(part_new_names)

part <- dem[, part_names, with = F]
setnames(part, part_names, part_new_names)

# IMPORTANT: Remove participants who did not complete the contact survey
# =====================================================================

incomplete <- dem[!(`CP Number` %in% dw$`CP number`)]
part <- part[!cp_number %in% incomplete$`CP Number`]


# Create participant age groups
part[, part_age_group := cut(part_age,
                             breaks = age_bins,
                             right = FALSE)]
part[, wave := wave_name]
part[, panel := panel_name]
part[, week := week]
part[, wave_id := wave_id]
part[, country_code := "sc"]
part[, part_id := cp_number]


### Clean, Reshape, and Merge Multiple Contacts
######################################################

# particiapnt data from data wave
# =================================
pdwl <- dwl[row_id == 0]

ptables_list <- list()
for (var in unique(pdwl$var_name)) {
  # browser()

  q <- pdwl[var_name == var]
  if (sum(unique(q$row_id) != 0) == 0 & !var %in% c("ID", "date")) {
    print(var)
    ns <- c(ns, var)
    qd <- dcast(cp_number + date + row_id ~ var_name, data = q, var.value = "value")
    qd <- as.data.table(qd)
    qd <- qd[!is.na(get(var))]
    ptables_list[[var]] <- qd
  }
}

m_dt <- Reduce(function(...) merge(..., by = match_vars, all = T), ptables_list)

id_cols <- c("cp_number", "part_id", "date", "panel", "wave", "wave_id", "country",
             "country_code", "week", "weekday", "hhm_contact_yn",
             "part_reported_all_contacts")
mult_contacts_cols <- grep("multiple_contacts_", names(m_dt), value = TRUE)
mult_contacts_cols <- grep("precautions", mult_contacts_cols, value = TRUE, invert = T)
m_dt[, (mult_contacts_cols) :=  lapply(.SD, as.numeric), .SDcols = mult_contacts_cols]
m_dt[, part_id := cp_number]
part <- merge(part, m_dt[, c("part_id", mult_contacts_cols), with = F], by = "part_id", all.x = T)
# part[, (mult_contacts_cols) :=  lapply(.SD, as.numeric), .SDcols = mult_contacts_cols]



mult_contacts <- melt(m_dt,
                      measure.vars = mult_contacts_cols,
                      id_cols = id_cols)

mult_contacts[, week := 2]

mult_contacts[is.na(value), value := 0]
total_mult_contacts <- sum(as.numeric(mult_contacts$value))

if (length(mult_contacts_cols) == 0) {
  total_mult_contacts <- 0
} else {
  # Add contacts columns
  # =====================
  mult_contacts <- mult_contacts[, cont_id :=
                                   paste(cp_number, (2000 + rowid(cp_number)), week, sep = "-")]
  mult_contacts[, cnt_work := ifelse(grepl("work", variable), 1, 0)]
  mult_contacts[, cnt_school := ifelse(grepl("school", variable), 1, 0)]
  mult_contacts[, cnt_other := ifelse(grepl("other", variable), 1, 0)]
  mult_contacts[, phys_contact := NA_real_]
  mult_contacts[, cnt_home := 0]
  mult_contacts[, hhm_contact_yn := 0]
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
  # ====================================
  mult_contacts <- mult_contacts[, list(variable = rep(variable, each = value)),by = names(mult_contacts)]
  mult_contacts[, individually_reported := 0]


  if(nrow(mult_contacts) != total_mult_contacts) {
    stop("Check total number of multiple contact rows")
  }

  bind_cols <- intersect(names(contacts), names(mult_contacts))

  contacts <- rbind(contacts, mult_contacts[, c(bind_cols), with = FALSE],
                    fill = T, all.x = TRUE)

  table(contacts$individually_reported)
  summ <- contacts[individually_reported == 0, .(n = .N), by = "part_id"]
  summ[n == 1250]$part_id
  contacts
  n_mult_contacts <- nrow(mult_contacts)
  if (nrow(contacts) != (n_individual_contacts + n_mult_contacts)) {
    stop("Multiple contacts not added")
  }

  # contacts[, phys_contact := as.numeric(phys_contact)]
}

contacts[, part_id := cp_number]
contacts[, panel := panel_name]
contacts[, wave := wave_name]
contacts[, wave_id := wave_id]
contacts[, country_code := "sc"]
contacts[, week := week]

part <- add_n_cnts_location_cols_scotland(part, contacts, replace_existing_cols = T)

summary(part$n_cnt_mass_reported)



# ============================
# ============================
# Test outputs and truncation
# ============================
# ============================

ctrim <- trim_contacts(contacts, 50)
ptrim <- add_n_cnts_location_cols_scotland(part, ctrim, replace_existing_cols = T)
summary(ptrim$n_cnt_all)
summary(part$n_cnt_individually_reported)
summary(ptrim$n_cnt_individually_reported)
summary(part$n_cnt_mass_reported)
summary(ptrim$n_cnt_mass_reported)

if(max(part$n_cnt_mass_reported) == 0 | is.na(max(part$n_cnt_mass_reported))) {
  stop("Multiple contact count cols not calculated")
}

if (nrow(dw) != nrow(part)) stop("Check participant data")
summary(part$n_cnt_all)

if (nrow(contacts) != (n_individual_contacts)) {
  stop("Multiple contacts not added")
}
# ============================
# ============================
# Save data to clean data path
# ============================
# ============================

write.csv(part, file.path(clean_data_path, "clean_participants.csv"))
saveRDS(part, file.path(clean_data_path, "clean_participants.rds"))


write.csv(contacts, file.path(clean_data_path, "clean_contacts.csv"))
saveRDS(contacts, file.path(clean_data_path, "clean_contacts.rds"))

