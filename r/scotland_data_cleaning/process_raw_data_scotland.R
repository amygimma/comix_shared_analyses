library(data.table)
library(readxl)
source("r/functions/utility_functions.R")

dem <- read_xlsx("data/raw_data/sc/panel_a/wave_1/Wave 1A registration data.xlsx",
                 sheet = "SAMPLE A")
dem <- as.data.table(dem)
# names(dem) <- snakecase::to_snake_case(names(dem))

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

dem <- as.data.table(dem)
dw <- read_xlsx("data/raw_data/sc/panel_a/wave_1/Wave 1A dataset.xlsx")
dw <- as.data.table(dw)
names(dw) <- read_xlsx("data/raw_data/sc/panel_a/wave_1/IPSOS_scot_key.xlsx")
dmap <- as.data.table(dmap)
dmap <- dmap[!is.na(new_name)]


dmap[, new_name := gsub(" \\/.*|n_","",  new_name)]

dmap[is.na(contact) & is.na(hhm), hhm := 0]
dmap[grepl("salon", Question), new_name := "cnt_salon"]
dmap[grepl("In a healthcare setting", Question), new_name := "cnt_healthcare"]
dmap[grepl("Please list all other", Question), new_name := "contact_nickname"]
dmap[grepl("throat swab PRIOR to the past 14", Question), new_name := "part_hhm_covid_test_past"]

dmap[is.na(contact) & !is.na(hhm), contact := hhm]
dmap[!is.na(contact) & is.na(hhm), contact := contact + 19]
dmap[, contact_id := contact]
table(dmap$contact_id, useNA = "always")


# dw[, contact_id := ]
id_vars <- c("CP number", "Date")

questions <- intersect(names(dw), dmap$Question)
dw <- dw[, questions, with = F]
dwl <- melt(dw, id.vars = id_vars)

mname <- match(dwl$variable, dmap$Question)
nmes <- dmap$new_name[mname]


# mname <- match(dwl$variable, dmap$contact_id)
cids <- dmap$contact_id[mname]

dwl$new_name <- nmes
dwl$row_id <- cids

dwl[,variable := NULL]

names(dwl) <- snakecase::to_snake_case(names(dwl))

# dt <- dcast(cp_number + date + row_id ~ new_name, data = dwl, value.var = "value")

tables_list <- list()
for (var in unique(dwl$new_name)) {
  print(var)
  # browser()

  q <- dwl[new_name == var]
  if (sum(unique(q$row_id) != 0) != 0) {


    qd <- dcast(cp_number + date + row_id ~ new_name, data = q, var.value = "value")
    qd <- qd[!is.na(get(var))]
    tables_list[[var]] <- qd
  }
}

match_vars <- c("cp_number", "date", "row_id")
dt <- Reduce(function(...) merge(..., by = match_vars, all = T), tables_list)
dt[is.na(cnt_age) & !is.na(hhm_age), cnt_age := hhm_age]
dt[,hhm_gender := NULL]
dt[,hhm_age := NULL]


# demographics
#
names(dem)

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
dtm <- dtm[row_id < 999]
table(dtm$cnt_age, useNA = "always")
dtm <- dtm[!(row_id < 20 & is.na(hhm_contact_yn))]
dtm[, wave := "Wave 1"]
dtm[, panel := "Panel A"]
dtm[, week := 1]
dtm[, wave_id := "A 1"]
dtm[, country_code := "sc"]
dtm[, part_id := cp_number]

write.csv(dtm, "data/raw_data/sc/panel_a/wave_1/contacts_data_v6.csv")
contacts <- dtm

part_names <- names(dem)[1:10]
part_new_names <- c("cp_number", "part_age", "part_gender", "part_urban_rural",
                    "part_ethic_group", "part_ethicity", "part_ethnicity_other",
                    "part_high_risk", "part_medium_risk", "hh_size")
length(part_new_names)

part <- dem[, part_names, with = F]
# IMPORTANT: Remove participants who did not complete the contact survey
incomplete <- dem[!(`CP Number` %in% dw$`CP number`)]
part <- part[!cp_number %in% incomplete$`CP Number`]
setnames(part, part_names, part_new_names)
age_bins <- c(0, 5, 13, 18, 30, 40, 50, 60, 70, 120)

part[, part_age_group := cut(part_age,
                       breaks = age_bins,
                       right = FALSE)]
part[, wave := "Wave 1"]
part[, panel := "Panel A"]
part[, week := 1]
part[, wave_id := "A 1"]
part[, country_code := "sc"]
part[, part_id := cp_number]

part <- add_n_cnts_location_cols_scotland(part, contacts, replace_existing_cols = T)
write.csv(part, "data/raw_data/sc/panel_a/wave_1/participants_data_v5.csv")
saveRDS(part, "data/raw_data/sc/panel_a/wave_1/participants_data_v5.rds")



### Clean, Reshape, and Merge Multiple Contacts
######################################################

ptables_list <- list()
for (var in unique(dwl$new_name)) {
  # browser()

  q <- dwl[new_name == var]
  if (sum(unique(q$row_id) != 0) == 0) {
    print(var)

    qd <- dcast(cp_number + date + row_id ~ new_name, data = q, var.value = "value")
    qd <- qd[!is.na(get(var))]
    ptables_list[[var]] <- qd
  }
}

# match_vars <-
m_dt <- Reduce(function(...) merge(..., by = match_vars, all = T), ptables_list)

id_cols <- c("part_id", "date", "panel", "wave", "wave_id", "country",
             "country_code", "week", "weekday", "hhm_contact_yn",
             "part_reported_all_contacts")
mult_contacts_cols <- grep("multiple_contacts_", names(m_dt), value = TRUE)
mult_contacts_cols <- grep("precautions", mult_contacts_cols, value = TRUE, invert = T)
m_dt[, (mult_contacts_cols) :=  lapply(.SD, as.numeric), .SDcols = mult_contacts_cols]
# part[, (mult_contacts_cols) :=  lapply(.SD, as.numeric), .SDcols = mult_contacts_cols]


mult_contacts <- melt(m_dt,
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

