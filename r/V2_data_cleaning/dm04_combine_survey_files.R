## Combine survey files
source("r/functions/utility_functions.R")


if(file.exists("r/user_setup.R")) source("r/user_setup.R")
if (!is.null(USER_DATA_PATH) & !SAVE_LOCAL) data_path <- USER_DATA_PATH

SAVE_LOCAL <- T
data_path <- "data"
country_code <- "uk"

combine_dts <- function(base_file_name, country_code) {
  base_data_path <- file.path(data_path, country_code)
  data_files <- list.files(base_data_path, recursive = TRUE)
  data_paths <- grep(base_file_name, data_files, value = TRUE)
  data_paths <- grep("panel_e|panel_ec|panel_f|panel_fc", data_paths, value = TRUE)

  data_paths <- grep("interim|test|archive|raw_data|adj", data_paths,
                     value = TRUE, invert = TRUE)

  data_dts <- lapply(data_paths, function(data_path) {
    readRDS(file.path(base_data_path, data_path))
  })

  combined_data_dt <- data_dts[[1]]
  if (length(data_dts) > 1) {
    for(i in 2:length(data_dts)) {
      message(unique(data_dts[[i]]$wave_id))
      message(i)
      if (class(data_dts[[i]]$phys_contact) == "character") browser()

      if(TRUE) {
        mult_contacts_cols <- grep("multiple_contacts_", names(data_dts[[i]]), value = TRUE)
        if(length(mult_contacts_cols) > 0) {
          data_dts[[i]][, (mult_contacts_cols) :=  lapply(.SD, as.character), .SDcols = mult_contacts_cols]
        }
        mult_contacts_cols <- grep("multiple_contacts_", names(combined_data_dt), value = TRUE)
        if(length(mult_contacts_cols) > 0){
          combined_data_dt[, (mult_contacts_cols) :=  lapply(.SD, as.character), .SDcols = mult_contacts_cols]
        }
      }

      by_vars <- intersect(names(combined_data_dt), names(data_dts[[i]]))
      combined_data_dt <- merge(combined_data_dt, data_dts[[i]],
                                by = by_vars, all = TRUE)
    }
  }
  # browser()

  return(combined_data_dt)
}

# PARTICIPANTS
#
part_base_file <- "clean_participants.rds"
part_dt <- combine_dts(part_base_file, country_code)
table(part_dt$panel, part_dt$wave)
ncol(part_dt)

# CONTACTS
#
cont_base_file <- "clean_contacts.rds"
cont_dt <- combine_dts(cont_base_file, country_code)
table(cont_dt$panel, cont_dt$wave)
ncol(cont_dt)

# HOUSEHOLDS
# #
hh_base_file <- "clean_households.rds"
hh_dt <- combine_dts(hh_base_file, country_code)
table(hh_dt$panel, hh_dt$wave)
ncol(hh_dt)

part_dt <- add_n_cnts_location_cols(part_dt, cont_dt, replace_existing_cols = TRUE)

# dir.create(file.path("data", country_code, "panels_e_f"))
saveRDS(part_dt,
        file.path(data_path, country_code, "panels_e_f", part_base_file))
saveRDS(cont_dt,
        file.path(data_path, country_code, "panels_e_f", cont_base_file))
saveRDS(hh_dt,
        file.path(data_path, country_code, "panels_e_f", hh_base_file))


