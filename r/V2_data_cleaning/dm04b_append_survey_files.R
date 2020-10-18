library(snakecase)

source("r/functions/utility_functions.R")
source("r/user_setup.R")

# SETUP
base_data_path <- "data"
if (!is.null(USER_DATA_PATH)) base_data_path <- USER_DATA_PATH
panel_paths <- c("Panel E", "Panel EC")
wave_paths <- c("Wave 5")


combine_dts <- function(base_file_name, country_code, panels, waves) {
  panels_s <- to_snake_case(panels)
  waves_s <- to_snake_case(waves)

  # base_data_path <- file.path("data", country_code)
  base_data_file_path <- file.path(base_data_path, country_code, base_file_name)

  data_files <- list.files(base_data_path, recursive = TRUE)
  data_paths <- grep(base_file_name, data_files, value = TRUE)

  data_paths <- grep(paste(panels_s, collapse = "|"), data_paths, value = TRUE)
  data_paths <- grep(paste(waves_s, collapse = "|"), data_paths, value = TRUE)
  data_paths <- grep("interim|test|archive|raw_data|adj|^clean_", data_paths,
                     value = TRUE, invert = TRUE)

  if (length(data_paths) < (length(panel_paths) * length(wave_paths))) {
    stop("Not all data found, check wave and panel paths")
  }

  data_dts <- lapply(data_paths, function(data_path) {
    readRDS(file.path(base_data_path, data_path))
  })

  combined_data_dt <- readRDS(file.path(base_data_file_path))
  if (length(data_dts) > 0) {
    for(i in 1:length(data_dts)) {
      # if (grepl("panel_e", data_paths[i])) browser()
      message(data_paths[i])
      # browser()
      if (unique(data_dts[[i]]$panel) %in% unique(combined_data_dt$panel) &
          unique(data_dts[[i]]$wave_id) %in% unique(combined_data_dt$wave_id)){
        stop("Data for panel and wave already merged, new data file not saved")
      } else {
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

  return(combined_data_dt)
}

country_codes <- c("uk", "be", "nl", "no")[c(1)]
for (country_code in country_codes) {
  # country_code <- "uk"

  # # PARTICIPANTS
  part_base_file <- "clean_participants.rds"

  part_dt <- combine_dts(
    part_base_file, country_code,panels = panel_paths, waves = wave_paths)


  mult_contacts_cols <- grep("multiple_contacts_", names(part_dt), value = TRUE)
  part_dt[, (mult_contacts_cols) :=  lapply(.SD, as.numeric), .SDcols = mult_contacts_cols]
  table(part_dt$wave_id)

  # # CONTACTS
  # #
  cont_base_file <- "clean_contacts.rds"
  cont_dt <- combine_dts(
    cont_base_file, country_code,panels = panel_paths, waves = wave_paths)

  table(cont_dt$panel, cont_dt$wave)
  ncol(cont_dt)

  # HOUSEHOLDS
  # #
  hh_base_file <- "clean_households.rds"
  hh_dt <- combine_dts(hh_base_file, country_code, panels = panel_paths, waves = wave_paths)
  table(hh_dt$panel, hh_dt$wave)
  ncol(hh_dt)

  part_dt <- add_n_cnts_location_cols(part_dt, cont_dt, replace_existing_cols = TRUE)
  saveRDS(part_dt,
          file.path(base_data_path, country_code, part_base_file))
  saveRDS(cont_dt,
          file.path(base_data_path, country_code, cont_base_file))
  saveRDS(hh_dt,
          file.path(base_data_path, country_code, hh_base_file))
}

source("r/r_estimates/regional/00_setup_regions.R")
