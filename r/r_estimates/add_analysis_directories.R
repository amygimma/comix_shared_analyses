# Setup paths for analyses

panel_path <- paste("panel", tolower(panel_name), sep = "_")
if(TEST) { panel_path <- paste(panel_path, "test", sep = "_")}
wave_path <- paste("wave", wave_name, sep = "_")
survey_sub_path <- file.path(country_code_path, panel_path, wave_path)

base_data_path <- here("data")

if(!dir.exists(sprintf("%s/outputs", here()))){
  dir.create(sprintf("%s/outputs", here()))
}

base_outputs_path <- here("outputs")

data_path <- file.path(base_data_path, country_code_path)

outputs_path <- file.path(base_outputs_path, survey_sub_path)
matrices_path <- file.path(outputs_path, "contact_matrices")
online_matrices_path <- file.path(outputs_path, "contact_matrices_online")

# Create directories if they do not exist

# if (length(list.files(data_path)) == 0) {
#   stop(paste("Add CoMix contact and participant data.tables to: ", data_path))
# }

if (!file.exists(outputs_path)) {
  if(!file.exists(file.path(base_outputs_path, country_code_path))) {
    dir.create(file.path(base_outputs_path, country_code_path))
  }
  if(!file.exists(file.path(base_outputs_path, country_code_path, panel_path))) {
    dir.create(file.path(base_outputs_path, country_code_path, panel_path))
  }
  dir.create(file.path(base_outputs_path, country_code_path, panel_path, wave_path))

}

if (!file.exists(matrices_path)) dir.create(matrices_path)


if (!file.exists(online_matrices_path)) dir.create(online_matrices_path)

