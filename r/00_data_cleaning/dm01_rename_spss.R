### Rename spss data
library(foreign)
library(data.table)

## Update to the latest data and then savem
##

spss_country_path <- c("nl_be", "no", "uk")[3]
path <- file.path("data", "raw_data", "spss_files", spss_country_path)
spss_files <- list.files(path)
spss_files

# Change index here
spss_file <- spss_files[7]

spss_file <- here(path, spss_file)
# spss_file <- here(path, "20-037762_PCW1_interim_v1_130520_ICUO_sav.sav")

df <- read.spss(spss_file)

dt <- as.data.table(df)
ncol(dt)
nrow(dt)

table(dt$Qcountry)
grep("Q76", names(dt), value = TRUE)



# Needed when the wave is recorded as "Wave3" instead of "Wave 3"
dt[, Wave := as.character(gsub("([a-z])([0-9])", "\\1 \\2", Wave))]
data_path <- "data"


#Sometimes needed for early waves
if (!"uk" %in% spss_country_path) dt$Panel <- "Panel A"
if (grepl("NLBE_Wave4", spss_file)) dt$Wave <- "Wave 4"

if (grepl("LSHTM_NO_Wave", spss_file)) dt$Qcountry <- "NO"
table(dt$Panel, dt$Wave, dt$Qcountry)
country_codes <- unique(as.character(dt$Qcountry))


for (country_code in country_codes) {
  dt_country <- dt[as.character(Qcountry) == country_code]
  country_code <- tolower(as.character(dt_country$Qcountry[1]))
  table(dt_country$Qcountry)

  panel_name <- tolower(gsub(" ", "_", as.character(dt_country$Panel[1])))
  wave_name <- tolower(gsub(" ", "_", as.character(dt_country$Wave[1])))
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

  survey_path <- file.path(survey_path, "survey_data.rds")
  saveRDS(dt_country, survey_path)

  message(paste("Saved to:", survey_path))
}

