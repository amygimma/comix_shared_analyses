### Rename spss data

library(foreign)
library(data.table)
library(here)

## Update to the latest data and then savem
##
# spss_country_path <- c("nl_be", "no", "uk")[3]
# spss_path <- file.path(data_path, "raw_data", spss_country_path)

find_q63_spss <- function(panel_, wave_, country_code_, spss_files_) {
  # browser()
  panel_path <- tolower(gsub(" ", "_", panel_))
  wave_name <- gsub(" ","", wave_)

  regpattern <- paste(panel_path, wave_name, "Q63_flags.sav", sep = ".*")
  file_path <- grep(regpattern,  all_spss_files, value = T, ignore.case = T)
  if (length(file_path) == 1) {
    q63_spss_file <- file.path(spss_path, file_path)
    q63_spss_ <- as.data.table(read.spss(q63_spss_file))
    if (country_code_ == "uk") {
      message ("Contact flag spss found")
      q63_spss_$Qcountry <- "UK"
    }
    return(q63_spss_)
  } else {
    message ("Contact flag spss NOT found")
    return(NA)
  }
}


source("r/user_setup.R")
spss_path <- USER_SPSS_PATH

if (exists("spss_ref_")) {
  spss_data_path <- file.path(spss_path, country_code_, panel_)
  all_spss_files <- list.files(spss_data_path)
  spss_file <- grep(spss_ref_,  all_spss_files, value = TRUE)
  spss_file <- grep("\\.sav", spss_file, value = TRUE)
  spss_file <- file.path(spss_data_path, spss_file)
} else {
  all_spss_files <- list.files(file.path(spss_path), recursive = T)
  p <- "20-023770-01_LSHTM_Final_PanelA_v1_02042020_ICUO.*sav"
  spss_files <- grep(p, all_spss_files, value = T, ignore.case = T)

  # spss_files <- grep("uk.*panela.*wave1.*sav", all_spss_files, value = T, ignore.case = T)
  spss_files
  # Change index here
  spss_file <- spss_files[1]

  spss_file <- file.path(spss_path, spss_file)
  file.exists(spss_file)
}

spss_file
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
if (!"uk" %in% tolower(dt$Qcountry)) dt$Panel <- "Panel A"
if (grepl("NLBE_Wave4", spss_file)) dt$Wave <- "Wave 4"
if (dt$Wave[1] == "Wave4")

if (grepl("LSHTM_NO_Wave", spss_file)) dt$Qcountry <- "NO"
table(dt$Panel, dt$Wave, dt$Qcountry)
country_codes <- unique(as.character(dt$Qcountry))


for (country_code in country_codes) {
  dt_country <- dt[tolower(as.character(Qcountry)) == tolower(country_code)]
  country_code <- tolower(as.character(dt_country$Qcountry[1]))

  table(dt_country$Qcountry)

  panel_name <- tolower(gsub(" ", "_", as.character(dt_country$Panel[1])))
  wave_name <- tolower(gsub(" ", "_", as.character(dt_country$Wave[1])))

  # Add contact flags where available (not required in most waves)
  q63_spss <- find_q63_spss(dt_country$Panel[1], dt_country$Wave[1], country_code, spss_files)
  if (!is.na(q63_spss)) {
    if (is.null(dt_country$Respondent_ID)) setnames(dt_country, "Respondent_id", "Respondent_ID")
    dt_country <- merge(dt_country, q63_spss, by = c("Respondent_ID", "resp_age", "resp_gender", "Qcountry"))
    if (is.null(dt_country$contact1)) stop("Contact flags not added correctly")
  }

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

