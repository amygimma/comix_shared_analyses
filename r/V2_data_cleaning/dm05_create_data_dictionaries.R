# SETUP
library(readxl)
varnames <- fread('raw_data/var_names.csv')
part_dt <- readRDS("inst/data/uk/clean_participants.rds")
cont_dt <- readRDS("inst/data/uk/clean_contacts.rds")
hh_dt <- readRDS("inst/data/uk/clean_households.rds")

data_check_dictionary <- fread("raw_data/dictionary.csv")
cb <- as.data.table(read_xlsx("raw_data/codebook/codebook.xlsx", 
                                      sheet = "spss_vars", skip = 1))

cb <- cb[, ipsos_varname := tolower(Variable)]
cb <- cb[, list(ipsos_varname, Position, Label)]


# ==============
# Participants 
# ==============

# CREATE DATA DICTIONARY
classes <- unlist(lapply(names(part_dt), function(var) class(part_dt[[var]])))
data_dictionary <- data.table(new_name = names(part_dt), 
                              var_class = classes)


# MERGE WITH VARNAMES
data_dictionary <- merge(data_dictionary, varnames[, -c("var")], all.x = TRUE)


# HANDLE HHM COLUMNS
data_dictionary <- data_dictionary[, hhm_var_name := ifelse(
  is.na(ipsos_varname), gsub("part_","hhm_",new_name), NA_character_
)]
data_dictionary$h_ipsos_var <- 
  varnames$ipsos_varname[match(data_dictionary$hhm_var_name, varnames$new_name)]
data_dictionary <- data_dictionary[, all_ipsos_varname := ifelse(
                                     is.na(ipsos_varname), h_ipsos_var, ipsos_varname)]
data_dictionary <- data_dictionary[, hhm_var_name := ifelse(
                                     !is.na(h_ipsos_var), hhm_var_name, NA_character_)]

                            
setnames(data_dictionary, "all_ipsos_varname", "ipsos_varname")

# MERGE DESCRIPTIONS
data_dictionary <- merge(data_dictionary, cb, by = "ipsos_varname", 
                          all.x = TRUE)

# FORMAT
setnames(data_dictionary, 
         old = c("Position", "new_name", "Label", "ipsos_varname", 
                 "var_class", "type", "tablename", "hhm_var_name"),
         new = c("position", "comix_name", "description", "ipsos_varname", 
                 "var_class", "type", "tablename",  "comix_hhm_name"))
data_dictionary <- 
  data_dictionary[order(position), c("position", "comix_name", "ipsos_varname","description",  
                  "var_class", "type", "tablename",  "comix_hhm_name"),
                  with = FALSE
                  ]

# SAVE 
write.csv(data_dictionary, "raw_data/codebook/participants_dictionary.csv", 
          row.names = FALSE)



# ===========
# CONTACTS 
# ===========

classes <- unlist(lapply(names(cont_dt), function(var) class(cont_dt[[var]])))
data_dictionary <- data.table(new_name = names(cont_dt), 
                              var_class = classes)
nrow(data_dictionary)

# MERGE WITH VARNAMES
data_dictionary <- merge(data_dictionary, varnames[, -c("var")], all.x = TRUE)
data_dictionary <- data_dictionary[, ipsos_varname := 
                                     gsub("loop_100_", "loop_1_", ipsos_varname)]

# MERGE DESCRIPTIONS
data_dictionary <- merge(data_dictionary, cb, by = "ipsos_varname", 
                         all.x = TRUE)

# FORMAT
setnames(data_dictionary, 
         old = c("Position", "new_name", "Label", "ipsos_varname", 
                 "var_class", "type", "tablename"),
         new = c("position", "comix_name", "description", "ipsos_varname", 
                 "var_class", "type", "tablename"))
data_dictionary <- 
  data_dictionary[order(position), c("position", "comix_name", "ipsos_varname","description",  
                                     "var_class", "type", "tablename"),
                  with = FALSE
                  ]

# SAVE 
write.csv(data_dictionary, "raw_data/codebook/contacts_dictionary.csv", 
          row.names = FALSE)





# ============
# HOUSEHOLDS
# ============

classes <- unlist(lapply(names(hh_dt), function(var) class(hh_dt[[var]])))
data_dictionary <- data.table(new_name = names(hh_dt), 
                              var_class = classes)
nrow(data_dictionary)

# MERGE WITH VARNAMES
data_dictionary <- merge(data_dictionary, varnames[, -c("var")], all.x = TRUE)
data_dictionary <- data_dictionary[, ipsos_varname := 
                                     gsub("loop_100_", "loop_1_", ipsos_varname)]

# MERGE DESCRIPTIONS
data_dictionary <- merge(data_dictionary, cb, by = "ipsos_varname", 
                         all.x = TRUE)

# FORMAT
setnames(data_dictionary, 
         old = c("Position", "new_name", "Label", "ipsos_varname", 
                 "var_class", "type", "tablename"),
         new = c("position", "comix_name", "description", "ipsos_varname", 
                 "var_class", "type", "tablename"))
data_dictionary <- 
  data_dictionary[order(position), c("position", "comix_name", "ipsos_varname","description",  
                                     "var_class", "type", "tablename"),
                  with = FALSE
                  ]

# SAVE 
write.csv(data_dictionary, "raw_data/codebook/households_dictionary.csv", 
          row.names = FALSE)
