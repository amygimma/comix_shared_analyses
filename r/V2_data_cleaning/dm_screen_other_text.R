
library(data.table)
source('r/functions/utility_functions.R')
# rm(country_code)
country_code_ <- "uk"
panel_ <- "panel_b"
wave_ <- "wave_3"


data_path <- "data"
wave_path <- file.path(data_path, country_code_, panel_, wave_)


part <- readRDS(file.path(wave_path, "clean_participants.rds"))
table(part$panel, part$wave, part$country)


contacts <- readRDS(file.path(wave_path, "clean_contacts.rds"))
table(contacts$panel, contacts$wave, contacts$country)

# EXPLORE

cnt <- contacts[!is.na(cnt_other_text)]$cnt_other_text
part_ids <- contacts[cnt_other_text %in% cnt]$part_id
length(unique(part_ids))
length(unique(cnt))
cnt[1:10]

# PATTERNS

none_pattern <- c("i am|is myself|is me|did not|didnt|no one|noone|nobody|no body|anyone|any one|none|no-one|NA|N-a|n-a|nowhere|no where|sorry|me|no contact|applicable|n\\/a|exist|stupid|dumb|fix|wrong|flaw|hell|x|what|should|blank|none|have not|dk|didn\\'t|error|no were|no\\-where|0|video|why|alone|know where|knowwhere")
none_phrases <- c(".", "am home alone", "at home met no one", "did n not have any contact", 
                 "did not meet", "does not apply", "everywhere - its me", "i did not", 
                 "i didn't", "i didn't go out but I couldn't select no persons", 
                 "I didn’t have contact with snyone", "i don't get the question", 
                 "i haven't been out", "i made no contact with anyone outside my household", 
                 "its me", "n o", "n/a", "no contact", "no direct contact", "no such person", 
                 "non", "not a person", "not applicable", "not at all", "nothing happened", 
                 "prefer not to answer", "they don't exist")

none_pattern <- paste(none_pattern, none_phrases, collapse = "|")


except <- "garden|street|door|outside|except|hall|stair|corridor|except|other than|at her|village|grounds|allotment|alotment|at allotment|donation|exercise|excercise|council|office|at my|at his|s home|taxi|lift"

# FILTER
none <- grep(none_pattern, cnt, value = T, ignore.case = T)
length(none)
none <- grep(except, none, invert = T,
             value = T, ignore.case = T)
keep <- grep(except, none, invert = F,
             value = T, ignore.case = T)
keep
length(none)
none <- unique(none)
none <- c(none_phrases, none)

lower_none <- unlist(lapply(none, tolower))
not_contacts <- contacts[tolower(cnt_other_text) %in% lower_none]
contacts_keep <- contacts[!(tolower(cnt_other_text) %in% lower_none)]

#CHECK MATCHING, APDATE AS NEEDED
unique(not_contacts$cnt_other_text)
unique(contacts_keep$cnt_other_text)
sort(unique(tolower(none)))
sort(unique(tolower(setdiff(cnt, none))))
adj_contacts <- contacts[!(tolower(cnt_other_text) %in% lower_none)]
nrow(adj_contacts)


# SAVE ADJ CONTACTS FILE
saveRDS(adj_contacts, file.path(wave_path, "clean_contacts_adjusted.rds"))

# ADD ADJ N_CNT COLS TO PARTICIPANTS AND SAVE

part_adj <- add_n_cnts_location_cols(part, adj_contacts, replace_existing_cols = TRUE)
# check for expected number of cols
n_cnt_names <- grep("n_cnt_", names(part_adj), value = T)
# if (length(n_cnt_names) != 18) stop("Check and update expected number of cols")

saveRDS(part_adj, file.path(wave_path, "clean_participants_adjusted.rds"))

message(nrow(contacts) - nrow(adj_contacts))
# message(nrow(part))
# message(nrow(contacts))
message(round(mean(part$n_cnt_all), 2))
# message(nrow(part))
# message(nrow(adj_contacts))
message(round(mean(part_adj$n_cnt_all), 2))

