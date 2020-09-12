source("r/functions/utility_functions.R")

cnt <- readRDS("data/sc/clean_contacts.rds")
part <- readRDS("data/sc/clean_participants.rds")

table(part$wave, part$panel)
table(cnt$wave, cnt$panel)
table(part$week, part$panel)
table(cnt$week, cnt$panel)

# cnt[, cnt_work_education := ifelse(cnt_work == "Yes" | cnt_school == "Yes", "Yes", "No")]
# cnt[, cnt_other := ifelse(!(cnt_work == "Yes" | cnt_school == "Yes" | cnt_home == "Yes"), "Yes", "No")]
#
# cnt[, cnt_setting := fcase(
#   cnt_home == "Yes", "cnt_home",
#   cnt_work_education == "Yes", "cnt_work_education",
#   cnt_other == "Yes", "cnt_other"
# )]

expressions <- list(
  # Special cases (need to code into for loop)
  list(desc_name = "All"),
  list(desc_name = "Truncate 100 contacts", args = "trim_100"),
  list(desc_name = "Truncate 50 contacts", args = "trim_50"),
  list(desc_name = "Individually reported", cexp = expression(individually_reported == 1))
)

summ_list <- vector('list', length(expressions))

i <- 0
for(exp in expressions){
  # if (!is.null(exp$cexp)) browser()

  if (is.null(exp$byvars)) exp$byvars <- c("part_age_group", "wave")
  if (is.null(exp$pexp)) exp$pexp <- expression(TRUE)
  if (is.null(exp$cexp)) exp$cexp <- expression(TRUE)
  pexp <- part[eval(exp$pexp)]
  cexp <- cnt[eval(exp$cexp)]
  # cexp <- cexp[suspected_non_contact == 0 & suspected_multiple_contact == 0]
  if("trim_100" %in% exp$args){
    cexp <- trim_contacts(cexp, 100)
  }
  if("trim_50" %in% exp$args){
    cexp <- trim_contacts(cexp, 50)
  }

  pexp <- add_n_cnts_location_cols_scotland(pexp, cexp, replace_existing_cols = T)
  # if("trim_100" %in% exp$args) browser()
  summ_exp <- mean_summary_dt_by(pexp, cexp, exp$desc_name,
                                 byvars = exp$byvars)
  i = i + 1
  summ_list[[i]] <- summ_exp

}

summary_dt <- rbindlist(summ_list , fill = TRUE)

fwrite(summary_dt,
       paste0("outputs/sc/scotland_mean_contacts_part_age",".csv"))

