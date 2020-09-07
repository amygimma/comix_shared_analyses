
library(data.table)
source('r/functions/utility_functions.R')

part <- readRDS(file.path("data", "uk", "clean_participants.rds"))
hh <- readRDS(file.path("data", "uk", "clean_households.rds"))
contacts <- readRDS(file.path("data", "uk", "clean_contacts.rds"))


part <- add_n_cnts_location_cols(part, contacts, replace_existing_cols = TRUE)


by <- c("week", "panel")

means_summary <- part[, .(mean_all = mean(n_cnt_all),
                          mean_all_phys = mean(n_cnt_all_phys),
                          mean_hh = mean(n_cnt_household),
                          mean_not_hh = mean(n_cnt_not_household),
                          mean_work = mean(n_cnt_work), 
                          mean_school = mean(n_cnt_school),
                          mean_home = mean(n_cnt_home),
                          mean_not_home = mean(n_cnt_not_home)),
                      by = by
                      ]


hh_summary <- hh[, .(hh_total = .N), by = by]
hh_contacts <- hh[hhm_contact_yn == "Yes", .(hh_contact = .N), by = by]

hh_summary <- merge(hh_summary, hh_contacts, by = by)
hh_summary[, hh_cont_prop := hh_contact / hh_total]


table_type <- table_to_df(expression(table(contacts$cnt_type, contacts$wave_id)))
table_freq <- table_to_df(expression(table(contacts$cnt_frequency, contacts$wave_id)))



means_summary[order(panel, week)]
hh_summary
table_type
table_freq


write.csv(means_summary[order(panel, week)], "contact_setting_means.csv")
write.csv(hh_summary, "hh_contacts.csv")
write.csv(table_type, "contacts_type.csv")
write.csv(table_freq, "contacts_freq.csv")



part_s <- part[panel == "Panel B" & part_id %in% part6$part_id]
contacts_s <- contacts[panel == "Panel B" & part_id %in% part6$part_id]
hh_s <- hh[panel == "Panel B" & part_id %in% part6$part_id]

# by <- c("week", "panel", "sniffer_device_type_final")
by <- c("week", "panel")

means_summary <- part_s[, .(mean_all = mean(n_cnt_all),
                          mean_all_phys = mean(n_cnt_all_phys),
                          mean_hh = mean(n_cnt_household),
                          mean_not_hh = mean(n_cnt_not_household),
                          mean_work = mean(n_cnt_work), 
                          mean_school = mean(n_cnt_school),
                          mean_home = mean(n_cnt_home),
                          mean_not_home = mean(n_cnt_not_home)),
                      by = by
                      ]


hh_summary <- hh_s[, .(hh_total = .N), by = by]
hh_contacts <- hh_s[hhm_contact_yn == "Yes", .(hh_contact = .N), by = by]

hh_summary <- merge(hh_summary, hh_contacts, by = by)
hh_summary[, hh_cont_prop := hh_contact / hh_total]


table_type <- table_to_df(expression(table(contacts_s$cnt_type, contacts_s$wave_id)))
table_freq <- table_to_df(expression(table(contacts_s$cnt_frequency, contacts_s$wave_id)))

means_summary[order(panel, week)]
hh_summary
table_type
table_freq

write.csv(means_summary[order(panel, week)], "subset_contact_setting_means.csv")
write.csv(hh_summary, "subset_hh_contacts.csv")
write.csv(table_type, "subset_contacts_type.csv")
write.csv(table_freq, "subset_contacts_freq.csv")


ggplot


# individual
# 
mean_not_hh <- part_s[, .(mean_all = mean(n_cnt_all),
  mean_hh = mean(n_cnt_household),
  mean_not_hh = mean(n_cnt_not_household)),
  by = c(by, "part_id")
]

by(mean_not_hh$mean_not_hh, mean_not_hh$week, summary)

library(ggplot2)
ggplot(mean_not_hh, aes(x = week, y = mean_not_hh)) +
  geom_boxplot(aes(group = week))

table(part_s[n_cnt_not_household > 10]$n_cnt_not_household, 
      part_s[n_cnt_not_household > 3]$week, part_s[n_cnt_not_household > 3]$weekday)


fweek <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
part[, weekday := factor(weekday, levels = fweek)]

