library(ggplot2)
library(patchwork)
library(cowplot)
library(data.table)
library(boot)

source("r/functions/utility_functions.R")

cntall <- readRDS("data/uk/clean_contacts.rds")
cntall <- cntall[(!week %in% 1:7)]
table(cntall$wave_id, cntall$survey_type)
# cntall <- cntall[week %in% 20:23 &  panel %in% c("Panel A", "Panel B", "Panel E", "Panel F")]
# cntall <- cntall[week %in% 20:23]

partall <- readRDS("data/uk/clean_participants.rds")
table(partall$part_age_group, useNA = "always")

partall <- partall[(!is.na(part_age_group) & !week %in% 1:7) & survey_type == "child"]
table(partall$part_age_group, useNA = "always")
# # partall <- partall[ week %in% 20:23 &  panel %in% c("Panel A", "Panel B", "Panel E", "Panel F")]
# partall <- partall[ week %in% 20:23 ]
table(partall$wave_id, partall$survey_type)

part <- partall


part[, date_category := fcase(
  # week %in% 1:5, "Weeks 1 to 5",
  week %in% 8:15, "Weeks 8 to 15",
  week %in% 16:19, "Weeks 16 to 19",
  # week == 20, "Week 20",
  # week == 21, "Week 21",
  # week == 22, "Week 22",
  # week == 23, "Week 23"
  week %in% 20:23, "Weeks 20 to 23"
)]

date_levs <- c(#"Weeks 1 to 5", 
              "Weeks 8 to 15", "Weeks 16 to 19",
              # "Week 20", "Week 21", "Week 22", "Week 23"
              "Weeks 20 to 23"
)
# dput(part[, .(date_cat_labs = paste0(
#   format(min(date), format = "%d %b"), " to ",
#   format(max(date), format = "%d %b"))), 
#   by = "date_category"]$date_cat_labs)
date_labs <- c(#"24 Mar to 27 Apr", 
               "14 May to 08 Jul", "08 Jul to 08 Aug", "09 Aug to 03 Sep" 
)
part[, date_category := factor(date_category, levels = date_levs, labels = date_labs)]
table(part$date_category)

age_levs <- c("[0,1)", "[1,5)","[5,12)", "[12,16)", "[12,18)", "[16,17)")
age_labels <- c("0-1", "1-4", "5-11", "12-15", "12-17", "16-17")
part[, part_age_group := factor(as.character(part_age_group), 
                                levels = age_levs, labels = age_labels)]
# table(part$part_age_group3, useNA = "always")
part[, part_age_group2 := as.character(part_age_group)]
part[, part_age_group3 := fcase(
  part_age_group2 %in% c("0-1", "1-4"), "0-4",
  part_age_group2 %in% c("5-11"), "5-11",
  part_age_group2 %in% c("12-15", "12-17", "16-17"), "12-17"
)]
part[, part_age_group := factor(part_age_group3, levels = c("0-4", "5-11", "12-17"))]
# partall[ ,part_age_group3 := factor(part_age_group,
#                                     levels = age_levs,
#                                     labels = age_labels)]



## Summarize contacts
##########################
# part <- partall[panel %in% c("Panel A", "Panel B")]
cnt <- cntall[part_id %in% part$part_id]
cnt <- cnt[suspected_multiple_contact == 0 & suspected_non_contact == 0]

part <- add_n_cnts_location_cols(part, cnt, replace_existing_cols = T)
part <- add_england_col(part)
# part <- part[regions == "Scotland"]
# part[, age_category := fcase(
#   part_age < 18, "Age < 18"
#   part_age >= 18 & < 40, "Age 18 - 39", 
#   part_age >= 40 & part_age < 60, "Age 40 - 59",
#   part_age >= 60, "Age 60 +")]
table(part$wave, part$panel)
table(cnt$wave, cnt$panel)
table(part$week, part$panel)
table(cnt$week, cnt$panel)
table(part$part_age_group)
table(part$date_category)
table(part$n_cnt_not_home_not_household)

part[, .(mean = mean(n_cnt_all), n = .N), 
     by = c("date_category", "nhs_regions")][order(nhs_regions, date_category)]

set.seed(12345)


part_age_groups <- unique(part$part_age_group)
date_categories <- levels(part$date_category)

cnt_trim <- trim_contacts(cnt, 100)
part_trim <- add_n_cnts_location_cols(part, cnt_trim, replace_existing_cols = T)
part_genders <- c("Male", "Female")
part <- part[part_gender_nb %in% part_genders]


part_cnt_means_subset <- function(vals, indices) {
  # browser()
  samples <- vals[indices]
  
  c(
    mean(samples)
  )
}
dt_lista <- list()

# for (k in 1:length(part_genders)){
# 
# 
# for(i in 1:length(part_age_groups)) {
#   for(j in 1:length(date_categories)) {
#     part_age_group_ <- part_age_groups[i]
#     date_category_ <- date_categories[j]
#     # part_gender_ <- part_genders[k]
#     part_gender_ <- "All"
#     k <- "All"
#     print(part_age_group_)
#     print(date_category_)
#     # expression_ <- expression(
#     #   as.character(part_age_group) == eval(part_age_group_) & 
#     #     as.character(date_category) == eval(date_category_) &
#     # as.character(part_gender_nb) == eval(part_gender_))
#     expression_ <- expression(
#       as.character(part_age_group) == eval(part_age_group_) & 
#         as.character(date_category) == eval(date_category_))
#     pids <- part[]$part_id
#     parts <- part[part_id %in% pids]
#     part_subset <- parts[eval(expression_)]
#     # browser()
#     bsr <- boot(part_subset$n_cnt_not_home_not_household, part_cnt_means_subset, R=1000)
#     # bsr$t
#     # plot(bsr, index=1)
#     bsr_ci <- boot.ci(bsr, index=1, type = "norm")
#     # browser()
#     dt_lista[[paste(k,i,j, sep = "-")]] <- data.table(part_age_group = part_age_group_, 
#                                                       date_category = date_category_,
#                                                       # part_gender = part_gender_,
#                                                       mean_original = bsr$t0,
#                                                       bs_mean = mean(bsr$t),
#                                                       bs_sd = sd(bsr$t),
#                                                       mean_low_ci = bsr_ci$norm[2],
#                                                       mean_high_ci = bsr_ci$norm[3],
#                                                       ci_level = bsr_ci$norm[1],
#                                                       R = bsr$R
#     )
#   }}
# 
# summary_dta <- rbindlist(dt_lista)
# summary_dta[, date_category := factor(date_category, date_levs)]
# # age_labs_18 <- c("18-24", "25-34", "35-44", 
# #                 "45-54", "55-64", "65-74", "75+")
# # age_levs_18 <- c("[18,30)", "[30,40)", "[40,50)", "[50,60)", "[60,70)", "[70,120)")
# # age_labs_18 <- c("18-30", "30-39", "40-49", "50-59", "60-69", "70+")
# table(summary_dta$part_age_group)
# summary_dta[, part_age_group := factor(part_age_group)]
# 
# # date_levs <- c("Weeks 1 to 5", "Weeks 8 to 15", "Weeks 16 to 19", "Weeks 20 to 23")
# # date_labs <- c("Weeks 1 to 5", "Weeks 8 to 15", "Weeks 16 to 19", "Weeks 20 to 23"
# # )
# # summary_dta[, date_category := factor(date_category, 
# #                                       levels = date_levs,
# #                                       labels = date_levs)]
# 
# summary_dta[mean_low_ci < 0, mean_low_ci := 0]
# ggthemr::ggthemr("dust")
# all <- ggplot(summary_dta, aes(x = part_age_group, y = mean_original, fill = date_category)) +
#   geom_col(position = "dodge") +
#   geom_errorbar(
#     aes(ymin = mean_low_ci, ymax = mean_high_ci, group = date_category), color = "#555555", 
#     width=0.35, position = position_dodge(width = 0.9), size = 0.3) +
#   scale_y_continuous(limits = c(0,4), breaks = seq(0,13,1)) +
#   ylab("Mean contacts") +
#   # facet_grid(col = vars(part_gender)) +
#   xlab("Participant age") +
#   labs(fill = "Lockdown phase") + 
#   ggtitle("All") +
#   theme_bw() 
# # theme(legend.position = "bottom") 
# all
# 
# 
# 
# ggsave("outputs/uk/combined/all_contacts_means_boots_conf_not_home_not_household_conf_not_home_not_household.png", all, width = 6, height = 4)############
# # TRIM 100
# ############
# # part_age_groups <- levels(part$part_age_group2)[4:10]
# # part_age_groups <- levels(part$part_age_group)
# # date_categories <- levels(part$date_category)
# 
# cnt_trim <- trim_contacts(cnt, 100)
# part_trim <- add_n_cnts_location_cols(part, cnt_trim, replace_existing_cols = T)
# # summ_trim <- part_trim[nhs_regions == "Scotland", .(mean = mean(n_cnt_all), n = .N),
# #      by = c("part_age_group")][order(part_age_group)]
# # summ_trim
# # summ_trim[, mean := round(mean, 2)]
# # summ_trim_mean <- dcast(data = summ_trim, nhs_regions ~ date_category, value.var = "mean")
# # summ_trim_n <- dcast(data = summ_trim, nhs_regions ~ date_category, value.var = "n")
# # 
# # summ_trimw <- merge(summ_trim_mean, summ_trim_n, by = "nhs_regions", suffixes = c(".mean", ".n"))
# # ord <- sort(names(summ_trimw))
# # summ_trimw <- summ_trimw[ , ord, with = F]
# # write.csv(summ_trimw, "outputs/uk/combined/panel_E_F_summary_wide_trunc_100.csv", row.names = F)
# # write.csv(summ_trim, "outputs/uk/combined/panel_E_F_summary.csv", row.names = F)
# 
# dt_list <- list()
# # for (k in 1:length(part_genders)){
# for(i in 1:length(part_age_groups)) {
#   for(j in 1:length(date_categories)) {
#     part_age_group_ <- part_age_groups[i]
#     date_category_ <- date_categories[j]
#     # part_gender_ <- part_genders[k]
#     k <- "All"
#     print(part_age_group_)
#     print(date_category_)
#     # expression_ <- expression(
#     #   as.character(part_age_group) == eval(part_age_group_) & 
#     #     as.character(date_category) == eval(date_category_) &
#     #     as.character(part_gender_nb) == eval(part_gender_))
#     expression_ <- expression(
#       as.character(part_age_group) == eval(part_age_group_) & 
#         as.character(date_category) == eval(date_category_))
#     pids <- part_trim[]$part_id
#     parts <- part_trim[part_id %in% pids]
#     part_subset <- part_trim[eval(expression_)]
#     
#     bsr <- boot(part_subset$n_cnt_not_home_not_household, part_cnt_means_subset, R=1000)
#     # bsr$t
#     # plot(bsr, index=1)
#     bsr_ci <- boot.ci(bsr, index=1, type = "norm")
#     # browser()
#     dt_list[[paste(k, i,j, sep = "-")]] <- data.table(part_age_group = part_age_group_, 
#                                                       date_category = date_category_,
#                                                       # part_gender = part_gender_,
#                                                       mean_original = bsr$t0,
#                                                       bs_mean = mean(bsr$t),
#                                                       bs_sd = sd(bsr$t),
#                                                       mean_low_ci = bsr_ci$norm[2],
#                                                       mean_high_ci = bsr_ci$norm[3],
#                                                       ci_level = bsr_ci$norm[1],
#                                                       R = bsr$R
#     )
#   }}#}
# 
# summary_dt <- rbindlist(dt_list)
# summary_dt[, date_category := factor(date_category, date_levs)]
# # summary_dt[, part_age_group := factor(part_age_group, 
# #                                       levels = age_levs_18,
# #                                       labels = age_labs_18)]
# 
# 
# ggthemr::ggthemr("dust")
# summary_dt[mean_low_ci < 0, mean_low_ci := 0]
# trim <- ggplot(summary_dt, aes(x = part_age_group, y = mean_original, fill = date_category)) +
#   geom_col(position = "dodge") +
#   geom_errorbar(
#     aes(ymin = mean_low_ci, ymax = mean_high_ci, group = date_category), color = "#555555", 
#     width=0.35, position = position_dodge(width = 0.9), size = 0.3) +
#   scale_y_continuous(limits = c(0,8), breaks = seq(0,20,0.5)) +
#   # facet_grid(col = vars(part_gender)) +
#   ylab("Mean contacts ") +
#   xlab("Participant age") +
#   labs(fill = "Lockdown phase") + 
#   ggtitle("B. Mean contacts (truncated to 100 contacts)") +
#   theme_bw() 
# # theme(legend.position = "bottom") 
# trim
# 
# 



############
# TRIM 50
############

cnt_trim25 <- trim_contacts(cnt, 50)
part_trim25 <- add_n_cnts_location_cols(part, cnt_trim25, replace_existing_cols = T)
summary(part_trim25$n_cnt_not_home)
dt_list25 <- list()
# for (k in 1:length(part_genders)){
for(i in 1:length(part_age_groups)) {
  for(j in 1:length(date_categories)) {
    part_age_group_ <- part_age_groups[i]
    date_category_ <- date_categories[j]
    # part_gender_ <- part_genders[k]
    k <- "All"
    print(part_age_group_)
    print(date_category_)
    # expression_ <- expression(
    #   as.character(part_age_group) == eval(part_age_group_) & 
    #     as.character(date_category) == eval(date_category_) &
    #     as.character(part_gender_nb) == eval(part_gender_))
    expression_ <- expression(
      as.character(part_age_group) == eval(part_age_group_) & 
        as.character(date_category) == eval(date_category_))
    pids <- part_trim25[]$part_id
    parts <- part_trim25[part_id %in% pids]
    part_subset <- parts[eval(expression_)]
    
    bsr <- boot(part_subset$n_cnt_not_home_not_household, part_cnt_means_subset, R=1000)
    # bsr$t
    # plot(bsr, index=1)
    bsr_ci <- boot.ci(bsr, index=1, type = "norm")
    # browser()
    dt_list25[[paste(k,i,j, sep = "-")]] <- data.table(part_age_group = part_age_group_, 
                                                       date_category = date_category_,
                                                       # part_gender = part_gender_,
                                                       mean_original = bsr$t0,
                                                       bs_mean = mean(bsr$t),
                                                       bs_sd = sd(bsr$t),
                                                       mean_low_ci = bsr_ci$norm[2],
                                                       mean_high_ci = bsr_ci$norm[3],
                                                       ci_level = bsr_ci$norm[1],
                                                       R = bsr$R
    )
  }}#}

summary_dt25 <- rbindlist(dt_list25)
summary_dt25[, date_category := factor(date_category, date_labs)]

# summary_dt25[, part_age_group := factor(part_age_group, 
#                                         levels = age_levs_18,
#                                         labels = age_labs_18)]


ggthemr::ggthemr("dust")
summary_dt25[mean_low_ci < 0, mean_low_ci := 0]
trim25 <- ggplot(summary_dt25, aes(x = part_age_group, y = mean_original, fill = date_category)) +
  geom_col(position = "dodge") +
  geom_errorbar(
    aes(ymin = mean_low_ci, ymax = mean_high_ci, group = date_category), color = "#555555", 
    width=0.35, position = position_dodge(width = 0.9), size = 0.3) +
  scale_y_continuous(limits = c(0,3), breaks = seq(0,20,1)) +
  ylab("Mean contacts ") +
  # facet_grid(col = vars(part_gender)) +
  xlab("Participant age") +
  labs(fill = "Lockdown phase") + 
  ggtitle("A. Mean contacts - UK (truncated to 50 contacts)") +
  theme_bw() 
# theme(legend.position = "bottom") 
trim25


############
# PLOT ALL
############
############
############
common_themes <- theme(plot.title = element_text(size = 10, face = "bold"),
                       axis.title.x = element_text(size = 9),
                       axis.title.y = element_text(size = 9),
                       legend.title = element_text(size = 9)) 
# y_scale <-   scale_y_continuous(limits = c(0,3))

# # comb_means <- ((all + common_themes)/ (trim + common_themes ) / (trim25 + common_themes )) + plot_layout(guides = "collect")
# comb_means <- ((trim + common_themes ) / (trim25 + common_themes )) + plot_layout(guides = "collect")
# comb_means 
# 
# ggsave("outputs/uk/combined/mean_contacts_lockdown_by_age_comb_conf_not_home_not_household_conf_not_home_not_household.png", comb_means,
#        width = 7.5, height = 6)

# proportion summary 

byvars <- c("part_age_group", "date_category")
p1 <- part[n_cnt_not_home_not_household > 0, .(m = mean(n_cnt_not_home_not_household), 
                                               n = .N, max = max(n_cnt_not_home_not_household),
                                               iqrl = quantile(n_cnt_not_home_not_household, p = 0.25),
                                               iqrh = quantile(n_cnt_not_home_not_household, p = 0.75)),
           by = byvars][order(part_age_group, date_category)]
pt <- part[, .(total = .N), by = byvars]

pp <- merge(p1, pt, by = byvars)

pp[, proportion := n/total]

ggplot(pp, aes(x = part_age_group, y = proportion, fill = date_category)) + 
  geom_col(position = "dodge")





############
# TRIM 50 SCOTLAND
############

cnt_trim25 <- trim_contacts(cnt, 50)
part_trim25 <- add_n_cnts_location_cols(part, cnt_trim25, replace_existing_cols = T)
summary(part_trim25$n_cnt_not_home)
dt_list25 <- list()
# for (k in 1:length(part_genders)){
for(i in 1:length(part_age_groups)) {
  for(j in 1:length(date_categories)) {
    part_age_group_ <- part_age_groups[i]
    date_category_ <- date_categories[j]
    # part_gender_ <- part_genders[k]
    k <- "All"
    print(part_age_group_)
    print(date_category_)
    # expression_ <- expression(
    #   as.character(part_age_group) == eval(part_age_group_) & 
    #     as.character(date_category) == eval(date_category_) &
    #     as.character(part_gender_nb) == eval(part_gender_))
    expression_ <- expression(
      as.character(part_age_group) == eval(part_age_group_) & 
        as.character(date_category) == eval(date_category_))
    pids <- part_trim25[regions == "Scotland"]$part_id
    parts <- part_trim25[part_id %in% pids]
    part_subset <- parts[eval(expression_)]
    
    bsr <- boot(part_subset$n_cnt_not_home_not_household, part_cnt_means_subset, R=1000)
    # bsr$t
    # plot(bsr, index=1)
    bsr_ci <- boot.ci(bsr, index=1, type = "norm")
    # browser()
    dt_list25[[paste(k,i,j, sep = "-")]] <- data.table(part_age_group = part_age_group_, 
                                                       date_category = date_category_,
                                                       # part_gender = part_gender_,
                                                       mean_original = bsr$t0,
                                                       bs_mean = mean(bsr$t),
                                                       bs_sd = sd(bsr$t),
                                                       mean_low_ci = bsr_ci$norm[2],
                                                       mean_high_ci = bsr_ci$norm[3],
                                                       ci_level = bsr_ci$norm[1],
                                                       R = bsr$R
    )
  }}#}

summary_dt25 <- rbindlist(dt_list25)
summary_dt25[, date_category := factor(date_category, date_labs)]

# summary_dt25[, part_age_group := factor(part_age_group, 
#                                         levels = age_levs_18,
#                                         labels = age_labs_18)]


ggthemr::ggthemr("dust")
summary_dt25[mean_low_ci < 0, mean_low_ci := 0]
trim25_scotland <- ggplot(summary_dt25, aes(x = part_age_group, y = mean_original, fill = date_category)) +
  geom_col(position = "dodge") +
  geom_errorbar(
    aes(ymin = mean_low_ci, ymax = mean_high_ci, group = date_category), color = "#555555", 
    width=0.35, position = position_dodge(width = 0.9), size = 0.3) +
  scale_y_continuous( breaks = seq(0,20,2)) +
  ylab("Mean contacts ") +
  # facet_grid(col = vars(part_gender)) +
  xlab("Participant age") +
  labs(fill = "Lockdown phase") + 
  ggtitle("B. Mean contacts - Scotland (truncated to 50 contacts)") +
  theme_bw() 
# theme(legend.position = "bottom") 
trim25_scotland


############
# TRIM 50 England
############

cnt_trim25 <- trim_contacts(cnt, 50)
part_trim25 <- add_n_cnts_location_cols(part, cnt_trim25, replace_existing_cols = T)
summary(part_trim25$n_cnt_not_home)
dt_list25 <- list()
# for (k in 1:length(part_genders)){
for(i in 1:length(part_age_groups)) {
  for(j in 1:length(date_categories)) {
    part_age_group_ <- part_age_groups[i]
    date_category_ <- date_categories[j]
    # part_gender_ <- part_genders[k]
    k <- "All"
    print(part_age_group_)
    print(date_category_)
    # expression_ <- expression(
    #   as.character(part_age_group) == eval(part_age_group_) & 
    #     as.character(date_category) == eval(date_category_) &
    #     as.character(part_gender_nb) == eval(part_gender_))
    expression_ <- expression(
      as.character(part_age_group) == eval(part_age_group_) & 
        as.character(date_category) == eval(date_category_))
    pids <- part_trim25[england == TRUE]$part_id
    parts <- part_trim25[part_id %in% pids]
    part_subset <- parts[eval(expression_)]
    
    bsr <- boot(part_subset$n_cnt_not_home_not_household, part_cnt_means_subset, R=1000)
    # bsr$t
    # plot(bsr, index=1)
    bsr_ci <- boot.ci(bsr, index=1, type = "norm")
    # browser()
    dt_list25[[paste(k,i,j, sep = "-")]] <- data.table(part_age_group = part_age_group_, 
                                                       date_category = date_category_,
                                                       # part_gender = part_gender_,
                                                       mean_original = bsr$t0,
                                                       bs_mean = mean(bsr$t),
                                                       bs_sd = sd(bsr$t),
                                                       mean_low_ci = bsr_ci$norm[2],
                                                       mean_high_ci = bsr_ci$norm[3],
                                                       ci_level = bsr_ci$norm[1],
                                                       R = bsr$R
    )
  }}#}

summary_dt25 <- rbindlist(dt_list25)
summary_dt25[, date_category := factor(date_category, date_labs)]

# summary_dt25[, part_age_group := factor(part_age_group, 
#                                         levels = age_levs_18,
#                                         labels = age_labs_18)]


ggthemr::ggthemr("dust")
summary_dt25[mean_low_ci < 0, mean_low_ci := 0]
trim25_england <- ggplot(summary_dt25, aes(x = part_age_group, y = mean_original, fill = date_category)) +
  geom_col(position = "dodge") +
  geom_errorbar(
    aes(ymin = mean_low_ci, ymax = mean_high_ci, group = date_category), color = "#555555", 
    width=0.35, position = position_dodge(width = 0.9), size = 0.3) +
  scale_y_continuous( breaks = seq(0,20,1)) +
  ylab("Mean contacts ") +
  # facet_grid(col = vars(part_gender)) +
  xlab("Participant age") +
  labs(fill = "Lockdown phase") + 
  ggtitle("C. Mean contacts - England (truncated to 50 contacts)") +
  theme_bw() 
# theme(legend.position = "bottom") 
trim25_england


comb_means_c <- ((trim25 + common_themes ) / (trim25_scotland + common_themes ) / (trim25_england + common_themes )) + plot_layout(guides = "collect")
comb_means_c

ggsave(filename = "outputs/uk/combined/childrens_bootstrapped_means_nh_nhh.png", plot = comb_means_c, height = 5, width = 5)
