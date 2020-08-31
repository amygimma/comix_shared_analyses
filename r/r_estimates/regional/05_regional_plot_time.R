library(data.table)
library(stringr)
library(ggplot2)
library(patchwork)
library(ggthemr)
library(cowplot)
library(forcats)


##NOTE THIS FILE NEEDS SOME REFACTORING AND FILLING OUT. PLEASE SEE THE PCLOUD DRIVE FOR RELEVANT DATA

outputs_dir <- "outputs/uk"
fname_id <- "week_19"
outputs_files <- list.files(outputs_dir, recursive = T)

# a5_r0_estimate_files <- grep("panel_a(.*)/wave_5(.*)data_filter_details.csv", 
#                           outputs_files, value = TRUE)
r0_estimate_files_regional <- grep("panel_all_regions_trim_100(.*)contacts_200(.*)data_filter_details.csv", outputs_files, value = TRUE)
r0_estimate_files_regional <- grep("week_2|week_8", r0_estimate_files_regional, invert = T, value = T)

r0_estimate_files <- c(r0_estimate_files_regional)
file_extract <- function(x) {
  # browser()
  # Get analysis details
  data_filter <- sub("\\/.*", "", x)
  data_details <- fread(file.path(outputs_dir, x))
  data_details[, data_filter := tolower(data_filter)]
  
  
  # Get r0 estimates
  data_dir <- sub("/data_filter_details.csv", "", x)
  r0_file_path <- file.path(outputs_dir, data_dir, "r0_estimates_panel.csv")
  data_R0 <- fread(r0_file_path)
  week <- as.integer(sub(".*?week_.*?(\\d+).*", "\\1", x))
  data_R0[, week := week]
  # if (file.exists(r0_file_path)){
  #   data_R0 <- fread(r0_file_path)
  #   data_R0[, data_filter := gsub("panel_", "", data_filter)]
  #   data <- merge(data_details, data_R0, by = "data_filter")
  #   setnames(data, old = "variable", new = "baseline_matrices")
  #   
  # } else {
  #   data <- data_details
  # }
  # 
  # data
}

r0_estimates <- lapply(r0_estimate_files, file_extract)

r0_estimates <- rbindlist(r0_estimates, fill = TRUE)
r0_estimates <- r0_estimates[variable == "r_oth"]
# r0_estimates <- r0_estimates[nboots == "boots_1000"]

fpath <- file.path(outputs_dir, "combined", 
                   paste0("combined_R0_regional_estimates_", fname_id, ".csv"))
fwrite(r0_estimates, fpath)
pdt <- readRDS("data/uk/clean_participants.rds")
weeks <- pdt[order(week), .(min_date = strftime(min(survey_date), format = "%d-%b"), 
                            max_date = strftime(max(survey_date), format = "%d-%b")), by = week]


r0_estimates <- merge(r0_estimates, weeks)
r0_estimates[, summary_col := paste0(median, " (", low, " to ", high, ")") ]


summ_r0 <- r0_estimates[, list(region, min_date, summary_col)]
date_levs <- c("21-May", "28-May", "04-Jun", "11-Jun", "18-Jun", "25-Jun", "02-Jul", "09-Jul", "16-Jul", "23-Jul", "31-Jul")

summ_r0[, min_date := factor(min_date, levels = date_levs)]
summ_r0 <- dcast(summ_r0,region ~ min_date)
fwrite(summ_r0, "outputs/uk/combined/regional_summary_r0_estimates_week_19.csv")

pylim <- 2

plot_r0_estimates <- r0_estimates[, off_plot_star := 
                                    ifelse(high > pylim , "*", NA_character_)]
plot_r0_estimates[, high := ifelse(high > pylim, pylim, high)]

plot_r0_estimatesb <- plot_r0_estimates
region_levs <- c( "Scotland", "Wales", "Northern Ireland", "South East", "North West", "Midlands", "South West", "London","North East and Yorkshire", "East of England")

plot_r0_estimatesa <- plot_r0_estimates[region %in% c("Scotland", "Wales", "Northern Ireland")]

plot_r0_estimatesa[, region := factor(region, levels = region_levs)]
nhs_regions <- ggplot(plot_r0_estimatesb[order(week, decreasing = F)], 
                      aes(x = factor(min_date, levels = date_levs), y = median)) +
  geom_hline(yintercept =  1, color = "#468284", alpha = 0.5, linetype = "dashed")+
  geom_errorbar(aes(ymin = low, ymax = high),
                color = "#C29365", width = 0.005, size = 0.65) +
  geom_point(color = "#C29365") +
  facet_wrap(vars(region), ncol = 2) +
  theme_bw() +
  # scale_x_continuous(breaks = seq(1,18,1)) +# +
  scale_y_continuous(breaks = seq(0,7,0.5), limits = c(0,pylim)) +
  geom_text(aes(label = off_plot_star, y = pylim - 0.2), size = 5, hjust = -0.15) +
  ylab("R0 Estimate") +
  xlab("Date") +
  # scale_x_date(date_labels = "%b-%d") + p
  theme(axis.text.x=element_text(angle = 90, size = 8))
nhs_regions

ggsave("outputs/uk/combined/r_region_rt_week19.png", nhs_regions,  width=150, height=150, units="mm")

ggthemr::ggthemr("fresh")

r0_estimate_files_england2weeks <- grep("panel_bd_england_trim_100_rm_non_contacts_rm_suspected_multiple_contacts_boots_200(.*)data_filter_details.csv", outputs_files, value = TRUE)

er0_estimatesl <- lapply(r0_estimate_files_england2weeks, file_extract)

er0_estimates <- rbindlist(er0_estimatesl, fill = TRUE)[!is.na(week)]
er0_estimates <- er0_estimates[variable == "BBC Pandemic baseline"]
er0_estimates[, region := "England"]
er0_estimates[, summary_col := paste0(median, " (", low, " to ", high, ")")]
er0_estimates <- er0_estimates[, off_plot_star := 
                                 ifelse(high > pylim , "*", NA_character_)]
er0_estimates[, high := ifelse(high > pylim, pylim, high)]
er0_estimates <- merge(er0_estimates, weeks)

country_levs <- c( "England", "Northern Ireland", "Scotland", "Wales")
plot_r0_estimatesa <- rbind(plot_r0_estimatesa, er0_estimates)
plot_r0_estimates[, region := factor(region, 
                                     levels = (c( "England", "Northern Ireland", "Scotland", "Wales")))]

countries <- ggplot(plot_r0_estimatesa[order(week, decreasing = F)], 
                    aes(x = factor(min_date, levels = date_levs), y = median)) +
  geom_hline(yintercept =  1, color = "#468284", alpha = 0.5, linetype = "dashed") +
  geom_errorbar(aes(color = region, ymin = low, ymax = high), size = 0.65, position = position_dodge(width = 0.5), width = 0.05) +
  geom_point(aes(color = region), position = position_dodge(width = 0.5)) +
  # facet_wrap(vars(region), ncol = 2) +
  theme_bw() +
  # scale_x_continuous(breaks = seq(1,18,1)) +# +
  scale_y_continuous(breaks = seq(0,7,0.5), limits = c(0, pylim)) +
  geom_text(aes(label = off_plot_star),  size = 5, hjust = -2, vjust = -7.5 ) +
  ylab("R0 Estimate") +
  xlab("Date") +
  # scale_x_date(date_labels = "%b-%d") + 
  theme(axis.text.x=element_text(angle = 90, size = 8)) #+
# guides(color = guide_legend(override.aes = list(linetype = 1, size=5, shape = 1)))
countries

(countries / nhs_regions) + plot_layout(guides = 'collect', heights = c(1,4))