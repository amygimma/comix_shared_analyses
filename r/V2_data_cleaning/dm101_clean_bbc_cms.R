## Clean the BBC contact matrix data
##

library(ggplot2)
library(RColorBrewer)

## Need the dev verions of data.table
## remotes::install_github("Rdatatable/data.table")

library(data.table)


load("data/uk/bbc/overall_bbc_regional_matrices.RData")
load("data/uk/bbc/regional_matrices_by_context_extra.RData")

## 3 types raw, fill, reciprocal
## Raw is the raw number, fill has used POLYMOD to impute missing ages, reciprocal is the stage between raw and fill


# Contacts are rows but need to be but need to be columns
# It is a data.table with the first variable being the age labels.



change_bbc_mat <- function(bbc_cm, 
                           row_names = 
                             c("0-4","5-12","13-17","18-29",
                               "30-39", "40-49", "50-59",
                               "60-69", "70+")) {
  
  bbc_mat <- bbc_cm[,-1]
  t(as.matrix(bbc_mat, rownames = row_names))
}


change_bbc_mat <- function(bbc_cm, 
                           row_names = 
                             c("0-4","5-12","13-17","18-29",
                               "30-39", "40-49", "50-59",
                               "60-69", "70+")) {
  
  bbc_mat <- bbc_cm[,-1]
  t(as.matrix(bbc_mat, rownames = row_names))
}


names(lapply(regional_matrices_fill, "["))
names(lapply(regional_matrices_fill, "[")[[1]])


lapply(regional_matrices_fill, "[")[[1]]


# lapply(lapply(lapply(lapply(regional_matrices_fill, "["), "[["), "["), "[")
# change_bbc_mat(x1[[1]][[1]])
# dim(x1[[1]][[1]])
# names(x1$`North East`$all_home)
names(bbc_regions)
names(regions_cms_raw)

pop_2018 <- readRDS((file.path(data_path, "uk_pop_2018.rds")))

contacts$cnt_age <- NULL # Need to remove for social mixr

# part_m <- part[, .(
#   date,
#   country,
#   part_id,
#   part_gender,
#   part_age,
#   part_pregnant,
#   part_isolate,
#   part_quarantine,
#   part_limit_work,
#   part_limit_school,
#   part_work_closed,
#   part_covid_test_result,
#   part_covid_contact,
#   area_2_name,
#   area_3_name
# )
# ]
# comix_survey <- survey(part_m, contacts)
# comix <- create_cm(comix_survey, 
#                       countries = part$country,
#                       age_limits = age_limits_sym, 
#                       symmetric = TRUE, 
#                       estimated.contact.age = "sample",
#                       survey.pop = pop_2018
#                       
# )


# load(file.path(matrices_path, "comix_cms.RData"))
readRDS(file.path(matrices_path, "comix_cm_imputed.rds"))


imputed_cm <- impute_cm(comix_cm, bbc_all)

image(comix_cm)
image(bbc_all)
image(imputed_cm)

bbc_regions
CoMix::max_eigen_ratio()

lapply(bbc_regions, eigen)


# MEAN CONTACTS

matrix_colSums_dt <- function(cm) {
  # matrix colsums to dt
  means <- colSums(cm)
  data.table(age_group = names(means), mean_contacts = means)
}

dt_colSums_dt <- function(dt) {
  # dt > matrix > colSums > dt
  cm <- as.matrix(dt[,-c(1), with = F][, names(dt)[-1] := lapply(.SD, as.numeric)], 
                  rownames.value =  names(dt)[-1])
  means <- colSums(cm)
  data.table(age_group = names(means), mean_contacts = means)
}

age_groups <- bbc_matrix_all_fill$estimated_age_group
age_levels <- c("[0,5)", "[5,13)", "[13,18)", "[18,30)", "[30,40)", "[40,50)", 
                "[50,60)", "[60,70)", "[70,120)")

# Overall BBC matrix
bbc_all_raw <- change_bbc_mat(bbc_matrix_all_raw)
bbc_all_fill <- change_bbc_mat(bbc_matrix_all_fill)


# Do for all regions
bbc_regions_fill <- lapply(regional_matrices_all_fill, change_bbc_mat)
bbc_regions_phys_fill <- lapply(regional_matrices_physical_fill, change_bbc_mat)

bbc_regions_raw <- lapply(regional_matrices_raw_all, change_bbc_mat)
bbc_regions_phys_raw <- lapply(regional_matrices_raw_physical, change_bbc_mat)


# !!!IMPORTANT = WHICH WAY ARE THE MATRICES?


#Fill all
bbc_means_all_fill <- lapply(regional_matrices_all_fill, dt_colSums_dt)
bbc_means_all_fill <- rbindlist(bbc_means_all_fill, idcol = TRUE)
setnames(bbc_means_all_fill, old = ".id", new = "region")
bbc_means_all_fill[, study := "BBC"]
bbc_means_all_fill[, type := "All"]


#Fill phys
bbc_means_phys_fill <- lapply(regional_matrices_physical_fill,dt_colSums_dt)
bbc_means_phys_fill <- rbindlist(bbc_means_phys_fill, idcol = TRUE)
setnames(bbc_means_phys_fill, old = ".id", new = "region")
bbc_means_phys_fill[, study := "BBC"]
bbc_means_phys_fill[, type := "Physical"]



part[regions %in% c("East of England", "Greater London", 
                   "Yorkshire and The Humber"), regions := fcase(
                     regions == "East of England", "East",
                     regions == "Greater London", "London",
                     regions == "Yorkshire and The Humber", "Yorkshire and the Humber"
                   )]
# Comix all
comix_means_all <- part[, .(mean_contacts = mean(n_cnt_all), 
                           type = "All",
                           study = "CoMix", 
                           N = .N), by = c("regions", "part_age_group")]
comix_means_all[, part_age_group := factor(as.character(part_age_group), 
                                          levels = levels(part_age_group),
                                          labels = age_labs)]
comix_means_all[, part_age_group := as.character(part_age_group)]
setnames(comix_means_all, old = c("regions", "part_age_group"), new = c("region", "age_group"))

# Comix phys
comix_means_phys <- part[, .(mean_contacts = mean(n_cnt_all_phys), 
                           type = "Physical",
                           study = "CoMix", 
                           N = .N), by = c("regions", "part_age_group")]
comix_means_phys[, part_age_group := factor(as.character(part_age_group), 
                                          levels = levels(part_age_group),
                                          labels = age_labs)]
comix_means_phys[, part_age_group := as.character(part_age_group)]
setnames(comix_means_phys, old = c("regions", "part_age_group"), 
         new = c("region", "age_group"))


                
mean_region <- rbindlist(list(bbc_means_all_fill,
                              bbc_means_phys_fill,
                              comix_means_all,
                              comix_means_phys
                              ), 
                         use.names = TRUE, fill = TRUE
                         # all = TRUE
                         )

mean_region[, age_group := factor(age_group, levels = levels(age_groups))]

# ggplot(mean_region, aes(x = age_group, y = mean_contacts)) +
#   geom_col(aes(fill = study), position = "dodge") +
#   facet_grid(rows = vars(region), cols = vars(type)) +
#   scale_fill_brewer(palette = "Paired") + 
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# # facet_grid(rows = vars(region), cols = vars(setting))


ggplot(mean_region, aes(x = age_group, y = mean_contacts)) +
  geom_point(aes(color = study), alpha = 0.5) +
  # geom_line(aes(color = study, group = interaction(study, type))) +
  geom_line(data = mean_region[type == "All"], 
            aes(color = study, group = interaction(study, type)), 
            alpha = 0.7, linetype = 1) +
  geom_line(data = mean_region[type == "Physical"], 
            aes(color = study, group = interaction(study, type)),
            alpha = 0.7, linetype = 2) +
  facet_wrap(vars(region)) +
  # scale_color_brewer(palette = "Accent") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) #+
  # scale_linetype_manual(aes(line_type))
  # 
  # ggplot(mean_region, aes(x = age_group, y = mean_contacts)) +
geom_point(aes(color = study), alpha = 0.5) +
  # geom_line(aes(color = study, group = interaction(study, type))) +
  geom_line(data = mean_region[type == "All"], 
            aes(color = study, group = interaction(study, type)), 
            alpha = 0.7, linetype = 1) +
  geom_line(data = mean_region[type == "Physical"], 
            aes(color = study, group = interaction(study, type)),
            alpha = 0.7, linetype = 2) +
  facet_wrap(vars(region)) +
  # scale_color_brewer(palette = "Accent") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) #+
                
 
lapply(regional_matrices_fill, change_bbc_mat
change_bbc_mat(regional_matrices_all_fill[[1]])

mapply()