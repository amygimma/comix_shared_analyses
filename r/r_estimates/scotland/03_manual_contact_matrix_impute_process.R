library(here)
library(data.table)
library(ggplot2)
here::here()
source("./r/r_estimates/manual_contact_matrix_functions.R")
#source("./r/change_R2/run_ngm_matrix.R")

##########################################################
#
#                 User defined options
#
##########################################################

# !! Uncomment to set manually, leave commented out to run as series
# panel_ <- c("A", "B", "AB", "AC", "BD")[1]
# filter_region <- c(NA, "ENGLAND")[2]
# panel_details <- c(NA, "cap_100", "ind_reported")[1]
# nboots <- c(NA, "boots_0", "boots_1000")[1]
#
# settings <- c(panel_, filter_region, panel_details, nboots)
# settings <- settings[!is.na(settings)]
# panel_name <- paste(settings, collapse = "_")
# # Save to specific folders so we don't overwrite data
# panel_name_ <- panel_name
# # panel_name
# wave_name <- "5"
# scripts_path <- here("r/change_in_R/")
# source(file.path(scripts_path, "add_analysis_directories.R"))
# comix_matrices_path <- matrices_path
# list.files(comix_matrices_path)
##########################################################
#
#                 END user defined options
#
##########################################################
#Read correct COMIX data
# scripts_path <- here("r/change_in_R/")
# source(file.path(scripts_path, "add_analysis_directories.R"))
# comix_matrices_path <- matrices_path
# panel_name <- strsplit(panel_name, "_")[[1]][1]
# comix_matrices_path <- "/Users/amygimma/comix/comix_shared_analyses/outputs/sc/panel_a_boots_10/wave_A_1/contact_matrices"
# outputs_path <- "/Users/amygimma/comix/comix_shared_analyses/outputs/sc/panel_a_boots_10/wave_A_1"
#get comix bootstrapped matrices
comix_boots <- readRDS(file.path(comix_matrices_path, "bootstrap_samples.rds"))
#get polymod bootstrapped matrices
# polymod_boots <- readRDS(file.path(polymod_matrices_path, "bootstrap_samples.rds"))

#get BBC national bootstrapped matrices
load("../contact_survey_2020/data/uk/bbc/bootstrapped_BBC_UK_filled.RData")
bbc_boots <- lapply(
  bootstrap.bbc.filled,
  function(x){
    return(as.matrix(x[["overall_all"]][, -"estimated_age_group"]))
  }
)
if(length(bbc_boots) != length(comix_boots)){
  bbc_boots <- sample(bbc_boots, length(comix_boots), replace = TRUE)
}

#get distribution of R0
r_values <- rnorm(2000, 2.6, 0.54)

# comix_boots_imputed <- comix_boots

#impute comix matrices
comix_boots_imputed <- lapply(
  1:length(comix_boots),
  function(x){
    out <- lapply(
      1:length(comix_boots[[x]]),
      function(z, x){
        # browser()
        zcomix <- comix_boots[[x]][[z]]
        # zpolym <- polymod_boots[[x]][[z]]
        zpolym <- bbc_boots[[x]]


        non_missing_rows <- which(!is.na(zcomix[1,]))
        non_missing_columns <- which(!is.na(zcomix[,1]))

        missing_rows <- which(is.na(zcomix[1,]))
        missing_columns <- which(is.na(zcomix[,1]))

        scaling_factor <- max(Re(eigen(
          zcomix[non_missing_rows, non_missing_columns],
          only.values = TRUE
        )$values))/max(Re(eigen(
          zpolym[non_missing_rows, non_missing_columns],
          only.values = TRUE
        )$values))

        zcomix[missing_rows, missing_columns] <- zpolym[missing_rows, missing_columns] * scaling_factor
        return(zcomix)
      }, x=x
    )
    names(out) <- names(comix_boots[[x]])
    return(out)
  }
)

#get imputed matrices by setting
comix_boots_imputed_setting <- lapply(
  1:length(comix_boots_imputed),
  function(x){
    out <- lapply(
      names(comix_boots_imputed[[x]])[which(names(comix_boots_imputed[[x]]) != "all")],
      function(name, x){
        return(comix_boots_imputed[[x]][[name]])
      }, x=x
    )
    names(out) <- names(comix_boots_imputed[[x]])[which(names(comix_boots_imputed[[x]]) != "all")]
    return(out)
  }
)
comix_boots_imputed_all2 <- lapply(
  comix_boots_imputed_setting,
  function(x){Reduce('+',x)}
)


comix_boots_imputed_all <- lapply(
  1:length(comix_boots_imputed),
  function(x){
    out <- lapply(
      names(comix_boots_imputed[[x]])[which(names(comix_boots_imputed[[x]]) == "all")],
      function(name, x){
        return(comix_boots_imputed[[x]][[name]])
      }, x=x
    )
    names(out) <- names(comix_boots_imputed[[x]])[which(names(comix_boots_imputed[[x]]) == "all")]
    return(out)
  }
)

#get POLYMOD matrices by setting
# polymod_boots_all <- lapply(polymod_boots, "[[", "all")
# polymod_boots_setting <- lapply(
#   1:length(polymod_boots),
#   function(x){
#     out <- lapply(
#       names(polymod_boots[[x]])[which(names(polymod_boots[[x]]) != "all")],
#       function(name, x){
#         return(polymod_boots[[x]][[name]])
#       }, x=x
#     )
#     names(out) <- names(polymod_boots[[x]])[which(names(polymod_boots[[x]]) != "all")]
#     return(out)
#   }
# )
# polymod_boots_all2 <- lapply(
#   polymod_boots_setting,
#   function(x){Reduce('+',x)}
# )

#get r0 scaling values
r0_scaling2 <- sapply(
  1:length(bbc_boots),
  function(x){
    max(Re(eigen(comix_boots_imputed_all2[[x]], only.values=T)$values[1]))/max(Re(eigen(bbc_boots[[x]], only.values=T)$values[1]))
  }
)


# Choose matrices for R scaling (POLYMOD)
# #########################################

# ***Use $all matrices***
# r0_scaling <- sapply(
#   1:length(comix_boots_imputed_all),
#   function(x){
#     max(Re(eigen(comix_boots_imputed_all[[x]]$all, only.values=T)$values[1]))/max(Re(eigen(polymod_boots_all[[x]], only.values=T)$values[1]))
#   }
# )
#

# ***Use combined setting matrices matrices***
# r0_scaling <- sapply(
#   1:length(comix_boots_imputed_all2),
#   function(x){
#     max(Re(eigen(comix_boots_imputed_all2[[x]], only.values=T)$values[1]))/max(Re(eigen(polymod_boots_all2[[x]], only.values=T)$values[1]))
#   }
# )

# Estimate R
# #################################
r_oth_scaled_bbc <- r0_scaling2 * r_values
# r_oth_scaled_poly <- r0_scaling * r_values

#process estimates
out <- data.table(
  r_bbc = r_oth_scaled_bbc,
  # r_poly = r_oth_scaled_poly,
  scaling = r0_scaling2
)
out[,"panel"] <- panel_name
out <- melt(out, id.vars="panel")
out_values <- out[, .(
  median=median(value),
  mean=mean(value),
  low=quantile(value, 0.025),
  iqr_low=quantile(value, 0.25),
  iqr_high=quantile(value, 0.75),
  high=quantile(value, 0.975)
), by=c("panel", "variable")]
out_values <- melt(out_values, measure.vars=c("median","mean","low", "iqr_low", "iqr_high","high"), variable.name = "estimate")
out_values <- data.table::dcast(out_values, variable~estimate)

out_values <- melt(out_values, measure.vars=c("median","mean","low", "iqr_low", "iqr_high","high"))
out_values[, value := round(value, 2)]
out_values_rounded <- data.table::dcast(out_values[variable != "scaling"], variable~variable.1)
out_values_rounded[, "variable"] <- factor(out_values_rounded[, variable], c("r_bbc","r_poly"), c("BBC Pandemic baseline", "POLYMOD baseline"))

fwrite(out_values_rounded, file.path(comix_matrices_path, "r0_estimates_panel.csv"))

saveRDS(comix_boots_imputed_all2, file.path(comix_matrices_path, "comix_boots_imputed_all_bbc.rds"))

# comix_boots_imputed
saveRDS(comix_boots_imputed, file.path(comix_matrices_path, "comix_boots_imputed_bbc.rds"))

message(file.path(comix_matrices_path, "comix_boots_imputed_bbc.rds"))
print(out_values_rounded)


reduced_matrix <- Reduce("+", lapply(comix_boots,function(x) {x$all})) / length(comix_boots_imputed_all2)
saveRDS(reduced_matrix, file.path(comix_matrices_path, "comix_reduced_matrix.rds"))
sm_dt <- sm_to_dt_matrix(reduced_matrix, paste("Scottish CoMix", gsub(" ", "",wave_ids)))
saveRDS(reduced_matrix, file.path(comix_matrices_path, "comix_reduced_matrix_dt.rds"))
write.csv(reduced_matrix, file.path(comix_matrices_path, "comix_reduced_matrix_dt.csv"))

matrix_plot <- gg_matrix(sm_dt, breaks = seq(0,15,1))
ggsave(plot =matrix_plot, filename = file.path(comix_matrices_path, "comix_reduced_matrix_plot.png"), width = 7, height =  3.5)


# w2 <- sm_dt
# w2m <- reduced_matrix
