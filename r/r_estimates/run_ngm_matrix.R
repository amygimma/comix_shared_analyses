library(data.table)
#library(socialmixr)
#library(CoMix)

cm_path = "~/workspace/covidm/"; ### CHANGE THIS to reflect the path to covidm.
cm_force_rebuild = F;
cm_build_verbose = F;
source(paste0(cm_path, "/R/covidm.R"))
source("~/workspace/covidm_reports/helper_functions.R")

## Setup contact matrix data
getR0ngm <- function(old_matrix, new_matrix, target_R0){
  params = cm_parameters_SEI3R(
    dem_locations = "United Kingdom",
    mat_locations = "United Kingdom of Great Britain",
    deterministic = T
  );
  
  age_groups <- data.table(
    age_low = c(0, 5, 13, 18, seq(30, 70, 10)),
    age_high = c(4, 12, 17, seq(29, 69, 10), 100)
  )
  age_groups[, "name"] <- paste0(age_groups[, age_low], "-", age_groups[, age_high])
  data_path <- "./data/uk/"
  country_code_path <- "uk"
  popyear <- 2020
  popdata <- getPopdata(country_code_path, year=popyear)
  
  params$pop[[1]]$size <- sapply(
    1:nrow(age_groups),
    function(a){
      sum(popdata[age >= age_groups[a, age_low] & age <= age_groups[a, age_high], total])
    }
  )
  params$pop[[1]]$fIp <- rep(1, nrow(age_groups))
  params$pop[[1]]$fIs <- rep(1, nrow(age_groups))
  params$pop[[1]]$fIa <- rep(0.5, nrow(age_groups))
  params$pop[[1]]$rho <- rep(1, nrow(age_groups))
  params$pop[[1]]$tau <- rep(1, nrow(age_groups))
  params$pop[[1]]$u <- rep(0.08, nrow(age_groups))
  
  params$pop[[1]]$y <- c(rep(0.056, 3), rep(0.49, 4), rep(0.74, 2))
  #c(0.14737184, 0.07302868, 0.29612258, 0.41910356, 0.44458672, 0.56357198, 0.81694435, 0.75055987)
  #params$pop[[1]]$y <- rep(0.5, nrow(age_groups))
  
  params$pop[[1]]$matrices <- list(old_matrix)
  
  params$pop[[1]]$u <- params$pop[[1]]$u * (target_R0 / cm_calc_R0_extended(params))
  
  params$pop[[1]]$matrices <- list(new_matrix)
  return(cm_calc_R0_extended(params)) 
}
