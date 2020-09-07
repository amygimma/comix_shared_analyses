library(here)
library(data.table)
here::here()
source("./r/r_estimates/manual_contact_matrix_functions.R")
#source("./r/r_estimates/run_ngm_matrix.R")
##########################################################
#
#                 User defined options
#
##########################################################

#get bootstrapped datasets POLYMOD
#polymod_boots <- readRDS(file.path(polymod_matrices_path, "bootstrap_samples.rds"))

#get BBC regional data
load(file.path(data_path, "bbc/bootstrapped_BBC_NHS_regions.RData"))

#get Rt estimates
#r0_estimates <- readRDS(file.path(data_path, "uk_nowcasts_public.rds"))
#r0_estimates[region == "London", region := "Greater London"]

#loop through all regions

outlist <- list()
for(region_ in regions){
  print(region_)

 # r_est <- r0_estimates[region==region_][date == min(date), R]
  #r_est <- sample(r_est, 2000)

  r_other <- rnorm(2000, 2.6, 0.54)

  #get COMIX regional data
  comix_boots <- readRDS(file.path(comix_matrices_path, sprintf("bootstrap_samples_%s.rds", region_)))

  if(replace_na) comix_boots <- rapply(comix_boots, f=function(x) ifelse(is.na(x),0,x), how="replace" )

  #make matrices from BBC regional data
  bbc_boots <- lapply(
    bootstrap.bbc.nhsregions[[region_]]$bootmat.fill,
    function(x){
      return(as.matrix(x[["overall_all"]][, -"estimated_age_group"]))
    }
  )

  if(length(bbc_boots) != length(comix_boots)){
    bbc_boots <- sample(bbc_boots, length(comix_boots), replace = TRUE)
  }

  #get COMIX matrices by setting
  comix_boots_setting <- lapply(
    1:length(comix_boots),
    function(x){
      out <- lapply(
        names(comix_boots[[x]])[which(names(comix_boots[[x]]) != "all")],
        function(name, x){
          return(comix_boots[[x]][[name]])
        }, x=x
      )
      names(out) <- names(comix_boots[[x]])[which(names(comix_boots[[x]]) != "all")]
      return(out)
    }
  )
  comix_boots_setting <- lapply(
    1:length(comix_boots),
    function(x){
      out <- lapply(
        names(comix_boots[[x]])[which(names(comix_boots[[x]]) == "all")],
        function(name, x){
          return(comix_boots[[x]][[name]])
        }, x=x
      )
      names(out) <- names(comix_boots[[x]])[which(names(comix_boots[[x]]) == "all")]
      return(out)
    }
  )
  comix_boots_all2 <- lapply(
    comix_boots_setting,
    function(x){Reduce('+',x)}
  )

  ## Might remove? Could do a comprison of POLYMOD and BBC seperately
  # #get POLYMOD matrices by setting
  # polymod_boots_all2 <- lapply(
  #   polymod_boots_setting,
  #   function(x){Reduce('+',x)}
  # )

  #get r0 scaling values
  r0_scaling2 <- sapply(
    1:length(bbc_boots),
    function(x){
      max(Re(eigen(comix_boots_all2[[x]], only.values=T)$values[1]))/max(Re(eigen(bbc_boots[[x]], only.values=T)$values[1]))
    }
  )

  #apply scaling matrices to Rt and R0 estimates
  #r_est_scaled <- r0_scaling2*r_est
  r_oth_scaled <- r0_scaling2*r_other

  out <- data.table(
    #r_est = r_est_scaled,
    r_oth = r_oth_scaled,
    scaling = r0_scaling2
  )

  out[, "region"] <- region_
  out <- melt(out, id.vars="region")

  outlist[[length(outlist)+1]] <- out

  #Calculate NGM estimates
  #r_est_ngm <- sapply(
  #  1:length(bbc_boots),
  #  function(x){
  #    getR0ngm(bbc_boots[[x]], comix_boots_imputed_all2[[x]], r_est[x])
  #  }
  #)
  #r_oth_ngm <- sapply(
  #  1:length(bbc_boots),
  #  function(x){
  #    getR0ngm(bbc_boots[[x]], comix_boots_imputed_all2[[x]], r_other[x])
  #  }
  #)
}
out <- rbindlist(outlist)
saveRDS(out, file.path(comix_outputs_path, "outcomes_by_region.rds"))

#process results
out_values <- out[, .(
  median=median(value),
  mean=mean(value),
  low=quantile(value, 0.025),
  iqr_low=quantile(value, 0.25),
  iqr_high=quantile(value, 0.75),
  high=quantile(value, 0.975)
), by=c("region", "variable")]

out_values <- melt(out_values, measure.vars=c("median","mean","low", "iqr_low", "iqr_high","high"), variable.name = "estimate")
out_values[, value := round(value, 2)]
out_values_rounded <- dcast(out_values, region+variable~estimate)

fwrite(out_values_rounded, file.path(comix_matrices_path, "r0_estimates_panel.csv"))
