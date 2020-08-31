library(ggplot2)
ggplot(
  out_values[variable != "scaling"],
  aes(
    x=factor(
      region,
      levels=out_values[variable=="r_oth"][order(median), region]
    ),
    xend=factor(
      region,
      levels=out_values[variable=="r_oth"][order(-median), region]
    )
  )
)+geom_segment(
  aes(
    y=low,
    yend=high
  ),
  colour="#A7A8AA"
)+geom_point(
 aes(
   y=median
 ),
 colour="#0D5257"
)+facet_grid(
  .~factor(
    variable,
    c("r_est", "r_oth"),
    c("Rt scaled", "R0 scaled")
  )
)+scale_y_continuous(
  limits=c(0,NA),
  breaks=seq(0,2,0.25)
)+geom_hline(
  yintercept=1,
  colour="#777777"
)+labs(
  x="Region",
  y="R0"
)+theme_bw(
)+theme(
  #axis.text.x = element_text(angle=60, hjust = 1)
)+coord_flip()
ggsave("./r_region_both.png", width=200, height=75, units="mm")

ggplot(
  out_values[variable == "r_oth"],
  aes(
    x=factor(
      region,
      levels=out_values[variable=="r_oth"][order(median), region]
    ),
    xend=factor(
      region,
      levels=out_values[variable=="r_oth"][order(-median), region]
    )
  )
)+geom_segment(
  aes(
    y=low,
    yend=high
  ),
  colour="#A7A8AA"
)+geom_point(
  aes(
    y=median
  ),
  colour="#0D5257"
)+facet_grid(
  .~factor(
    variable,
    c("r_est", "r_oth"),
    c("Rt scaled", "R0 scaled")
  )
)+scale_y_continuous(
  limits=c(0,NA),
  breaks=seq(0,2,0.25)
)+geom_hline(
  yintercept=1,
  colour="#777777"
)+labs(
  x="Region",
  y="R0"
)+theme_bw(
)+theme(
  #axis.text.x = element_text(angle=60, hjust = 1)
)+coord_flip()
ggsave("./r_region_r0.png", width=150, height=75, units="mm")

ggplot(
  out_values[variable == "r_est"],
  aes(
    x=factor(
      region,
      levels=out_values[variable=="r_est"][order(median), region]
    ),
    xend=factor(
      region,
      levels=out_values[variable=="r_est"][order(-median), region]
    )
  )
)+geom_segment(
  aes(
    y=low,
    yend=high
  ),
  colour="#A7A8AA"
)+geom_point(
  aes(
    y=median
  ),
  colour="#0D5257"
)+facet_grid(
  .~factor(
    variable,
    c("r_est", "r_oth"),
    c("Rt scaled", "R0 scaled")
  )
)+scale_y_continuous(
  limits=c(0,NA),
  breaks=seq(0,2,0.25)
)+geom_hline(
  yintercept=1,
  colour="#777777"
)+labs(
  x="Region",
  y="R0"
)+theme_bw(
)+theme(
  #axis.text.x = element_text(angle=60, hjust = 1)
)+coord_flip()
ggsave("./r_region_rt.png", width=150, height=75, units="mm")

out_values <- melt(out_values, measure.vars=c("median","mean","low","high"))
out_values[, value := round(value, 2)]
out_values_rounded <- dcast(out_values[variable != "scaling"], variable+region~variable.1)
out_values_rounded[, "variable"] <- factor(out_values_rounded[, variable], c("r_est","r_oth"), c("Rt scaled", "R0 scaled"))
fwrite(out_values, "r0_estimates_region.csv")

rt <- rbindlist(lapply(regions, function(reg){r0_estimates[region == reg & date == first(r0_estimates[order(date)][region == reg, date]), .(median = median(R), low=quantile(R, 0.025), high=quantile(R, 0.975)), by=c("region", "date")]}))
rt <- dcast(melt(rt, id.vars=c("region", "date"))[, value := round(value, 2)], region+date~variable)[, out := paste0(median, " (", low, "-", high, ")")][, c("region", "date", "out")]
rt <- rt[order(date)]
fwrite(rt, "first_rt.csv")
