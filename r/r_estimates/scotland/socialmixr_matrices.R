devtools::install_github('sbfnk/socialmixr')
library(socialmixr)
library("reshape2")
library("ggplot2")

part <- readRDS(file.path(data_path, "clean_participants.rds"))
contacts <- readRDS(file.path(data_path, "clean_contacts.rds"))


# c <- merge(contacts, part[, list(part_id, dayofweek)], by = "part_id")
# contacts$age
contacts[, date := (as.Date(as.character(date)) - 1)]
contacts[, weekday := weekdays(date)]


cdates <- unique(contacts[, list(part_id, date, weekday)])
part <- merge(part, cdates, by = "part_id", all.x = T)
part[n_cnt_all == 0, date := as.Date("2020-08-06")]
part[n_cnt_all == 0, weekday := weekdays(date)]
part[, dayofweek := weekday]
part[, country = "Scotland"]
comix_age_groups <- data.table(
  age_low = c(NA,  NA, NA, NA, 0,  65,  85,  0, 18, 1, 5,  5, 10, 12, 12, 15, 16, 18, 20, 25, 35, 45, 55, 65, 70, 75, 80, 85),
  age_high = c(NA, NA, NA, NA, 0, 100, 100, 17, 64, 4, 9, 11, 14, 17, 15, 19, 17, 19, 24, 34, 44, 54, 64, 69, 74, 79, 84, 100)
)
comix_age_groups[, "name"] <- c(
  "Don't know", "Don’t know", "This person is me", "Prefer not to answer", "Under 1", "65+", "85+",
  paste0(
    comix_age_groups[8:(nrow(comix_age_groups-1)), age_low],
    "-",
    comix_age_groups[8:(nrow(comix_age_groups-1)), age_high]
  )
)

contacts <- merge(contacts, comix_age_groups, by.x="cnt_age", by.y="name")
names(contacts)
setnames(contacts,
         old = c("cnt_age", "age_low", "age_high"),
         new = c("cnt_age_exact", "cnt_age_est_min", "cnt_age_est_max"))
names(contacts)

surv <- survey(contacts = contacts, participants = part)

results <- contact_matrix(survey = surv,
               survey.pop = popdata,
               n = 5,
               # bootstrap = ,
               weigh.dayofweek = TRUE,
               missing.contact.age = "sample",
               estimated.contact.age= "sample",
               age.limits = c(0,5,12,18,25,35,45,55,65,75,120),
               symmetric = T
               # country = "Sco"
              )
matrices <- results$matrices
mr <- Reduce("+", lapply(matrices, function(x) {x$matrix})) / length(matrices)


names(polymod$contacts)

df <- melt(mr, varnames = c("age1", "age2"), value.name = "contacts")
ggplot(df, aes(x = age2, y = age1, fill = contacts)) + theme(legend.position = "bottom") +
  geom_tile()

