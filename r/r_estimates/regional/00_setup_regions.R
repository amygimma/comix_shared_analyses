part <- readRDS('data/uk/clean_participants.rds')

part[,
     nhs_regions := fcase(
       regions == "Scotland", "Scotland",
       regions == "Northern Ireland", "Northern Ireland",
       regions == "Wales", "Wales",
       regions == "Greater London", "London",
       regions == "South East",  "South East",
       regions == "South West", "South West",
       regions == "North West", "North West",
       regions == "West Midlands", "Midlands",
       regions == "East Midlands", "Midlands",
       regions == "North East", "North East and Yorkshire",
       regions == "Yorkshire and The Humber", "North East and Yorkshire",
       regions == "East of England", "East of England"
       
     )
]

part[,
     nhs_regions_combined := fcase(
       regions == "Scotland", "Scotland",
       regions == "Northern Ireland", "Northern Ireland",
       regions == "Wales", "Wales",
       regions == "Greater London", "London",
       regions == "South East",  "South",
       regions == "South West", "South",
       regions == "North West", "North",
       regions == "West Midlands", "Midlands",
       regions == "East Midlands", "Midlands",
       regions == "North East", "North East and Yorkshire",
       regions == "Yorkshire and The Humber", "North",
       regions == "East of England", "Midlands"
       
     )
                 ]

table(part$nhs_regions)

saveRDS(part, "data/uk/clean_participants.rds")

