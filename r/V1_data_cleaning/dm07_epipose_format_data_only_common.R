####################################################################################
########### File for formatting the Comix survey into the Zenodo format   ##########
### Last modified 04/06 by Pietro Coletti
###  - Updated 08/06 by Amy Gimma for local structure
### 
### PLEASE DO NOT MODIFY OUTPUT DATA
### 
### 
### Cleaning 
# rm(list=ls(all=TRUE))
# ### Automatically set working directory
# if(require(rstudioapi) && isAvailable()){
#   current_path <- getActiveDocumentContext()$path 
#   setwd(dirname(current_path ))
# }
### Load libraries
library(stringr)
library(dplyr) # for "mutate"

#####################       INPUT SECTION ##################### 
id_wave=3   # Id of the wave
id_wave_string=paste0("A ",id_wave)
country_code <- c("be", "nl", "uk")[3]
folder <- file.path("data", country_code)   ## Location of "XXX_clean.rds" files
folder <- file.path("outputs", country_code)country_code="BE"
###############################################################


## Load participant data
fname <- file.path(folder,"clean_participants.rds")
part_data<-readRDS(fname)
part_data<-subset(part_data,part_data$wave_id==id_wave_string)


colums2keep<-c("part_id",
               "part_age",
               "part_gender"
)
indexes_to_keep<-which(colnames(part_data) %in% colums2keep)
part_data_common<-part_data[,indexes_to_keep]

#Fixing participant gender
part_data_common$part_gender[part_data_common$part_gender=="Female"]<-"F"
part_data_common$part_gender[part_data_common$part_gender=="Male"]<-"M"


part_data_common$HH_id<-unlist(lapply(part_data_common$part_id, function(x) paste0('HH',x)))
### save part data_common
fname=paste0("./output_dataset/CoMix_wave_",id_wave,"_participant_common.csv")
write.table(sep=";",part_data_common,file=fname,row.names =FALSE)

colums2drop<-c("part_age",
               "part_gender"
)
indexes_to_drop<-which(colnames(part_data) %in% colums2drop)
part_data_extra<-part_data[,-indexes_to_drop]
### save part data_extra
fname=paste0(output_folder,"CoMix_",country_code,"_wave_",id_wave,"_participant_extra.csv")
write.table(sep=";",part_data_extra,file=fname,row.names =FALSE)


## Load contact data
fname=paste0(folder,"clean_contacts.rds")
cont_data<-readRDS(fname)
cont_data<-subset(cont_data,cont_data$wave_id==id_wave_string)

## Formatting duration according to POLYMOD intervals:
#1: less than 5 minutes
#2: 5 - 15 mins
#3: 15 mins- 1hour
#4: 1hour- 4hours
#5: 4 hours or more
cont_data <- cont_data %>% mutate(duration_multi=cut(cnt_total_mins,breaks=c(-Inf, 5, 15, 60, 240, Inf), 
                                                     labels=c(as.integer(1),as.integer(2),as.integer(3),as.integer(4),as.integer(5))))



cont_data$phys<-str_remove_all(cont_data$phys,",")



colums2keep<-c("cont_id",
               "cnt_age_est_min",
               "cnt_age_est_max",
               "cnt_gender",
               "cnt_home",
               "cnt_work",
               "cnt_school",
               "cnt_public_transport",
               "cnt_leisure",
               "phys_contact",
               "cnt_frequency"
)

indexes_to_keep<-which(names(cont_data) %in% colums2keep)
cont_data_common<-cont_data[ ,indexes_to_keep]
#cont_data_common<-data.frame(cont_data_common)
#Fixing contact gender
cont_data_common$cnt_gender[cont_data_common$cnt_gender=="Female"]<-"F"
cont_data_common$cnt_gender[cont_data_common$cnt_gender=="Male"]<-"M"
cont_data_common$cnt_gender[cont_data_common$cnt_gender=="Don’t know"]<-"DK"
cont_data_common$cnt_gender[cont_data_common$cnt_gender=="Prefer not to answer"]<-"No answer"
cont_data_common$cnt_gender[cont_data_common$cnt_gender=="In another way"]<-"Other"
#fixing cnt_location
# Home
cont_data_common$cnt_home[cont_data_common$cnt_home=="Yes"]<-as.integer(1)
cont_data_common$cnt_home[cont_data_common$cnt_home=="No"] <-as.integer(0)
# Work
cont_data_common$cnt_work[cont_data_common$cnt_work=="Yes"]<-as.integer(1)
cont_data_common$cnt_work[cont_data_common$cnt_work=="No"] <-as.integer(0)
# School
cont_data_common$cnt_school[cont_data_common$cnt_school=="Yes"]<-as.integer(1)
cont_data_common$cnt_school[cont_data_common$cnt_school=="No"] <-as.integer(0)
# Transport
cont_data_common$cnt_public_transport[cont_data_common$cnt_public_transport=="Yes"]<-as.integer(1)
cont_data_common$cnt_public_transport[cont_data_common$cnt_public_transport=="No"] <-as.integer(0)
colnames(cont_data_common)[colnames(cont_data_common) == "cnt_public_transport"] <- "cnt_transport"
# Leisure
cont_data_common$cnt_leisure[cont_data_common$cnt_leisure=="Yes"]<-as.integer(1)
cont_data_common$cnt_leisure[cont_data_common$cnt_leisure=="No"] <-as.integer(0)
# Merging several columns into "other"
colums2keep_other<-c("cnt_outside_other",
  "cnt_otheryn",
  "cnt_public_market",	
  "cnt_other_house",
  "cnt_worship",
  "cnt_supermarket",
  "cnt_shop"
)
indexes_to_keep<-which(names(cont_data) %in% colums2keep_other)
df_contact_other<-cont_data[ ,indexes_to_keep]
cnt_other<-apply(df_contact_other, 1, function(r) any(r == "Yes"))
cont_data_common$cnt_otherplace<-cnt_other
cont_data_common$cnt_otherplace[cont_data_common$cnt_otherplace==TRUE]<-as.integer(1)
cont_data_common$cnt_otherplace[cont_data_common$cnt_otherplace==FALSE]<-as.integer(0)
cont_data_common$part_id<-as.integer(str_split_fixed(cont_data$cont_id,"-",n=2)[,1])

### Fixing frequency values
cont_data_common$cnt_frequency[cont_data_common$cnt_frequency=="Every day or almost every day"]<-as.integer(1)
cont_data_common$cnt_frequency[cont_data_common$cnt_frequency=="About once or twice a week"]<-as.integer(2)
cont_data_common$cnt_frequency[cont_data_common$cnt_frequency=="Every 2-3 weeks"]<-as.integer(3)
cont_data_common$cnt_frequency[cont_data_common$cnt_frequency=="About once per month"]<-as.integer(4)
cont_data_common$cnt_frequency[cont_data_common$cnt_frequency=="Less often than once per month"]<-as.integer(5)
cont_data_common$cnt_frequency[cont_data_common$cnt_frequency=="Never met them before"]<-as.integer(6)
cont_data_common$cnt_frequency[cont_data_common$cnt_frequency=="Prefer not to answer"]<-as.integer(7)


cont_data_common$age_exact<-rep(NA,length(cont_data_common$cont_id))
cont_data_common<-data.frame(cont_data_common)
cont_data_common$cnt_age_exact<-as.integer(rowMeans(cont_data_common[c("cnt_age_est_min","cnt_age_est_max")] , na.rm=FALSE))
fname=paste0(output_folder,"CoMix_",country_code,"_wave_",id_wave,"_contact_common.csv")
write.table(sep=";",cont_data_common,file=fname,row.names =FALSE,quote = FALSE)

## Load household data
fname=paste0(folder,"clean_households.rds")
HH_data<-readRDS(fname)
HH_data<-subset(HH_data,HH_data$wave_id==id_wave_string)

colums2keep<-c("part_id","hh_size","country_code")
indexes_to_keep<-which(colnames(HH_data) %in% colums2keep)
HH_common<-HH_data[,indexes_to_keep]
HH_common<-HH_common[!duplicated(HH_common[ , "part_id"]),]
HH_common$HH_id<-unlist(lapply(HH_common$part_id, function(x) paste0('HH',x)))
fname=paste0(output_folder,"CoMix_",country_code,"_wave_",id_wave,"_hh_common.csv")
write.table(sep=";",HH_common,file=fname,row.names =FALSE,quote = FALSE)

## Sday file
colums2keep<-c("part_id",
               "date",
               "day",
               "month",
               "year",
               "weekday"
)
indexes_to_keep<-which(colnames(part_data) %in% colums2keep)
sday<-part_data[,indexes_to_keep]
colnames(sday)[colnames(sday) == "date"] <- "sday_id"
sday$sday_id<-sday$sday_id
sday$sday_id<-str_replace_all(sday$sday_id,"-",".")


sday$weekday[sday$weekday=="Monday"]<-as.integer(1)
sday$weekday[sday$weekday=="Tuesday"]<-as.integer(2)
sday$weekday[sday$weekday=="Wednesday"]<-as.integer(3)
sday$weekday[sday$weekday=="Thursday"]<-as.integer(4)
sday$weekday[sday$weekday=="Friday"]<-as.integer(5)
sday$weekday[sday$weekday=="Saturday"]<-as.integer(6)
sday$weekday[sday$weekday=="Sunday"]<-as.integer(7)


fname=paste0(output_folder,"CoMix_",country_code,"_wave_",id_wave,"_sday.csv")
write.csv(sday,file=fname,row.names =FALSE,quote = FALSE)

