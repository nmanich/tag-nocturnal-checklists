# Determine nocturnal tags for eBird checklists
# Written by Gabriel Foley
# Modified by Nick Anich 7/19/21

library(here)
library(dplyr)
library(lubridate)
library(suncalc)

#gabriel's crazy data import
#month <- "may"
#year <- 2021
# results from bba3
#bba3 <- read.delim(here("data", "ebird", "1_raw",
#                        paste0("mddcbba3_", month, year, ".txt")), 
#                   quote = "")

# simple data import                   
bba3 <- read.delim("ebd_US-WI_sancra_201501_201912_relJun-2021.txt", sep="\t", header=TRUE, quote = "", stringsAsFactors = FALSE, na.strings=c(""))

#drop any rows with no time listed
bba3 <- bba3[!is.na(bba3$TIME.OBSERVATIONS.STARTED),]

## label nocturnal checklists
# nocturnal = 20 min after sunset, 40 min before sunrise
bba3$observation_datetime <- as_datetime(paste(bba3$OBSERVATION.DATE, 
                                               bba3$TIME.OBSERVATIONS.STARTED), 
                                         tz = "EST")
# create new columns with suncalc info
bba3[, (length(bba3) + 1):(length(bba3) + 7)] <-   
  getSunlightTimes(data = data.frame(date = as.Date(bba3$OBSERVATION.DATE),
                                     lat = bba3$LATITUDE,
                                     lon = bba3$LONGITUDE), 
                   keep = c("sunrise", "nauticalDawn", 
                            "sunset", "nauticalDusk"), 
                   tz = "EST")
# remove duplicate columns
bba3[, c("date", "lat", "lon")] <- NULL

# getSunlightTimes() doesn't account for daylight savings time
## compensate for this
### DST starts March 8 2020 02:00:00
### DST ends November 1 2020 02:00:00
dst_2015 <- as_datetime("2020-03-08 02:00:00", tz = "EST") %--% 
  as_datetime("2020-11-01 02:00:00", tz = "EST")
dst_2016 <- as_datetime("2021-03-13 02:00:00", tz = "EST") %--%
  as_datetime("2021-11-06 02:00:00", tz = "EST")
dst_2017 <- as_datetime("2020-03-12 02:00:00", tz = "EST") %--% 
  as_datetime("2020-11-05 02:00:00", tz = "EST")
dst_2018 <- as_datetime("2021-03-11 02:00:00", tz = "EST") %--%
  as_datetime("2021-11-04 02:00:00", tz = "EST")
dst_2019 <- as_datetime("2020-03-10 02:00:00", tz = "EST") %--% 
  as_datetime("2020-11-03 02:00:00", tz = "EST")
dst_2020 <- as_datetime("2020-03-08 02:00:00", tz = "EST") %--% 
  as_datetime("2020-11-01 02:00:00", tz = "EST")
dst_2021 <- as_datetime("2021-03-14 02:00:00", tz = "EST") %--%
  as_datetime("2021-11-07 02:00:00", tz = "EST")

indx <- which(bba3$observation_datetime %within% dst_2015 |
                bba3$observation_datetime %within% dst_2016 |
                   bba3$observation_datetime %within% dst_2017 |
                       bba3$observation_datetime %within% dst_2018 |
                           bba3$observation_datetime %within% dst_2019 |
                              bba3$observation_datetime %within% dst_2020 |    
                                  bba3$observation_datetime %within% dst_2021
              )
bba3[indx, c("sunrise")] <- bba3[indx, c("sunrise")] + hours(1)
bba3[indx, c("nauticalDawn")] <- bba3[indx, c("nauticalDawn")] + hours(1)
bba3[indx, c("sunset")] <- bba3[indx, c("sunset")] + hours(1)
bba3[indx, c("nauticalDusk")] <- bba3[indx, c("nauticalDusk")] + hours(1)

rm(indx)

# add columns for what ebird considers nocturnal
bba3$ebird_dawn <- bba3$sunrise - hms::as_hms(40*60)
bba3$ebird_dusk <- bba3$sunset + hms::as_hms(20*60)
bba3 <- bba3 %>% 
  rename(nautical_dawn = nauticalDawn, nautical_dusk = nauticalDusk) %>%
  mutate(nocturnal = if_else(observation_datetime <= ebird_dawn | 
                                 observation_datetime >= ebird_dusk, 
                             "nocturnal", "diurnal"))

# change the checklist's designation to diurnal if it extends past dawn (Optional)
#notna <-which(!is.na(bba3$duration_minutes))
#indx <- which(bba3$nocturnal[notna] == "nocturnal" &
#                ms(paste(bba3$duration_minutes[notna], 0)) > 
#                abs(as_datetime(bba3$ebird_dawn[notna]) -
#                      as_datetime(bba3$observation_datetime[notna])))
#bba3[indx, "nocturnal"] <- "diurnal"
#rm(indx, notna)

#export the whole table
write.csv(bba3, "alldata_withnocturnaltags.csv")

#OR

#limit to just subid and nocturnal columns
twocolumns <- bba3[, c("SAMPLING.EVENT.IDENTIFIER","nocturnal")]

#drop duplicate subids
noctag <- twocolumns[!duplicated(twocolumns$SAMPLING.EVENT.IDENTIFIER), ]

#then export table of unique subids and nocturnal tags
write.csv(noctag, "just_nocturnal_tags.csv")
