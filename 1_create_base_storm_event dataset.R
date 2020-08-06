library(data.table)
library(tidyverse)
library(sf)

downloads <- "/Users/josephripberger/Dropbox/Severe Weather and Society Dashboard/local files/downloads/" # define locally!!!
outputs <- "/Users/josephripberger/Dropbox/Severe Weather and Society Dashboard/local files/outputs/" # define locally!!!

storm_data <- list.files(downloads, pattern = "StormEvents_details", full.names = TRUE) # https://www1.ncdc.noaa.gov/pub/data/swdi/stormevents/csvfiles/
storm_data <- lapply(storm_data, fread, sep = ",")
storm_data <- rbindlist(storm_data)
storm_data$BEGIN_DATE <- substr(storm_data$BEGIN_DATE_TIME, 1, 9)

storm_data$EVENT_TYPE_REC <- NA
storm_data$EVENT_TYPE_REC <- ifelse(storm_data$EVENT_TYPE %in% storm_data$EVENT_TYPE[grep("Heat", storm_data$EVENT_TYPE, ignore.case = TRUE)], 
                                    "HEAT", storm_data$EVENT_TYPE_REC)
storm_data$EVENT_TYPE_REC <- ifelse(storm_data$EVENT_TYPE %in% storm_data$EVENT_TYPE[grep("Cold", storm_data$EVENT_TYPE, ignore.case = TRUE)], 
                                    "COLD", storm_data$EVENT_TYPE_REC)
storm_data$EVENT_TYPE_REC <- ifelse(storm_data$EVENT_TYPE %in% storm_data$EVENT_TYPE[grep("Snow", storm_data$EVENT_TYPE, ignore.case = TRUE)], 
                                    "SNOW", storm_data$EVENT_TYPE_REC)
storm_data$EVENT_TYPE_REC <- ifelse(storm_data$EVENT_TYPE %in% storm_data$EVENT_TYPE[grep("Blizzard", storm_data$EVENT_TYPE, ignore.case = TRUE)], 
                                    "SNOW", storm_data$EVENT_TYPE_REC)
storm_data$EVENT_TYPE_REC <- ifelse(storm_data$EVENT_TYPE %in% storm_data$EVENT_TYPE[grep("Winter", storm_data$EVENT_TYPE, ignore.case = TRUE)], 
                                    "SNOW", storm_data$EVENT_TYPE_REC)
storm_data$EVENT_TYPE_REC <- ifelse(storm_data$EVENT_TYPE %in% storm_data$EVENT_TYPE[grep("Ice", storm_data$EVENT_TYPE, ignore.case = TRUE)], 
                                    "SNOW", storm_data$EVENT_TYPE_REC)
storm_data$EVENT_TYPE_REC <- ifelse(storm_data$EVENT_TYPE %in% storm_data$EVENT_TYPE[grep("Tornado", storm_data$EVENT_TYPE, ignore.case = TRUE)], 
                                    "TORN", storm_data$EVENT_TYPE_REC)
storm_data$EVENT_TYPE_REC <- ifelse(storm_data$EVENT_TYPE %in% storm_data$EVENT_TYPE[grep("Flood", storm_data$EVENT_TYPE, ignore.case = TRUE)], 
                                    "FLOOD", storm_data$EVENT_TYPE_REC)
storm_data$EVENT_TYPE_REC <- ifelse(storm_data$EVENT_TYPE %in% storm_data$EVENT_TYPE[grep("Surge", storm_data$EVENT_TYPE, ignore.case = TRUE)], 
                                    "FLOOD", storm_data$EVENT_TYPE_REC)
storm_data$EVENT_TYPE_REC <- ifelse(storm_data$EVENT_TYPE %in% storm_data$EVENT_TYPE[grep("Hurricane", storm_data$EVENT_TYPE, ignore.case = TRUE)], 
                                    "HURR", storm_data$EVENT_TYPE_REC)
storm_data$EVENT_TYPE_REC <- ifelse(storm_data$EVENT_TYPE %in% storm_data$EVENT_TYPE[grep("Depression", storm_data$EVENT_TYPE, ignore.case = TRUE)], 
                                    "HURR", storm_data$EVENT_TYPE_REC)
storm_data$EVENT_TYPE_REC <- ifelse(storm_data$EVENT_TYPE %in% storm_data$EVENT_TYPE[grep("Tropical Storm", storm_data$EVENT_TYPE, ignore.case = TRUE)], 
                                    "HURR", storm_data$EVENT_TYPE_REC)
storm_data$EVENT_TYPE_REC <- ifelse(storm_data$EVENT_TYPE %in% storm_data$EVENT_TYPE[grep("Wildfire", storm_data$EVENT_TYPE, ignore.case = TRUE)], 
                                    "FIRE", storm_data$EVENT_TYPE_REC)

storm_data$ST_FIPS <- str_pad(storm_data$STATE_FIPS, 2, side = "left", pad = 0)
storm_data$CNTY_FIPS <- str_pad(storm_data$CZ_FIPS, 3, side = "left", pad = 0)
storm_data$FIPS <- paste0(storm_data$ST_FIPS, storm_data$CNTY_FIPS)

cwa_storm_data_counts <- storm_data %>% 
  drop_na(EVENT_TYPE_REC) %>% 
  group_by(CWA = WFO, EVENT_TYPE_REC, BEGIN_DATE) %>% 
  summarize(n = n()) %>%
  group_by(CWA, EVENT_TYPE_REC) %>% 
  summarize(n = n()) %>%
  mutate(EVENT_DAYS = n / length(min(storm_data$YEAR):max(storm_data$YEAR))) %>% 
  select(CWA, EVENT_TYPE_REC, EVENT_DAYS) %>% 
  spread(EVENT_TYPE_REC, EVENT_DAYS) %>% 
  as.data.frame() 

county_storm_data_counts <- storm_data %>% 
  drop_na(EVENT_TYPE_REC) %>% 
  group_by(FIPS, EVENT_TYPE_REC, BEGIN_DATE) %>% 
  summarize(n = n()) %>%
  group_by(FIPS, EVENT_TYPE_REC) %>% 
  summarize(n = n()) %>%
  mutate(EVENT_DAYS = n / length(min(storm_data$YEAR):max(storm_data$YEAR))) %>% 
  select(FIPS, EVENT_TYPE_REC, EVENT_DAYS) %>% 
  spread(EVENT_TYPE_REC, EVENT_DAYS) %>% 
  as.data.frame()

# FIX NECESSARY: Many events are reported by forecast zone (CZTYPE of "Z") rather than county (CZTYPE of "C"). For these events, the CNTY_FIPS is likely incorrect. 
# Perhaps use CZ_NAME to get county info in the future? See https://cran.r-project.org/web/packages/noaastormevents/vignettes/details.html for example.

drought_data <- list.files(downloads, pattern = "dm_export", full.names = TRUE) # https://droughtmonitor.unl.edu/Data/DataDownload/ComprehensiveStatistics.aspx
drought_data <- lapply(drought_data, fread, sep = ",")
drought_data <- rbindlist(drought_data)
drought_data$drought <- ifelse(drought_data$D1 > 0 | drought_data$D2 > 0 | drought_data$D3 > 0 | drought_data$D4 > 0, 1, 0)
drought_data$YEAR <- as.numeric(substr(drought_data$MapDate, 1, 4))
drought_data$FIPS <- str_pad(drought_data$FIPS, 5, side = "left", pad = 0)

drought_data <- left_join(drought_data, storm_data %>% select(CWA = WFO, FIPS) %>% distinct(FIPS, .keep_all = TRUE), by = "FIPS") # add WFO to drought data

cwa_drought_data_counts <- drought_data %>% 
  filter(drought == 1) %>%
  distinct(MapDate, CWA, .keep_all = TRUE) %>% 
  group_by(CWA) %>%
  summarize(n = n()) %>%
  mutate(EVENT_DAYS = n / length(min(drought_data$YEAR):max(drought_data$YEAR))) %>% 
  select(CWA, EVENT_DAYS) %>% 
  rename("DROUGHT" = "EVENT_DAYS")

county_drought_data_counts <- drought_data %>% 
  filter(drought == 1) %>% 
  group_by(FIPS) %>%
  summarize(n = n()) %>%
  mutate(EVENT_DAYS = n / length(min(drought_data$YEAR):max(drought_data$YEAR))) %>% 
  select(FIPS, EVENT_DAYS) %>% 
  rename("DROUGHT" = "EVENT_DAYS")

cwa_data <- left_join(cwa_storm_data_counts, cwa_drought_data_counts, by = "CWA")
county_data <- left_join(county_storm_data_counts, county_drought_data_counts, by = "FIPS")

nrow(cwa_data) # 125 CWAs
nrow(county_data) # 5460 FIPS codes (many are incorrect, see fix note above)

# Join With County Shapefile (c_03mr20) -------------------------
cwa_cnty_shp <- read_sf(paste0(downloads, "c_03mr20"), "c_03mr20") %>% as_tibble() %>% select(CWA, FIPS)
cwa_cnty_shp$CWA <- substr(cwa_cnty_shp$CWA, start = 1, stop = 3) # Only keep first CWA in counties that span multiple CWAs
cwa_cnty_shp$CWA <- ifelse(cwa_cnty_shp$FIPS == "12087", "KEY", cwa_cnty_shp$CWA) # Fix KEY (assign FIPS 12087 to KEY alone, not KEY and MFL)
cwa_cnty_shp <- cwa_cnty_shp %>% distinct(FIPS, .keep_all = TRUE) # Remove duplicate FIPS codes (counties that span multiple CWAs)
cwa_cnty_shp <- cwa_cnty_shp %>% filter(!CWA %in% c("PPG", "SJU", "GUM", "HFO", "AFC", "AFG", "AJK")) # No census data

cwa_data <- cwa_cnty_shp %>% 
  select(CWA, FIPS) %>% 
  distinct(CWA, .keep_all = FALSE) %>% 
  left_join(., cwa_data, by = "CWA") 
cwa_data[is.na(cwa_data)] <- 0 # Change all NAs to 0
nrow(cwa_data) # 123 CWAs, removes ASO, EYW, GUA

county_data <- cwa_cnty_shp %>% 
  left_join(., county_data, by = "FIPS")
county_data[is.na(county_data)] <- 0 # Change all NAs to 0
nrow(county_data) # 3231 FIPS codes, removes incorrect FIPS codes from forecast zones

# Standardize -------------------------
cwa_data <- cwa_data %>% 
  select(CWA, COLD:DROUGHT) %>% 
  mutate_at(vars(-CWA), list(~scale(.) %>% as.vector))

county_data <- county_data %>% 
  select(FIPS, COLD:DROUGHT) %>% 
  mutate_at(vars(-FIPS), list(~scale(.) %>% as.vector))

# Output Data -------------------------
write_csv(cwa_data, paste0(outputs, "base_cwa_storm_data.csv"))
write_csv(county_data, paste0(outputs, "base_county_storm_data.csv"))