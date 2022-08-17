library(tidyverse)
library(robustbase)
library(sf)
library(rmapshaper)

downloads <- "/Users/josephripberger/Dropbox (Univ. of Oklahoma)/Severe Weather and Society Dashboard/local files/downloads/" # define locally!!!
outputs <- "/Users/josephripberger/Dropbox (Univ. of Oklahoma)/Severe Weather and Society Dashboard/local files/outputs/" # define locally!!!

# Census Data -------------------------
census_data <- read_csv(paste0(outputs, "base_county_census_data.csv"))

census_data$FIPS_RPL_THEME1 <- scale(census_data$FIPS_RPL_THEME1)
census_data$FIPS_RPL_THEME2 <- scale(census_data$FIPS_RPL_THEME2)
census_data$FIPS_RPL_THEME3 <- scale(census_data$FIPS_RPL_THEME3)
census_data$FIPS_RPL_THEME4 <- scale(census_data$FIPS_RPL_THEME4)

# Load Models  -------------------------
county_to_recep_fit <- readRDS(paste0(outputs, "county_models/fips_to_recep_fit.Rds"))
county_hu_recep_fit <- readRDS(paste0(outputs, "county_models/fips_hu_recep_fit.Rds"))
county_to_subj_comp_fit <- readRDS(paste0(outputs, "county_models/fips_to_subj_comp_fit.Rds"))
county_hu_subj_comp_fit <- readRDS(paste0(outputs, "county_models/fips_hu_subj_comp_fit.Rds"))
county_to_obj_comp_fit <- readRDS(paste0(outputs, "county_models/fips_to_obj_comp_fit.Rds"))
county_hu_obj_comp_fit <- readRDS(paste0(outputs, "county_models/fips_hu_obj_comp_fit.Rds"))
county_to_resp_fit <- readRDS(paste0(outputs, "county_models/fips_to_resp_fit.Rds"))
county_hu_resp_fit <- readRDS(paste0(outputs, "county_models/fips_hu_resp_fit.Rds"))
county_to_eff_fit <- readRDS(paste0(outputs, "county_models/fips_to_eff_fit.Rds"))
county_heat_fit <- readRDS(paste0(outputs, "county_models/fips_heat_fit.Rds"))
county_drought_fit <- readRDS(paste0(outputs, "county_models/fips_drought_fit.Rds"))
county_cold_fit <- readRDS(paste0(outputs, "county_models/fips_cold_fit.Rds"))
county_snow_fit <- readRDS(paste0(outputs, "county_models/fips_snow_fit.Rds"))
county_torn_fit <- readRDS(paste0(outputs, "county_models/fips_torn_fit.Rds"))
county_flood_fit <- readRDS(paste0(outputs, "county_models/fips_flood_fit.Rds"))
county_hurr_fit <- readRDS(paste0(outputs, "county_models/fips_hurr_fit.Rds"))
county_fire_fit <- readRDS(paste0(outputs, "county_models/fips_fire_fit.Rds"))
county_all_ready_fit <- readRDS(paste0(outputs, "county_models/fips_all_ready_fit.Rds"))

# FIPS Estimates -------------------------
to_recep_predictions <- census_data %>%
  mutate(person_z = predict(fips_to_recep_fit, newdata = ., allow.new.levels = TRUE)) %>%
  mutate(person_z = person_z * DEMGRP_PROP) %>%
  group_by(FIPS) %>%
  summarise(person_z = sum(person_z)) %>%
  mutate(measure = "TO_RECEP")

hu_recep_predictions <- census_data %>%
  mutate(person_z = predict(fips_hu_recep_fit, newdata = ., allow.new.levels = TRUE)) %>%
  mutate(person_z = person_z * DEMGRP_PROP) %>%
  group_by(FIPS) %>%
  summarise(person_z = sum(person_z)) %>%
  mutate(measure = "HU_RECEP")

to_subj_comp_predictions <- census_data %>%
  mutate(person_z = predict(fips_to_subj_comp_fit, newdata = ., allow.new.levels = TRUE)) %>%
  mutate(person_z = person_z * DEMGRP_PROP) %>%
  group_by(FIPS) %>%
  summarise(person_z = sum(person_z)) %>%
  mutate(measure = "TO_SUB_COM")

hu_subj_comp_predictions <- census_data %>%
  mutate(person_z = predict(fips_hu_subj_comp_fit, newdata = ., allow.new.levels = TRUE)) %>%
  mutate(person_z = person_z * DEMGRP_PROP) %>%
  group_by(FIPS) %>%
  summarise(person_z = sum(person_z)) %>%
  mutate(measure = "HU_SUB_COM")

to_obj_comp_predictions <- census_data %>%
  mutate(person_z = predict(fips_to_obj_comp_fit, newdata = ., allow.new.levels = TRUE)) %>%
  mutate(person_z = person_z * DEMGRP_PROP) %>%
  group_by(FIPS) %>%
  summarise(person_z = sum(person_z)) %>%
  mutate(measure = "TO_OBJ_COM")

hu_obj_comp_predictions <- census_data %>%
  mutate(person_z = predict(fips_hu_obj_comp_fit, newdata = ., allow.new.levels = TRUE)) %>%
  mutate(person_z = person_z * DEMGRP_PROP) %>%
  group_by(FIPS) %>%
  summarise(person_z = sum(person_z)) %>%
  mutate(measure = "HU_OBJ_COM")

to_resp_predictions <- census_data %>%
  mutate(person_z = predict(fips_to_resp_fit, newdata = ., allow.new.levels = TRUE)) %>%
  mutate(person_z = person_z * DEMGRP_PROP) %>%
  group_by(FIPS) %>%
  summarise(person_z = sum(person_z)) %>%
  mutate(measure = "TO_RESP")

hu_resp_predictions <- census_data %>%
  mutate(person_z = predict(fips_hu_resp_fit, newdata = ., allow.new.levels = TRUE)) %>%
  mutate(person_z = person_z * DEMGRP_PROP) %>%
  group_by(FIPS) %>%
  summarise(person_z = sum(person_z)) %>%
  mutate(measure = "HU_RESP")

to_eff_predictions <- census_data %>%
  mutate(person_z = predict(fips_to_eff_fit, newdata = ., allow.new.levels = TRUE)) %>%
  mutate(person_z = person_z * DEMGRP_PROP) %>%
  group_by(FIPS) %>%
  summarise(person_z = sum(person_z)) %>%
  mutate(measure = "TO_EFFICAC")

heat_predictions <- census_data %>%
  mutate(person_z = predict(fips_heat_fit, newdata = ., allow.new.levels = TRUE)) %>%
  mutate(person_z = person_z * DEMGRP_PROP) %>%
  group_by(FIPS) %>%
  summarise(person_z = sum(person_z)) %>%
  mutate(measure = "PER_HEA")

drought_predictions <- census_data %>%
  mutate(person_z = predict(fips_drought_fit, newdata = ., allow.new.levels = TRUE)) %>%
  mutate(person_z = person_z * DEMGRP_PROP) %>%
  group_by(FIPS) %>%
  summarise(person_z = sum(person_z)) %>%
  mutate(measure = "PER_DRO")

cold_predictions <- census_data %>%
  mutate(person_z = predict(fips_cold_fit, newdata = ., allow.new.levels = TRUE)) %>%
  mutate(person_z = person_z * DEMGRP_PROP) %>%
  group_by(FIPS) %>%
  summarise(person_z = sum(person_z)) %>%
  mutate(measure = "PER_COL")

snow_predictions <- census_data %>%
  mutate(person_z = predict(fips_snow_fit, newdata = ., allow.new.levels = TRUE)) %>%
  mutate(person_z = person_z * DEMGRP_PROP) %>%
  group_by(FIPS) %>%
  summarise(person_z = sum(person_z)) %>%
  mutate(measure = "PER_SNO")

tornado_predictions <- census_data %>%
  mutate(person_z = predict(fips_torn_fit, newdata = ., allow.new.levels = TRUE)) %>%
  mutate(person_z = person_z * DEMGRP_PROP) %>%
  group_by(FIPS) %>%
  summarise(person_z = sum(person_z)) %>%
  mutate(measure = "PER_TOR")

flood_predictions <- census_data %>%
  mutate(person_z = predict(fips_flood_fit, newdata = ., allow.new.levels = TRUE)) %>%
  mutate(person_z = person_z * DEMGRP_PROP) %>%
  group_by(FIPS) %>%
  summarise(person_z = sum(person_z)) %>%
  mutate(measure = "PER_FLO")

hurricane_predictions <- census_data %>%
  mutate(person_z = predict(fips_hurr_fit, newdata = ., allow.new.levels = TRUE)) %>%
  mutate(person_z = person_z * DEMGRP_PROP) %>%
  group_by(FIPS) %>%
  summarise(person_z = sum(person_z)) %>%
  mutate(measure = "PER_HUR")

fire_predictions <- census_data %>%
  mutate(person_z = predict(fips_fire_fit, newdata = ., allow.new.levels = TRUE)) %>%
  mutate(person_z = person_z * DEMGRP_PROP) %>%
  group_by(FIPS) %>%
  summarise(person_z = sum(person_z)) %>%
  mutate(measure = "PER_FIR")

all_ready_predictions <- census_data %>%
  mutate(person_z = predict(fips_all_ready_fit, newdata = ., allow.new.levels = TRUE)) %>%
  mutate(person_z = person_z * DEMGRP_PROP) %>%
  group_by(FIPS) %>%
  summarise(person_z = sum(person_z)) %>%
  mutate(measure = "ALL_READY")

# Make Shapefile ----------------------------------------
all_predictions <- bind_rows(to_recep_predictions, hu_recep_predictions, to_subj_comp_predictions, hu_subj_comp_predictions, 
                             to_obj_comp_predictions, hu_obj_comp_predictions, to_resp_predictions, hu_resp_predictions,
                             to_eff_predictions, heat_predictions, drought_predictions, cold_predictions, snow_predictions, 
                             tornado_predictions, flood_predictions, hurricane_predictions, fire_predictions, all_ready_predictions) %>%
  pivot_wider(names_from = measure, values_from = person_z) # only person z-scores

cnty_shp <- read_sf(paste0(downloads, "cb_2018_us_county_20m"), "cb_2018_us_county_20m") %>% rename("FIPS" = "GEOID")
cwa_cnty_shp <- read_sf(paste0(downloads, "c_03mr20"), "c_03mr20")
cwa_cnty_shp$CWA <- substr(cwa_cnty_shp$CWA, start = 1, stop = 3) # Only keep first CWA in counties that span multiple CWAs
cwa_cnty_shp$CWA <- ifelse(cwa_cnty_shp$FIPS == "12087", "KEY", cwa_cnty_shp$CWA) # Fix KEY (assign FIPS 12087 to KEY alone, not KEY and MFL)
cwa_cnty_shp <- cwa_cnty_shp %>% distinct(FIPS, .keep_all = TRUE) # Remove duplicate FIPS codes (counties that span multiple CWAs)
cnty_shp <- left_join(cnty_shp, cwa_cnty_shp %>% select(CWA, FIPS) %>% st_drop_geometry(), by = "FIPS")
cnty_shp <- cnty_shp %>% filter(!CWA %in% c("PPG", "SJU", "GUM", "HFO", "AFC", "AFG", "AJK")) # No census data
cnty_shp <- left_join(cnty_shp, all_predictions, by = "FIPS") %>% filter(is.na(CWA) == FALSE)

# Include Event Data ----------------------------------------
cnty_storm_data <- read_csv(paste0(outputs, "base_county_storm_data.csv"))
cnty_shp <- left_join(cnty_shp, cnty_storm_data, by = "FIPS")
cnty_shp <- cnty_shp %>% select(FIPS, everything())

# Write Shapefile ----------------------------------------
cnty_shp <- ms_simplify(cnty_shp, keep = 0.05)
st_write(cnty_shp, paste0(outputs, "county_estimates"), "county_estimates", driver = "ESRI Shapefile", append = FALSE)
