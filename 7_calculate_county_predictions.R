library(rstan) # notes on installation: https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
library(rstanarm)
library(tidyverse)
library(robustbase)
library(sf)
library(rmapshaper)
options(scipen = 999)
options(max.print = 99999)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

downloads <- "/Users/josephripberger/Dropbox/Severe Weather and Society Dashboard/local files/downloads/" # define locally!!!
outputs <- "/Users/josephripberger/Dropbox/Severe Weather and Society Dashboard/local files/outputs/" # define locally!!!

# Census Data -------------------------
census_data <- read_csv(paste0(outputs, "base_county_census_data.csv"))

census_data$FIPS_RPL_THEME1 <- scale(census_data$FIPS_RPL_THEME1)
census_data$FIPS_RPL_THEME2 <- scale(census_data$FIPS_RPL_THEME2)
census_data$FIPS_RPL_THEME3 <- scale(census_data$FIPS_RPL_THEME3)
census_data$FIPS_RPL_THEME4 <- scale(census_data$FIPS_RPL_THEME4)

# Load Models  -------------------------
county_recep_fit <- readRDS(paste0(outputs, "county_models/county_recep_fit.Rds"))
county_subj_comp_fit <- readRDS(paste0(outputs, "county_models/county_subj_comp_fit.Rds"))
county_obj_comp_fit <- readRDS(paste0(outputs, "county_models/county_obj_comp_fit.Rds"))
county_resp_fit <- readRDS(paste0(outputs, "county_models/county_resp_fit.Rds"))
county_efficacy_fit <- readRDS(paste0(outputs, "county_models/county_efficacy_fit.Rds"))
county_myth_fit <- readRDS(paste0(outputs, "county_models/county_myth_fit.Rds"))
county_heat_fit <- readRDS(paste0(outputs, "county_models/county_heat_fit.Rds"))
county_drought_fit <- readRDS(paste0(outputs, "county_models/county_drought_fit.Rds"))
county_cold_fit <- readRDS(paste0(outputs, "county_models/county_cold_fit.Rds"))
county_snow_fit <- readRDS(paste0(outputs, "county_models/county_snow_fit.Rds"))
county_torn_fit <- readRDS(paste0(outputs, "county_models/county_torn_fit.Rds"))
county_flood_fit <- readRDS(paste0(outputs, "county_models/county_flood_fit.Rds"))
county_hurr_fit <- readRDS(paste0(outputs, "county_models/county_hurr_fit.Rds"))
county_fire_fit <- readRDS(paste0(outputs, "county_models/county_fire_fit.Rds"))

# FIPS Estimates -------------------------
recep_predictions <- census_data %>%
  mutate(person_z = colMedians(posterior_predict(county_recep_fit, newdata = ., allow.new.levels = TRUE))) %>%
  mutate(person_z = person_z * DEMGRP_PROP) %>%
  group_by(FIPS) %>%
  summarise(person_z = sum(person_z)) %>%
  mutate(measure = "RECEP")

subj_comp_predictions <- census_data %>%
  mutate(person_z = colMedians(posterior_predict(county_subj_comp_fit, newdata = ., allow.new.levels = TRUE))) %>%
  mutate(person_z = person_z * DEMGRP_PROP) %>%
  group_by(FIPS) %>%
  summarise(person_z = sum(person_z)) %>%
  mutate(measure = "SUB_COM")

obj_comp_predictions <- census_data %>%
  mutate(person_z = colMedians(posterior_predict(county_obj_comp_fit, newdata = ., allow.new.levels = TRUE))) %>%
  mutate(person_z = person_z * DEMGRP_PROP) %>%
  group_by(FIPS) %>%
  summarise(person_z = sum(person_z)) %>%
  mutate(measure = "OBJ_COM")

resp_predictions <- census_data %>%
  mutate(person_z = colMedians(posterior_predict(county_resp_fit, newdata = ., allow.new.levels = TRUE))) %>%
  mutate(person_z = person_z * DEMGRP_PROP) %>%
  group_by(FIPS) %>%
  summarise(person_z = sum(person_z)) %>%
  mutate(measure = "RESP")

efficacy_predictions <- census_data %>%
  mutate(person_z = colMedians(posterior_predict(county_efficacy_fit, newdata = ., allow.new.levels = TRUE))) %>%
  mutate(person_z = person_z * DEMGRP_PROP) %>%
  group_by(FIPS) %>%
  summarise(person_z = sum(person_z)) %>%
  mutate(measure = "EFFICAC")

myth_predictions <- census_data %>%
  mutate(person_z = colMedians(posterior_predict(county_myth_fit, newdata = ., allow.new.levels = TRUE))) %>%
  mutate(person_z = person_z * DEMGRP_PROP) %>%
  group_by(FIPS) %>%
  summarise(person_z = sum(person_z)) %>%
  mutate(measure = "MYTH")

heat_predictions <- census_data %>%
  mutate(person_z = colMedians(posterior_predict(county_heat_fit, newdata = ., allow.new.levels = TRUE))) %>%
  mutate(person_z = person_z * DEMGRP_PROP) %>%
  group_by(FIPS) %>%
  summarise(person_z = sum(person_z)) %>%
  mutate(measure = "PER_HEA")

drought_predictions <- census_data %>%
  mutate(person_z = colMedians(posterior_predict(county_drought_fit, newdata = ., allow.new.levels = TRUE))) %>%
  mutate(person_z = person_z * DEMGRP_PROP) %>%
  group_by(FIPS) %>%
  summarise(person_z = sum(person_z)) %>%
  mutate(measure = "PER_DRO")

cold_predictions <- census_data %>%
  mutate(person_z = colMedians(posterior_predict(county_cold_fit, newdata = ., allow.new.levels = TRUE))) %>%
  mutate(person_z = person_z * DEMGRP_PROP) %>%
  group_by(FIPS) %>%
  summarise(person_z = sum(person_z)) %>%
  mutate(measure = "PER_COL")

snow_predictions <- census_data %>%
  mutate(person_z = colMedians(posterior_predict(county_snow_fit, newdata = ., allow.new.levels = TRUE))) %>%
  mutate(person_z = person_z * DEMGRP_PROP) %>%
  group_by(FIPS) %>%
  summarise(person_z = sum(person_z)) %>%
  mutate(measure = "PER_SNO")

tornado_predictions <- census_data %>%
  mutate(person_z = colMedians(posterior_predict(county_torn_fit, newdata = ., allow.new.levels = TRUE))) %>%
  mutate(person_z = person_z * DEMGRP_PROP) %>%
  group_by(FIPS) %>%
  summarise(person_z = sum(person_z)) %>%
  mutate(measure = "PER_TOR")

flood_predictions <- census_data %>%
  mutate(person_z = colMedians(posterior_predict(county_flood_fit, newdata = ., allow.new.levels = TRUE))) %>%
  mutate(person_z = person_z * DEMGRP_PROP) %>%
  group_by(FIPS) %>%
  summarise(person_z = sum(person_z)) %>%
  mutate(measure = "PER_FLO")

hurricane_predictions <- census_data %>%
  mutate(person_z = colMedians(posterior_predict(county_hurr_fit, newdata = ., allow.new.levels = TRUE))) %>%
  mutate(person_z = person_z * DEMGRP_PROP) %>%
  group_by(FIPS) %>%
  summarise(person_z = sum(person_z)) %>%
  mutate(measure = "PER_HUR")

fire_predictions <- census_data %>%
  mutate(person_z = colMedians(posterior_predict(county_fire_fit, newdata = ., allow.new.levels = TRUE))) %>%
  mutate(person_z = person_z * DEMGRP_PROP) %>%
  group_by(FIPS) %>%
  summarise(person_z = sum(person_z)) %>%
  mutate(measure = "PER_FIR")

# Make Shapefile ----------------------------------------
all_predictions <- bind_rows(recep_predictions, subj_comp_predictions, obj_comp_predictions, resp_predictions, efficacy_predictions, 
                             myth_predictions, heat_predictions, drought_predictions, cold_predictions, snow_predictions, 
                             tornado_predictions, flood_predictions, hurricane_predictions, fire_predictions) %>%
  pivot_wider(names_from = measure, values_from = person_z) # only person z-scores

cwa_cnty_shp <- read_sf(paste0(downloads, "c_03mr20"), "c_03mr20")
cwa_cnty_shp$CWA <- substr(cwa_cnty_shp$CWA, start = 1, stop = 3) # Only keep first CWA in counties that span multiple CWAs
cwa_cnty_shp$CWA <- ifelse(cwa_cnty_shp$FIPS == "12087", "KEY", cwa_cnty_shp$CWA) # Fix KEY (assign FIPS 12087 to KEY alone, not KEY and MFL)
# cwa_cnty_shp <- cwa_cnty_shp %>% distinct(FIPS, .keep_all = TRUE) # Remove duplicate FIPS codes (counties that span multiple CWAs)
cwa_cnty_shp <- cwa_cnty_shp %>% filter(!CWA %in% c("PPG", "SJU", "GUM", "HFO", "AFC", "AFG", "AJK")) # No census data
cwa_cnty_shp <- left_join(cwa_cnty_shp, all_predictions, by = "FIPS")

# Include Event Data ----------------------------------------
cnty_storm_data <- read_csv(paste0(outputs, "base_county_storm_data.csv"))
cwa_cnty_shp <- left_join(cwa_cnty_shp, cnty_storm_data, by = "FIPS")

# Include SVI ----------------------------------------
svi_data <- read_csv(paste0(downloads, "SVI2018_US_COUNTY.csv")) %>%
  select(FIPS, starts_with("EP_"), starts_with("RPL")) %>%
  na_if(-999)

cwa_cnty_shp <- left_join(cwa_cnty_shp, svi_data, by = "FIPS")

# Write Shapefile ----------------------------------------
cwa_cnty_shp <- ms_simplify(cwa_cnty_shp, keep = 0.05)
st_write(cwa_cnty_shp, paste0(outputs, "county_estimates"), "county_estimates", driver = "ESRI Shapefile", append = FALSE)