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
census_data <- read_csv(paste0(outputs, "base_cwa_census_data.csv"))

census_data$CWA_RPL_THEME1 <- scale(census_data$CWA_RPL_THEME1)
census_data$CWA_RPL_THEME2 <- scale(census_data$CWA_RPL_THEME2)
census_data$CWA_RPL_THEME3 <- scale(census_data$CWA_RPL_THEME3)
census_data$CWA_RPL_THEME4 <- scale(census_data$CWA_RPL_THEME4)

# Load Models  -------------------------
cwa_recep_fit <- readRDS(paste0(outputs, "cwa_models/cwa_recep_fit.Rds"))
cwa_subj_comp_fit <- readRDS(paste0(outputs, "cwa_models/cwa_subj_comp_fit.Rds"))
cwa_obj_comp_fit <- readRDS(paste0(outputs, "cwa_models/cwa_obj_comp_fit.Rds"))
cwa_resp_fit <- readRDS(paste0(outputs, "cwa_models/cwa_resp_fit.Rds"))
cwa_efficacy_fit <- readRDS(paste0(outputs, "cwa_models/cwa_efficacy_fit.Rds"))
cwa_myth_fit <- readRDS(paste0(outputs, "cwa_models/cwa_myth_fit.Rds"))
cwa_heat_fit <- readRDS(paste0(outputs, "cwa_models/cwa_heat_fit.Rds"))
cwa_drought_fit <- readRDS(paste0(outputs, "cwa_models/cwa_drought_fit.Rds"))
cwa_cold_fit <- readRDS(paste0(outputs, "cwa_models/cwa_cold_fit.Rds"))
cwa_snow_fit <- readRDS(paste0(outputs, "cwa_models/cwa_snow_fit.Rds"))
cwa_torn_fit <- readRDS(paste0(outputs, "cwa_models/cwa_torn_fit.Rds"))
cwa_flood_fit <- readRDS(paste0(outputs, "cwa_models/cwa_flood_fit.Rds"))
cwa_hurr_fit <- readRDS(paste0(outputs, "cwa_models/cwa_hurr_fit.Rds"))
cwa_fire_fit <- readRDS(paste0(outputs, "cwa_models/cwa_fire_fit.Rds"))
cwa_ready_fit <- readRDS(paste0(outputs, "cwa_models/cwa_ready_fit.Rds"))

# CWA Estimates -------------------------
recep_predictions <- census_data %>%
  mutate(person_z = colMedians(posterior_predict(cwa_recep_fit, newdata = ., allow.new.levels = TRUE))) %>%
  mutate(person_z = person_z * DEMGRP_PROP) %>%
  group_by(CWA) %>%
  summarise(person_z = sum(person_z)) %>%
  mutate(measure = "RECEP")

subj_comp_predictions <- census_data %>%
  mutate(person_z = colMedians(posterior_predict(cwa_subj_comp_fit, newdata = ., allow.new.levels = TRUE))) %>%
  mutate(person_z = person_z * DEMGRP_PROP) %>%
  group_by(CWA) %>%
  summarise(person_z = sum(person_z)) %>%
  mutate(measure = "SUB_COM")

obj_comp_predictions <- census_data %>%
  mutate(person_z = colMedians(posterior_predict(cwa_obj_comp_fit, newdata = ., allow.new.levels = TRUE))) %>%
  mutate(person_z = person_z * DEMGRP_PROP) %>%
  group_by(CWA) %>%
  summarise(person_z = sum(person_z)) %>%
  mutate(measure = "OBJ_COM")

resp_predictions <- census_data %>%
  mutate(person_z = colMedians(posterior_predict(cwa_resp_fit, newdata = ., allow.new.levels = TRUE))) %>%
  mutate(person_z = person_z * DEMGRP_PROP) %>%
  group_by(CWA) %>%
  summarise(person_z = sum(person_z)) %>%
  mutate(measure = "RESP")

efficacy_predictions <- census_data %>%
  mutate(person_z = colMedians(posterior_predict(cwa_efficacy_fit, newdata = ., allow.new.levels = TRUE))) %>%
  mutate(person_z = person_z * DEMGRP_PROP) %>%
  group_by(CWA) %>%
  summarise(person_z = sum(person_z)) %>%
  mutate(measure = "EFFICAC")

myth_predictions <- census_data %>%
  mutate(person_z = colMedians(posterior_predict(cwa_myth_fit, newdata = ., allow.new.levels = TRUE))) %>%
  mutate(person_z = person_z * DEMGRP_PROP) %>%
  group_by(CWA) %>%
  summarise(person_z = sum(person_z)) %>%
  mutate(measure = "MYTH")

heat_predictions <- census_data %>%
  mutate(person_z = colMedians(posterior_predict(cwa_heat_fit, newdata = ., allow.new.levels = TRUE))) %>%
  mutate(person_z = person_z * DEMGRP_PROP) %>%
  group_by(CWA) %>%
  summarise(person_z = sum(person_z)) %>%
  mutate(measure = "PER_HEA")

drought_predictions <- census_data %>%
  mutate(person_z = colMedians(posterior_predict(cwa_drought_fit, newdata = ., allow.new.levels = TRUE))) %>%
  mutate(person_z = person_z * DEMGRP_PROP) %>%
  group_by(CWA) %>%
  summarise(person_z = sum(person_z)) %>%
  mutate(measure = "PER_DRO")

cold_predictions <- census_data %>%
  mutate(person_z = colMedians(posterior_predict(cwa_cold_fit, newdata = ., allow.new.levels = TRUE))) %>%
  mutate(person_z = person_z * DEMGRP_PROP) %>%
  group_by(CWA) %>%
  summarise(person_z = sum(person_z)) %>%
  mutate(measure = "PER_COL")

snow_predictions <- census_data %>%
  mutate(person_z = colMedians(posterior_predict(cwa_snow_fit, newdata = ., allow.new.levels = TRUE))) %>%
  mutate(person_z = person_z * DEMGRP_PROP) %>%
  group_by(CWA) %>%
  summarise(person_z = sum(person_z)) %>%
  mutate(measure = "PER_SNO")

tornado_predictions <- census_data %>%
  mutate(person_z = colMedians(posterior_predict(cwa_torn_fit, newdata = ., allow.new.levels = TRUE))) %>%
  mutate(person_z = person_z * DEMGRP_PROP) %>%
  group_by(CWA) %>%
  summarise(person_z = sum(person_z)) %>%
  mutate(measure = "PER_TOR")

flood_predictions <- census_data %>%
  mutate(person_z = colMedians(posterior_predict(cwa_flood_fit, newdata = ., allow.new.levels = TRUE))) %>%
  mutate(person_z = person_z * DEMGRP_PROP) %>%
  group_by(CWA) %>%
  summarise(person_z = sum(person_z)) %>%
  mutate(measure = "PER_FLO")

hurricane_predictions <- census_data %>%
  mutate(person_z = colMedians(posterior_predict(cwa_hurr_fit, newdata = ., allow.new.levels = TRUE))) %>%
  mutate(person_z = person_z * DEMGRP_PROP) %>%
  group_by(CWA) %>%
  summarise(person_z = sum(person_z)) %>%
  mutate(measure = "PER_HUR")

fire_predictions <- census_data %>%
  mutate(person_z = colMedians(posterior_predict(cwa_fire_fit, newdata = ., allow.new.levels = TRUE))) %>%
  mutate(person_z = person_z * DEMGRP_PROP) %>%
  group_by(CWA) %>%
  summarise(person_z = sum(person_z)) %>%
  mutate(measure = "PER_FIR")

ready_predictions <- census_data %>%
  mutate(person_z = colMedians(posterior_predict(cwa_ready_fit, newdata = ., allow.new.levels = TRUE))) %>%
  mutate(person_z = person_z * DEMGRP_PROP) %>%
  group_by(CWA) %>%
  summarise(person_z = sum(person_z)) %>%
  mutate(measure = "READY")

# Make Shapefile ----------------------------------------
all_predictions <- bind_rows(recep_predictions, subj_comp_predictions, obj_comp_predictions, resp_predictions, efficacy_predictions, 
                             myth_predictions, heat_predictions, drought_predictions, cold_predictions, snow_predictions, 
                             tornado_predictions, flood_predictions, hurricane_predictions, fire_predictions, ready_predictions) %>%
  pivot_wider(names_from = measure, values_from = person_z) # only person z-scores

cwa_shp <- read_sf(paste0(downloads, "w_03mr20"), "w_03mr20")
cwa_shp <- cwa_shp %>% 
  filter(CWA %in% all_predictions$CWA) # no predictions for 7 CWAs (AFC, AFG, GUM, HFO, PPG, SJU, AJK)

cwa_shp <- left_join(cwa_shp, all_predictions, by = "CWA")

# # Include Event Data ----------------------------------------
cwa_storm_data <- read_csv(paste0(outputs, "base_cwa_storm_data.csv"))
cwa_shp <- left_join(cwa_shp, cwa_storm_data, by = "CWA")
cwa_shp <- cwa_shp %>% select(CWA, everything())

# Write Shapefile ----------------------------------------
cwa_shp <- ms_simplify(cwa_shp, keep = 0.05)
st_write(cwa_shp, paste0(outputs, "cwa_estimates"), "cwa_estimates", driver = "ESRI Shapefile", append = FALSE)
