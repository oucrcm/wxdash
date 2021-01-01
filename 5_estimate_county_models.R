library(rstan) # notes on installation: https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
library(rstanarm)
library(tidyverse)
options(scipen = 999)
options(max.print = 99999)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

downloads <- "/Users/josephripberger/Dropbox (Univ. of Oklahoma)/Severe Weather and Society Dashboard/local files/downloads/" # define locally!!!
outputs <- "/Users/josephripberger/Dropbox (Univ. of Oklahoma)/Severe Weather and Society Dashboard/local files/outputs/" # define locally!!!

# Survey Data -------------------------
survey_data <- read.csv(paste0(outputs, "base_survey_data.csv")) %>% tibble() # use read.csv because of a parsing issue

survey_data$scale_risk_heat <- scale(survey_data$risk_heat)
survey_data$scale_risk_drought <- scale(survey_data$risk_drought)
survey_data$scale_risk_cold <- scale(survey_data$risk_cold)
survey_data$scale_risk_snow <- scale(survey_data$risk_snow)
survey_data$scale_risk_tor <- scale(survey_data$risk_tor)
survey_data$scale_risk_flood <- scale(survey_data$risk_flood)
survey_data$scale_risk_hur <- scale(survey_data$risk_hur)
survey_data$scale_risk_fire <- scale(survey_data$risk_fire)
survey_data$FIPS_RPL_THEME1 <- scale(survey_data$FIPS_RPL_THEME1)
survey_data$FIPS_RPL_THEME2 <- scale(survey_data$FIPS_RPL_THEME2)
survey_data$FIPS_RPL_THEME3 <- scale(survey_data$FIPS_RPL_THEME3)
survey_data$FIPS_RPL_THEME4 <- scale(survey_data$FIPS_RPL_THEME4)

# Composite Scale Models - Already Run, Output Saved -------------------------
warmup <- 500
iter <- 1000
chains <- 4

county_recep_fit <- stan_lmer(recep ~ 1 + (1|MALE) + (1|AGE_GROUP) + (1|HISP) + (1|RACE_GROUP) + (1|CWA) + (1|FIPS) + (1|MALE:AGE_GROUP) +
                             FIPS_TORN + FIPS_RPL_THEME1 + FIPS_RPL_THEME2 + FIPS_RPL_THEME3 + FIPS_RPL_THEME4,
                    data = survey_data, seed = 50, QR = TRUE, warmup = warmup, iter = iter, chains = chains, adapt_delta = 0.99999,
                    cores = 20)
saveRDS(county_recep_fit, paste0(outputs, "county_models/county_recep_fit.Rds"))

county_subj_comp_fit <- stan_lmer(subj_comp ~ 1 + (1|MALE) + (1|AGE_GROUP) + (1|HISP) + (1|RACE_GROUP) + (1|CWA) + (1|FIPS) + (1|MALE:AGE_GROUP) +
                                 FIPS_TORN + FIPS_RPL_THEME1 + FIPS_RPL_THEME2 + FIPS_RPL_THEME3 + FIPS_RPL_THEME4,
                   data = survey_data, seed = 50, QR = TRUE, warmup = warmup, iter = iter, chains = chains, adapt_delta = 0.99999,
                   cores = 20)
saveRDS(county_subj_comp_fit, paste0(outputs, "county_models/county_subj_comp_fit.Rds"))

county_obj_comp_fit <- stan_lmer(obj_comp ~ 1 + (1|MALE) + (1|AGE_GROUP) + (1|HISP) + (1|RACE_GROUP) + (1|CWA) + (1|FIPS) + (1|MALE:AGE_GROUP) +
                                FIPS_TORN + FIPS_RPL_THEME1 + FIPS_RPL_THEME2 + FIPS_RPL_THEME3 + FIPS_RPL_THEME4,
                   data = survey_data, seed = 50, QR = TRUE, warmup = warmup, iter = iter, chains = chains, adapt_delta = 0.99999,
                   cores = 20)
saveRDS(county_obj_comp_fit, paste0(outputs, "county_models/county_obj_comp_fit.Rds"))

county_resp_fit <- stan_lmer(resp ~ 1 + (1|MALE) + (1|AGE_GROUP) + (1|HISP) + (1|RACE_GROUP) + (1|CWA) + (1|FIPS) + (1|MALE:AGE_GROUP) +
                            FIPS_TORN + FIPS_RPL_THEME1 + FIPS_RPL_THEME2 + FIPS_RPL_THEME3 + FIPS_RPL_THEME4,
                           data = survey_data, seed = 50, QR = TRUE, warmup = warmup, iter = iter, chains = chains, adapt_delta = 0.99999,
                           cores = 20)
saveRDS(county_resp_fit, paste0(outputs, "county_models/county_resp_fit.Rds"))

county_efficacy_fit <- stan_lmer(efficacy ~ 1 + (1|MALE) + (1|AGE_GROUP) + (1|HISP) + (1|RACE_GROUP) + (1|CWA) + (1|FIPS) + (1|MALE:AGE_GROUP) +
                                FIPS_TORN + FIPS_RPL_THEME1 + FIPS_RPL_THEME2 + FIPS_RPL_THEME3 + FIPS_RPL_THEME4,
                           data = survey_data, seed = 50, QR = TRUE, warmup = warmup, iter = iter, chains = chains, adapt_delta = 0.99999,
                           cores = 20)
saveRDS(county_efficacy_fit, paste0(outputs, "county_models/county_efficacy_fit.Rds"))

county_myth_fit <- stan_lmer(myth ~ 1 + (1|MALE) + (1|AGE_GROUP) + (1|HISP) + (1|RACE_GROUP) + (1|CWA) + (1|FIPS) + (1|MALE:AGE_GROUP) +
                            FIPS_TORN + FIPS_RPL_THEME1 + FIPS_RPL_THEME2 + FIPS_RPL_THEME3 + FIPS_RPL_THEME4,
                              data = survey_data, seed = 50, QR = TRUE, warmup = warmup, iter = iter, chains = chains, adapt_delta = 0.99999,
                            cores = 20)
saveRDS(county_myth_fit, paste0(outputs, "county_models/county_myth_fit.Rds"))

# Risk Perception Models - Already Run, Output Saved -------------------------
county_heat_fit <- stan_lmer(scale_risk_heat ~ 1 + (1|MALE) + (1|AGE_GROUP) + (1|HISP) + (1|RACE_GROUP) + (1|CWA) + (1|FIPS) + (1|MALE:AGE_GROUP) +
                            FIPS_HEAT + FIPS_RPL_THEME1 + FIPS_RPL_THEME2 + FIPS_RPL_THEME3 + FIPS_RPL_THEME4,
                          data = survey_data, seed = 50, QR = TRUE, warmup = warmup, iter = iter, chains = chains, adapt_delta = 0.99999,
                          cores = 20)
saveRDS(county_heat_fit, paste0(outputs, "county_models/county_heat_fit.Rds"))

county_drought_fit <- stan_lmer(scale_risk_drought ~ 1 + (1|MALE) + (1|AGE_GROUP) + (1|HISP) + (1|RACE_GROUP) + (1|CWA) + (1|FIPS) + (1|MALE:AGE_GROUP) +
                               FIPS_DROUGHT + FIPS_RPL_THEME1 + FIPS_RPL_THEME2 + FIPS_RPL_THEME3 + FIPS_RPL_THEME4,
                             data = survey_data, seed = 50, QR = TRUE, warmup = warmup, iter = iter, chains = chains, adapt_delta = 0.99999,
                             cores = 20)
saveRDS(county_drought_fit, paste0(outputs, "county_models/county_drought_fit.Rds"))

county_cold_fit <- stan_lmer(scale_risk_cold ~ 1 + (1|MALE) + (1|AGE_GROUP) + (1|HISP) + (1|RACE_GROUP) + (1|CWA) + (1|FIPS) + (1|MALE:AGE_GROUP) +
                            FIPS_COLD + FIPS_RPL_THEME1 + FIPS_RPL_THEME2 + FIPS_RPL_THEME3 + FIPS_RPL_THEME4,
                          data = survey_data, seed = 50, QR = TRUE, warmup = warmup, iter = iter, chains = chains, adapt_delta = 0.99999,
                          cores = 20)
saveRDS(county_cold_fit, paste0(outputs, "county_models/county_cold_fit.Rds"))

county_snow_fit <- stan_lmer(scale_risk_snow ~ 1 + (1|MALE) + (1|AGE_GROUP) + (1|HISP) + (1|RACE_GROUP) + (1|CWA) + (1|FIPS) + (1|MALE:AGE_GROUP) +
                            FIPS_SNOW + FIPS_RPL_THEME1 + FIPS_RPL_THEME2 + FIPS_RPL_THEME3 + FIPS_RPL_THEME4,
                          data = survey_data, seed = 50, QR = TRUE, warmup = warmup, iter = iter, chains = chains, adapt_delta = 0.99999,
                          cores = 20)
saveRDS(county_snow_fit, paste0(outputs, "county_models/county_snow_fit.Rds"))

county_torn_fit <- stan_lmer(scale_risk_tor ~ 1 + (1|MALE) + (1|AGE_GROUP) + (1|HISP) + (1|RACE_GROUP) + (1|CWA) + (1|FIPS) + (1|MALE:AGE_GROUP) +
                            FIPS_TORN + FIPS_RPL_THEME1 + FIPS_RPL_THEME2 + FIPS_RPL_THEME3 + FIPS_RPL_THEME4,
                          data = survey_data, seed = 50, QR = TRUE, warmup = warmup, iter = iter, chains = chains, adapt_delta = 0.99999,
                          cores = 20)
saveRDS(county_torn_fit, paste0(outputs, "county_models/county_torn_fit.Rds"))

county_flood_fit <- stan_lmer(scale_risk_flood ~ 1 + (1|MALE) + (1|AGE_GROUP) + (1|HISP) + (1|RACE_GROUP) + (1|CWA) + (1|FIPS) + (1|MALE:AGE_GROUP) +
                             FIPS_FLOOD + FIPS_RPL_THEME1 + FIPS_RPL_THEME2 + FIPS_RPL_THEME3 + FIPS_RPL_THEME4,
                           data = survey_data, seed = 50, QR = TRUE, warmup = warmup, iter = iter, chains = chains, adapt_delta = 0.99999,
                           cores = 20)
saveRDS(county_flood_fit, paste0(outputs, "county_models/county_flood_fit.Rds"))

county_hurr_fit <- stan_lmer(scale_risk_hur ~ 1 + (1|MALE) + (1|AGE_GROUP) + (1|HISP) + (1|RACE_GROUP) + (1|CWA) + (1|FIPS) + (1|MALE:AGE_GROUP) +
                            FIPS_HURR + FIPS_RPL_THEME1 + FIPS_RPL_THEME2 + FIPS_RPL_THEME3 + FIPS_RPL_THEME4,
                          data = survey_data, seed = 50, QR = TRUE, warmup = warmup, iter = iter, chains = chains, adapt_delta = 0.99999,
                          cores = 20)
saveRDS(county_hurr_fit, paste0(outputs, "county_models/county_hurr_fit.Rds"))

county_fire_fit <- stan_lmer(scale_risk_fire ~ 1 + (1|MALE) + (1|AGE_GROUP) + (1|HISP) + (1|RACE_GROUP) + (1|CWA) + (1|FIPS) + (1|MALE:AGE_GROUP) +
                            FIPS_FIRE + FIPS_RPL_THEME1 + FIPS_RPL_THEME2 + FIPS_RPL_THEME3 + FIPS_RPL_THEME4,
                          data = survey_data, seed = 50, QR = TRUE, warmup = warmup, iter = iter, chains = chains, adapt_delta = 0.99999,
                          cores = 20)
saveRDS(county_fire_fit, paste0(outputs, "county_models/county_fire_fit.Rds"))

# Ready Models - Already Run, Output Saved -------------------------
county_ready_fit <- stan_lmer(ready ~ 1 + (1|MALE) + (1|AGE_GROUP) + (1|HISP) + (1|RACE_GROUP) + (1|CWA) + (1|FIPS) + (1|MALE:AGE_GROUP) +
                             FIPS_HEAT + FIPS_DROUGHT + FIPS_COLD + FIPS_SNOW + FIPS_TORN + FIPS_FLOOD + FIPS_HURR + FIPS_FIRE + 
                             FIPS_RPL_THEME1 + FIPS_RPL_THEME2 + FIPS_RPL_THEME3 + FIPS_RPL_THEME4,
                             data = survey_data, seed = 50, QR = TRUE, warmup = warmup, iter = iter, chains = chains, adapt_delta = 0.99999,
                             cores = 20)
saveRDS(county_ready_fit, paste0(outputs, "county_models/county_ready_fit.Rds"))
