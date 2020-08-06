library(rstan) # notes on installation: https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
library(rstanarm)
library(tidyverse)
options(scipen = 999)
options(max.print = 99999)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

downloads <- "/Users/josephripberger/Dropbox/Severe Weather and Society Dashboard/local files/downloads/" # define locally!!!
outputs <- "/Users/josephripberger/Dropbox/Severe Weather and Society Dashboard/local files/outputs/" # define locally!!!

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
survey_data$CWA_RPL_THEME1 <- scale(survey_data$CWA_RPL_THEME1)
survey_data$CWA_RPL_THEME2 <- scale(survey_data$CWA_RPL_THEME2)
survey_data$CWA_RPL_THEME3 <- scale(survey_data$CWA_RPL_THEME3)
survey_data$CWA_RPL_THEME4 <- scale(survey_data$CWA_RPL_THEME4)

# Composite Scale Models - Already Run, Output Saved -------------------------
warmup <- 10
iter <- 50
chains <- 1

cwa_recep_fit <- stan_lmer(recep ~ 1 + (1|MALE) + (1|AGE_GROUP) + (1|HISP) + (1|RACE_GROUP) + (1|CWA) + (1|MALE:AGE_GROUP) +
                             CWA_TORN + CWA_RPL_THEME1 + CWA_RPL_THEME2 + CWA_RPL_THEME3 + CWA_RPL_THEME4,
                    data = survey_data, seed = 50, QR = TRUE, warmup = warmup, iter = iter, chains = chains, adapt_delta = 0.99999)
saveRDS(cwa_recep_fit, paste0(outputs, "cwa_models/cwa_recep_fit.Rds"))

cwa_subj_comp_fit <- stan_lmer(subj_comp ~ 1 + (1|MALE) + (1|AGE_GROUP) + (1|HISP) + (1|RACE_GROUP) + (1|CWA) + (1|MALE:AGE_GROUP) +
                                 CWA_TORN + CWA_RPL_THEME1 + CWA_RPL_THEME2 + CWA_RPL_THEME3 + CWA_RPL_THEME4,
                   data = survey_data, seed = 50, QR = TRUE, warmup = warmup, iter = iter, chains = chains, adapt_delta = 0.99999)
saveRDS(cwa_subj_comp_fit, paste0(outputs, "cwa_models/cwa_subj_comp_fit.Rds"))

cwa_obj_comp_fit <- stan_lmer(obj_comp ~ 1 + (1|MALE) + (1|AGE_GROUP) + (1|HISP) + (1|RACE_GROUP) + (1|CWA) + (1|MALE:AGE_GROUP) +
                                CWA_TORN + CWA_RPL_THEME1 + CWA_RPL_THEME2 + CWA_RPL_THEME3 + CWA_RPL_THEME4,
                   data = survey_data, seed = 50, QR = TRUE, warmup = warmup, iter = iter, chains = chains, adapt_delta = 0.99999)
saveRDS(cwa_obj_comp_fit, paste0(outputs, "cwa_models/cwa_obj_comp_fit.Rds"))

cwa_resp_fit <- stan_lmer(resp ~ 1 + (1|MALE) + (1|AGE_GROUP) + (1|HISP) + (1|RACE_GROUP) + (1|CWA) + (1|MALE:AGE_GROUP) +
                            CWA_TORN + CWA_RPL_THEME1 + CWA_RPL_THEME2 + CWA_RPL_THEME3 + CWA_RPL_THEME4,
                           data = survey_data, seed = 50, QR = TRUE, warmup = warmup, iter = iter, chains = chains, adapt_delta = 0.99999)
saveRDS(cwa_resp_fit, paste0(outputs, "cwa_models/cwa_resp_fit.Rds"))

cwa_efficacy_fit <- stan_lmer(efficacy ~ 1 + (1|MALE) + (1|AGE_GROUP) + (1|HISP) + (1|RACE_GROUP) + (1|CWA) + (1|MALE:AGE_GROUP) +
                                CWA_TORN + CWA_RPL_THEME1 + CWA_RPL_THEME2 + CWA_RPL_THEME3 + CWA_RPL_THEME4,
                           data = survey_data, seed = 50, QR = TRUE, warmup = warmup, iter = iter, chains = chains, adapt_delta = 0.99999)
saveRDS(cwa_efficacy_fit, paste0(outputs, "cwa_models/cwa_efficacy_fit.Rds"))

cwa_myth_fit <- stan_lmer(myth ~ 1 + (1|MALE) + (1|AGE_GROUP) + (1|HISP) + (1|RACE_GROUP) + (1|CWA) + (1|MALE:AGE_GROUP) +
                            CWA_TORN + CWA_RPL_THEME1 + CWA_RPL_THEME2 + CWA_RPL_THEME3 + CWA_RPL_THEME4,
                              data = survey_data, seed = 50, QR = TRUE, warmup = warmup, iter = iter, chains = chains, adapt_delta = 0.99999)
saveRDS(cwa_myth_fit, paste0(outputs, "cwa_models/cwa_myth_fit.Rds"))

# Risk Perception Models - Already Run, Output Saved -------------------------
cwa_heat_fit <- stan_lmer(scale_risk_heat ~ 1 + (1|MALE) + (1|AGE_GROUP) + (1|HISP) + (1|RACE_GROUP) + (1|CWA) + (1|MALE:AGE_GROUP) +
                            CWA_HEAT + CWA_RPL_THEME1 + CWA_RPL_THEME2 + CWA_RPL_THEME3 + CWA_RPL_THEME4,
                          data = survey_data, seed = 50, QR = TRUE, warmup = warmup, iter = iter, chains = chains, adapt_delta = 0.99999, )
saveRDS(cwa_heat_fit, paste0(outputs, "cwa_models/cwa_heat_fit.Rds"))

cwa_drought_fit <- stan_lmer(scale_risk_drought ~ 1 + (1|MALE) + (1|AGE_GROUP) + (1|HISP) + (1|RACE_GROUP) + (1|CWA) + (1|MALE:AGE_GROUP) +
                               CWA_DROUGHT + CWA_RPL_THEME1 + CWA_RPL_THEME2 + CWA_RPL_THEME3 + CWA_RPL_THEME4,
                             data = survey_data, seed = 50, QR = TRUE, warmup = warmup, iter = iter, chains = chains, adapt_delta = 0.99999, )
saveRDS(cwa_drought_fit, paste0(outputs, "cwa_models/cwa_drought_fit.Rds"))

cwa_cold_fit <- stan_lmer(scale_risk_cold ~ 1 + (1|MALE) + (1|AGE_GROUP) + (1|HISP) + (1|RACE_GROUP) + (1|CWA) + (1|MALE:AGE_GROUP) +
                            CWA_COLD + CWA_RPL_THEME1 + CWA_RPL_THEME2 + CWA_RPL_THEME3 + CWA_RPL_THEME4,
                          data = survey_data, seed = 50, QR = TRUE, warmup = warmup, iter = iter, chains = chains, adapt_delta = 0.99999, )
saveRDS(cwa_cold_fit, paste0(outputs, "cwa_models/cwa_cold_fit.Rds"))

cwa_snow_fit <- stan_lmer(scale_risk_snow ~ 1 + (1|MALE) + (1|AGE_GROUP) + (1|HISP) + (1|RACE_GROUP) + (1|CWA) + (1|MALE:AGE_GROUP) +
                            CWA_SNOW + CWA_RPL_THEME1 + CWA_RPL_THEME2 + CWA_RPL_THEME3 + CWA_RPL_THEME4,
                          data = survey_data, seed = 50, QR = TRUE, warmup = warmup, iter = iter, chains = chains, adapt_delta = 0.99999, )
saveRDS(cwa_snow_fit, paste0(outputs, "cwa_models/cwa_snow_fit.Rds"))

cwa_torn_fit <- stan_lmer(scale_risk_tor ~ 1 + (1|MALE) + (1|AGE_GROUP) + (1|HISP) + (1|RACE_GROUP) + (1|CWA) + (1|MALE:AGE_GROUP) +
                            CWA_TORN + CWA_RPL_THEME1 + CWA_RPL_THEME2 + CWA_RPL_THEME3 + CWA_RPL_THEME4,
                          data = survey_data, seed = 50, QR = TRUE, warmup = warmup, iter = iter, chains = chains, adapt_delta = 0.99999, )
saveRDS(cwa_torn_fit, paste0(outputs, "cwa_models/cwa_torn_fit.Rds"))

cwa_flood_fit <- stan_lmer(scale_risk_flood ~ 1 + (1|MALE) + (1|AGE_GROUP) + (1|HISP) + (1|RACE_GROUP) + (1|CWA) + (1|MALE:AGE_GROUP) +
                             CWA_FLOOD + CWA_RPL_THEME1 + CWA_RPL_THEME2 + CWA_RPL_THEME3 + CWA_RPL_THEME4,
                           data = survey_data, seed = 50, QR = TRUE, warmup = warmup, iter = iter, chains = chains, adapt_delta = 0.99999, )
saveRDS(cwa_flood_fit, paste0(outputs, "cwa_models/cwa_flood_fit.Rds"))

cwa_hurr_fit <- stan_lmer(scale_risk_hur ~ 1 + (1|MALE) + (1|AGE_GROUP) + (1|HISP) + (1|RACE_GROUP) + (1|CWA) + (1|MALE:AGE_GROUP) +
                            CWA_HURR + CWA_RPL_THEME1 + CWA_RPL_THEME2 + CWA_RPL_THEME3 + CWA_RPL_THEME4,
                          data = survey_data, seed = 50, QR = TRUE, warmup = warmup, iter = iter, chains = chains, adapt_delta = 0.99999, )
saveRDS(cwa_hurr_fit, paste0(outputs, "cwa_models/cwa_hurr_fit.Rds"))

cwa_fire_fit <- stan_lmer(scale_risk_fire ~ 1 + (1|MALE) + (1|AGE_GROUP) + (1|HISP) + (1|RACE_GROUP) + (1|CWA) + (1|MALE:AGE_GROUP) +
                            CWA_FIRE + CWA_RPL_THEME1 + CWA_RPL_THEME2 + CWA_RPL_THEME3 + CWA_RPL_THEME4,
                          data = survey_data, seed = 50, QR = TRUE, warmup = warmup, iter = iter, chains = chains, adapt_delta = 0.99999, )
saveRDS(cwa_fire_fit, paste0(outputs, "cwa_models/cwa_fire_fit.Rds"))