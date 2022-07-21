library(ltm)
library(readxl)
library(sf)
library(tidyverse)
library(data.table)
options(scipen = 9999)

downloads <- "/Users/josephripberger/Dropbox (Univ. of Oklahoma)/Severe Weather and Society Dashboard/local files/downloads/" # define locally!!!
outputs <- "/Users/josephripberger/Dropbox (Univ. of Oklahoma)/Severe Weather and Society Dashboard/local files/outputs/" # define locally!!!

# Import Survey Data -----------------------------
WX17 <- read_csv(paste0(downloads, "WX17_data_wtd.csv")) # Survey Data
WX17$p_id <- as.character(WX17$p_id)
WX17$zip <- as.numeric(WX17$zip)
WX17$long_months <- as.character(WX17$long_months)
WX17$adults <- as.character(WX17$adults)
WX17$warn_prob_area <- as.numeric(WX17$warn_prob_area)
WX17$warn_cons <- as.character(WX17$warn_cons)
WX17$watch_cons <- as.character(WX17$watch_cons)
WX17$nws_region <- NA
WX17$nws_region <- ifelse(WX17$region == 1, "Eastern Region", WX17$nws_region)
WX17$nws_region <- ifelse(WX17$region == 2, "Southern Region", WX17$nws_region)
WX17$nws_region <- ifelse(WX17$region == 3, "Central Region", WX17$nws_region)
WX17$nws_region <- ifelse(WX17$region == 4, "Western Region", WX17$nws_region)
WX17 <- WX17 %>% 
  select(-c(rec_all:rec_time)) %>%  # scale changes form 1-7 to 1-5 in 2018/2019
  select(-c(resp_ignore:resp_unsure)) # scale changes form 1-7 to 1-5 in 2018/2019

WX18 <- read_csv(paste0(downloads, "WX18_data_wtd.csv")) # Survey Data
WX18$warn_prob_area <- as.numeric(WX18$warn_prob_area)

WX19 <- read_csv(paste0(downloads, "WX19_data_wtd.csv")) # Survey Data
WX19$adults <- as.character(WX19$adults)
WX19$bigbucks <- as.numeric(WX19$bigbucks)
WX17$choir <- as.numeric(WX17$choir)
WX17$fiveside <- as.numeric(WX17$fiveside)
WX17$mushroom <- as.numeric(WX17$mushroom)

WX20 <- read_csv(paste0(downloads, "WX20_data_wtd.csv")) # Survey Data
WX20$zip <- as.numeric(WX20$zip)
WX20$children <- as.numeric(WX20$children)
WX20$bigbucks <- as.numeric(WX20$bigbucks)
WX20$choir <- as.numeric(WX20$choir)
WX20$mushroom <- as.numeric(WX20$mushroom)

WX21 <- read_csv(paste0(downloads, "WX21_data_wtd.csv")) # Survey Data
# WX21$zip <- as.numeric(WX21$zip)
# WX21$children <- as.numeric(WX21$children)
WX21$bigbucks <- as.numeric(WX21$bigbucks)
WX21$choir <- as.numeric(WX21$choir)
WX21$mushroom <- as.numeric(WX21$mushroom)

WX21SP <- read_csv(paste0(downloads, "WX21_spanish_data_unwtd.csv")) # Survey Data
WX21SP$rand_aft <- as.character(WX21SP$rand_aft)
WX21SP$rand_eve <- as.character(WX21SP$rand_eve)

WX17$survey_year <- 2017 
WX18$survey_year <- 2018
WX19$survey_year <- 2019
WX20$survey_year <- 2020
WX21$survey_year <- 2021
WX21SP$survey_year <- 2021

WX17$lang <- "English" 
WX18$lang <- "English"
WX19$lang <- "English"
WX20$lang <- "English"
WX21$lang <- "English"
WX21SP$lang <- "Spanish" ## MISSING FIPS!??

survey_data <- rbindlist(list(WX17, WX18, WX19, WX20, WX21), fill = TRUE)

# Identify respondent FIPS, CWA, and Region ------------------
zip_to_county <- read_excel(paste0(downloads, "ZIP_COUNTY_032020.xlsx"), 
                            col_types = c("text", "text", "numeric", "numeric", "numeric", "numeric")) # https://www.huduser.gov/portal/datasets/usps_crosswalk.html
zip_to_county <- zip_to_county %>% 
  select(ZIP, FIPS = COUNTY, RES_RATIO) %>% 
  arrange(ZIP, -RES_RATIO) %>% 
  distinct(ZIP, .keep_all = TRUE)

survey_data$zip <- str_pad(survey_data$zip, 5, side = "left", pad = 0)
survey_data <- left_join(survey_data, zip_to_county, by = c("zip" = "ZIP")) 
survey_data <- drop_na(survey_data, FIPS) # drops 59 respondents (invalid zipcodes)

cwa_cnty_shp <- read_sf(paste0(downloads, "c_03mr20"), "c_03mr20") %>% as_tibble() %>% select(CWA, FIPS)
cwa_cnty_shp$CWA <- substr(cwa_cnty_shp$CWA, start = 1, stop = 3) # Only keep first CWA in counties that span multiple CWAs
cwa_cnty_shp$CWA <- ifelse(cwa_cnty_shp$FIPS == "12087", "KEY", cwa_cnty_shp$CWA) # Fix KEY (assign FIPS 12087 to KEY alone, not KEY and MFL)
cwa_cnty_shp <- cwa_cnty_shp %>% distinct(FIPS, .keep_all = TRUE) # Remove duplicate FIPS codes (counties that span multiple CWAs)
cwa_cnty_shp <- cwa_cnty_shp %>% filter(!CWA %in% c("PPG", "SJU", "GUM", "HFO", "AFC", "AFG", "AJK")) # No census data
survey_data <- left_join(survey_data, cwa_cnty_shp %>% select(FIPS, CWA), by = "FIPS")
survey_data <- drop_na(survey_data, CWA) # drops 2 respondents (FIPS codes in PR)

cwa_shp <- read_sf(paste0(downloads, "w_03mr20"), "w_03mr20") %>% as_tibble() %>% select(CWA, Region)
survey_data <- left_join(survey_data, cwa_shp, by = "CWA")

# Recode/Rename Variables -----------------------------
survey_data$MALE <- factor(survey_data$gend)
survey_data$AGE <- survey_data$age
survey_data$AGE_GROUP <- factor(car::recode(survey_data$age, "18:34 = 1; 35:59 = 2; 60:110 = 3"))
survey_data$HISP <-factor(survey_data$hisp)
survey_data$RACE_GROUP <- factor(car::recode(survey_data$race, "1 = 1; 2 = 2; else = 3"))

# Add Variables -----------------------------
cointoss_correct <- car::recode(as.numeric(as.character(survey_data$cointoss)),"500 = 1; NA = NA; else = 0")
bigbucks_correct <- car::recode(as.numeric(as.character(survey_data$bigbucks)),"10 = 1; NA = NA; else = 0")
acme_pub_correct <- car::recode(as.numeric(as.character(survey_data$acme_pub)),"0.1 = 1; NA = NA; else = 0")
choir_correct <- car::recode(as.numeric(as.character(survey_data$choir)),"25 = 1;NA = NA; else = 0")
fiveside_correct <- car::recode(as.numeric(as.character(survey_data$fiveside)),"30 = 1; NA = NA; else = 0")
sixside_correct <- car::recode(as.numeric(as.character(survey_data$sixside)),"20 = 1; NA = NA; else = 0")
mushroom_correct <- car::recode(as.numeric(as.character(survey_data$mushroom)),"50 = 1; NA = NA; else = 0")
part1_score <- cointoss_correct + bigbucks_correct + acme_pub_correct
part2_score <- NA
advance <- ifelse(part1_score < 2, 0, 1)
part2_score <- ifelse(advance == 0,0,part2_score)
part2_score <- ifelse(advance == 1 & choir_correct == 0 & fiveside_correct == 0,1,part2_score)
part2_score <- ifelse(advance == 1 & choir_correct == 0 & fiveside_correct == 1,2,part2_score)
part2_score <- ifelse(advance == 1 & choir_correct == 1 & sixside_correct == 0 & mushroom_correct == 0, 3, part2_score)
part2_score <- ifelse(advance == 1 & choir_correct == 1 & sixside_correct == 0 & mushroom_correct == 1, 4, part2_score)
part2_score <- ifelse(advance == 1 & choir_correct == 1 & sixside_correct == 1, 4, part2_score)
numeracy <- part1_score + part2_score
survey_data$Numeracy <- car::recode(numeracy, "0:2 = 'Low'; 2:4 = 'Moderate'; 5:7 = 'High'")

# Event Data -------------------------
cwa_storm_data <- read_csv(paste0(outputs, "base_cwa_storm_data.csv"))
county_storm_data <- read_csv(paste0(outputs, "base_county_storm_data.csv"))

cwa_storm_data <- cwa_storm_data %>% rename_at(vars(COLD:DROUGHT), ~paste0("CWA_", .))
county_storm_data <- county_storm_data %>% rename_at(vars(COLD:DROUGHT), ~paste0("FIPS_", .))

survey_data <- left_join(survey_data, cwa_storm_data, by = "CWA")
survey_data <- left_join(survey_data, county_storm_data, by = "FIPS")

# Risk Data (from FEMA) -------------------------
cwa_risk_data <- read_csv(paste0(outputs, "base_cwa_risk_data.csv"))
county_risk_data <- read_csv(paste0(outputs, "base_county_risk_data.csv"))

cwa_risk_data <- cwa_risk_data %>% rename_at(vars(CWA_SOVI:CWA_SNOW), ~paste0("FEMA_", .))
county_risk_data <- county_risk_data %>% rename_at(vars(FIPS_SOVI:FIPS_SNOW), ~paste0("FIPS_", .))

survey_data <- left_join(survey_data, cwa_risk_data, by = "CWA")
survey_data <- left_join(survey_data, county_risk_data, by = "FIPS")

# Census Data -------------------------
cwa_census_data <- read_csv(paste0(outputs, "base_cwa_census_data.csv"))
county_census_data <- read_csv(paste0(outputs, "base_county_census_data.csv"))

cwa_svi_data <- cwa_census_data %>% 
  select(CWA, CWA_EP_POV:CWA_RPL_THEMES) %>% 
  distinct(CWA, .keep_all = TRUE)

county_svi_data <- county_census_data %>% 
  select(FIPS, FIPS_EP_POV:FIPS_RPL_THEMES) %>% 
  distinct(FIPS, .keep_all = TRUE)

survey_data <- left_join(survey_data, cwa_svi_data, by = "CWA")
survey_data <- left_join(survey_data, county_svi_data, by = "FIPS")

# Measures for Models -------------------------
recep_data <- survey_data %>%
  filter(!survey_year == 2017) %>%
  select(rec_all, rec_most, rec_soon, rec_sleep, rec_driving, rec_work, rec_store, rec_small_group, rec_large_group, rec_morn, rec_aft, rec_eve)
recep_fit <- grm(recep_data)
recep_scores <- tibble(p_id = filter(survey_data, !survey_year == 2017)$p_id, recep = ltm::factor.scores(recep_fit, resp.patterns = recep_data)$score.dat$z1)
survey_data <- left_join(survey_data, recep_scores, by = "p_id")

subj_comp_data <- survey_data %>%
  filter(!survey_year == 2017) %>%
  select(alert_und, tor_watchwarn_und, tor_map_und, tor_radar_und, svr_watchwarn_und, und_morn, und_aft, und_eve)
subj_comp_fit <- grm(subj_comp_data)
subj_comp_scores <- tibble(p_id = filter(survey_data, !survey_year == 2017)$p_id, subj_comp = ltm::factor.scores(subj_comp_fit, resp.patterns = subj_comp_data)$score.dat$z1)
survey_data <- left_join(survey_data, subj_comp_scores, by = "p_id")

survey_data$watch_warn_group <- ifelse(is.na(survey_data$torwatch) == FALSE, "watch", "warn")
survey_data$watch_warn_correct <- NA
survey_data$watch_warn_correct <- ifelse(survey_data$watch_warn_group == "watch" & survey_data$torwatch == 1, 1, survey_data$watch_warn_correct)
survey_data$watch_warn_correct <- ifelse(survey_data$watch_warn_group == "watch" & survey_data$torwatch != 1, 0, survey_data$watch_warn_correct)
survey_data$watch_warn_correct <- ifelse(survey_data$watch_warn_group == "warn" & survey_data$torwarn == 2, 1, survey_data$watch_warn_correct)
survey_data$watch_warn_correct <- ifelse(survey_data$watch_warn_group == "warn" & survey_data$torwarn != 2, 0, survey_data$watch_warn_correct)
survey_data$warn_time_correct <- ifelse(survey_data$warn_time == 1 & survey_data$warn_time_minutes < 30, 1, 0)
survey_data$watch_time_correct <- ifelse(survey_data$watch_time == 2 & survey_data$watch_time_hours >= 1 & survey_data$watch_time_hours <= 3, 1, 0)
survey_data$warn_size_correct <- ifelse(survey_data$warn_size == 1 | survey_data$warn_size == 2, 1, 0)
survey_data$watch_size_correct <- ifelse(survey_data$watch_size == 3 | survey_data$watch_size == 4 | survey_data$watch_size == 5, 1, 0)
obj_comp_data <- survey_data %>%
  filter(!survey_year == 2017) %>%
  select(watch_warn_correct, warn_time_correct, watch_time_correct, watch_size_correct)
obj_comp_fit <- ltm(obj_comp_data ~ z1)
obj_comp_scores <- tibble(p_id = filter(survey_data, !survey_year == 2017)$p_id, obj_comp = ltm::factor.scores(obj_comp_fit, resp.patterns = obj_comp_data)$score.dat$z1)
survey_data <- left_join(survey_data, obj_comp_scores, by = "p_id")

resp_data <- survey_data %>%
  filter(!survey_year == 2017) %>%
  select(resp_prot, resp_sleep, resp_driving, resp_work, resp_store, resp_small_group, resp_large_group, resp_morn, resp_aft, resp_eve)
resp_fit <- grm(resp_data)
resp_scores <- tibble(p_id = filter(survey_data, !survey_year == 2017)$p_id, resp = ltm::factor.scores(resp_fit, resp.patterns = resp_data)$score.dat$z1)
survey_data <- left_join(survey_data, resp_scores, by = "p_id")

trust_data <- survey_data %>% select(nws_trust, lotv_trust, natv_trust, em_trust)
trust_fit <- grm(trust_data)
survey_data$trust <- ltm::factor.scores(trust_fit, resp.patterns = trust_data)$score.dat$z1

efficacy_data <- survey_data %>%
  filter(survey_year == 2018) %>%
  select(tor_eff1, tor_eff2, tor_eff3, tor_eff4, tor_eff5, tor_eff6, tor_eff7, tor_eff8)
efficacy_fit <- grm(efficacy_data)
efficacy_scores <- tibble(p_id = filter(survey_data, survey_year == 2018)$p_id, efficacy = ltm::factor.scores(efficacy_fit, resp.patterns = efficacy_data)$score.dat$z1)
survey_data <- left_join(survey_data, efficacy_scores, by = "p_id")

survey_data$myth_tall_correct <- ifelse(survey_data$myth_tall == 0, 1, 0)
survey_data$myth_mtns_correct <- ifelse(survey_data$myth_mtns == 1, 1, 0)
survey_data$myth_brdg_correct <- ifelse(survey_data$myth_brdge == 1, 1, 0)
survey_data$myth_open_correct <- ifelse(survey_data$myth_open == 0, 1, 0)
myth_data <- survey_data %>%
  filter(survey_year == 2017) %>%
  select(myth_tall_correct, myth_mtns_correct, myth_brdg_correct, myth_open_correct)
myth_fit <- ltm(myth_data ~ z1)
myth_scores <- tibble(p_id = filter(survey_data, survey_year == 2017)$p_id, myth = ltm::factor.scores(myth_fit, resp.patterns = myth_data)$score.dat$z1)
survey_data <- left_join(survey_data, myth_scores, by = "p_id")

ready_comp_data <- survey_data %>%
  filter(survey_year %in% c(2019, 2020, 2021)) %>%
  select(c(rq_4:rq_8)) %>% 
  mutate_all(funs(ifelse(. == 1, 1, 0)))
ready_comp_fit <- ltm(ready_comp_data ~ z1)
ready_comp_scores <- tibble(p_id = filter(survey_data, survey_year %in% c(2019, 2020, 2021))$p_id, ready = ltm::factor.scores(ready_comp_fit, resp.patterns = ready_comp_data)$score.dat$z1)
survey_data <- left_join(survey_data, ready_comp_scores, by = "p_id")

# Write Data -----------------------------
write_csv(survey_data, paste0(outputs, "base_survey_data.csv"))
