library(ltm)
library(tidyverse)
library(data.table)
library(readxl)
library(sf)

downloads <- "/Users/josephripberger/Dropbox (Univ. of Oklahoma)/Severe Weather and Society Dashboard/local files/downloads/" # define locally!!!
outputs <- "/Users/josephripberger/Dropbox (Univ. of Oklahoma)/Severe Weather and Society Dashboard/local files/outputs/" # define locally!!!

# Import Survey Data -----------------------------
WX17 <- read_csv(paste0(downloads, "WX17_data_wtd.csv")) %>% 
  mutate(survey_year = "2017", 
         survey_hazard = "WX",
         survey_language = "English",
         p_id = as.character(p_id),
         zip = as.numeric(zip),
         nws_region = case_when(
           region == 1 ~ "Eastern Region",
           region == 2 ~ "Southern Region",
           region == 3 ~ "Central Region",
           region == 4 ~ "Western Region")) %>% 
  select(-c(rec_all:rec_time)) %>%  # remove because scale changes form 1-7 to 1-5 in 2018/2019
  select(-c(resp_ignore:resp_unsure)) # remove because scale changes form 1-7 to 1-5 in 2018/2019
WX18 <- read_csv(paste0(downloads, "WX18_data_wtd.csv")) %>% 
  mutate(survey_year = "2018", 
         survey_hazard = "WX",
         survey_language = "English")
WX19 <- read_csv(paste0(downloads, "WX19_data_wtd.csv")) %>% 
  mutate(survey_year = "2019", 
         survey_hazard = "WX", 
         survey_language = "English")
WX20 <- read_csv(paste0(downloads, "WX20_data_wtd.csv")) %>% 
  mutate(survey_year = "2020", 
         survey_hazard = "WX",
         survey_language = "English", 
         zip = as.numeric(zip))
WX21 <- read_csv(paste0(downloads, "WX21_data_wtd.csv")) %>% 
  mutate(survey_year = "2021", 
         survey_hazard = "WX",
         survey_language = "English")
# WX21SP <- read_csv(paste0(downloads, "WX21_spanish_data_wtd.csv")) %>% 
#   mutate(survey_year = "2021", 
#          survey_hazard = "WX",
#          survey_language = "Spanish")
WX22 <- read_csv(paste0(downloads, "WX22_data_wtd.csv")) %>% 
  mutate(survey_year = "2022", 
         survey_hazard = "WX",
         survey_language = "English")
# WX22SP <- read_csv(paste0(downloads, "WX22_spanish_data_wtd.csv")) %>% 
#   mutate(survey_year = "2022", 
#          survey_hazard = "WX",
#          survey_language = "Spanish")
WX23 <- read_csv(paste0(downloads, "WX23_data_wtd.csv")) %>% 
  mutate(survey_year = "2023", 
         survey_hazard = "WX",
         survey_language = "English")
# WX23SP <- read_csv(paste0(downloads, "WX23_spanish_data_wtd.csv")) %>% 
#   mutate(survey_year = "2023", 
#          survey_hazard = "WX",
#          survey_language = "Spanish")

TC20 <- read_csv(paste0(downloads, "TC20_data_wtd.csv")) %>% 
  mutate(survey_year = "2020",
         survey_hazard = "TC",
         survey_language = "English")
TC21 <- read_csv(paste0(downloads, "TC21_data_wtd.csv")) %>% 
  mutate(survey_year = "2021",
         survey_hazard = "TC",
         survey_language = "English")
TC22 <- read_csv(paste0(downloads, "TC22_data_wtd.csv")) %>% 
  mutate(survey_year = "2022",
         survey_hazard = "TC",
         survey_language = "English")
TC23 <- read_csv(paste0(downloads, "TC23_data_wtd.csv")) %>% 
  mutate(survey_year = "2023",
         survey_hazard = "TC",
         survey_language = "English")
# TC23SP <- read_csv(paste0(downloads, "TC23_spanish_data_wtd.csv")) %>% 
#   mutate(survey_year = "2023", 
#          survey_hazard = "TC",
#          survey_language = "Spanish")

survey_data <- rbindlist(list(WX17, WX18, WX19, WX20, WX21, WX22, WX23, TC20, TC21, TC22, TC23), fill = TRUE)
survey_data %>% count(survey_hazard, survey_year)

# Identify respondent FIPS, CWA, and Region ------------------
zip_to_county <- read_excel(paste0(downloads, "ZIP_COUNTY_032020.xlsx"), 
                            col_types = c("text", "text", "numeric", "numeric", "numeric", "numeric")) # https://www.huduser.gov/portal/datasets/usps_crosswalk.html
zip_to_county <- zip_to_county %>% 
  select(ZIP, FIPS = COUNTY, RES_RATIO) %>% 
  arrange(ZIP, -RES_RATIO) %>% 
  distinct(ZIP, .keep_all = TRUE)

survey_data$zip <- str_pad(survey_data$zip, 5, side = "left", pad = 0)
survey_data <- left_join(survey_data, zip_to_county, by = c("zip" = "ZIP")) 
survey_data <- drop_na(survey_data, FIPS) # drops 82 respondents (invalid zipcodes)

cwa_cnty_shp <- read_sf(paste0(downloads, "c_03mr20"), "c_03mr20") %>% as_tibble() %>% select(CWA, FIPS)
cwa_cnty_shp$CWA <- substr(cwa_cnty_shp$CWA, start = 1, stop = 3) # Only keep first CWA in counties that span multiple CWAs
cwa_cnty_shp$CWA <- ifelse(cwa_cnty_shp$FIPS == "12087", "KEY", cwa_cnty_shp$CWA) # Fix KEY (assign FIPS 12087 to KEY alone, not KEY and MFL)
cwa_cnty_shp <- cwa_cnty_shp %>% distinct(FIPS, .keep_all = TRUE) # Remove duplicate FIPS codes (counties that span multiple CWAs)
cwa_cnty_shp <- cwa_cnty_shp %>% filter(!CWA %in% c("PPG", "SJU", "GUM", "HFO", "AFC", "AFG", "AJK")) # No census data
survey_data <- left_join(survey_data, cwa_cnty_shp %>% select(FIPS, CWA), by = "FIPS")
survey_data <- drop_na(survey_data, CWA) # drops 2 respondents (FIPS codes in PR)

cwa_shp <- read_sf(paste0(downloads, "w_03mr20"), "w_03mr20") %>% as_tibble() %>% select(CWA, Region)
survey_data <- left_join(survey_data, cwa_shp, by = "CWA")
survey_data %>% count(CWA, sort = TRUE)

# Recode/Rename Variables -----------------------------
survey_data$MALE <- factor(survey_data$gend)
survey_data$AGE <- survey_data$age
survey_data$AGE_GROUP <- factor(car::recode(survey_data$age, "18:34 = 1; 35:59 = 2; 60:110 = 3"))
survey_data$HISP <-factor(survey_data$hisp) # missing 18 HISP obs. in WX17
survey_data$RACE_GROUP <- factor(car::recode(survey_data$race, "1 = 1; 2 = 2; else = 3"))

# Add Variables -----------------------------
# survey_data <- survey_data %>%
#   mutate(cointoss_correct = ifelse(as.numeric(as.character(cointoss)) == 500, 1, 0),
#          bigbucks_correct = ifelse(as.numeric(as.character(bigbucks)) == 10, 1, 0),
#          acme_pub_correct = ifelse(as.numeric(as.character(acme_pub)) == 0.1, 1, 0),
#          choir_correct = ifelse(as.numeric(as.character(choir)) == 25, 1, 0),
#          fiveside_correct = ifelse(as.numeric(as.character(fiveside)) == 30, 2, 0),
#          sixside_correct = ifelse(as.numeric(as.character(sixside)) == 20, 3, 0),
#          mushroom_correct = ifelse(as.numeric(as.character(mushroom)) == 50, 2, 0),
#          numeracy_scale = rowSums(across(cointoss_correct:mushroom_correct), na.rm = TRUE)) %>%
#   mutate(Numeracy = case_when(
#     numeracy_scale %in% 0:2 ~ "(1) Low",
#     numeracy_scale %in% 3:4 ~ "(2) Moderate",
#     numeracy_scale %in% 5:7 ~ "(3) High")) # TODO: fix this!!

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
  select(CWA, CWA_EP_POV150:CWA_RPL_THEMES) %>% 
  distinct(CWA, .keep_all = TRUE)

county_svi_data <- county_census_data %>% 
  select(FIPS, FIPS_EP_POV150:FIPS_RPL_THEMES) %>% 
  distinct(FIPS, .keep_all = TRUE)

survey_data <- left_join(survey_data, cwa_svi_data, by = "CWA")
survey_data <- left_join(survey_data, county_svi_data, by = "FIPS")

# Measures for Models -------------------------
to_recep_data <- survey_data %>%
  filter(survey_hazard == "WX" & !survey_year == 2017) %>%
  select(p_id, rec_all, rec_most, rec_soon, rec_sleep, rec_driving, rec_work, rec_store, rec_small_group, rec_large_group, rec_morn, rec_aft, rec_eve)
to_recep_fit <- grm(to_recep_data %>% select(-p_id))
to_recep_scores <- tibble(to_recep_data %>% select(p_id), 
                          to_recep = ltm::factor.scores(to_recep_fit, resp.patterns = to_recep_data %>% select(-p_id))$score.dat$z1)
survey_data <- left_join(survey_data, to_recep_scores, by = "p_id")

hu_recep_data <- survey_data %>%
  filter(survey_hazard == "TC") %>%
  select(p_id, rec_most, rec_time)
hu_recep_fit <- grm(hu_recep_data %>% select(-p_id))
hu_recep_scores <- tibble(hu_recep_data %>% select(p_id),
                          hu_recep = ltm::factor.scores(hu_recep_fit, resp.patterns = hu_recep_data %>% select(-p_id))$score.dat$z1)
survey_data <- left_join(survey_data, hu_recep_scores, by = "p_id")

to_subj_comp_data <- survey_data %>%
  filter(survey_hazard == "WX" & !survey_year == 2017) %>%
  select(p_id, alert_und, tor_watchwarn_und, tor_map_und, tor_radar_und, svr_watchwarn_und)
to_subj_comp_fit <- grm(to_subj_comp_data %>% select(-p_id))
to_subj_comp_scores <- tibble(to_subj_comp_data  %>% select(p_id), 
                              to_subj_comp = ltm::factor.scores(to_subj_comp_fit, resp.patterns = to_subj_comp_data %>% select(-p_id))$score.dat$z1)
survey_data <- left_join(survey_data, to_subj_comp_scores, by = "p_id")

hu_subj_comp_data <- survey_data %>%
  filter(survey_hazard == "TC") %>%
  select(p_id, alert_und, huralerts, hur_map_und, tor_watchwarn_und, flood_watchwarn_und, flood_srg_und)
hu_subj_comp_fit <- grm(hu_subj_comp_data %>% select(-p_id))
hu_subj_comp_scores <- tibble(hu_subj_comp_data %>% select(p_id),
                          hu_subj_comp = ltm::factor.scores(hu_subj_comp_fit, resp.patterns = hu_subj_comp_data %>% select(-p_id))$score.dat$z1)
survey_data <- left_join(survey_data, hu_subj_comp_scores, by = "p_id")

survey_data <- survey_data %>% 
  mutate(to_watch_warn_group = case_when(
    survey_hazard == "WX" & is.na(torwatch) == FALSE ~ "watch",
    survey_hazard == "WX" & is.na(torwarn) == FALSE ~ "warn")) %>% 
  mutate(to_watch_warn_correct = case_when(
    to_watch_warn_group == "watch"  & torwatch == 1 ~ 1,
    to_watch_warn_group == "watch"  & torwatch != 1 ~ 0,
    to_watch_warn_group == "warn"  & torwarn == 2 ~ 1,
    to_watch_warn_group == "warn"  & torwarn != 2 ~ 0)) %>% 
  mutate(to_warn_time_correct = ifelse(warn_time == 1 & warn_time_minutes < 30, 1, 0)) %>% 
  mutate(to_watch_time_correct = ifelse(watch_time == 2 & watch_time_hours >= 1 & watch_time_hours <= 3, 1, 0)) %>% 
  mutate(to_warn_size_correct = ifelse(warn_size == 1 | warn_size == 2, 1, 0)) %>% # not including these for now
  mutate(to_watch_size_correct = ifelse(watch_size == 3 | watch_size == 4 | watch_size == 5, 1, 0)) # not including these for now
to_obj_comp_data <- survey_data %>%
  filter(survey_hazard == "WX" & !survey_year == 2017) %>%
  select(p_id, to_watch_warn_correct, to_warn_time_correct, to_watch_time_correct)
to_obj_comp_fit <- ltm(to_obj_comp_data %>% select(-p_id) ~ z1)
to_obj_comp_scores <- tibble(to_obj_comp_data  %>% select(p_id), 
                              to_obj_comp = ltm::factor.scores(to_obj_comp_fit, resp.patterns = to_obj_comp_data %>% select(-p_id))$score.dat$z1)
survey_data <- left_join(survey_data, to_obj_comp_scores, by = "p_id")

survey_data <- survey_data %>% 
  mutate(hu_watch_warn_group = case_when(
    survey_hazard == "TC" & is.na(hurwatch) == FALSE ~ "hurwatch",
    survey_hazard == "TC" & is.na(hurwarn) == FALSE ~ "hurwarn")) %>% 
  mutate(hu_watch_warn_correct = case_when(
    hu_watch_warn_group == "watch" & hurwatch == 1 ~ 1,
    hu_watch_warn_group == "watch" & hurwatch != 1 ~ 0,
    hu_watch_warn_group == "warn" & hurwarn == 2 ~ 1,
    hu_watch_warn_group == "warn" & hurwarn != 2 ~ 0)) %>% 
  mutate(ts_warn_time_correct = ifelse(ts_warn_time == 3, 1, 0)) %>% 
  mutate(hur_warn_time_correct = ifelse(hur_warn_time == 3, 1, 0)) %>% 
  mutate(srg_warn_time_correct = ifelse(srg_warn_time == 3, 1, 0)) %>% 
  mutate(ts_watch_time_correct = ifelse(ts_watch_time == 4, 1, 0)) %>% 
  mutate(hur_watch_time_correct = ifelse(hur_watch_time == 4, 1, 0)) %>% 
  mutate(srg_watch_time_correct = ifelse(srg_watch_time == 4, 1, 0))
hu_obj_comp_data <- survey_data %>%
  filter(survey_hazard == "TC") %>%
  select(p_id, hu_watch_warn_correct, ts_warn_time_correct, hur_warn_time_correct, srg_warn_time_correct, 
         ts_watch_time_correct, hur_watch_time_correct, srg_watch_time_correct)
hu_obj_comp_fit <- ltm(hu_obj_comp_data %>% select(-p_id) ~ z1)
hu_obj_comp_scores <- tibble(hu_obj_comp_data  %>% select(p_id), 
                             hu_obj_comp = ltm::factor.scores(hu_obj_comp_fit, resp.patterns = hu_obj_comp_data %>% select(-p_id))$score.dat$z1)
survey_data <- left_join(survey_data, hu_obj_comp_scores, by = "p_id")

to_resp_data <- survey_data %>%
  filter(survey_hazard == "WX" & !survey_year == 2017) %>%
  select(p_id, resp_prot, resp_sleep, resp_driving, resp_work, resp_store, resp_small_group, resp_large_group, resp_morn, resp_aft, resp_eve)
to_resp_fit <- grm(to_resp_data %>% select(-p_id))
to_resp_scores <- tibble(to_resp_data %>% select(p_id), 
                          to_resp = ltm::factor.scores(to_resp_fit, resp.patterns = to_resp_data %>% select(-p_id))$score.dat$z1)
survey_data <- left_join(survey_data, to_resp_scores, by = "p_id")

hu_resp_data <- survey_data %>%
  filter(survey_hazard == "TC") %>%
  select(p_id, resp_always, resp_ignore) %>% 
  mutate(resp_ignore = (resp_ignore * -1) + 6)
hu_resp_fit <- grm(hu_resp_data %>% select(-p_id))
hu_resp_scores <- tibble(hu_resp_data %>% select(p_id),
                          hu_resp = ltm::factor.scores(hu_resp_fit, resp.patterns = hu_resp_data %>% select(-p_id))$score.dat$z1)
survey_data <- left_join(survey_data, hu_resp_scores, by = "p_id")

all_trust_data <- survey_data %>% 
  select(p_id, nws_trust, lotv_trust, natv_trust, em_trust)
all_trust_fit <- grm(all_trust_data %>% select(-p_id))
all_trust_scores <- tibble(all_trust_data %>% select(p_id),
                         all_trust = ltm::factor.scores(all_trust_fit, resp.patterns = all_trust_data %>% select(-p_id))$score.dat$z1)
survey_data <- left_join(survey_data, all_trust_scores, by = "p_id")

all_ready_data <- survey_data %>%
  select(p_id, rq_4:rq_8) %>% 
  mutate_at(vars(rq_4:rq_8), list(~ifelse(. == 1, 1, 0)))
all_ready_fit <- ltm(all_ready_data %>% select(-p_id) ~ z1)
all_ready_scores <- tibble(all_ready_data %>% select(p_id),
                           all_ready = ltm::factor.scores(all_ready_fit, resp.patterns = all_ready_data %>% select(-p_id))$score.dat$z1)
survey_data <- left_join(survey_data, all_ready_scores, by = "p_id")

to_eff_data <- survey_data %>%
  filter(survey_hazard == "WX", survey_year == 2018) %>%
  select(p_id, tor_eff1, tor_eff2, tor_eff3, tor_eff4, tor_eff5, tor_eff6, tor_eff7, tor_eff8)
to_eff_fit <- grm(to_eff_data %>% select(-p_id))
to_eff_scores <- tibble(to_eff_data %>% select(p_id), 
                         to_eff = ltm::factor.scores(to_eff_fit, resp.patterns = to_eff_data %>% select(-p_id))$score.dat$z1)
survey_data <- left_join(survey_data, to_eff_scores, by = "p_id")

# survey_data$myth_tall_correct <- ifelse(survey_data$myth_tall == 0, 1, 0)
# survey_data$myth_mtns_correct <- ifelse(survey_data$myth_mtns == 1, 1, 0)
# survey_data$myth_brdg_correct <- ifelse(survey_data$myth_brdge == 1, 1, 0)
# survey_data$myth_open_correct <- ifelse(survey_data$myth_open == 0, 1, 0)
# myth_data <- survey_data %>%
#   filter(survey_year == 2017) %>%
#   select(myth_tall_correct, myth_mtns_correct, myth_brdg_correct, myth_open_correct)
# myth_fit <- ltm(myth_data ~ z1)
# myth_scores <- tibble(p_id = filter(survey_data, survey_year == 2017)$p_id, myth = ltm::factor.scores(myth_fit, resp.patterns = myth_data)$score.dat$z1)
# survey_data <- left_join(survey_data, myth_scores, by = "p_id")


# Write Data -----------------------------
write_csv(survey_data, paste0(outputs, "base_survey_data.csv"))
