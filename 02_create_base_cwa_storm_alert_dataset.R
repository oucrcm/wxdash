library(tidyverse)
library(sf)
library(data.table)
library(lubridate)

downloads <- "/Users/josephripberger/Dropbox (Univ. of Oklahoma)/Severe Weather and Society Dashboard/local files/downloads/" # define locally!!!
outputs <- "/Users/josephripberger/Dropbox (Univ. of Oklahoma)/Severe Weather and Society Dashboard/local files/outputs/" # define locally!!!

# Import Shapefiles -----------------------------
wwa_paths <- list.files(downloads, full.names = TRUE, pattern = "_all") # source: https://mesonet.agron.iastate.edu/request/gis/watchwarn.phtml 

# Function to Process Shapefiles -----------------
process_wwa_summary <- function(shp_path) {
  year <- str_extract(basename(shp_path), "\\d{4}")
  cat("Processing file for year:", year, "\n")
  
  wwa_sf <- read_sf(shp_path)

  wwa_summary <- wwa_sf %>%
    st_drop_geometry() %>%
    mutate(
      index = row_number(),
      issue_date = as_date(ymd_hm(ISSUED)),
      year = year(issue_date),
      PHENOM = as.character(PHENOM), 
      CATEGORY = case_when(
        PHENOM %in% c("EH", "HT", "XH") ~ "HEAT", # https://github.com/akrherz/pyIEM/blob/main/src/pyiem/nws/vtec.py
        PHENOM %in% c("CW", "EC", "WC") ~ "COLD",
        PHENOM %in% c("BS", "HS", "LB", "LE", "SB", "SN", "SQ", "WS", "WW") ~ "SNOW",
        PHENOM %in% c("TO") ~ "TORN",
        PHENOM %in% c("CF", "FA", "FF", "FL", "LS", "SS", "TS") ~ "FLOOD",
        PHENOM %in% c("EW", "HF", "HU", "SS", "TR", "TY") ~ "HURR",
        PHENOM %in% c("FW") ~ "FIRE",
        TRUE ~ NA_character_
        ))  |> 
    distinct(WFO, CATEGORY, year, issue_date) |> 
    count(WFO, CATEGORY, year, name = "day_count")
  
  # Cleanup large objects
  rm(wwa_sf)
  gc()
  
  return(wwa_summary)
}

# Initialize results list
summary_list <- list()

# Loop through each file sequentially and store results
for (path in wwa_paths) {
  yearly_summary <- process_wwa_summary(path)
  if (!is.null(yearly_summary)) {
    summary_list[[length(summary_list) + 1]] <- yearly_summary
  }
  gc()
}

# Combine all joined results into a single dataframe
combined_summary <- bind_rows(summary_list)

# Aggregate final results
result_summary <- combined_summary %>%
  filter(!is.na(CATEGORY)) %>%
  group_by(WFO, CATEGORY) %>%
  summarise(day_count = sum(day_count), .groups = "drop") %>%
  pivot_wider(
    names_from = CATEGORY,
    values_from = day_count,
    values_fill = 0
  ) %>%
  mutate(across(-WFO, ~replace_na(.x, 0)))

# Save results
write_csv(result_summary, paste0(outputs, "base_cwa_alert_data.csv"))
