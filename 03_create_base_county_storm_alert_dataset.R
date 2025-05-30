library(tidyverse)
library(sf)
library(data.table)
library(lubridate)
sf_use_s2(FALSE)

downloads <- "/Users/josephripberger/Dropbox (Univ. of Oklahoma)/Severe Weather and Society Dashboard/local files/downloads/" # define locally!!!
outputs <- "/Users/josephripberger/Dropbox (Univ. of Oklahoma)/Severe Weather and Society Dashboard/local files/outputs/" # define locally!!!

# Import Shapefiles -----------------------------
wwa_paths <- list.files(downloads, full.names = TRUE, pattern = "_all") # source: https://mesonet.agron.iastate.edu/request/gis/watchwarn.phtml 
cnty_shp <- st_read(paste0(downloads, "cb_2023_us_county_20m")) |>
  st_transform(crs = 5070)

# Function to Process Shapefiles -----------------
process_wwa_year <- function(shp_path) {
  year <- str_extract(basename(shp_path), "\\d{4}")
  cat("Processing file for year:", year, "\n")
  
  wwa_sf <- read_sf(shp_path) |>
    st_transform(crs = st_crs(cnty_shp))
  
  intersections <- st_intersects(cnty_shp, wwa_sf)
  names(intersections) <- cnty_shp$GEOID
  
  overlaps <- map2_dfr(intersections, names(intersections), ~ {
    if (length(.x) == 0) return(NULL)
    tibble(GEOID = .y, wwa_index = .x)
  })
  
  if (nrow(overlaps) == 0) return(NULL)
  
  wwa_data <- st_drop_geometry(wwa_sf) |>
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
        )) |>
    select(index, issue_date, CATEGORY, year)
  
  joined <- left_join(overlaps, wwa_data, by = c("wwa_index" = "index")) |>
    distinct(GEOID, CATEGORY, issue_date, year)
  
  # Cleanup large objects
  rm(wwa_sf, intersections, overlaps, wwa_data)
  gc()
  
  return(joined)
}

# Initialize results list
all_joined <- list()

# Loop through each file sequentially and store only joined results
for (path in wwa_paths) {
  joined_result <- process_wwa_year(path)
  if (!is.null(joined_result)) {
    all_joined[[length(all_joined) + 1]] <- joined_result
  }
  gc()  # extra cleanup between iterations
}

# Combine all joined results into a single dataframe
combined_summary <- bind_rows(all_joined)

# Aggregate final results
result_summary <- cnty_shp %>%
  st_drop_geometry() %>%
  select(GEOID, NAME, NAMELSAD, STUSPS, STATE_NAME) %>%
  left_join(
    combined_summary %>%
      drop_na(CATEGORY) %>%
      count(GEOID, CATEGORY, name = "day_count") %>%
      pivot_wider(
        names_from = CATEGORY,
        values_from = day_count,
        values_fill = 0
      ),
    by = "GEOID"
  ) %>%
  mutate(across(-c(GEOID, NAME, NAMELSAD, STUSPS, STATE_NAME), ~replace_na(.x, 0)))

# Save results
write_csv(result_summary, paste0(outputs, "base_county_alert_data.csv"))
