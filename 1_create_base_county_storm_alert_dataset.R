rm(list = ls())

library(tidyverse)
library(sf)
library(data.table)
library(lubridate)

sf_use_s2(FALSE)

# county shapefile -----------------
cwa_cnty_shp <- st_read("~/Dropbox (Univ. of Oklahoma)/Severe Weather and Society Dashboard/local files/downloads/c_18mr25") |>
  st_transform(crs = 5070)
cnty_shp <- st_read("~/Univ. of Oklahoma Dropbox/Joe Ripberger/nws_product_climatology/wwa_data/wwa_shiny_app/data/cb_2023_us_county_500k") |>
  st_transform(crs = 5070)

cwa_cnty_shp |> filter(!FIPS %in% cnty_shp$GEOID) |> print(n = Inf) # includes Connecticut counties, Palau, Marshall Islands, Micronesia
cnty_shp |> filter(!GEOID %in% cwa_cnty_shp$FIPS) |> print(n = Inf) # missing Connecticut planning regions, Kalawao County Hawaii
cwa_cnty_shp %>%
  group_by(FIPS) %>%
  filter(n_distinct(CWA) > 1) %>%
  ungroup() # TODO: need to reassign MFL to KEY so that all parts of Monroe County are in KEY

# wwa shapefiles -----------------
wwa_paths <- list.files("~/Univ. of Oklahoma Dropbox/Joe Ripberger/nws_product_climatology/wwa_data/", full.names = TRUE, pattern = "_all")

# function to process shapefiles -----------------
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
      year = year,
      PHENOM = as.character(PHENOM)) |>
    select(index, issue_date, PHENOM, year)
  
  joined <- left_join(overlaps, wwa_data, by = c("wwa_index" = "index")) |>
    distinct(GEOID, PHENOM, issue_date, year)
  
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
yearly_results <- bind_rows(all_joined)

# Aggregate final results
result_summary <- cnty_shp %>%
  st_drop_geometry() %>%
  select(GEOID, NAME, NAMELSAD, STUSPS, STATE_NAME) %>%
  left_join(
    yearly_results %>%
      count(GEOID, PHENOM, name = "day_count") %>%
      pivot_wider(
        names_from = PHENOM,
        values_from = day_count,
        values_fill = 0
      ),
    by = "GEOID"
  ) %>%
  mutate(across(-c(GEOID, NAME, NAMELSAD, STUSPS, STATE_NAME), ~replace_na(.x, 0)))

# Save results
write_csv(result_summary, "~/Univ. of Oklahoma Dropbox/Joe Ripberger/nws_product_climatology/wwa_data/wwa_shiny_app/data/wwa_county_counts.csv")
