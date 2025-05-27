library(tidyverse)
library(sf)
library(data.table)
library(lubridate)

# wwa shapefiles -----------------
wwa_paths <- list.files("~/Univ. of Oklahoma Dropbox/Joe Ripberger/nws_product_climatology/wwa_data/", full.names = TRUE, pattern = "_all")

# function to process shapefiles -----------------
process_wwa_summary <- function(shp_path) {
  year <- str_extract(basename(shp_path), "\\d{4}")
  cat("Processing file for year:", year, "\n")
  
  wwa_sf <- read_sf(shp_path)

  wwa_summary <- wwa_sf %>%
    st_drop_geometry() %>%
    mutate(
      index = row_number(),
      issue_date = as_date(ymd_hm(ISSUED)),
      year = year,
      PHENOM = as.character(PHENOM))  |> 
    distinct(WFO, PHENOM, issue_date) |> 
    count(WFO, PHENOM, name = "day_count")
  
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
  group_by(WFO, PHENOM) %>%
  summarise(day_count = sum(day_count), .groups = "drop") %>%
  pivot_wider(
    names_from = PHENOM,
    values_from = day_count,
    values_fill = 0
  ) %>%
  mutate(across(-WFO, ~replace_na(.x, 0)))

# Save results
write_csv(result_summary, "~/Univ. of Oklahoma Dropbox/Joe Ripberger/nws_product_climatology/wwa_data/wwa_shiny_app/data/wwa_cwa_counts.csv")
