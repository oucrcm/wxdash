library(rgdal)
library(tidyverse)

downloads <- "/Users/josephripberger/Dropbox (Univ. of Oklahoma)/Severe Weather and Society Dashboard/local files/downloads/" # define locally!!!
outputs <- "/Users/josephripberger/Dropbox (Univ. of Oklahoma)/Severe Weather and Society Dashboard/local files/outputs/" # define locally!!!

map <- readOGR("outputs/cwa_estimates")
map@data <- map@data %>%
  mutate_at(vars(RECEP:DROUGHT), funs(pnorm(.) * 100)) # convert z-scores to percentile ranks
map$CWA <- as.character(map$CWA)

fips_map <- readOGR("outputs/county_estimates")
fips_map@data <- fips_map@data %>%
  mutate_at(vars(RECEP:DROUGHT), funs(pnorm(.) * 100)) # convert z-scores to percentile ranks
fips_map$CWA <- as.character(fips_map$CWA)

survey_data <- read.csv("outputs/base_survey_data.csv")
psych::describe(survey_data)

# Write CSV files for MDL Beta
write_csv(map, "")


st_write(cnty_shp, paste0(outputs, "county_estimates"), "county_estimates", driver = "ESRI Shapefile", append = FALSE)
