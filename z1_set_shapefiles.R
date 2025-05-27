library(data.table)
library(tidyverse)
library(sf)
library(plotly)

downloads <- "/Users/josephripberger/Dropbox (Univ. of Oklahoma)/Severe Weather and Society Dashboard/local files/downloads/" # define locally!!!
outputs <- "/Users/josephripberger/Dropbox (Univ. of Oklahoma)/Severe Weather and Society Dashboard/local files/outputs/" # define locally!!!

# shapefiles -----------------
cwa_shp <- st_read("~/Dropbox (Univ. of Oklahoma)/Severe Weather and Society Dashboard/local files/downloads/w_18mr25/") |> st_transform(crs = 5070)
cwa_cnty_shp <- st_read("~/Dropbox (Univ. of Oklahoma)/Severe Weather and Society Dashboard/local files/downloads/c_18mr25") |> st_transform(crs = 5070)
cnty_shp <- st_read("~/Univ. of Oklahoma Dropbox/Joe Ripberger/nws_product_climatology/wwa_data/wwa_shiny_app/data/cb_2023_us_county_500k") |> st_transform(crs = 5070)

# plot ct -----------------
p <- ggplot(cwa_shp) +
  geom_sf(data = cwa_shp |> filter(CWA %in% c("BOX", "ALY", "OKX")), aes(fill = CWA)) + 
  # geom_sf(data = cwa_cnty_shp |> filter(STATE == "CT"), fill = "blue", color = "blue", alpha = 0.2) + 
  geom_sf(data = cnty_shp |> filter(STATEFP == "09"), fill = "white", color = "red", alpha = 0.2)
ggplotly(p)
