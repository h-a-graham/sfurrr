## code to prepare `DATASET` dataset goes here

library(sf)
library(dplyr)
library(osmextract)
library(rmapshaper)
library(mapview)
mapviewOptions(fgb = FALSE)
#------------------------- Download and load data ------------------------------
if (!dir.exists('./data-raw/cache')) dir.create('./data-raw/cache')
# GB bundaries =================================================================
if (!file.exists("./data-raw/data/bdline_gb.gpkg")) {
  download.file("https://api.os.uk/downloads/v1/products/BoundaryLine/downloads?area=GB&format=GeoPackage&redirect",
                destfile = 'cache/boundaries.zip')
  unz <- unzip('cache/boundaries.zip')
}

gb_counties <- read_sf("./data-raw/data/bdline_gb.gpkg",
                         layer='boundary_line_ceremonial_counties') %>%
  rmapshaper::ms_simplify(keep=0.05)

mapview(gb_counties)

usethis::use_data(gb_counties, overwrite = TRUE)

# ENG cycle ways ===============================================================
if (!file.exists('./data-raw/cache/cycleways_england.rds')){
  options(timeout = max(1200, getOption("timeout")))
  cycleways_england = oe_get(
    "England",
    force_download = TRUE,
    quiet = FALSE,
    query = "SELECT * FROM 'lines' WHERE highway = 'cycleway'"
  ) %>%
    st_transform(st_crs(gb_counties)) %>%
    select(osm_id,highway)

  saveRDS(cycleways_england, './data-raw/cache/cycleways_england.rds')
} else {
  cycleways_england <- readRDS('./data-raw/cache/cycleways_england.rds') %>%
    st_transform(st_crs(gb_counties)) %>%
    select(osm_id,highway)
}

usethis::use_data(cycleways_england, overwrite = TRUE)
