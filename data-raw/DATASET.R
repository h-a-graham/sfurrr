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
  options(timeout = max(1200, getOption("timeout")))
  download.file("https://api.os.uk/downloads/v1/products/BoundaryLine/downloads?area=GB&format=GeoPackage&redirect",
                destfile = 'data-raw/cache/boundaries.zip')
  unz <- unzip('data-raw/cache/boundaries.zip', exdir = "data-raw/cache")
}

gb_counties <- read_sf("./data-raw/cache/Data/bdline_gb.gpkg",
                         layer='boundary_line_ceremonial_counties') %>%
  rmapshaper::ms_simplify(keep=0.05)

mapview(gb_counties)

if(!dir.exists("inst/gb_counties")) dir.create("inst/gb_counties", recursive = TRUE)

geoarrow::write_geoparquet(gb_counties, "inst/gb_counties/gb_counties.parquet")
# usethis::use_data(gb_counties, overwrite = TRUE)

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

# usethis::use_data(cycleways_england, overwrite = TRUE)


if(!dir.exists("inst/cycleways_england")) dir.create("inst/cycleways_england", recursive = TRUE)

# write_sf(cycleways_england, "inst/cycleways_england/cycleways_england.shp")
geoarrow::write_geoparquet(cycleways_england, "inst/cycleways_england/cycleways_england.parquet")


