
#' Reads the example GB counties data
#'
#' @return an sf object of all GB counties
#' @export
#' @details
#' Downloaded from: "https://api.os.uk/downloads/v1/products/BoundaryLine/downloads?area=GB&format=GeoPackage&redirect"
#'
#' @examples
#' gb_counties()
gb_counties <- function(){
  geoarrow::read_geoparquet_sf(system.file("gb_counties/gb_counties.parquet", package = "sfurrr"))
}


#' Reads the example England Cycle Network data
#'
#' @return an sf object of the English cycle network
#' @export
#' @details
#' Downloaded from Open Strem Map using the {osmextract} package https://docs.ropensci.org/osmextract/
#' @examples
#' cycleways_england()
cycleways_england <- function(){
  geoarrow::read_geoparquet_sf(system.file("cycleways_england/cycleways_england.parquet", package = "sfurrr"))

}
