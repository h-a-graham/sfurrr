#' GB Counties
#'
#' An sf object with GB counties from Ordance survey:
#'
#'
#' @format A sf object with 91 rows and 3 variables (including geometry:
#' \describe{
#'   \item{Name}{County Name}
#'   \item{Area_Description}{county type (all the same)}
#'   \item{geometry}{MULTIPOLYGON geometry}
#' }
#' @source \url{https://osdatahub.os.uk/downloads/open/BoundaryLine}
"gb_counties"

#' Cycleways for England
#'
#' An sf object with All cyclways for England downloaded using the example
#' from the {osmextract package}:
#'
#'
#' @format A sf object with 105184 rows and 3 variables (including geometry:
#' \describe{
#'   \item{Name}{County Name}
#'   \item{Area_Description}{county type (all the same)}
#'   \item{geometry}{LINESTRING geometry}
#' }
#' @source \url{https://docs.ropensci.org/osmextract/}
"cycleways_england"

