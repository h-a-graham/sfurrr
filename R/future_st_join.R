#' Parallel implementations of `sf::st_join` and `sf::st_filter`
#'
#' Where the first sf object object (x) is split into chunks and joined with /
#' filtered by the second (y). This will work best when x is large and y is
#' small. It won't be kind to your machine's memory.
#'
#' @name future_st_join
#' @export
future_st_join = function(x, y, join, ...) UseMethod("future_st_join")

#' @name future_st_join
#' @param x object of class \code{sf}
#' @param y object of class \code{sf}
#' @param join geometry predicate function with the same profile
#' as \link[sf]{st_intersects}; see details
#' @param ... for \code{st_join}: arguments passed on to the \code{join}
#' function or to \code{st_intersection} when \code{largest} is \code{TRUE};
#' for \code{st_filter} arguments passed on to the \code{.predicate} function,
#' e.g. \code{prepared}, or a pattern for \link{st_relate}
#' @param suffix length 2 character vector; see \link[base]{merge}
#' @param largest logical; if \code{TRUE}, return \code{x} features augmented
#' with the fields of \code{y} that have the largest overlap with each of the
#' features of \code{x}; see https://github.com/r-spatial/sf/issues/578
#' @param left left logical; if \code{TRUE} return the left join, otherwise
#' an inner join; see details.
#' see also \link[dplyr:mutate-joins]{left_join}
#' @param nchunks The number of chunks to run on each core. Default is 1.
#' Can improve or worsen performance depending on dataset size and number of cores
#' @param .progress Show progress bar. Only useful when nchunks > 1. default is FALSE
#'
#' @details See \link[sf]{st_join} for detais regarding join types.
#'
#' @return an object of class \code{sf}, joined based on geometry
#' @export
#' @import future sf
#' @examples
#' plan(multisession, workers = 2)
#'
#' future_st_join(cycleways_england(), gb_counties())
future_st_join.sf <- function(x, y, join=st_intersects, ...,
                              suffix = c(".x", ".y"), largest=FALSE,
                              left = TRUE, nchunks=1, .progress=FALSE){

  dist.list <- setup_distribute(x, y, tempdir(), future::nbrOfWorkers(), nchunks)

  #run parallel join
  sf.join <- dist.list$tab_ranges |>
    furrr::future_map(.f = function(.x, ...){

      x1 <- geoarrow_filter(dist.list$xpath,
                            rs=.x$a,
                            re=.x$b)

      y1 <- geoarrow::read_geoparquet_sf(dist.list$ypath)

      sf::st_join(x=x1, y=y1, join=join, ...,
                      suffix=suffix, left=left,
                      largest=largest)
    }, ..., .progress = .progress, .options = furrr::furrr_options(seed = TRUE)) |>
    purrr::list_rbind() |>
    sf::st_as_sf()

  return(sf.join)

}


#' @export
#' @name future_st_join
future_st_filter = function(x, y, ...) UseMethod("future_st_filter")

#' @export
#' @name future_st_join
#' @param .predicate geometry predicate function with the same profile as \link[sf]{st_intersects}; see details
#' @import future sf
#' @examples
#' plan(multisession, workers = 2)
#' future_st_filter(cycleways_england(), gb_counties()[1:50,])
#'
future_st_filter.sf = function(x, y, ..., .predicate = st_intersects,
                               nchunks=1, .progress=FALSE) {

  dist.list <- setup_distribute(x, y, tempdir(), future::nbrOfWorkers(), nchunks)

  sf.filt <- dist.list$tab_ranges |>
    furrr::future_map(.f = function(.x, ...){

      x1 <- geoarrow_filter(dist.list$xpath, rs=.x$a, re=.x$b)
      y1 <- geoarrow::read_geoparquet_sf(dist.list$ypath)
      x1[lengths(.predicate(x1, y1, ...)) > 0,]

      }, ..., .progress=.progress, .options = furrr::furrr_options(seed = TRUE)) |>
    purrr::list_rbind() |>
    sf::st_as_sf()

  return(sf.filt)

}




