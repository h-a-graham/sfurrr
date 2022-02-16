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
#' as \link{st_intersects}; see details
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
#' @param ncores Integer many processes do you want to use. Default is all
#' available cores
#' @param nchunks The number of chunks to run on each core. Default is 1.
#' Can improve or worsen performance depending on dataset size and number of cores
#'
#' @details See \link[sf]{st_join} for detais regarding join types.
#'
#' @return an object of class \code{sf}, joined based on geometry
#' @export
future_st_join.sf <- function(x, y, join=st_intersects, ...,
                              suffix = c(".x", ".y"), largest=FALSE,
                              left = TRUE,  ncores = parallel::detectCores(),
                              nchunks=1, .progress=FALSE){
  nr <- nrow(x)
  grp_size <- round(nr/ncores+1)/nchunks
  future::plan(multisession, workers = ncores)

  sf.join <- split(x, as.integer(gl(nr, grp_size, nr))) %>%
    furrr::future_map(.,
                      .f = function(.x, ...)
                        {sf:::st_join.sf(x=.x, y=y, join=join, ...,
                                         suffix=suffix, left=left,
                                         largest=largest)}
      , ..., .progress = .progress) %>%
    do.call(rbind, .)

  future:::ClusterRegistry("stop")
  return(sf.join)
}


#' @export
#' @name future_st_join
future_st_filter = function(x, y, ...) UseMethod("future_st_filter")

#' @export
#' @name future_st_join
#' @param .predicate geometry predicate function with the same profile as \link{st_intersects}; see details
future_st_filter.sf = function(x, y, ..., .predicate = st_intersects,
                               ncores = parallel::detectCores(), n_chunks=1,
                               .progress=FALSE) {
  if (!requireNamespace("dplyr", quietly = TRUE))
    stop("dplyr is not installed: install first?")

  nr <- nrow(x)
  grp_size <- round(nr/ncores+1)/n_chunks
  future::plan(multisession, workers = ncores)

  sf.filt <- split(x, as.integer(gl(nr, grp_size, nr))) %>%
    furrr::future_map(., .f = function(.x, ...){
      dplyr::filter(.x, lengths(.predicate(.x, y, ...)) > 0)}, ...,
      .progress=.progress) %>%
    do.call(rbind, .)

  future:::ClusterRegistry("stop")
  return(sf.filt)
}
