
#' Parallel implementation of `rmapshaper::ms_simplify`
#'
#' simply splits up an sf and runs the simplify algorithm on each chunk.
#' Uses \href{https://github.com/mbloch/mapshaper}{mapshaper} to simplify
#' polygons. Currently only supports sf objects.
#'
#' @param x spatial object to simplify. One of:
#' \itemize{
#'  \item \code{sf} or \code{sfc} polygons or lines object
#'  }
#' @param keep proportion of points to retain (0-1; default 0.05)
#' @param method simplification method to use: \code{"vis"} for Visvalingam
#'   algorithm, or \code{"dp"} for Douglas-Peuker algorithm. If left as
#'   \code{NULL} (default), uses Visvalingam simplification but modifies the
#'   area metric by underweighting the effective area of points at the vertex of
#'   more acute angles, resulting in a smoother appearance. See this
#'   \url{https://github.com/mbloch/mapshaper/wiki/Simplification-Tips}{link}
#'   for more information.
#' @param weighting Coefficient for weighting Visvalingam simplification
#' (default is 0.7). Higher values produce smoother output. weighting=0 is
#' equivalent to unweighted Visvalingam simplification.
#' @param keep_shapes Prevent small polygon features from disappearing at high
#'   simplification (default \code{FALSE})
#' @param no_repair disable intersection repair after simplification (default
#'   \code{FALSE}).
#' @param snap Snap together vertices within a small distance threshold to fix
#'   small coordinate misalignment in adjacent polygons. Default \code{TRUE}.
#' @param explode Should multipart polygons be converted to singlepart polygons?
#'   This prevents small shapes from disappearing during simplification if
#'   \code{keep_shapes = TRUE}. Default \code{FALSE}
#' @param drop_null_geometries should Features with null geometries be dropped?
#'   Ignored for \code{Spatial*} objects, as it is always \code{TRUE}.
#' @param snap_interval Specify snapping distance in source units, must be a
#'   numeric. Default \code{NULL}
#' @param ncores Integer many processes do you want to use. Default is all
#' available cores
#' @param nchunks The number of chunks to run on each core. Default is 1.
#' Can improve or worsen performance depending on dataset size and number of cores
#' @param .progress Show progress bar. Only useful when nchunks > 1. default is FALSE
#'
#' @return a simplified representation of the geometry in the same class as the
#'   input

future_simplify <- function(x, keep = 0.05,
                            method = NULL,
                            weighting = 0.7,
                            keep_shapes = FALSE,
                            no_repair = FALSE,
                            snap = TRUE,
                            explode = FALSE,
                            force_FC = TRUE,
                            drop_null_geometries = TRUE,
                            snap_interval = NULL,
                            ncores = parallel::detectCores(),
                            nchunks=1, .progress=FALSE) {
  nr <- nrow(x)
  grp_size <- round(nr/ncores+1)/nchunks
  future::plan(future::multisession, workers = ncores)

  # write temp paraquet files. perhaps fractionally faster than passing across cores
  t_dir <- tempdir()
  geoarrow::write_geoarrow_parquet(x, file.path(t_dir, "x.parquet"))

  # get data ranges
  h1 <- round(c(seq(0,nr, grp_size), nr))
  df_ranges <- data.frame(a=h1[1:length(h1)-1]+1, b=h1[2:length(h1)])
  df_ranges <- split(df_ranges, 1:nrow(df_ranges))

  sf.simp <- df_ranges %>%
    furrr::future_map(.f = function(.x){

      x1 <- geoarrow::read_geoarrow_parquet_sf(file.path(t_dir, "x.parquet"))
      x1 <- x1[.x$a:.x$b,]

      rmapshaper::ms_simplify(x1, keep=keep, method=method, weighting = weighting,
                              keep_shapes=keep_shapes, no_repair=no_repair,
                              snap=snap, explode=explode, drop_null_geometries=drop_null_geometries,
                              snap_interval=snap_interval)
    }, .progress = .progress)

  future:::ClusterRegistry("stop")

  #merge and clean up
  j <- do.call(rbind, sf.simp)
  rownames(j)<-NULL
  j

}
