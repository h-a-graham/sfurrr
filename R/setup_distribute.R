setup_distribute <- function(x, y, t_dir, nwrkers, nchunks) {
  nr <- nrow(x)
  block <- nwrkers + 1 #ifelse(nwrkers == 1, nwrkers, nwrkers + 1)
  grp_size <- round(nr / block) / nchunks

  geoarrow::write_geoparquet(x, file.path(t_dir, "x.parquet"))
  geoarrow::write_geoparquet(y, file.path(t_dir, "y.parquet"))

  # get data ranges
  h1 <- round(c(seq(0, nr, grp_size), nr))
  df_ranges <-
    data.frame(a = h1[1:length(h1) - 1] + 1, b = h1[2:length(h1)])
  df_ranges <- split(df_ranges, 1:nrow(df_ranges))

  return(list(
    xpath = file.path(t_dir, "x.parquet"),
    ypath = file.path(t_dir, "y.parquet"),
    tab_ranges = df_ranges
  ))

}


geoarrow_filter <- function(data.path, rs, re){
  x <- arrow::open_dataset(data.path)
  x <- x[rs:re,]
  geoarrow::geoarrow_collect_sf(x)
}
