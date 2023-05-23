sfurrr
================

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/h-a-graham/sfurrr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/h-a-graham/sfurrr/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

An experimental R package to parallelise some functions from the
excellent [{sf}](https://r-spatial.github.io/sf/) and
[{rmapshaper}](https://github.com/ateucher/rmapshaper) packages using
the also brilliant [{furrr}](https://github.com/DavisVaughan/furrr)
package. Right now, it’s just parallel versions of [`st_join` and
`st_filter`](https://r-spatial.github.io/sf/reference/st_join.html) from
{sf} and
[`ms_simplify`](https://github.com/ateucher/rmapshaper/blob/master/R/simplify.R)
from {rmapshaper} (Although this now seems essentially redundant after
some major performance improvements in rmapshaper). They won’t always
help and may be slower but, sometimes it might be useful. This is just
messing about right now tbh. The code is deliberately copied from {sf},
{rmapshaper} and {furrr} so that it can be used as a drop in
replacement.

I’ve added [{geoarrow}](https://github.com/paleolimbot/geoarrow) as a
dependency to play with using it to pass data between cores - it seems
to be fractionally faster and solves the issue of passing objects larer
than the limit allowed by furrr… make sure to install {arrow} if you
want to try this out.

*Make sure to experiment with the number of cores - often it will be
much more efficient to use a small number of processes than all of your
machines’s availabel processes due to the start up time of those
processes*.

## Install

    # install.packages("remotes")
    remotes::install_github('h-a-graham/sfurrr')

## Examples…

So here is the some data included in the package - it is the English
cycle network from Open Street Map (downloaded with
[{osmextract}](https://docs.ropensci.org/osmextract/)) and British
counties from [Ordnance
survey](https://osdatahub.os.uk/downloads/open/BoundaryLine).

``` r
library(sfurrr)

#built in functions to load the data.
cwe <- cycleways_england()
gbc <-  gb_counties()

basetheme::basetheme("dark") # makes it pretty

plot(gbc['geometry'],  axes = TRUE)
plot(cwe['geometry'], add=TRUE, col='#39C17360')
```

![](man/figures/show_test_data-1.png)<!-- -->

``` r
summary(cwe)
```

    ##     osm_id            highway                   geometry     
    ##  Length:120451      Length:120451      LINESTRING   :120451  
    ##  Class :character   Class :character   epsg:27700   :     0  
    ##  Mode  :character   Mode  :character   +proj=tmer...:     0

``` r
summary(gbc)
```

    ##      Name           Area_Description            geometry 
    ##  Length:91          Length:91          MULTIPOLYGON :91  
    ##  Class :character   Class :character   epsg:27700   : 0  
    ##  Mode  :character   Mode  :character   +proj=tmer...: 0

### Spatial Joins

Now, let’s say we want to do a spatial join between the cycleways and
the counties so we attach the county data to the cycleway network. This
might allow us to do some summarised stats on the cycle network of
different counties, for example.

So let’s do this with {sf} which is loaded by default with {sfurr}.
Let’s also get some timings with {tictoc}

``` r
library(tictoc)

tic()
join.sf <-  st_join(cwe,
                  gbc)
toc()
```

    ## 6.165 sec elapsed

``` r
plan(multisession, workers = 4)

tic()
join.sfurr <-  future_st_join(cwe,
                  gbc)
toc()
```

    ## 4.45 sec elapsed

Okay.. so {sf} is actually pretty fast! by using a small number of
cores - here 4, we can get a slight speed up - any more cores and it
would be increasingly slow. But what about more costly spatial
operations? Let’s try now with the option `largest=TRUE` which joins
based on the largest amount of intersection.

``` r
# ------------ `st_join` ----------------
tic()
joinL.sf <-  st_join(cwe,
                  gbc, largest=TRUE)
toc()
```

    ## 91.203 sec elapsed

``` r
# ------------ `future_st_join` ----------------
plan(multisession, workers = 8)

tic()
joinL.sfurr <-  future_st_join(cwe,
                  gbc, largest=TRUE)
toc()
```

    ## 25.912 sec elapsed

Okay so now we see that going parallel does indeed offer some potential
uses when using a costly spatial function. Here we use 8 processes and
it pays off more due to the expensive computation.

### Spatial filtering

Once again, here is a comparison of the simplest approach with the
`st_intersect` spatial predicate.

``` r
# ------------ `st_fiter` ----------------
tic()
filt_t1 <- st_filter(cwe['highway'],
                     gbc[1:50,])
toc()
```

    ## 5.472 sec elapsed

``` r
# ----------- `future_st_filter` -----------------
plan(multisession, workers = 4)
tic()
filt_t2 <- future_st_filter(cwe['highway'],
                            gbc[1:50,])
toc()
```

    ## 4.28 sec elapsed

Again with using a limited number of cores there is a small speed up but
not that much… Let’s use the `st_within` spatial predicate to filter out
cycleways that are not located entirely within the county areas… This is
kind of pointless for this use case and is just illustrative really…

``` r
# ------------ `st_filter` ----------------
tic()
within_filt_t1 <- st_filter(joinL.sfurr,
                     gbc[1:50,], .predicate = st_within)
toc()
```

    ## 68.937 sec elapsed

``` r
# ----------- `future_st_filter` -----------------
plan(multisession, workers = 6)

tic()
within_filt_t2 <- future_st_filter(joinL.sfurr,
                            gbc[1:50,], .predicate = st_within)
toc()
```

    ## 23.74 sec elapsed

Cool, so in this case it is faster!

## Summary

This is not a globally useful idea but in some cases, when using very
large spatial datasets, you may get a speed up by running spatial
filters/joins in parallel. Any speed up will depend on the number of
processes you can run; rememeber it is probably not wise to use all the
cores at your disposal - sometimes less is more!

``` r
plot(gbc['geometry'],  axes = TRUE)
plot(within_filt_t2['Name'], add=TRUE)
```

![](man/figures/final_plot-1.png)<!-- -->
