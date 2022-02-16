sfurrr
================

An experimental R package to parallelise some functions from the
excellent {sf} package using also brilliant {furrr} package. Right now
it’s just parallel versions of `st_join` and `st_filter`. They won’t
always help and will often be slower but sometimes it might be useful.
This is just messing about right now tbh. The code is deliberately
copied from {sf} and {furrr} so that it can be used as basically a drop
in replacement.

``` r
message('We need examples!?')
```

    ## We need examples!?
