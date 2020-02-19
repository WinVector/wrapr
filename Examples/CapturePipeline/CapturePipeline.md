CapturePipeline.Rmd
================

Example showing how to build an object that captures pipeline stages,
instead of executing them.

``` r
library(wrapr)
source("CapturePipeline.R")
```

``` r
seq <- Collector() %.>% paste("a", .) %.>% paste(., "b")
f <- sequence_as_function(seq)
```

``` r
f("x")
```

    ## [1] "a x b"

``` r
"z" %.>% f
```

    ## [1] "a z b"
