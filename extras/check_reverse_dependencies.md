check\_reverse\_dependencies
================

``` r
library("prrd")
td <- tempdir()
package = "wrapr"
packageVersion(package)
```

    ## [1] '1.9.3'

``` r
date()
```

    ## [1] "Sun Nov  3 06:28:56 2019"

``` r
parallelCluster <- NULL
ncores <- 0
# # parallel doesn't work due to https://github.com/r-lib/liteq/issues/22
# ncores <- parallel::detectCores()
# parallelCluster <- parallel::makeCluster(ncores)

orig_dir <- getwd()
print(orig_dir)
```

    ## [1] "/Users/johnmount/Documents/work/wrapr/extras"

``` r
setwd(td)
print(td)
```

    ## [1] "/var/folders/7q/h_jp2vj131g5799gfnpzhdp80000gn/T//RtmpIwPPlG"

``` r
options(repos = c(CRAN="https://cloud.r-project.org"))
jobsdfe <- enqueueJobs(package=package, directory=td)

print("checking:")
```

    ## [1] "checking:"

``` r
print(jobsdfe)
```

    ##   id       title status
    ## 1  1       cdata  READY
    ## 2  2 RcppDynProg  READY
    ## 3  3      replyr  READY
    ## 4  4 rqdatatable  READY
    ## 5  5      rquery  READY
    ## 6  6      seplyr  READY
    ## 7  7        sigr  READY
    ## 8  8      vtreat  READY
    ## 9  9     WVPlots  READY

``` r
mk_fn <- function(package, directory) {
  force(package)
  force(directory)
  function(i) {
    library("prrd")
    setwd(directory)
    Sys.sleep(1*i)
    dequeueJobs(package=package, directory=directory)
  }
}
f <- mk_fn(package=package, directory=td)

if(!is.null(parallelCluster)) {
  parallel::parLapply(parallelCluster, seq_len(ncores), f)
} else {
  f(0)
}
```

    ## cdata_1.1.3 started at 2019-11-03 06:28:58 success at 2019-11-03 06:29:35 (1/0/0) 
    ## RcppDynProg_0.1.3 started at 2019-11-03 06:29:35 success at 2019-11-03 06:31:01 (2/0/0) 
    ## replyr_1.0.5 started at 2019-11-03 06:31:02 success at 2019-11-03 06:31:39 (3/0/0) 
    ## rqdatatable_1.2.3 started at 2019-11-03 06:31:39 success at 2019-11-03 06:32:12 (4/0/0) 
    ## rquery_1.3.9 started at 2019-11-03 06:32:12 success at 2019-11-03 06:33:14 (5/0/0) 
    ## seplyr_0.8.4 started at 2019-11-03 06:33:14 success at 2019-11-03 06:33:45 (6/0/0) 
    ## sigr_1.0.6 started at 2019-11-03 06:33:45 success at 2019-11-03 06:34:12 (7/0/0) 
    ## vtreat_1.4.7 started at 2019-11-03 06:34:12 success at 2019-11-03 06:35:30 (8/0/0) 
    ## WVPlots_1.2.1 started at 2019-11-03 06:35:30 success at 2019-11-03 06:36:43 (9/0/0)

    ## [1] id     title  status
    ## <0 rows> (or 0-length row.names)

``` r
summariseQueue(package=package, directory=td)
```

    ## Test of wrapr had 9 successes, 0 failures, and 0 skipped packages. 
    ## Ran from 2019-11-03 06:28:58 to 2019-11-03 06:36:43 for 7.75 mins 
    ## Average of 51.667 secs relative to 51.608 secs using 1 runners
    ## 
    ## Failed packages:   
    ## 
    ## Skipped packages:   
    ## 
    ## None still working
    ## 
    ## None still scheduled

``` r
setwd(orig_dir)
if(!is.null(parallelCluster)) {
  parallel::stopCluster(parallelCluster)
}
```
