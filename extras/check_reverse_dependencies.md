check\_reverse\_dependencies
================

``` r
library("prrd")
td <- tempdir()
package = "wrapr"
packageVersion(package)
```

    ## [1] '1.9.7'

``` r
date()
```

    ## [1] "Thu Jan 30 18:59:06 2020"

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

    ## [1] "/var/folders/7q/h_jp2vj131g5799gfnpzhdp80000gn/T//RtmppzqunI"

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
    ## 3  3 rqdatatable  READY
    ## 4  4      rquery  READY
    ## 5  5      seplyr  READY
    ## 6  6        sigr  READY
    ## 7  7      vtreat  READY
    ## 8  8     WVPlots  READY

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

    ## cdata_1.1.5 started at 2020-01-30 18:59:08 success at 2020-01-30 18:59:44 (1/0/0) 
    ## RcppDynProg_0.1.3 started at 2020-01-30 18:59:44 success at 2020-01-30 19:01:17 (2/0/0) 
    ## rqdatatable_1.2.6 started at 2020-01-30 19:01:17 success at 2020-01-30 19:01:46 (3/0/0) 
    ## rquery_1.4.2 started at 2020-01-30 19:01:46 success at 2020-01-30 19:02:35 (4/0/0) 
    ## seplyr_0.8.5 started at 2020-01-30 19:02:35 success at 2020-01-30 19:03:08 (5/0/0) 
    ## sigr_1.0.6 started at 2020-01-30 19:03:08 success at 2020-01-30 19:03:31 (6/0/0) 
    ## vtreat_1.5.1 started at 2020-01-30 19:03:31 success at 2020-01-30 19:04:56 (7/0/0) 
    ## WVPlots_1.2.3 started at 2020-01-30 19:04:56 success at 2020-01-30 19:06:24 (8/0/0)

    ## [1] id     title  status
    ## <0 rows> (or 0-length row.names)

``` r
summariseQueue(package=package, directory=td)
```

    ## Test of wrapr had 8 successes, 0 failures, and 0 skipped packages. 
    ## Ran from 2020-01-30 18:59:08 to 2020-01-30 19:06:24 for 7.267 mins 
    ## Average of 54.5 secs relative to 54.413 secs using 1 runners
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
