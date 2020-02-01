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

    ## [1] "Sat Feb  1 11:44:15 2020"

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

    ## [1] "/var/folders/7q/h_jp2vj131g5799gfnpzhdp80000gn/T//RtmpJAkOIX"

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

    ## cdata_1.1.6 started at 2020-02-01 11:44:18 success at 2020-02-01 11:45:01 (1/0/0) 
    ## RcppDynProg_0.1.3 started at 2020-02-01 11:45:01 success at 2020-02-01 11:46:35 (2/0/0) 
    ## rqdatatable_1.2.6 started at 2020-02-01 11:46:36 success at 2020-02-01 11:47:03 (3/0/0) 
    ## rquery_1.4.3 started at 2020-02-01 11:47:03 success at 2020-02-01 11:47:44 (4/0/0) 
    ## seplyr_0.8.5 started at 2020-02-01 11:47:44 success at 2020-02-01 11:48:10 (5/0/0) 
    ## sigr_1.0.6 started at 2020-02-01 11:48:10 success at 2020-02-01 11:48:31 (6/0/0) 
    ## vtreat_1.5.1 started at 2020-02-01 11:48:31 success at 2020-02-01 11:49:41 (7/0/0) 
    ## WVPlots_1.2.3 started at 2020-02-01 11:49:41 success at 2020-02-01 11:50:57 (8/0/0)

    ## [1] id     title  status
    ## <0 rows> (or 0-length row.names)

``` r
summariseQueue(package=package, directory=td)
```

    ## Test of wrapr had 8 successes, 0 failures, and 0 skipped packages. 
    ## Ran from 2020-02-01 11:44:18 to 2020-02-01 11:50:57 for 6.65 mins 
    ## Average of 49.875 secs relative to 49.856 secs using 1 runners
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
