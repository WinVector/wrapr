check\_reverse\_dependencies
================

``` r
library("prrd")
td <- tempdir()
package = "wrapr"
packageVersion(package)
```

    ## [1] '1.8.9'

``` r
date()
```

    ## [1] "Wed Jul 24 07:47:02 2019"

``` r
parallelCluster <- NULL
# # parallel doesn't work due to https://github.com/r-lib/liteq/issues/22
#ncores <- parallel::detectCores()
#parallelCluster <- parallel::makeCluster(ncores)

orig_dir <- getwd()
print(orig_dir)
```

    ## [1] "/Users/johnmount/Documents/work/wrapr/extras"

``` r
setwd(td)
print(td)
```

    ## [1] "/var/folders/7q/h_jp2vj131g5799gfnpzhdp80000gn/T//RtmpZMl2ue"

``` r
options(repos = c(CRAN="https://cloud.r-project.org"))
jobsdfe <- enqueueJobs(package=package, directory=td)

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

    ## cdata_1.1.0 started at 2019-07-24 07:47:05 success at 2019-07-24 07:47:35 (1/0/0) 
    ## RcppDynProg_0.1.2 started at 2019-07-24 07:47:35 success at 2019-07-24 07:49:12 (2/0/0) 
    ## replyr_1.0.3 started at 2019-07-24 07:49:13 success at 2019-07-24 07:49:51 (3/0/0) 
    ## rqdatatable_1.1.9 started at 2019-07-24 07:49:51 success at 2019-07-24 07:50:18 (4/0/0) 
    ## rquery_1.3.6 started at 2019-07-24 07:50:18 success at 2019-07-24 07:51:06 (5/0/0) 
    ## seplyr_0.8.3 started at 2019-07-24 07:51:06 success at 2019-07-24 07:51:36 (6/0/0) 
    ## sigr_1.0.5 started at 2019-07-24 07:51:36 success at 2019-07-24 07:52:01 (7/0/0) 
    ## vtreat_1.4.3 started at 2019-07-24 07:52:01 success at 2019-07-24 07:53:13 (8/0/0) 
    ## WVPlots_1.1.0 started at 2019-07-24 07:53:13 success at 2019-07-24 07:54:22 (9/0/0)

    ## [1] id     title  status
    ## <0 rows> (or 0-length row.names)

``` r
summariseQueue(package=package, directory=td)
```

    ## Test of wrapr had 9 successes, 0 failures, and 0 skipped packages. 
    ## Ran from 2019-07-24 07:47:05 to 2019-07-24 07:54:22 for 7.283 mins 
    ## Average of 48.556 secs relative to 48.51 secs using 1 runners
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
