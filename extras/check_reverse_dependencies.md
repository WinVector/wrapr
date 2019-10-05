check\_reverse\_dependencies
================

``` r
library("prrd")
td <- tempdir()
package = "wrapr"
packageVersion(package)
```

    ## [1] '1.9.1'

``` r
date()
```

    ## [1] "Sat Oct  5 14:59:04 2019"

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

    ## [1] "/var/folders/7q/h_jp2vj131g5799gfnpzhdp80000gn/T//RtmpCxqeh7"

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

    ## cdata_1.1.2 started at 2019-10-05 14:59:07 success at 2019-10-05 14:59:39 (1/0/0) 
    ## RcppDynProg_0.1.3 started at 2019-10-05 14:59:39 success at 2019-10-05 15:00:51 (2/0/0) 
    ## replyr_1.0.4 started at 2019-10-05 15:00:51 success at 2019-10-05 15:01:26 (3/0/0) 
    ## rqdatatable_1.2.2 started at 2019-10-05 15:01:26 success at 2019-10-05 15:01:53 (4/0/0) 
    ## rquery_1.3.8 started at 2019-10-05 15:01:53 success at 2019-10-05 15:02:33 (5/0/0) 
    ## seplyr_0.8.4 started at 2019-10-05 15:02:33 success at 2019-10-05 15:02:59 (6/0/0) 
    ## sigr_1.0.6 started at 2019-10-05 15:02:59 success at 2019-10-05 15:03:21 (7/0/0) 
    ## vtreat_1.4.7 started at 2019-10-05 15:03:21 success at 2019-10-05 15:04:29 (8/0/0) 
    ## WVPlots_1.2.0 started at 2019-10-05 15:04:29 success at 2019-10-05 15:05:41 (9/0/0)

    ## [1] id     title  status
    ## <0 rows> (or 0-length row.names)

``` r
summariseQueue(package=package, directory=td)
```

    ## Test of wrapr had 9 successes, 0 failures, and 0 skipped packages. 
    ## Ran from 2019-10-05 14:59:07 to 2019-10-05 15:05:41 for 6.567 mins 
    ## Average of 43.778 secs relative to 43.741 secs using 1 runners
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
