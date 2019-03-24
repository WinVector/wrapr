check\_reverse\_dependencies
================

``` r
library("prrd")
td <- tempdir()
package = "wrapr"

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

    ## [1] "/var/folders/7q/h_jp2vj131g5799gfnpzhdp80000gn/T//RtmpJhs0MX"

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

    ## cdata_1.0.7 started at 2019-03-24 15:15:52 success at 2019-03-24 15:16:12 (1/0/0) 
    ## RcppDynProg_0.1.1 started at 2019-03-24 15:16:12 success at 2019-03-24 15:17:19 (2/0/0) 
    ## replyr_0.9.9 started at 2019-03-24 15:17:19 success at 2019-03-24 15:17:48 (3/0/0) 
    ## rqdatatable_1.1.4 started at 2019-03-24 15:17:48 success at 2019-03-24 15:18:12 (4/0/0) 
    ## rquery_1.3.2 started at 2019-03-24 15:18:12 success at 2019-03-24 15:18:48 (5/0/0) 
    ## seplyr_0.8.3 started at 2019-03-24 15:18:48 success at 2019-03-24 15:19:12 (6/0/0) 
    ## sigr_1.0.5 started at 2019-03-24 15:19:12 success at 2019-03-24 15:19:34 (7/0/0) 
    ## vtreat_1.3.7 started at 2019-03-24 15:19:34 success at 2019-03-24 15:20:45 (8/0/0) 
    ## WVPlots_1.0.9 started at 2019-03-24 15:20:45 success at 2019-03-24 15:21:50 (9/0/0)

    ## [1] id     title  status
    ## <0 rows> (or 0-length row.names)

``` r
summariseQueue(package=package, directory=td)
```

    ## Test of wrapr had 9 successes, 0 failures, and 0 skipped packages. 
    ## Ran from 2019-03-24 15:15:52 to 2019-03-24 15:21:50 for 5.967 mins 
    ## Average of 39.778 secs relative to 39.732 secs using 1 runners
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
