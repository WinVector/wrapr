check\_reverse\_dependencies
================

``` r
library("prrd")
td <- tempdir()
package = "wrapr"
packageVersion(package)
```

    ## [1] '1.8.7'

``` r
date()
```

    ## [1] "Thu May 23 16:41:37 2019"

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

    ## [1] "/var/folders/7q/h_jp2vj131g5799gfnpzhdp80000gn/T//RtmpyqZwVj"

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

    ## cdata_1.1.0 started at 2019-05-23 16:41:40 success at 2019-05-23 16:42:06 (1/0/0) 
    ## RcppDynProg_0.1.2 started at 2019-05-23 16:42:06 success at 2019-05-23 16:43:54 (2/0/0) 
    ## replyr_1.0.0 started at 2019-05-23 16:43:55 success at 2019-05-23 16:44:28 (3/0/0) 
    ## rqdatatable_1.1.7 started at 2019-05-23 16:44:28 success at 2019-05-23 16:44:54 (4/0/0) 
    ## rquery_1.3.2 started at 2019-05-23 16:44:54 success at 2019-05-23 16:45:42 (5/0/0) 
    ## seplyr_0.8.3 started at 2019-05-23 16:45:42 success at 2019-05-23 16:46:08 (6/0/0) 
    ## sigr_1.0.5 started at 2019-05-23 16:46:08 success at 2019-05-23 16:46:29 (7/0/0) 
    ## vtreat_1.4.0 started at 2019-05-23 16:46:29 success at 2019-05-23 16:47:42 (8/0/0) 
    ## WVPlots_1.1.0 started at 2019-05-23 16:47:42 success at 2019-05-23 16:48:54 (9/0/0)

    ## [1] id     title  status
    ## <0 rows> (or 0-length row.names)

``` r
summariseQueue(package=package, directory=td)
```

    ## Test of wrapr had 9 successes, 0 failures, and 0 skipped packages. 
    ## Ran from 2019-05-23 16:41:40 to 2019-05-23 16:48:54 for 7.233 mins 
    ## Average of 48.222 secs relative to 48.199 secs using 1 runners
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
