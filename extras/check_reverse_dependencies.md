check\_reverse\_dependencies
================

``` r
library("prrd")
td <- tempdir()
package = "wrapr"
packageVersion(package)
```

    ## [1] '1.8.6'

``` r
date()
```

    ## [1] "Tue Apr  2 08:06:35 2019"

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

    ## [1] "/var/folders/7q/h_jp2vj131g5799gfnpzhdp80000gn/T//RtmprD1NIY"

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

    ## cdata_1.0.8 started at 2019-04-02 08:06:37 success at 2019-04-02 08:06:59 (1/0/0) 
    ## RcppDynProg_0.1.2 started at 2019-04-02 08:06:59 success at 2019-04-02 08:08:04 (2/0/0) 
    ## replyr_1.0.0 started at 2019-04-02 08:08:04 success at 2019-04-02 08:08:36 (3/0/0) 
    ## rqdatatable_1.1.4 started at 2019-04-02 08:08:36 success at 2019-04-02 08:09:02 (4/0/0) 
    ## rquery_1.3.2 started at 2019-04-02 08:09:02 success at 2019-04-02 08:09:41 (5/0/0) 
    ## seplyr_0.8.3 started at 2019-04-02 08:09:41 success at 2019-04-02 08:10:04 (6/0/0) 
    ## sigr_1.0.5 started at 2019-04-02 08:10:04 success at 2019-04-02 08:10:26 (7/0/0) 
    ## vtreat_1.3.8 started at 2019-04-02 08:10:26 success at 2019-04-02 08:11:31 (8/0/0) 
    ## WVPlots_1.0.9 started at 2019-04-02 08:11:31 success at 2019-04-02 08:12:27 (9/0/0)

    ## [1] id     title  status
    ## <0 rows> (or 0-length row.names)

``` r
summariseQueue(package=package, directory=td)
```

    ## Test of wrapr had 9 successes, 0 failures, and 0 skipped packages. 
    ## Ran from 2019-04-02 08:06:37 to 2019-04-02 08:12:27 for 5.833 mins 
    ## Average of 38.889 secs relative to 38.874 secs using 1 runners
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
