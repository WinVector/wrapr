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

    ## [1] "/var/folders/7q/h_jp2vj131g5799gfnpzhdp80000gn/T//RtmpF0yCja"

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

    ## cdata_1.0.6 started at 2019-02-19 07:31:28 success at 2019-02-19 07:31:48 (1/0/0) 
    ## RcppDynProg_0.1.1 started at 2019-02-19 07:31:48 success at 2019-02-19 07:32:55 (2/0/0) 
    ## replyr_0.9.9 started at 2019-02-19 07:32:55 success at 2019-02-19 07:33:27 (3/0/0) 
    ## rqdatatable_1.1.3 started at 2019-02-19 07:33:27 success at 2019-02-19 07:33:52 (4/0/0) 
    ## rquery_1.3.1 started at 2019-02-19 07:33:52 success at 2019-02-19 07:34:29 (5/0/0) 
    ## seplyr_0.8.3 started at 2019-02-19 07:34:29 success at 2019-02-19 07:34:54 (6/0/0) 
    ## sigr_1.0.4 started at 2019-02-19 07:34:54 success at 2019-02-19 07:35:15 (7/0/0) 
    ## vtreat_1.3.6 started at 2019-02-19 07:35:15 success at 2019-02-19 07:36:17 (8/0/0) 
    ## WVPlots_1.0.8 started at 2019-02-19 07:36:17 success at 2019-02-19 07:37:13 (9/0/0)

    ## [1] id     title  status
    ## <0 rows> (or 0-length row.names)

``` r
summariseQueue(package=package, directory=td)
```

    ## Test of wrapr had 9 successes, 0 failures, and 0 skipped packages. 
    ## Ran from 2019-02-19 07:31:28 to 2019-02-19 07:37:13 for 5.75 mins 
    ## Average of 38.333 secs relative to 38.308 secs using 1 runners
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
