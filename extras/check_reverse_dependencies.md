check\_reverse\_dependencies
================

``` r
library("prrd")
td <- tempdir()
package = "wrapr"
packageVersion(package)
```

    ## [1] '1.9.2'

``` r
date()
```

    ## [1] "Sun Oct 13 10:12:16 2019"

``` r
parallelCluster <- NULL
ncores <- 0
# # parallel doesn't work due to https://github.com/r-lib/liteq/issues/22
ncores <- parallel::detectCores()
parallelCluster <- parallel::makeCluster(ncores)

orig_dir <- getwd()
print(orig_dir)
```

    ## [1] "/Users/johnmount/Documents/work/wrapr/extras"

``` r
setwd(td)
print(td)
```

    ## [1] "/var/folders/7q/h_jp2vj131g5799gfnpzhdp80000gn/T//RtmpHERUQS"

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

    ## [[1]]
    ##   id   title  status
    ## 1  9 WVPlots WORKING
    ## 
    ## [[2]]
    ##   id   title  status
    ## 1  8  vtreat WORKING
    ## 2  9 WVPlots WORKING
    ## 
    ## [[3]]
    ## [1] id     title  status
    ## <0 rows> (or 0-length row.names)
    ## 
    ## [[4]]
    ##   id       title  status
    ## 1  2 RcppDynProg WORKING
    ## 2  8      vtreat WORKING
    ## 3  9     WVPlots WORKING

``` r
summariseQueue(package=package, directory=td)
```

    ## Test of wrapr had 9 successes, 0 failures, and 0 skipped packages. 
    ## Ran from 2019-10-13 10:12:22 to 2019-10-13 10:15:39 for 3.283 mins 
    ## Average of 21.889 secs relative to 72.012 secs using 4 runners
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
