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

    ## [1] "Sun Oct 13 09:57:15 2019"

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

    ## [1] "/var/folders/7q/h_jp2vj131g5799gfnpzhdp80000gn/T//RtmpACdcPS"

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
    ## Ran from 2019-10-13 09:57:21 to 2019-10-13 10:00:54 for 3.55 mins 
    ## Average of 23.667 secs relative to 77.117 secs using 4 runners
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
