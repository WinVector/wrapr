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

    ## [1] "Sat Feb  1 15:13:45 2020"

``` r
parallelCluster <- NULL
ncores <- parallel::detectCores()
if(ncores > 1) {
  parallelCluster <- parallel::makeCluster(ncores)
}

orig_dir <- getwd()
print(orig_dir)
```

    ## [1] "/Users/johnmount/Documents/work/wrapr/extras"

``` r
setwd(td)
print(td)
```

    ## [1] "/var/folders/7q/h_jp2vj131g5799gfnpzhdp80000gn/T//Rtmpv5GCx9"

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

    ## [[1]]
    ## [1] id     title  status
    ## <0 rows> (or 0-length row.names)
    ## 
    ## [[2]]
    ##   id   title  status
    ## 1  7  vtreat WORKING
    ## 2  8 WVPlots WORKING
    ## 
    ## [[3]]
    ##   id       title  status
    ## 1  2 RcppDynProg WORKING
    ## 2  7      vtreat WORKING
    ## 3  8     WVPlots WORKING
    ## 
    ## [[4]]
    ##   id   title  status
    ## 1  8 WVPlots WORKING

``` r
summariseQueue(package=package, directory=td)
```

    ## Test of wrapr had 8 successes, 0 failures, and 0 skipped packages. 
    ## Ran from 2020-02-01 15:13:51 to 2020-02-01 15:17:54 for 4.05 mins 
    ## Average of 30.375 secs relative to 94.099 secs using 4 runners
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
