check\_reverse\_dependencies
================

``` r
repos <- c(CRAN="https://cloud.r-project.org")
library("prrd")
td <- tempdir()
package = "wrapr"
packageVersion(package)
```

    ## [1] '2.0.5'

``` r
date()
```

    ## [1] "Fri Nov 13 07:53:09 2020"

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

    ## [1] "/var/folders/7f/sdjycp_d08n8wwytsbgwqgsw0000gn/T//RtmpGGHmjM"

``` r
options(repos = repos)
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
mk_fn <- function(package, directory, repos) {
  force(package)
  force(directory)
  force(repos)
  function(i) {
    library("prrd")
    options(repos = repos)
    setwd(directory)
    Sys.sleep(1*i)
    dequeueJobs(package=package, directory=directory)
  }
}
f <- mk_fn(package=package, directory=td, repos=repos)

if(!is.null(parallelCluster)) {
  parallel::parLapply(parallelCluster, seq_len(ncores), f)
} else {
  f(0)
}
```

    ## [[1]]
    ##   id       title  status
    ## 1  2 RcppDynProg WORKING
    ## 2  4      rquery WORKING
    ## 3  5      seplyr WORKING
    ## 4  7      vtreat WORKING
    ## 5  8     WVPlots WORKING
    ## 
    ## [[2]]
    ##   id   title  status
    ## 1  5  seplyr WORKING
    ## 2  7  vtreat WORKING
    ## 3  8 WVPlots WORKING
    ## 
    ## [[3]]
    ##   id   title  status
    ## 1  5  seplyr WORKING
    ## 2  8 WVPlots WORKING
    ## 
    ## [[4]]
    ##   id       title  status
    ## 1  2 RcppDynProg WORKING
    ## 2  5      seplyr WORKING
    ## 3  7      vtreat WORKING
    ## 4  8     WVPlots WORKING
    ## 
    ## [[5]]
    ## [1] id     title  status
    ## <0 rows> (or 0-length row.names)
    ## 
    ## [[6]]
    ##   id  title  status
    ## 1  5 seplyr WORKING

``` r
summariseQueue(package=package, directory=td)
```

    ## Test of wrapr 2.0.5 had 8 successes, 0 failures, and 0 skipped packages. 
    ## Ran from 2020-11-13 07:53:13 to 2020-11-13 07:54:34 for 1.35 mins 
    ## Average of 10.125 secs relative to 38.347 secs using 6 runners
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
