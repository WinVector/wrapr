check\_reverse\_dependencies
================

``` r
repos <- c(CRAN="https://cloud.r-project.org")
library("prrd")
td <- tempdir()
package = "wrapr"
packageVersion(package)
```

    ## [1] '2.0.8'

``` r
date()
```

    ## [1] "Thu Jun 10 13:58:58 2021"

``` r
parallelCluster <- NULL
ncores <- parallel::detectCores()
#if(ncores > 1) {
#  parallelCluster <- parallel::makeCluster(ncores)
#}

orig_dir <- getwd()
print(orig_dir)
```

    ## [1] "/Users/johnmount/Documents/work/wrapr/extras"

``` r
setwd(td)
print(td)
```

    ## [1] "/var/folders/7f/sdjycp_d08n8wwytsbgwqgsw0000gn/T//RtmpOnzunx"

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

    ## ## Reverse depends check of wrapr 2.0.8 
    ## cdata_1.1.9 started at 2021-06-10 13:59:01 success at 2021-06-10 13:59:25 (1/0/0) 
    ## RcppDynProg_0.2.0 started at 2021-06-10 13:59:25 failure at 2021-06-10 13:59:35 (1/0/1) 
    ## rqdatatable_1.2.9 started at 2021-06-10 13:59:35 success at 2021-06-10 13:59:52 (2/0/1) 
    ## rquery_1.4.7 started at 2021-06-10 13:59:52 success at 2021-06-10 14:00:24 (3/0/1) 
    ## seplyr_1.0.1 started at 2021-06-10 14:00:24 success at 2021-06-10 14:00:45 (4/0/1) 
    ## sigr_1.1.3 started at 2021-06-10 14:00:45 success at 2021-06-10 14:01:02 (5/0/1) 
    ## vtreat_1.6.2 started at 2021-06-10 14:01:02 success at 2021-06-10 14:01:49 (6/0/1) 
    ## WVPlots_1.3.2 started at 2021-06-10 14:01:49 success at 2021-06-10 14:02:41 (7/0/1)

    ## [1] id     title  status
    ## <0 rows> (or 0-length row.names)

``` r
summariseQueue(package=package, directory=td)
```

    ## Test of wrapr 2.0.8 had 7 successes, 1 failures, and 0 skipped packages. 
    ## Ran from 2021-06-10 13:59:01 to 2021-06-10 14:02:41 for 3.667 mins 
    ## Average of 27.5 secs relative to 27.468 secs using 1 runners
    ## 
    ## Failed packages:  RcppDynProg 
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
