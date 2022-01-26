check\_reverse\_dependencies
================

``` r
repos <- c(CRAN="https://cloud.r-project.org")
library("prrd")
orig_dir <- getwd()
# td <- tempdir()
td <- paste0(orig_dir, '/', 'revdep_tests')
package = "wrapr"
packageVersion(package)
```

    ## [1] '2.0.9'

``` r
date()
```

    ## [1] "Wed Jan 26 10:19:34 2022"

``` r
parallelCluster <- NULL
ncores <- parallel::detectCores()
#if(ncores > 1) {
#  parallelCluster <- parallel::makeCluster(ncores)
#}


print(orig_dir)
```

    ## [1] "/Users/johnmount/Documents/work/wrapr/extras"

``` r
setwd(td)
print(td)
```

    ## [1] "/Users/johnmount/Documents/work/wrapr/extras/revdep_tests"

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
    ## 2  2   MultiATSM  READY
    ## 3  3 RcppDynProg  READY
    ## 4  4 rqdatatable  READY
    ## 5  5      rquery  READY
    ## 6  6      seplyr  READY
    ## 7  7        sigr  READY
    ## 8  8      vtreat  READY
    ## 9  9     WVPlots  READY

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

    ## ## Reverse depends check of wrapr 2.0.9 
    ## cdata_1.2.0 started at 2022-01-26 10:19:37 success at 2022-01-26 10:20:07 (1/0/0) 
    ## MultiATSM_0.0.1 started at 2022-01-26 10:20:07 failure at 2022-01-26 10:20:10 (1/0/1) 
    ## RcppDynProg_0.2.0 started at 2022-01-26 10:20:10 failure at 2022-01-26 10:20:57 (1/0/2) 
    ## rqdatatable_1.3.1 started at 2022-01-26 10:20:57 success at 2022-01-26 10:21:17 (2/0/2) 
    ## rquery_1.4.8 started at 2022-01-26 10:21:17 success at 2022-01-26 10:22:09 (3/0/2) 
    ## seplyr_1.0.4 started at 2022-01-26 10:22:09 success at 2022-01-26 10:22:35 (4/0/2) 
    ## sigr_1.1.4 started at 2022-01-26 10:22:35 success at 2022-01-26 10:23:03 (5/0/2) 
    ## vtreat_1.6.3 started at 2022-01-26 10:23:04 success at 2022-01-26 10:24:07 (6/0/2) 
    ## WVPlots_1.3.2 started at 2022-01-26 10:24:07 success at 2022-01-26 10:25:06 (7/0/2)

    ## [1] id     title  status
    ## <0 rows> (or 0-length row.names)

``` r
summariseQueue(package=package, directory=td)
```

    ## Test of wrapr 2.0.9 had 7 successes, 2 failures, and 0 skipped packages. 
    ## Ran from 2022-01-26 10:19:37 to 2022-01-26 10:25:06 for 5.483 mins 
    ## Average of 36.556 secs relative to 36.316 secs using 1 runners
    ## 
    ## Failed packages:  MultiATSM, RcppDynProg 
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
