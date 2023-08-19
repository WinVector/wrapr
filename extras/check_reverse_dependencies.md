check_reverse_dependencies
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

    ## [1] '2.1.0'

``` r
date()
```

    ## [1] "Sat Aug 19 09:37:26 2023"

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

    ##    id       title status
    ## 1   1       cdata  READY
    ## 2   2  formatdown  READY
    ## 3   3   midfieldr  READY
    ## 4   4   MultiATSM  READY
    ## 5   5 RcppDynProg  READY
    ## 6   6 rqdatatable  READY
    ## 7   7      rquery  READY
    ## 8   8        sigr  READY
    ## 9   9      vtreat  READY
    ## 10 10     WVPlots  READY

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

    ## ## Reverse depends check of wrapr 2.1.0 
    ## cdata_1.2.0 started at 2023-08-19 09:37:27 success at 2023-08-19 09:37:47 (1/0/0) 
    ## formatdown_0.1.2 started at 2023-08-19 09:37:47 success at 2023-08-19 09:38:03 (2/0/0) 
    ## midfieldr_1.0.1 started at 2023-08-19 09:38:03 failure at 2023-08-19 09:38:06 (2/0/1) 
    ## MultiATSM_0.3.3 started at 2023-08-19 09:38:06 success at 2023-08-19 09:39:04 (3/0/1) 
    ## RcppDynProg_0.2.0 started at 2023-08-19 09:39:04 success at 2023-08-19 09:40:02 (4/0/1) 
    ## rqdatatable_1.3.2 started at 2023-08-19 09:40:02 success at 2023-08-19 09:40:19 (5/0/1) 
    ## rquery_1.4.9 started at 2023-08-19 09:40:19 success at 2023-08-19 09:40:49 (6/0/1) 
    ## sigr_1.1.4 started at 2023-08-19 09:40:49 success at 2023-08-19 09:41:06 (7/0/1) 
    ## vtreat_1.6.3 started at 2023-08-19 09:41:06 success at 2023-08-19 09:41:54 (8/0/1) 
    ## WVPlots_1.3.5 started at 2023-08-19 09:41:54 success at 2023-08-19 09:42:51 (9/0/1)

    ## [1] id     title  status
    ## <0 rows> (or 0-length row.names)

``` r
summariseQueue(package=package, directory=td)
```

    ## Test of wrapr 2.1.0 had 9 successes, 1 failures, and 0 skipped packages. 
    ## Ran from 2023-08-19 09:37:27 to 2023-08-19 09:42:51 for 5.4 mins 
    ## Average of 32.4 secs relative to 32.384 secs using 1 runners
    ## 
    ## Failed packages:  midfieldr 
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
