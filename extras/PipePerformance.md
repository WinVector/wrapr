Some timings for [`%.>%`](http://www.win-vector.com/blog/2017/07/in-praise-of-syntactic-sugar/) ("dot arrow").

Keep in mind for any *serious* application the calculation time on data will far dominate any piping overhead, but it is fun to look.

So we will compare:

-   `magrittr*` `magrittr::%>%` substitution.
-   `DotArrow*` `wrapr::%.>%` substitution.
-   `BizarroPipe*` `->.;` substitution.

``` r
library("microbenchmark")
library("wrapr")
library("rlang")
suppressPackageStartupMessages(library("ggplot2"))
suppressPackageStartupMessages(library("dplyr"))

# load generated examples
source("pGenFns.R")

print(BizarroPipe_5)
```

    ## function () 
    ## {
    ##     . <- 5
    ##     . <- sin(.)
    ##     . <- sin(.)
    ##     . <- sin(.)
    ##     . <- sin(.)
    ##     sin(.)
    ## }

``` r
print(DotArrow_5)
```

    ## function () 
    ## {
    ##     5 %.>% sin(.) %.>% sin(.) %.>% sin(.) %.>% sin(.) %.>% sin(.)
    ## }

``` r
print(magrittr_5)
```

    ## function () 
    ## {
    ##     5 %>% sin(.) %>% sin(.) %>% sin(.) %>% sin(.) %>% sin(.)
    ## }

``` r
BizarroPipe_10()
```

    ## [1] -0.4774053

``` r
DotArrow_10()
```

    ## [1] -0.4774053

``` r
magrittr_10()
```

    ## [1] -0.4774053

``` r
bm <- microbenchmark(
  magrittr_1(),
  DotArrow_1(),
  BizarroPipe_1(),
  magrittr_2(),
  DotArrow_2(),
  BizarroPipe_2(),
  magrittr_5(),
  DotArrow_5(),
  BizarroPipe_5(),
  magrittr_10(),
  DotArrow_10(),
  BizarroPipe_10(),
  magrittr_15(),
  DotArrow_15(),
  BizarroPipe_15(),
  magrittr_20(),
  DotArrow_20(),
  BizarroPipe_20(),
  magrittr_25(),
  DotArrow_25(),
  BizarroPipe_25(), 
  magrittr_50(),
  DotArrow_50(),
  BizarroPipe_50(), 
  times=1000L
)
print(bm)
```

    ## Unit: nanoseconds
    ##              expr     min        lq        mean    median        uq
    ##      magrittr_1()   48278   57410.0   74216.642   62335.5   70707.0
    ##      DotArrow_1()   37588   41809.0   52815.890   47109.0   52947.0
    ##   BizarroPipe_1()     252    1049.0    3847.904    1467.5    1687.0
    ##      magrittr_2()   73407   82568.5   97809.177   88267.0   97404.5
    ##      DotArrow_2()   73481   81419.5  100928.773   88392.0   97672.0
    ##   BizarroPipe_2()     340    1217.5    3509.339    1563.0    1780.0
    ##      magrittr_5()  135607  152573.5  179806.616  159279.0  171085.0
    ##      DotArrow_5()  182917  199828.0  229331.954  209792.0  221823.0
    ##   BizarroPipe_5()     450    1383.0    5444.047    1892.0    2130.5
    ##     magrittr_10()  239538  266117.0  310490.505  276787.5  289833.5
    ##     DotArrow_10()  367405  399093.5  449396.838  410993.5  427876.5
    ##  BizarroPipe_10()     664    1761.5   10335.687    2209.0    2495.0
    ##     magrittr_15()  338597  379769.0  448122.763  396393.5  414006.0
    ##     DotArrow_15()  552261  596873.5  667747.475  611742.0  637799.0
    ##  BizarroPipe_15()     851    2006.5   11397.370    2554.0    2911.0
    ##     magrittr_20()  446938  497076.5  631725.125  519712.5  541603.5
    ##     DotArrow_20()  743556  796240.5  889188.471  812292.5  845972.0
    ##  BizarroPipe_20()    1011    2059.5   16645.098    2594.0    2918.0
    ##     magrittr_25()  544492  605358.0  698787.796  636039.0  659676.0
    ##     DotArrow_25()  946591  997046.0 1113292.066 1020783.0 1061814.0
    ##  BizarroPipe_25()    1203    2181.5   16147.803    2754.5    3039.5
    ##     magrittr_50() 1068858 1171986.5 1356338.414 1233707.5 1288004.5
    ##     DotArrow_50() 1945913 2049705.5 2288900.429 2096860.0 2200436.5
    ##  BizarroPipe_50()    2186    3198.0   32573.278    3723.0    4040.5
    ##       max neval
    ##   1818601  1000
    ##   1597759  1000
    ##   2455099  1000
    ##   2699588  1000
    ##   2772309  1000
    ##   2007713  1000
    ##   4983930  1000
    ##   5385437  1000
    ##   3645719  1000
    ##  11889289  1000
    ##   9278748  1000
    ##   8218859  1000
    ##  24017032  1000
    ##  22673827  1000
    ##   8927563  1000
    ##  52668432  1000
    ##  18866664  1000
    ##  14137162  1000
    ##  23786422  1000
    ##  22128966  1000
    ##  13471037  1000
    ##  50099265  1000
    ##  42803779  1000
    ##  28930449  1000

``` r
autoplot(bm)
```

![](PipePerformance_files/figure-markdown_github/timings-1.png)

``` r
d <- as.data.frame(bm)
d$size <- as.numeric(gsub("[^0-9]+", "", d$expr))
d$fn <- gsub("[_0-9].*$", "", d$expr)

mkPlot <- function(d, title) {
  d$size <- as.factor(d$size)
  highCut <- as.numeric(quantile(d$time, probs = 0.99))
  dcut <- d[d$time<=highCut, , drop=FALSE]
  
  ggplot(data=dcut, aes(x=time, group=expr, color=size)) +
    geom_density(adjust=0.3) +
    facet_wrap(~fn, ncol=1, scales = 'free_y') +
    xlab('time (NS)') + ggtitle(title)
}

mkPlot(d, 'all timings')
```

![](PipePerformance_files/figure-markdown_github/replot-1.png)

``` r
mkPlot(d[d$fn %in% c('magrittr', 'DotArrow'), , drop=FALSE], 
       'magrittr v.s. DotArrow')
```

![](PipePerformance_files/figure-markdown_github/replot-2.png)

``` r
# fit a linear function for runtime as a function of size
# per group.
fits <- d %.>%
  split(., .$fn) %.>%
  lapply(., 
         function(di) { 
           lm(time ~ size, data=di) 
         }) %.>%
  lapply(., coefficients) %.>%
  lapply(., 
         function(ri) {
           data.frame(Intercept= ri[["(Intercept)"]],
                      size= ri[['size']])
         }) 
dfits <- bind_rows(fits)
dfits$fn <- names(fits)

# "Intercept" is roughly start-up cost 
# "size" is slope or growth rate
print(dfits)
```

    ##   Intercept       size          fn
    ## 1  3061.743   589.1139 BizarroPipe
    ## 2 -3621.605 45473.2401    DotArrow
    ## 3 54050.950 26288.1987    magrittr

``` r
# solve for size where two lines interesect.
# Note: this is a naive estimate, and not stable
# in the face of estimated slopes and intercepts.
solve <- function(dfits, f1, f2) {
  idx1 <- which(dfits$fn==f1)
  idx2 <- which(dfits$fn==f2)
  size <- (dfits$Intercept[[idx1]] - dfits$Intercept[[idx2]]) /
    (dfits$size[[idx2]] - dfits$size[[idx1]])
  size
}

crossingPoint <- solve(dfits, 'DotArrow', 'magrittr')
print(crossingPoint)
```

    ## [1] 3.006121

``` r
ratio <- dfits$size[dfits$fn=='DotArrow'] / dfits$size[dfits$fn=='magrittr']
print(ratio)
```

    ## [1] 1.729797

Overall:

DotArrow is about 1.7 times slower than magrittr on large pipelines, though this cost will be unnoticeable with in any significant workload.
