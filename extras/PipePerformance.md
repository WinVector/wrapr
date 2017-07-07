Some timings for [`%.>%`](http://www.win-vector.com/blog/2017/07/in-praise-of-syntactic-sugar/).

Keep in mind for any *serious* application the calculation time on data will far dominate any piping overhead, but it is fun to look.

So we will compare:

-   `magrittr*` `magrittr::%>%` substitution (`"langsubs"` mode).
-   `BlockPipe*` `wrapr::%.>%` substitution.
-   `BizarroPipe*` `%->%` substitution.

``` r
library("microbenchmark")
library("wrapr")
library("rlang")
suppressPackageStartupMessages(library("ggplot2"))
suppressPackageStartupMessages(library("dplyr"))

# load generated examples
source("pGenFns.R")
```

``` r
bm <- microbenchmark(
  magrittr_1(),
  BlockPipe_1(),
  BizarroPipe_1(),
  magrittr_2(),
  BlockPipe_2(),
  BizarroPipe_2(),
  magrittr_5(),
  BlockPipe_5(),
  BizarroPipe_5(),
  magrittr_10(),
  BlockPipe_10(),
  BizarroPipe_10(),
  magrittr_15(),
  BlockPipe_15(),
  BizarroPipe_15(),
  magrittr_20(),
  BlockPipe_20(),
  BizarroPipe_20(),
  magrittr_25(),
  BlockPipe_25(),
  BizarroPipe_25(), 
  magrittr_50(),
  BlockPipe_50(),
  BizarroPipe_50(), 
  times=1000L
)
print(bm)
```

    ## Unit: nanoseconds
    ##              expr     min        lq        mean    median        uq
    ##      magrittr_1()   48316   56863.5   71918.921   61732.0   68562.5
    ##     BlockPipe_1()   36188   40672.0   51832.013   45501.0   51681.0
    ##   BizarroPipe_1()     215    1202.0    2649.251    1499.5    1716.0
    ##      magrittr_2()   71377   81419.0  104181.351   87758.0   97293.5
    ##     BlockPipe_2()   72621   79839.5   98994.105   87460.5   95898.0
    ##   BizarroPipe_2()     285    1330.0    3178.745    1562.5    1781.5
    ##      magrittr_5()  136134  150771.5  179633.412  158906.5  170897.5
    ##     BlockPipe_5()  177867  194641.5  229294.372  205080.0  218190.0
    ##   BizarroPipe_5()     430    1538.0    4820.808    1802.5    2072.0
    ##     magrittr_10()  236352  261990.5  311903.153  275393.0  296058.0
    ##    BlockPipe_10()  355982  388542.5  453868.735  401087.5  425110.0
    ##  BizarroPipe_10()     592    1938.5    8435.338    2275.0    2565.0
    ##     magrittr_15()  339805  373938.0  429188.069  392308.0  414505.0
    ##    BlockPipe_15()  540752  581177.0  665545.786  598609.5  631430.0
    ##  BizarroPipe_15()     739    2141.5    9748.042    2485.0    2846.0
    ##     magrittr_20()  443719  484255.5  555749.624  508877.5  534431.5
    ##    BlockPipe_20()  722703  775043.5  934686.442  796800.0  849892.5
    ##  BizarroPipe_20()     920    2224.0   13782.476    2569.0    2860.0
    ##     magrittr_25()  543337  596900.5  696679.241  625698.5  657863.5
    ##    BlockPipe_25()  905433  972366.0 1099549.149  996390.0 1059284.0
    ##  BizarroPipe_25()    1078    2343.0   15486.176    2676.0    3013.5
    ##     magrittr_50() 1050546 1167667.5 1381823.729 1230680.5 1312081.5
    ##    BlockPipe_50() 1875207 1999466.0 2321478.087 2050035.0 2255573.5
    ##  BizarroPipe_50()    1911    3511.5   32795.268    3817.0    4219.0
    ##       max neval
    ##   1782173  1000
    ##   1386379  1000
    ##   1174959  1000
    ##   6486205  1000
    ##   2238536  1000
    ##   1630916  1000
    ##   3608155  1000
    ##   5634863  1000
    ##   3025855  1000
    ##   6809934  1000
    ##   7337348  1000
    ##   6203739  1000
    ##  11147581  1000
    ##  10443111  1000
    ##   7231721  1000
    ##  16495116  1000
    ##  51263425  1000
    ##  11244890  1000
    ##  16673522  1000
    ##  16526137  1000
    ##  12824831  1000
    ##  41544571  1000
    ##  53038127  1000
    ##  28947926  1000

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
mkPlot(d[d$fn %in% c('magrittr', 'BlockPipe'), , drop=FALSE], 
       'magrittr v.s. BlockPipe')
```

![](PipePerformance_files/figure-markdown_github/replot-2.png)

``` r
# fit a linear function for runtime as a function of size
# per group.
fits <- d %>%
  split(., .$fn) %>%
  lapply(., 
         function(di) { 
           lm(time ~ size, data=di) 
         }) %>%
  lapply(., coefficients) %>%
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
    ## 1   1650.97   606.9402 BizarroPipe
    ## 2  -6002.64 46119.2954   BlockPipe
    ## 3  41395.86 26561.8020    magrittr

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

crossingPoint <- solve(dfits, 'BlockPipe', 'magrittr')
print(crossingPoint)
```

    ## [1] 2.423547
