Some timings for [`%.>%`](http://www.win-vector.com/blog/2017/07/in-praise-of-syntactic-sugar/).

Keep in mind for any *serious* application the calculation time on data will far dominate any piping overhead, but it is fun to look.

So we will compare:

-   `magrittr*` `magrittr::%>%` substitution.
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
print(BlockPipe_5)
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
BlockPipe_10()
```

    ## [1] -0.4774053

``` r
magrittr_10()
```

    ## [1] -0.4774053

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
    ##      magrittr_1()   49164   57684.5   70483.980   62535.0   70139.5
    ##     BlockPipe_1()   36478   41537.0   54304.177   46783.5   53209.5
    ##   BizarroPipe_1()     223    1046.0    2612.819    1422.5    1658.0
    ##      magrittr_2()   72094   82268.5  102902.873   87810.5   96616.0
    ##     BlockPipe_2()   71646   80874.0  101847.273   88985.0   97709.0
    ##   BizarroPipe_2()     292    1266.0    3249.247    1683.5    1956.5
    ##      magrittr_5()  133961  151405.0  177993.592  158536.0  170876.0
    ##     BlockPipe_5()  181052  197846.5  228548.214  207307.5  220056.0
    ##   BizarroPipe_5()     393    1436.5    4618.697    1792.5    2067.0
    ##     magrittr_10()  238320  263148.5  309381.547  273451.0  289787.5
    ##    BlockPipe_10()  368290  392761.0  448329.108  405507.0  425232.5
    ##  BizarroPipe_10()     561    1510.0    8517.747    1962.0    2319.5
    ##     magrittr_15()  335872  372382.5  435325.250  389409.0  412498.0
    ##    BlockPipe_15()  555207  588860.5  654151.810  604649.0  634009.0
    ##  BizarroPipe_15()     744    1919.0    9616.341    2374.0    2708.0
    ##     magrittr_20()  440762  485630.0  565786.662  510395.5  537440.5
    ##    BlockPipe_20()  736150  784075.5  875202.437  805391.5  843710.5
    ##  BizarroPipe_20()     890    2065.0   16507.985    2583.5    2948.0
    ##     magrittr_25()  546613  597405.5  690871.427  627941.5  655559.0
    ##    BlockPipe_25()  926342  983366.5 1151436.130 1009431.5 1053063.0
    ##  BizarroPipe_25()    1059    2371.5   14271.914    2747.5    3121.5
    ##     magrittr_50() 1053170 1144025.5 1318010.482 1206935.0 1257865.5
    ##    BlockPipe_50() 1902404 2023424.0 2248940.628 2078659.5 2199532.0
    ##  BizarroPipe_50()    1909    3055.0   25826.159    3548.0    3896.5
    ##       max neval
    ##   1354707  1000
    ##   1349448  1000
    ##   1224916  1000
    ##   2013864  1000
    ##   2195319  1000
    ##   1658811  1000
    ##   3845414  1000
    ##   3933321  1000
    ##   2849996  1000
    ##   6643774  1000
    ##   7234028  1000
    ##   6564714  1000
    ##   9767481  1000
    ##  10524485  1000
    ##   7325720  1000
    ##  14611038  1000
    ##  13889120  1000
    ##  14015480  1000
    ##  16547674  1000
    ##  52959131  1000
    ##  11555148  1000
    ##  33335624  1000
    ##  39928831  1000
    ##  22317535  1000

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
    ## 1  2998.829   478.3615 BizarroPipe
    ## 2  2186.588 44884.8990   BlockPipe
    ## 3 52330.049 25407.1517    magrittr

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

    ## [1] 2.574397

``` r
ratio <- dfits$size[dfits$fn=='BlockPipe'] / dfits$size[dfits$fn=='magrittr']
print(ratio)
```

    ## [1] 1.766625

Overall:

BlockPipe is about 1.8 times slower than magrittr on large pipelines, though this cost will be unnoticeable with in any significant workload.
