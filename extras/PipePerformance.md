Some timings for [`%.>%`](http://www.win-vector.com/blog/2017/07/in-praise-of-syntactic-sugar/).

Keep in mind for any *serious* application the calculation time on data will far dominate any piping overhead, but it is fun to look.

So we will compare:

-   `magrittr*` `magrittr::%>%` substitution.
-   `DotBlockPipe*` `wrapr::%.>%` substitution.
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
print(DotBlockPipe_5)
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
DotBlockPipe_10()
```

    ## [1] -0.4774053

``` r
magrittr_10()
```

    ## [1] -0.4774053

``` r
bm <- microbenchmark(
  magrittr_1(),
  DotBlockPipe_1(),
  BizarroPipe_1(),
  magrittr_2(),
  DotBlockPipe_2(),
  BizarroPipe_2(),
  magrittr_5(),
  DotBlockPipe_5(),
  BizarroPipe_5(),
  magrittr_10(),
  DotBlockPipe_10(),
  BizarroPipe_10(),
  magrittr_15(),
  DotBlockPipe_15(),
  BizarroPipe_15(),
  magrittr_20(),
  DotBlockPipe_20(),
  BizarroPipe_20(),
  magrittr_25(),
  DotBlockPipe_25(),
  BizarroPipe_25(), 
  magrittr_50(),
  DotBlockPipe_50(),
  BizarroPipe_50(), 
  times=1000L
)
print(bm)
```

    ## Unit: nanoseconds
    ##               expr     min        lq        mean    median        uq
    ##       magrittr_1()   47910   58908.0   71817.553   64412.0   71990.0
    ##   DotBlockPipe_1()   36828   42499.0   55758.271   48490.5   54572.0
    ##    BizarroPipe_1()     218    1281.5    2612.513    1535.0    1729.0
    ##       magrittr_2()   72195   84175.5  102198.356   90608.0   99014.5
    ##   DotBlockPipe_2()   74504   83037.0  102210.099   90956.5   99263.5
    ##    BizarroPipe_2()     293    1491.0    4547.017    1736.5    1973.0
    ##       magrittr_5()  135445  152112.0  183399.108  160059.0  171509.5
    ##   DotBlockPipe_5()  182178  201129.0  232582.597  211000.5  226030.5
    ##    BizarroPipe_5()     396    1660.0    4833.152    1962.0    2238.0
    ##      magrittr_10()  237164  261523.5  309981.789  271636.0  287165.5
    ##  DotBlockPipe_10()  367742  400036.5  476205.566  414518.5  440082.0
    ##   BizarroPipe_10()     568    2005.5    7402.956    2312.0    2645.5
    ##      magrittr_15()  333154  370912.0  431708.445  383412.5  407987.0
    ##  DotBlockPipe_15()  550780  599005.0  674982.800  616721.0  643054.5
    ##   BizarroPipe_15()     736    2257.0   10479.704    2639.5    2993.5
    ##      magrittr_20()  439156  483494.0  562019.036  499247.5  530138.5
    ##  DotBlockPipe_20()  743924  801646.0  891919.623  824291.5  875714.5
    ##   BizarroPipe_20()     896    2338.0   13805.195    2657.0    3002.5
    ##      magrittr_25()  542354  594943.0  675267.448  612798.0  648748.0
    ##  DotBlockPipe_25()  932516 1006112.0 1127922.400 1035132.0 1106644.5
    ##   BizarroPipe_25()    1053    2510.5   16928.549    2829.0    3236.5
    ##      magrittr_50() 1052330 1148805.5 1317047.834 1182381.0 1257919.0
    ##  DotBlockPipe_50() 1923771 2066405.0 2344830.673 2122579.0 2306038.5
    ##   BizarroPipe_50()    1944    3469.5   31100.647    3776.0    4194.5
    ##       max neval
    ##   1311071  1000
    ##   1697744  1000
    ##   1123363  1000
    ##   1946498  1000
    ##   2034688  1000
    ##   1571328  1000
    ##   3991477  1000
    ##   4010409  1000
    ##   2874873  1000
    ##   7025186  1000
    ##   7001563  1000
    ##   5095912  1000
    ##   9985808  1000
    ##  11010132  1000
    ##   7903346  1000
    ##  13554159  1000
    ##  16133461  1000
    ##  11143236  1000
    ##  16373739  1000
    ##  18006142  1000
    ##  14098287  1000
    ##  35398057  1000
    ##  57430530  1000
    ##  27278576  1000

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
mkPlot(d[d$fn %in% c('magrittr', 'DotBlockPipe'), , drop=FALSE], 
       'magrittr v.s. DotBlockPipe')
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

    ##   Intercept       size           fn
    ## 1  2257.676   575.3776  BizarroPipe
    ## 2 -4801.403 46443.9316 DotBlockPipe
    ## 3 52417.240 25266.4192     magrittr

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

crossingPoint <- solve(dfits, 'DotBlockPipe', 'magrittr')
print(crossingPoint)
```

    ## [1] 2.701859

``` r
ratio <- dfits$size[dfits$fn=='DotBlockPipe'] / dfits$size[dfits$fn=='magrittr']
print(ratio)
```

    ## [1] 1.838168

Overall:

DotBlockPipe is about 1.8 times slower than magrittr on large pipelines, though this cost will be unnoticeable with in any significant workload.
