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
library("cdata")
suppressPackageStartupMessages(library("ggplot2"))
suppressPackageStartupMessages(library("dplyr"))

# load generated examples
prevNames <- ls()
source("pGenFns.R")
genFns <- setdiff(ls(), c(prevNames, 'prevNames', 'genFns'))

# parser translates BizarroPipe to different code!
cat(buildFnsK(5), sep = '\n')
```

    ## magrittr_5 <- function() {
    ##  5 %>%
    ##    sin(.) %>%
    ##    sin(.) %>%
    ##    sin(.) %>%
    ##    sin(.) %>%
    ##    sin(.)
    ## }
    ## 
    ## 
    ## 
    ## DotArrow_5 <- function() {
    ##  5 %.>%
    ##    sin(.) %.>%
    ##    sin(.) %.>%
    ##    sin(.) %.>%
    ##    sin(.) %.>%
    ##    sin(.)
    ## }
    ## 
    ## 
    ## 
    ## BizarroPipe_5 <- function() {
    ##  5 ->.;
    ##    sin(.) ->.;
    ##    sin(.) ->.;
    ##    sin(.) ->.;
    ##    sin(.) ->.;
    ##    sin(.)
    ## }

``` r
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
# get expressions into a nice presentation order
fList <- data.frame(expr= genFns, 
                    stringsAsFactors = FALSE)
fList$size <- as.numeric(gsub("[^0-9]+", "", fList$expr))
fList$fn <- gsub("[_0-9].*$", "", fList$expr)
fList <- fList[order(fList$size, fList$fn), , drop=FALSE]

cmd <- parse(text=paste0(
  "microbenchmark(\n ",
  paste(paste0(fList$expr,'()'), collapse=',\n '),
  ", 
  times=1000L
  )\n"
))

print(cmd)
```

    ## expression(microbenchmark(BizarroPipe_1(), DotArrow_1(), magrittr_1(), 
    ##     BizarroPipe_2(), DotArrow_2(), magrittr_2(), BizarroPipe_5(), 
    ##     DotArrow_5(), magrittr_5(), BizarroPipe_10(), DotArrow_10(), 
    ##     magrittr_10(), BizarroPipe_15(), DotArrow_15(), magrittr_15(), 
    ##     BizarroPipe_20(), DotArrow_20(), magrittr_20(), BizarroPipe_25(), 
    ##     DotArrow_25(), magrittr_25(), BizarroPipe_50(), DotArrow_50(), 
    ##     magrittr_50(), BizarroPipe_100(), DotArrow_100(), magrittr_100(), 
    ##     BizarroPipe_200(), DotArrow_200(), magrittr_200(), BizarroPipe_500(), 
    ##     DotArrow_500(), magrittr_500(), BizarroPipe_1000(), DotArrow_1000(), 
    ##     magrittr_1000(), times = 1000L))

``` r
bm <- eval(cmd)
print(bm)
```

    ## Unit: nanoseconds
    ##                expr      min         lq         mean     median         uq
    ##     BizarroPipe_1()      224     1456.0     3028.468     1716.0     2068.0
    ##        DotArrow_1()     3606     6296.5    11296.372     8043.5    12000.0
    ##        magrittr_1()    52211    61881.0    97921.153    78134.0   113014.0
    ##     BizarroPipe_2()      280     1626.5     4300.036     1876.0     2203.0
    ##        DotArrow_2()     6539    11125.5    18430.612    13469.0    18591.0
    ##        magrittr_2()    74938    87578.0   128284.765   104058.0   144226.5
    ##     BizarroPipe_5()      419     1838.0     8586.817     2116.0     2517.5
    ##        DotArrow_5()    16669    23645.5    35724.839    26623.0    32285.5
    ##        magrittr_5()   140997   158903.0   219690.548   185172.0   238438.0
    ##    BizarroPipe_10()      576     1990.5     9570.262     2291.0     2691.5
    ##       DotArrow_10()    35499    44865.0    70486.198    50407.5    58230.0
    ##       magrittr_10()   243825   275742.5   358137.655   310144.0   370171.0
    ##    BizarroPipe_15()      762     2113.5    17485.068     2441.5     2935.5
    ##       DotArrow_15()    50319    66024.0    97642.487    72837.0    83993.5
    ##       magrittr_15()   347678   395967.5   498992.854   439967.5   505985.0
    ##    BizarroPipe_20()      936     2301.5    18904.232     2655.0     3137.5
    ##       DotArrow_20()    71899    88174.0   126849.781    96956.0   109189.5
    ##       magrittr_20()   453962   515434.5   659238.948   565808.0   655712.5
    ##    BizarroPipe_25()     1108     2548.0    16374.421     2904.5     3504.5
    ##       DotArrow_25()    90255   107883.0   178689.077   117539.0   132428.0
    ##       magrittr_25()   555360   635962.5   868610.024   692523.5   819621.5
    ##    BizarroPipe_50()     2015     3545.5    33183.461     3919.5     4477.5
    ##       DotArrow_50()   171382   206916.0   385677.092   225937.5   260326.5
    ##       magrittr_50()  1083284  1224776.0  1521985.645  1303377.5  1559205.5
    ##   BizarroPipe_100()     3725     5451.5    58015.548     5862.5     6540.0
    ##      DotArrow_100()   354725   416064.5   697291.211   453047.0   516655.0
    ##      magrittr_100()  2125274  2376781.0  2889768.959  2504523.5  2995811.5
    ##   BizarroPipe_200()     7232     9237.0   153575.795     9790.0    10612.0
    ##      DotArrow_200()   710946   858683.0  1233831.116   930739.5  1160171.5
    ##      magrittr_200()  4191558  4782150.5  5720562.327  5057867.0  6161918.5
    ##   BizarroPipe_500()    17759    20691.0   342367.868    21357.5    22457.0
    ##      DotArrow_500()  2162021  2425581.0  2944557.234  2568355.0  3226431.5
    ##      magrittr_500() 10820195 12087366.5 14058924.672 13465332.5 15254981.0
    ##  BizarroPipe_1000()    35103    39517.5   741656.471    40405.0    42584.0
    ##     DotArrow_1000()  4588653  5070909.0  6172572.879  5364865.0  7072312.5
    ##     magrittr_1000() 22425031 25855055.5 28538542.729 27675210.0 29454363.5
    ##        max neval
    ##    1299179  1000
    ##    1954328  1000
    ##    2204271  1000
    ##    2330361  1000
    ##    3250646  1000
    ##    3475298  1000
    ##    6427700  1000
    ##    6346690  1000
    ##   11096358  1000
    ##    7098441  1000
    ##   12766802  1000
    ##    7328721  1000
    ##   14959734  1000
    ##   13455706  1000
    ##   18590050  1000
    ##   16161281  1000
    ##   18389435  1000
    ##   21022635  1000
    ##   13340910  1000
    ##   45072878  1000
    ##   58478566  1000
    ##   29117284  1000
    ##   62158809  1000
    ##   56010384  1000
    ##   51894755  1000
    ##  121443622  1000
    ##   83520050  1000
    ##  143402276  1000
    ##  149967485  1000
    ##  174265324  1000
    ##  320129689  1000
    ##   12215359  1000
    ##   72601294  1000
    ##  698985083  1000
    ##   20369501  1000
    ##   87552746  1000

``` r
autoplot(bm)
```

![](PipePerformance_files/figure-markdown_github/timings-1.png)

``` r
d <- as.data.frame(bm)
d$size <- as.numeric(gsub("[^0-9]+", "", d$expr))
d$fn <- gsub("[_0-9].*$", "", d$expr)

d$fn <- reorder(d$fn, d$time)
ggplot(d, aes(x=fn, y=time, color=fn)) + 
  geom_violin() + 
  scale_y_log10() + 
  facet_wrap(~size, labeller="label_both") + 
  coord_flip() + 
  xlab("method") +
  ylab("time NS") +
  theme(legend.position = 'none') +
  scale_color_manual(values = c(magrittr='#7570b3',
                                DotArrow='#d95f02',
                                BizarroPipe='#1b9e77')) +
  ggtitle("distribution of runtime as function of method and problem size",
          subtitle = "log-time scale")
```

![](PipePerformance_files/figure-markdown_github/replot-1.png)

``` r
# ggplot 2 legend in reverse order, so re-order to get that
d$fn <- reorder(d$fn, -d$time)
ggplot(d, aes(x=size, y=time, color=fn)) +
  geom_smooth() +
  scale_y_log10() +
  scale_x_log10() +
  scale_color_manual(values = c(magrittr='#7570b3',
                                DotArrow='#d95f02',
                                BizarroPipe='#1b9e77')) +
  xlab('size (number of pipe stages)') +
  ylab("time NS") +
  ggtitle("complexity of runtime as function of method and problem size",
          subtitle = "log/log scale")
```

    ## `geom_smooth()` using method = 'gam'

![](PipePerformance_files/figure-markdown_github/replot-2.png)

``` r
# new that adds a name column to a list of dataframes
add_name_column <- function(dlist, destinationColumn) {
  res <- dlist
  for(ni in names(dlist)) {
    vi <- dlist[[ni]]
    vi[[destinationColumn]] <- ni
    res[[ni]] <- vi
  }
  res
}

# fit a linear function for runtime as a function of size
# per group.
dfits <- d %.>%
  split(., .$fn) %.>%
  lapply(., 
         function(di) { 
           lm(time ~ size, data=di) 
         }) %.>%
  lapply(., function(fi) {
    ctab <- as.data.frame(summary(fi)$coefficients)
    ctab$coef <- rownames(ctab)
    ctab <- moveValuesToRows(ctab, 
                             nameForNewKeyColumn = 'meas', 
                             nameForNewValueColumn = 'value', 
                             columnsToTakeFrom = c('Estimate',  
                                                   'Std. Error',   
                                                   't value',  
                                                   'Pr(>|t|)'))
    ctab
  }) %.>%
  add_name_column(., 'method') %.>%
  bind_rows(.) %.>%
  moveValuesToColumns(., 
                      columnToTakeKeysFrom = 'meas', 
                      columnToTakeValuesFrom = 'value',
                      rowKeyColumns = c('method', 'coef')) %.>%
  arrange(., method, coef) %.>%
  select(.,  method, coef, Estimate, `Std. Error`,  `t value`, `Pr(>|t|)`)

# "Intercept" is roughly start-up cost 
# "size" is roughly the slope or growth rate of execution time
# as a function of number of pipe stages.
print(dfits)
```

    ##        method        coef   Estimate  Std. Error       t value    Pr(>|t|)
    ## 1 BizarroPipe (Intercept)  -104.8383 74864.65818  -0.001400371 0.998882689
    ## 2 BizarroPipe        size   730.4494   227.11648   3.216188532 0.001302499
    ## 3    DotArrow (Intercept) 17321.8487 22163.65965   0.781542804 0.434498733
    ## 4    DotArrow        size  6102.2753    67.23777  90.756661231 0.000000000
    ## 5    magrittr (Intercept) 72776.6444 29194.32930   2.492834950 0.012686150
    ## 6    magrittr        size 28364.8032    88.56667 320.264986289 0.000000000
