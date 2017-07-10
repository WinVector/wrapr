Some timings for [`%.>%`](http://www.win-vector.com/blog/2017/07/in-praise-of-syntactic-sugar/) ("dot arrow").

Keep in mind for any *serious* application the calculation time on data will far dominate any piping overhead, but it is fun to look.

So we will compare:

-   `magrittr*` `magrittr::%>%` piping.
-   `DotArrow*` `wrapr::%.>%` piping.
-   `BizarroPipe*` `->.;` piping.

``` r
library("microbenchmark")
library("wrapr")
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
    ##     BizarroPipe_1()      218     1487.0     3139.755     1695.5     2001.0
    ##        DotArrow_1()     3433     6163.5    12999.901     7784.5    11858.0
    ##        magrittr_1()    49829    62822.0    95200.413    78610.5   114451.0
    ##     BizarroPipe_2()      280     1467.0     4301.379     1727.5     2031.0
    ##        DotArrow_2()     7113    10602.5    17390.713    12787.0    17695.5
    ##        magrittr_2()    73743    86724.5   127466.063   106197.5   147280.0
    ##     BizarroPipe_5()      407     1807.0    14244.722     2092.5     2472.5
    ##        DotArrow_5()    17296    23383.5    34216.489    26419.5    32318.5
    ##        magrittr_5()   135956   157452.0   211875.183   181659.5   229692.5
    ##    BizarroPipe_10()      581     1866.5    10304.842     2222.0     2635.5
    ##       DotArrow_10()    33662    44322.0    63515.168    49484.0    57845.5
    ##       magrittr_10()   238419   271837.5   357692.266   309189.0   370709.5
    ##    BizarroPipe_15()      748     2250.5    12670.308     2541.5     2945.5
    ##       DotArrow_15()    49363    66508.0   102125.599    73261.5    83083.0
    ##       magrittr_15()   339390   389158.5   480230.140   428515.0   502061.5
    ##    BizarroPipe_20()      951     2313.0    16949.975     2679.0     3125.5
    ##       DotArrow_20()    70845    85476.5   130378.549    93767.0   107204.0
    ##       magrittr_20()   442793   506885.0   628354.029   553673.0   648457.0
    ##    BizarroPipe_25()     1124     2532.0    15153.660     2850.5     3257.0
    ##       DotArrow_25()    88798   106773.5   144098.398   116323.0   131956.5
    ##       magrittr_25()   562559   622933.5   778756.911   670203.0   777214.0
    ##    BizarroPipe_50()     1974     3660.5    45936.433     4010.0     4435.5
    ##       DotArrow_50()   176262   206930.0   315354.165   225220.0   261132.0
    ##       magrittr_50()  1088647  1203396.0  1470626.672  1281050.5  1525488.5
    ##   BizarroPipe_100()     3726     5302.0    69946.426     5741.5     6368.0
    ##      DotArrow_100()   344709   422063.5   607110.504   456855.5   534813.5
    ##      magrittr_100()  2103665  2357188.0  2874376.488  2490454.0  2994347.0
    ##   BizarroPipe_200()     7222     8990.5   110747.350     9605.0    10236.0
    ##      DotArrow_200()   707313   859382.5  1226806.945   916473.0  1102727.0
    ##      magrittr_200()  4210584  4692415.5  5851795.199  4932842.5  6133636.5
    ##   BizarroPipe_500()    17720    20401.5   260509.678    21064.5    21861.0
    ##      DotArrow_500()  2137651  2397499.0  2884622.506  2530492.5  3248841.5
    ##      magrittr_500() 10476379 11885952.5 13912516.990 13098358.5 14854595.0
    ##  BizarroPipe_1000()    35125    39785.0   543036.769    40619.5    41972.0
    ##     DotArrow_1000()  4608568  5032309.5  6016676.741  5290646.5  6851192.5
    ##     magrittr_1000() 22409747 25208182.0 27492732.563 26932330.0 28467091.0
    ##        max neval
    ##    1449992  1000
    ##    2511012  1000
    ##    1481279  1000
    ##    2559523  1000
    ##    2943457  1000
    ##    2101715  1000
    ##   12154266  1000
    ##    5026276  1000
    ##   10253470  1000
    ##    8056899  1000
    ##    6714027  1000
    ##    6959494  1000
    ##   10085243  1000
    ##   17638852  1000
    ##   10249257  1000
    ##   14258632  1000
    ##   22047819  1000
    ##   13552797  1000
    ##   12207524  1000
    ##   16096811  1000
    ##   26037982  1000
    ##   41905593  1000
    ##   33789457  1000
    ##   38700690  1000
    ##   64057370  1000
    ##   81102424  1000
    ##   89230660  1000
    ##  100994956  1000
    ##  163409513  1000
    ##  136706251  1000
    ##  238808124  1000
    ##   10864113  1000
    ##   72084879  1000
    ##  501085619  1000
    ##   20900840  1000
    ##   82613695  1000

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
# verb that adds a name column to a list of dataframes
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
           mi <- lm(time ~ size, data=di) 
           ctab <- as.data.frame(summary(mi)$coefficients)
           ctab$coef <- rownames(ctab)
           ctab
         }) %.>%
  add_name_column(., 'method') %.>%
  bind_rows(.) %.>%
  arrange(., method, coef) %.>%
  select(.,  method, coef, Estimate, `Std. Error`, `Pr(>|t|)`)

# "Intercept" is roughly start-up cost 
# "size" is roughly the slope or growth rate of execution time
# as a function of number of pipe stages.
print(dfits)
```

    ##        method        coef    Estimate  Std. Error     Pr(>|t|)
    ## 1 BizarroPipe (Intercept)   6914.4621 54367.99799 8.988009e-01
    ## 2 BizarroPipe        size    531.1036   164.93588 1.285046e-03
    ## 3    DotArrow (Intercept)   4083.1405 18734.25030 8.274713e-01
    ## 4    DotArrow        size   5967.9969    56.83399 0.000000e+00
    ## 5    magrittr (Intercept) 110768.5593 28244.04012 8.836521e-05
    ## 6    magrittr        size  27464.9379    85.68378 0.000000e+00
