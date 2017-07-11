Some timings for [`%.>%`](http://www.win-vector.com/blog/2017/07/in-praise-of-syntactic-sugar/) ("dot arrow").

Keep in mind for any *serious* application the calculation time on data will far dominate any piping overhead, but it is fun to look.

So we will compare:

-   `magrittr*` `magrittr::%>%` piping.
-   `DotArrow*` `wrapr::%.>%` piping.
-   `BizarroPipe*` `->.;` piping.
-   `TidyPipe*` `%>%` [piping based on `rlang`/`tidyeval`](https://gist.github.com/lionel-/10cd649b31f11512e4aea3b7a98fe381) (renamed to "`%t>%`" in this run to avoid name collisions).

``` r
library("microbenchmark")
library("wrapr")
suppressPackageStartupMessages(library("ggplot2"))
suppressPackageStartupMessages(library("dplyr"))
library("glmnet")
```

    ## Loading required package: Matrix

    ## Loading required package: foreach

    ## Loaded glmnet 2.0-10

``` r
source('rlangPipe.R')

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
    ## 
    ## 
    ## 
    ## TidyPipe_5 <- function() {
    ##  5 %t>%
    ##    sin(.) %t>%
    ##    sin(.) %t>%
    ##    sin(.) %t>%
    ##    sin(.) %t>%
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
    ##     TidyPipe_1(), BizarroPipe_2(), DotArrow_2(), magrittr_2(), 
    ##     TidyPipe_2(), BizarroPipe_5(), DotArrow_5(), magrittr_5(), 
    ##     TidyPipe_5(), BizarroPipe_10(), DotArrow_10(), magrittr_10(), 
    ##     TidyPipe_10(), BizarroPipe_15(), DotArrow_15(), magrittr_15(), 
    ##     TidyPipe_15(), BizarroPipe_20(), DotArrow_20(), magrittr_20(), 
    ##     TidyPipe_20(), BizarroPipe_25(), DotArrow_25(), magrittr_25(), 
    ##     TidyPipe_25(), BizarroPipe_50(), DotArrow_50(), magrittr_50(), 
    ##     TidyPipe_50(), BizarroPipe_100(), DotArrow_100(), magrittr_100(), 
    ##     TidyPipe_100(), BizarroPipe_200(), DotArrow_200(), magrittr_200(), 
    ##     TidyPipe_200(), BizarroPipe_500(), DotArrow_500(), magrittr_500(), 
    ##     TidyPipe_500(), BizarroPipe_1000(), DotArrow_1000(), magrittr_1000(), 
    ##     TidyPipe_1000(), times = 1000L))

``` r
gc()
```

    ##           used (Mb) gc trigger  (Mb) max used (Mb)
    ## Ncells 1328831   71    2164898 115.7  1770749 94.6
    ## Vcells 1440654   11    2552219  19.5  2332274 17.8

``` r
datFile <- 'timings.RDS'
if(!file.exists(datFile)) {
  print("running")
  bm <- eval(cmd)
  saveRDS(bm, file=datFile)
} else {
  print("using cached results")
  bm <- readRDS(file=datFile)
}
```

    ## [1] "using cached results"

``` r
print(bm)
```

    ## Unit: nanoseconds
    ##                expr       min          lq         mean      median
    ##     BizarroPipe_1()       311      1601.0 3.151041e+03      1852.5
    ##        DotArrow_1()      3830      7593.0 1.192143e+04      9975.0
    ##        magrittr_1()     50716     79380.0 1.080513e+05    103043.5
    ##        TidyPipe_1()     73431     88769.0 1.327619e+05    111100.0
    ##     BizarroPipe_2()       296      1737.0 3.943770e+03      1999.0
    ##        DotArrow_2()      7564     12968.0 1.871724e+04     16515.0
    ##        magrittr_2()     73497    107428.0 1.448151e+05    139355.5
    ##        TidyPipe_2()    147710    173540.5 2.396801e+05    216920.0
    ##     BizarroPipe_5()       398      1965.5 5.850010e+03      2219.0
    ##        DotArrow_5()     18056     26495.5 3.959289e+04     30860.5
    ##        magrittr_5()    141619    184236.0 2.344888e+05    220201.0
    ##        TidyPipe_5()    360258    413047.0 5.291932e+05    467284.0
    ##    BizarroPipe_10()       655      2258.5 8.261371e+03      2551.5
    ##       DotArrow_10()     34342     46855.5 6.760176e+04     52749.5
    ##       magrittr_10()    248196    306015.5 3.839070e+05    348933.5
    ##       TidyPipe_10()    722322    807837.0 1.048367e+06    915988.5
    ##    BizarroPipe_15()       808      2377.0 1.067373e+04      2714.5
    ##       DotArrow_15()     52821     69429.5 1.012108e+05     76585.5
    ##       magrittr_15()    357424    427377.5 5.298084e+05    472379.0
    ##       TidyPipe_15()   1073739   1212790.5 1.509328e+06   1348848.0
    ##    BizarroPipe_20()       933      2793.0 1.365327e+04      3135.5
    ##       DotArrow_20()     71670     88752.5 1.234716e+05     96780.5
    ##       magrittr_20()    456209    548567.0 7.025874e+05    601202.5
    ##       TidyPipe_20()   1449365   1612122.0 1.956197e+06   1746227.5
    ##    BizarroPipe_25()      1108      2593.0 1.602300e+04      2959.0
    ##       DotArrow_25()     90575    109082.5 1.578983e+05    117980.0
    ##       magrittr_25()    567666    670062.5 8.588992e+05    733624.0
    ##       TidyPipe_25()   1810243   2015445.5 2.458534e+06   2181966.0
    ##    BizarroPipe_50()      1977      3648.0 2.836560e+04      4005.5
    ##       DotArrow_50()    178338    211978.0 3.181890e+05    229723.5
    ##       magrittr_50()   1095177   1271808.5 1.609811e+06   1368693.0
    ##       TidyPipe_50()   3695252   4047442.5 4.860431e+06   4335901.5
    ##   BizarroPipe_100()      3747      5619.0 5.799286e+04      6006.5
    ##      DotArrow_100()    363029    428630.0 6.226110e+05    467099.5
    ##      magrittr_100()   2146549   2443022.0 3.167060e+06   2612558.5
    ##      TidyPipe_100()   7578036   8406441.0 1.001283e+07   9016541.5
    ##   BizarroPipe_200()      7296      9405.5 1.212623e+05      9908.0
    ##      DotArrow_200()    748345    908387.0 1.293762e+06    975902.0
    ##      magrittr_200()   4283529   4868351.5 6.075153e+06   5232996.5
    ##      TidyPipe_200()  16371135  17895141.5 2.075790e+07  19047060.0
    ##   BizarroPipe_500()     17764     21088.0 2.729626e+05     21557.0
    ##      DotArrow_500()   2187918   2427491.0 3.050825e+06   2566260.0
    ##      magrittr_500()  10663756  12087194.5 1.459132e+07  13224984.5
    ##      TidyPipe_500()  49190881  54638294.5 6.022440e+07  58984792.0
    ##  BizarroPipe_1000()     35311     40418.5 5.639057e+05     40993.0
    ##     DotArrow_1000()   4664676   5086418.0 6.558716e+06   5341756.5
    ##     magrittr_1000()  22003314  24703752.0 2.955337e+07  27977892.5
    ##     TidyPipe_1000() 132999342 145483873.0 1.526779e+08 151929305.0
    ##           uq       max neval
    ##       2151.0   1268353  1000
    ##      12878.0   1549847  1000
    ##     123564.0   2110539  1000
    ##     171944.0   1479901  1000
    ##       2346.5   1908292  1000
    ##      18854.5   2299005  1000
    ##     161709.0   2150558  1000
    ##     282054.0   2397078  1000
    ##       2542.5   3559475  1000
    ##      35994.0   7195352  1000
    ##     257735.0   5801282  1000
    ##     582768.5   4146943  1000
    ##       2865.0   5645097  1000
    ##      63530.0   7756307  1000
    ##     418719.5   7812035  1000
    ##    1114030.5  16735656  1000
    ##       3135.5   7945124  1000
    ##      92641.5  12633368  1000
    ##     568128.5  10597817  1000
    ##    1640941.5  18130302  1000
    ##       3503.0  10499545  1000
    ##     115903.5  14438773  1000
    ##     758026.5  14623217  1000
    ##    2185758.0  16645950  1000
    ##       3318.5  13025408  1000
    ##     153371.5  17728721  1000
    ##     926632.5  22121278  1000
    ##    2742286.5  23128547  1000
    ##       4428.0  24268766  1000
    ##     298828.0  36442295  1000
    ##    1792959.0  40136380  1000
    ##    5619984.0  41135537  1000
    ##       6577.5  51726984  1000
    ##     645563.0  73742020  1000
    ##    3524055.5 109609792  1000
    ##   11363021.0 117788527  1000
    ##      10529.5 111070907  1000
    ##    1298696.5 151673919  1000
    ##    7131093.5 147704377  1000
    ##   22690128.5 149092740  1000
    ##      22297.5 250502705  1000
    ##    3666790.5  11351237  1000
    ##   16727506.0 123085711  1000
    ##   63261025.5 164081074  1000
    ##      42335.0 521124074  1000
    ##    7757298.0 115883484  1000
    ##   32264343.0 137927671  1000
    ##  157734354.5 480277048  1000

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
  scale_color_manual(values = colorAssignment) +
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
  scale_color_manual(values = colorAssignment) +
  xlab('size (number of pipe stages)') +
  ylab("time NS") +
  ggtitle("complexity of runtime as function of method and problem size",
          subtitle = "log/log scale")
```

    ## `geom_smooth()` using method = 'gam'

![](PipePerformance_files/figure-markdown_github/replot-2.png)

``` r
# fit a linear function for runtime as a function of size
# per group.
dfits <- d %.>%
  split(., .$fn) %.>%
  lapply(., 
         function(di) { 
           mi <- lm(time ~ size + I(size*size), data=di) 
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
# "I(size * size)" is where we try to detect super-linear cost,
# check that it is both statistically significant and that 
# it has a size that would affect predictions (is it times
# the typical variation in size*size large?).
print(dfits)
```

    ##         method           coef     Estimate   Std. Error      Pr(>|t|)
    ## 1  BizarroPipe    (Intercept) 3.212098e+03 6.296446e+04  9.593148e-01
    ## 2  BizarroPipe I(size * size) 1.856518e-02 6.832992e-01  9.783247e-01
    ## 3  BizarroPipe           size 5.411272e+02 6.582386e+02  4.110456e-01
    ## 4     DotArrow    (Intercept) 1.754318e+04 2.569601e+04  4.947969e-01
    ## 5     DotArrow I(size * size) 6.994202e-01 2.788567e-01  1.214890e-02
    ## 6     DotArrow           size 5.830933e+03 2.686294e+02 1.657644e-102
    ## 7     magrittr    (Intercept) 1.273818e+05 4.221528e+04  2.554597e-03
    ## 8     magrittr I(size * size) 4.011958e-01 4.581263e-01  3.811933e-01
    ## 9     magrittr           size 2.899771e+04 4.413240e+02  0.000000e+00
    ## 10    TidyPipe    (Intercept) 1.815486e+05 7.297796e+04  1.287005e-02
    ## 11    TidyPipe I(size * size) 6.356617e+01 7.919672e-01  0.000000e+00
    ## 12    TidyPipe           size 8.887253e+04 7.629210e+02  0.000000e+00

``` r
# re-run with non-negative least squares

dfitsnn <- d %.>%
  split(., .$fn) %.>%
  lapply(., 
         function(di) { 
           di$sizesq <- (di$size)^2
           # always call glmnet with a non-trivial lambda series
           # some notes: 
           mi <- glmnet(as.matrix(di[, c('size', 'sizesq')]), 
                        di$time, 
                        lower.limits = 0, 
                        alpha=0.0, 
                        lambda=c(0, 1.0e-5, 1.0e-3, 0.1, 1, 10),
                        intercept = TRUE, 
                        family = 'gaussian')
           ctab <- as.data.frame(as.matrix(coef(mi, s=0)))
           # lower.limites does not apply to intercept,
           # but intercept is always reported even if
           # turned off.
           if(ctab['(Intercept)',1]<0) {
             mi <- glmnet(as.matrix(di[, c('size', 'sizesq')]), 
                        di$time, 
                        lower.limits = 0, 
                        alpha=0.0, 
                        lambda=c(0, 1.0e-5, 1.0e-3, 0.1, 1, 10),
                        intercept = FALSE, 
                        family = 'gaussian')
             ctab <- as.data.frame(as.matrix(coef(mi, s=0)))
           }
           names(ctab) <- "Estimate"
           ctab$coef <- rownames(ctab)
           ctab
         }) %.>%
  add_name_column(., 'method') %.>%
  bind_rows(.) %.>%
  arrange(., method, coef) %.>%
  select(.,  method, coef, Estimate)

print(dfitsnn)
```

    ##         method        coef     Estimate
    ## 1  BizarroPipe (Intercept) 2.826857e+03
    ## 2  BizarroPipe        size 5.485702e+02
    ## 3  BizarroPipe      sizesq 1.110500e-02
    ## 4     DotArrow (Intercept) 1.653898e+04
    ## 5     DotArrow        size 5.850334e+03
    ## 6     DotArrow      sizesq 6.799738e-01
    ## 7     magrittr (Intercept) 1.242702e+05
    ## 8     magrittr        size 2.905782e+04
    ## 9     magrittr      sizesq 3.409400e-01
    ## 10    TidyPipe (Intercept) 1.667088e+05
    ## 11    TidyPipe        size 8.915924e+04
    ## 12    TidyPipe      sizesq 6.327880e+01
