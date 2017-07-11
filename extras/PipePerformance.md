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
bm <- eval(cmd)
print(bm)
```

    ## Unit: nanoseconds
    ##                expr       min          lq         mean      median
    ##     BizarroPipe_1()       218      1397.0 3.109475e+03      1653.5
    ##        DotArrow_1()      3238      7599.5 1.274373e+04     12109.5
    ##        magrittr_1()     51069     76941.5 1.148523e+05    114172.5
    ##        TidyPipe_1()     75174     91547.5 1.652012e+05    117884.5
    ##     BizarroPipe_2()       320      1748.5 3.936076e+03      2062.5
    ##        DotArrow_2()      7009     12872.5 2.009926e+04     17551.5
    ##        magrittr_2()     73854    106030.0 1.501026e+05    143907.5
    ##        TidyPipe_2()    148224    176038.0 2.650344e+05    231602.0
    ##     BizarroPipe_5()       443      2137.0 8.440663e+03      2482.0
    ##        DotArrow_5()     17042     26224.5 4.039380e+04     30954.5
    ##        magrittr_5()    140211    186803.5 2.437563e+05    222712.0
    ##        TidyPipe_5()    366313    429333.5 5.845052e+05    509491.5
    ##    BizarroPipe_10()       623      2394.0 8.344731e+03      2739.5
    ##       DotArrow_10()     34869     49264.0 7.343193e+04     55587.5
    ##       magrittr_10()    245946    310488.0 4.154114e+05    354013.5
    ##       TidyPipe_10()    723450    838566.5 1.068695e+06    936349.5
    ##    BizarroPipe_15()       817      2511.5 1.109206e+04      2920.0
    ##       DotArrow_15()     50378     70260.0 1.014334e+05     78234.5
    ##       magrittr_15()    347544    433219.5 5.595113e+05    485352.5
    ##       TidyPipe_15()   1087761   1250994.5 1.702316e+06   1361850.0
    ##    BizarroPipe_20()       927      2722.5 1.583884e+04      3181.5
    ##       DotArrow_20()     71378     90644.5 1.304372e+05     98714.0
    ##       magrittr_20()    457445    551873.0 7.120750e+05    615352.0
    ##       TidyPipe_20()   1449843   1654513.0 2.054775e+06   1814017.5
    ##    BizarroPipe_25()      1144      2980.5 1.638177e+04      3370.5
    ##       DotArrow_25()     89626    109885.0 1.601126e+05    120508.0
    ##       magrittr_25()    558557    677632.5 8.563041e+05    744380.0
    ##       TidyPipe_25()   1833798   2060648.5 2.585369e+06   2260987.5
    ##    BizarroPipe_50()      1973      3760.5 3.314512e+04      4173.5
    ##       DotArrow_50()    175390    211465.5 3.068196e+05    232438.5
    ##       magrittr_50()   1104993   1286926.5 1.664156e+06   1410583.5
    ##       TidyPipe_50()   3742446   4175182.5 5.083042e+06   4598547.0
    ##   BizarroPipe_100()      3712      5702.5 5.363308e+04      6243.5
    ##      DotArrow_100()    344215    425149.0 7.031771e+05    468162.5
    ##      magrittr_100()   2107606   2508324.5 3.325931e+06   2777386.0
    ##      TidyPipe_100()   7726495   8718501.5 1.088712e+07   9518883.0
    ##   BizarroPipe_200()      7275      9484.0 1.297401e+05     10017.0
    ##      DotArrow_200()    736451    885058.0 1.364754e+06    966577.0
    ##      magrittr_200()   4279834   4977490.0 6.386656e+06   5442331.0
    ##      TidyPipe_200()  16723246  18520060.5 2.198805e+07  20122123.0
    ##   BizarroPipe_500()     17810     21170.5 2.972812e+05     21803.0
    ##      DotArrow_500()   2210000   2462117.0 3.148965e+06   2665635.5
    ##      magrittr_500()  10810548  12515482.0 1.548989e+07  14198635.0
    ##      TidyPipe_500()  50019184  56973590.5 6.269668e+07  61106726.5
    ##  BizarroPipe_1000()     35102     40348.5 6.349440e+05     41203.0
    ##     DotArrow_1000()   4660448   5210341.0 6.685229e+06   5720847.0
    ##     magrittr_1000()  22192683  25681100.0 3.088134e+07  29275654.0
    ##     TidyPipe_1000() 135666126 149513024.0 1.589639e+08 156397013.0
    ##           uq       max neval
    ##       2005.0   1371813  1000
    ##      13867.0   1434366  1000
    ##     129594.0   1768695  1000
    ##     183349.0  18963935  1000
    ##       2406.0   1827841  1000
    ##      19988.5   2816118  1000
    ##     167296.0   2174940  1000
    ##     282599.5  19782742  1000
    ##       2874.5   5863130  1000
    ##      36396.0   7013798  1000
    ##     265092.0   4406331  1000
    ##     610405.0  18482301  1000
    ##       3143.5   5536736  1000
    ##      68791.5   9487947  1000
    ##     431857.0  12257768  1000
    ##    1134520.0  16129228  1000
    ##       3345.5   8126805  1000
    ##      95726.5  10677673  1000
    ##     608841.5  13105074  1000
    ##    1706243.0 114707611  1000
    ##       3678.0  12574343  1000
    ##     129267.5  16924244  1000
    ##     770767.0  19133834  1000
    ##    2249966.0  17156906  1000
    ##       3809.5  12966902  1000
    ##     157527.0  20774559  1000
    ##     959189.5  21815994  1000
    ##    2868463.0  18438574  1000
    ##       4627.5  28877616  1000
    ##     318863.0  34361583  1000
    ##    1806746.5  35988623  1000
    ##    5765907.5  37271803  1000
    ##       6951.0  47196303  1000
    ##     679238.0 131264362  1000
    ##    3646445.5 119240549  1000
    ##   11771859.0 124326385  1000
    ##      11251.0 119140479  1000
    ##    1382703.5 183948726  1000
    ##    7258105.0 205958593  1000
    ##   23548920.0 212091694  1000
    ##      24885.0 273875249  1000
    ##    3785857.0  10995095  1000
    ##   17776844.5 119153731  1000
    ##   65611460.5 256307063  1000
    ##      46789.5 590439319  1000
    ##    8360729.5  23072622  1000
    ##   33645249.5 141451526  1000
    ##  163852591.0 321431153  1000

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

    ##         method           coef      Estimate   Std. Error      Pr(>|t|)
    ## 1  BizarroPipe    (Intercept)  3.515498e+03 7.067359e+04  9.603282e-01
    ## 2  BizarroPipe I(size * size)  6.754961e-02 7.669598e-01  9.298189e-01
    ## 3  BizarroPipe           size  5.630833e+02 7.388308e+02  4.459989e-01
    ## 4     DotArrow    (Intercept)  1.871423e+04 2.565297e+04  4.657007e-01
    ## 5     DotArrow I(size * size)  4.842801e-01 2.783897e-01  8.195907e-02
    ## 6     DotArrow           size  6.167833e+03 2.681794e+02 1.408941e-114
    ## 7     magrittr    (Intercept)  1.070088e+05 4.761332e+04  2.462900e-02
    ## 8     magrittr I(size * size) -3.441200e-01 5.167066e-01  5.054320e-01
    ## 9     magrittr           size  3.110195e+04 4.977558e+02  0.000000e+00
    ## 10    TidyPipe    (Intercept)  2.539726e+05 8.564422e+04  3.028469e-03
    ## 11    TidyPipe I(size * size)  6.536292e+01 9.294232e-01  0.000000e+00
    ## 12    TidyPipe           size  9.324256e+04 8.953357e+02  0.000000e+00

``` r
# re-run with non-negative least squares

dfitsnn <- d %.>%
  split(., .$fn) %.>%
  lapply(., 
         function(di) { 
           di$sizesq <- (di$size)^2
           mi <- glmnet(as.matrix(di[, c('size', 'sizesq')]), 
                        di$time, 
                        lower.limits = 0, 
                        alpha=0.1, 
                        lambda=1.e-5,
                        intercept = TRUE, 
                        family = 'gaussian')
           ctab <- as.data.frame(as.matrix(coefficients(mi)))
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
    ## 1  BizarroPipe (Intercept) 6.889660e+02
    ## 2  BizarroPipe        size 6.176928e+02
    ## 3  BizarroPipe      sizesq 1.281390e-02
    ## 4     DotArrow (Intercept) 1.671043e+04
    ## 5     DotArrow        size 6.206547e+03
    ## 6     DotArrow      sizesq 4.454765e-01
    ## 7     magrittr (Intercept) 1.210439e+05
    ## 8     magrittr        size 3.078188e+04
    ## 9     magrittr      sizesq 0.000000e+00
    ## 10    TidyPipe (Intercept) 2.232033e+05
    ## 11    TidyPipe        size 9.383703e+04
    ## 12    TidyPipe      sizesq 6.476707e+01

A note on the rare very slow events with `Bizarro Pipe`. My *guess* is given `Bizarro Pipe` is the only pipe that gets translated into a multi-line/multi-statement/many-visible-assigments function (i.e., more than one expression) that possibly gives it more changes to win the "garbage collection lottery" and pay for everybody's object clean-up. I don't actually know `R`'s `gc()` trigger strategy, so this is just speculation on my part.
