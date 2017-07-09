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
    ##     BizarroPipe_1()      220     1475.0     2913.260     1723.0     1997.5
    ##        DotArrow_1()     3395     6248.0    11846.277     7753.0    12011.5
    ##        magrittr_1()    49822    59387.0    90991.149    72417.0   107088.0
    ##     BizarroPipe_2()      291     1403.0     3307.005     1692.5     2057.5
    ##        DotArrow_2()     6660    10675.5    16415.644    13002.5    17472.5
    ##        magrittr_2()    73774    85035.5   122530.482   102031.0   142637.5
    ##     BizarroPipe_5()      422     1729.0     5217.069     2032.0     2399.5
    ##        DotArrow_5()    16359    23209.0    32260.117    25786.5    30868.0
    ##        magrittr_5()   132876   154240.0   197617.217   176084.0   224851.5
    ##    BizarroPipe_10()      586     1886.5     7490.310     2163.0     2471.5
    ##       DotArrow_10()    34782    44524.5    59602.877    49459.0    57021.0
    ##       magrittr_10()   240267   271197.5   338881.215   304444.5   355260.5
    ##    BizarroPipe_15()      765     2081.0    13146.346     2384.5     2734.5
    ##       DotArrow_15()    50439    65978.5    92628.905    72399.0    82964.5
    ##       magrittr_15()   341735   384861.5   463293.882   420208.5   480203.0
    ##    BizarroPipe_20()      928     2268.5    12824.599     2565.5     2962.5
    ##       DotArrow_20()    69550    84877.0   115202.718    93325.0   104024.0
    ##       magrittr_20()   453060   508771.5   633045.981   550328.0   633579.5
    ##    BizarroPipe_25()     1117     2540.5    15645.421     2883.5     3399.0
    ##       DotArrow_25()    86803   104958.5   149803.575   113546.5   128668.0
    ##       magrittr_25()   547523   617842.5   756415.307   669203.0   755857.5
    ##    BizarroPipe_50()     1987     3377.0    28218.553     3737.0     4296.5
    ##       DotArrow_50()   174093   206088.5   363901.150   223747.0   254984.5
    ##       magrittr_50()  1073495  1201968.5  1436508.111  1279257.5  1463380.0
    ##   BizarroPipe_100()     3732     5190.0    72095.926     5588.5     6260.0
    ##      DotArrow_100()   343713   407538.5   567502.520   438037.5   506514.0
    ##      magrittr_100()  2118065  2369972.0  2819636.247  2492480.0  2855227.0
    ##   BizarroPipe_200()     7222     9052.0   104549.203     9449.0    10213.5
    ##      DotArrow_200()   687663   853362.5  1244634.735   908512.0  1064183.5
    ##      magrittr_200()  4234719  4713119.0  5497701.624  4938846.5  5928159.5
    ##   BizarroPipe_500()    17653    20584.5   286445.928    21233.0    22166.5
    ##      DotArrow_500()  2140367  2413798.0  2877015.734  2542775.0  3207561.5
    ##      magrittr_500() 10607697 11926663.5 13549359.779 12995600.0 14620547.0
    ##  BizarroPipe_1000()    35137    39580.0   570353.346    40332.0    41738.5
    ##     DotArrow_1000()  4564782  5062021.5  6053947.806  5275416.5  6852103.5
    ##     magrittr_1000() 22163183 25100244.0 27392940.854 26890737.5 28307344.5
    ##        max neval
    ##    1221518  1000
    ##    1522716  1000
    ##    1811425  1000
    ##    1606155  1000
    ##    2211588  1000
    ##    2225075  1000
    ##    3094747  1000
    ##    4305413  1000
    ##    4278804  1000
    ##    5337788  1000
    ##    6781066  1000
    ##    7430848  1000
    ##   10730168  1000
    ##   13238880  1000
    ##   10346453  1000
    ##   10207998  1000
    ##   15504194  1000
    ##   13560922  1000
    ##   12729939  1000
    ##   20140134  1000
    ##   16310270  1000
    ##   24412219  1000
    ##   57729821  1000
    ##   34216322  1000
    ##   66396660  1000
    ##   68500501  1000
    ##   66835519  1000
    ##   94854702  1000
    ##  155458168  1000
    ##  141914024  1000
    ##  264452670  1000
    ##    8874836  1000
    ##   68886883  1000
    ##  528628812  1000
    ##   63721323  1000
    ##   85247581  1000

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
  scale_color_brewer(palette = 'Dark2') +
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
  scale_color_brewer(palette = 'Dark2') +
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
# "coef_size" is slope or growth rate, 
# "coef_sizesq" is square term in growth.
print(dfits)
```

    ##        method        coef   Estimate  Std. Error      t value    Pr(>|t|)
    ## 1 BizarroPipe (Intercept)  2403.4505 57536.06286   0.04177294 0.966680402
    ## 2 BizarroPipe        size   567.0983   174.54682   3.24897529 0.001161403
    ## 3    DotArrow (Intercept)  1623.8450 19879.91543   0.08168269 0.934900405
    ## 4    DotArrow        size  5998.5871    60.30959  99.46324528 0.000000000
    ## 5    magrittr (Intercept) 61600.1986 24491.38908   2.51517782 0.011910196
    ## 6    magrittr        size 27261.2653    74.29939 366.91104062 0.000000000
