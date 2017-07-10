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
    ##     BizarroPipe_1()      226     1451.5     3073.388     1710.0     2020.0
    ##        DotArrow_1()     3399     6311.5    10960.226     8400.0    12234.0
    ##        magrittr_1()    51563    62392.0    96210.794    78167.0   112172.0
    ##     BizarroPipe_2()      301     1655.5     3615.250     1905.5     2286.5
    ##        DotArrow_2()     7417    11005.5    17173.361    13420.5    17945.0
    ##        magrittr_2()    71178    87227.0   126123.348   109092.0   149839.5
    ##     BizarroPipe_5()      413     1776.0     5258.844     2083.0     2490.5
    ##        DotArrow_5()    16564    23923.5    34814.715    26818.5    32473.0
    ##        magrittr_5()   140340   158722.0   210880.070   188033.0   232612.5
    ##    BizarroPipe_10()      586     1872.0     7183.610     2159.5     2649.0
    ##       DotArrow_10()    34786    44012.0    64027.275    49869.5    58536.5
    ##       magrittr_10()   239798   276751.0   360681.928   317073.5   375586.0
    ##    BizarroPipe_15()      747     2068.5    10572.351     2427.0     2865.5
    ##       DotArrow_15()    50390    67274.0    94111.351    73584.0    83614.5
    ##       magrittr_15()   343770   386401.5   508935.113   424565.5   508126.5
    ##    BizarroPipe_20()      941     2200.5    12900.858     2560.5     3085.0
    ##       DotArrow_20()    70570    86628.5   122932.077    95550.0   108929.0
    ##       magrittr_20()   444640   507337.0   653964.198   554364.5   653730.0
    ##    BizarroPipe_25()     1101     2464.0    16581.396     2810.0     3316.5
    ##       DotArrow_25()    88728   106344.5   161903.129   117039.5   137180.5
    ##       magrittr_25()   553256   621487.0   780145.463   677510.0   813299.0
    ##    BizarroPipe_50()     1991     3550.0    30843.050     3949.0     4542.5
    ##       DotArrow_50()   175679   205205.0   289839.507   225568.5   261596.0
    ##       magrittr_50()  1068384  1201436.5  1499192.711  1289780.5  1557395.0
    ##   BizarroPipe_100()     3715     5341.5    57339.328     5842.0     6547.0
    ##      DotArrow_100()   354255   417899.0   594851.072   452806.5   552232.0
    ##      magrittr_100()  2126916  2343174.0  3066243.276  2490463.0  3083770.0
    ##   BizarroPipe_200()     7254     9040.0   110353.522     9556.0    10359.5
    ##      DotArrow_200()   727334   870134.0  1226515.314   950500.0  1191301.5
    ##      magrittr_200()  4256147  4709369.0  5830680.017  5040547.5  6219399.5
    ##   BizarroPipe_500()    17689    20423.5   285866.857    21215.0    22358.5
    ##      DotArrow_500()  2207562  2408531.0  2957094.314  2553525.5  3353286.0
    ##      magrittr_500() 10670267 11961561.0 13941130.201 13374642.0 15067881.5
    ##  BizarroPipe_1000()    35055    39647.5   586863.439    40430.5    42015.5
    ##     DotArrow_1000()  4722164  5150269.0  6360691.370  5426054.0  6948660.0
    ##     magrittr_1000() 22350755 25585482.0 28207694.217 27239544.0 29197504.0
    ##        max neval
    ##    1327990  1000
    ##    1325586  1000
    ##    1661000  1000
    ##    1651533  1000
    ##    2475916  1000
    ##    2451826  1000
    ##    3140186  1000
    ##    4081814  1000
    ##    4513917  1000
    ##    4949999  1000
    ##    7315646  1000
    ##    7053937  1000
    ##    8108798  1000
    ##   14359380  1000
    ##   10488140  1000
    ##   10195019  1000
    ##   13676048  1000
    ##   17272650  1000
    ##   13649410  1000
    ##   19765553  1000
    ##   21657400  1000
    ##   26728284  1000
    ##   33481564  1000
    ##   37005550  1000
    ##   51266300  1000
    ##   70225446  1000
    ##  108386743  1000
    ##   98727860  1000
    ##  141129422  1000
    ##  149585199  1000
    ##  263803977  1000
    ##   10396681  1000
    ##   67986923  1000
    ##  544726576  1000
    ##   73819285  1000
    ##   96596682  1000

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
    ctab
  }) %.>%
  add_name_column(., 'method') %.>%
  bind_rows(.) %.>%
  arrange(., method, coef) %.>%
  select(.,  method, coef, Estimate, `Std. Error`,  `t value`, `Pr(>|t|)`)

# "Intercept" is roughly start-up cost 
# "size" is roughly the slope or growth rate of execution time
# as a function of number of pipe stages.
print(dfits)
```

    ##        method        coef    Estimate  Std. Error      t value
    ## 1 BizarroPipe (Intercept)    715.3056 58787.48847   0.01216765
    ## 2 BizarroPipe        size    581.8819   178.34327   3.26270720
    ## 3    DotArrow (Intercept) -15495.5141 19544.46333  -0.79283395
    ## 4    DotArrow        size   6286.7531    59.29193 106.03050754
    ## 5    magrittr (Intercept)  99423.0080 29018.69332   3.42617108
    ## 6    magrittr        size  28054.3596    88.03384 318.67698168
    ##       Pr(>|t|)
    ## 1 0.9902920618
    ## 2 0.0011066198
    ## 3 0.4278902489
    ## 4 0.0000000000
    ## 5 0.0006142065
    ## 6 0.0000000000
