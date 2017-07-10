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
    ##     BizarroPipe_1()      224     1374.0     3000.584     1582.0     1895.0
    ##        DotArrow_1()     3717     6418.0    12062.863     8326.5    12500.0
    ##        magrittr_1()    50312    65395.0   107375.008    85312.0   120950.0
    ##     BizarroPipe_2()      285     1617.0     3828.481     1874.5     2237.5
    ##        DotArrow_2()     7098    11138.0    17480.871    13395.0    18249.5
    ##        magrittr_2()    75269    91045.5   137129.556   112958.0   157870.5
    ##     BizarroPipe_5()      400     1855.5     5198.948     2110.0     2513.0
    ##        DotArrow_5()    16747    23639.5    36028.055    27166.5    33076.5
    ##        magrittr_5()   138650   163343.5   228081.179   189911.0   250294.0
    ##    BizarroPipe_10()      605     2179.5     7753.477     2537.0     2980.0
    ##       DotArrow_10()    34149    45100.5    66847.069    51079.0    59925.5
    ##       magrittr_10()   244337   279283.0   374630.448   317948.5   393869.5
    ##    BizarroPipe_15()      762     2071.5    11545.239     2352.0     2813.5
    ##       DotArrow_15()    54016    67322.5   105121.946    74552.0    86306.0
    ##       magrittr_15()   348138   395087.5   526553.973   437524.5   543987.0
    ##    BizarroPipe_20()      971     2463.5    19952.843     2855.0     3418.5
    ##       DotArrow_20()    71228    88482.0   124505.335    98250.5   112552.5
    ##       magrittr_20()   451704   516358.5   668593.876   570338.0   696255.5
    ##    BizarroPipe_25()     1107     2617.0    20045.347     2966.5     3488.5
    ##       DotArrow_25()    89359   109514.5   159103.100   119995.0   139182.5
    ##       magrittr_25()   556110   630239.5   818280.049   691886.0   857334.5
    ##    BizarroPipe_50()     1967     3489.5    28566.306     3863.0     4419.5
    ##       DotArrow_50()   181417   214834.5   304704.132   236130.0   288375.5
    ##       magrittr_50()  1048641  1208136.5  1538654.395  1305545.0  1671243.5
    ##   BizarroPipe_100()     3733     5468.0    71317.012     5870.0     6574.5
    ##      DotArrow_100()   353504   430405.5   623184.370   470504.0   603389.0
    ##      magrittr_100()  2113159  2367969.0  3053364.153  2530558.0  3352533.0
    ##   BizarroPipe_200()     7218     9478.5   119341.102     9907.0    10630.5
    ##      DotArrow_200()   741178   894144.5  1299342.444   963659.0  1264166.5
    ##      magrittr_200()  4172799  4692202.5  5997901.289  5062331.0  6508388.5
    ##   BizarroPipe_500()    17716    20660.5   273268.549    21273.0    22372.0
    ##      DotArrow_500()  2182032  2429767.5  3146159.832  2615482.5  3465941.0
    ##      magrittr_500() 10633947 11982111.0 14240211.168 13462193.0 15391922.0
    ##  BizarroPipe_1000()    35192    39791.0   631318.523    40598.0    42161.0
    ##     DotArrow_1000()  4595184  5073158.0  6281020.442  5472272.0  7202083.0
    ##     magrittr_1000() 22044441 25621458.0 28611179.824 27223106.0 29519666.5
    ##        max neval
    ##    1295491  1000
    ##    2189031  1000
    ##    5910495  1000
    ##    1879219  1000
    ##    2176748  1000
    ##    2181043  1000
    ##    3034347  1000
    ##    3998917  1000
    ##    5026379  1000
    ##    5162521  1000
    ##    7438299  1000
    ##    7256077  1000
    ##    9100085  1000
    ##   17719805  1000
    ##   13056462  1000
    ##   17000750  1000
    ##   13719427  1000
    ##   13262876  1000
    ##   16839046  1000
    ##   16674835  1000
    ##   19016158  1000
    ##   24457359  1000
    ##   36997513  1000
    ##   34917205  1000
    ##   65075865  1000
    ##   68372797  1000
    ##   69574299  1000
    ##  108778228  1000
    ##  142137236  1000
    ##  144707759  1000
    ##  250883136  1000
    ##   59691751  1000
    ##   87983616  1000
    ##  588172371  1000
    ##   15868854  1000
    ##  100727045  1000

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
  scale_y_log10('time (NS)') +
  scale_x_log10('size (number of pipe stages)') +
  scale_color_manual(values = c(magrittr='#7570b3',
                                DotArrow='#d95f02',
                                BizarroPipe='#1b9e77')) +
  xlab("method") +
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

    ##        method        coef    Estimate  Std. Error      t value
    ## 1 BizarroPipe (Intercept)   1080.5444 62278.56097   0.01735018
    ## 2 BizarroPipe        size    613.1587   188.93411   3.24535701
    ## 3    DotArrow (Intercept)   5446.8735 17664.54518   0.30835063
    ## 4    DotArrow        size   6281.2230    53.58883 117.21142408
    ## 5    magrittr (Intercept) 115736.2879 29700.14544   3.89682563
    ## 6    magrittr        size  28481.9084    90.10116 316.11033776
    ##       Pr(>|t|)
    ## 1 9.861575e-01
    ## 2 1.176250e-03
    ## 3 7.578209e-01
    ## 4 0.000000e+00
    ## 5 9.799139e-05
    ## 6 0.000000e+00
