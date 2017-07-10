Some timings for [`%.>%`](http://www.win-vector.com/blog/2017/07/in-praise-of-syntactic-sugar/) ("dot arrow").

Keep in mind for any *serious* application the calculation time on data will far dominate any piping overhead, but it is fun to look.

So we will compare:

-   `magrittr*` `magrittr::%>%` piping.
-   `DotArrow*` `wrapr::%.>%` piping.
-   `BizarroPipe*` `->.;` piping.

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
    ##     BizarroPipe_1()      223     1450.5     3042.800     1704.0     2015.0
    ##        DotArrow_1()     3814     6541.5    12424.995     8772.0    12853.0
    ##        magrittr_1()    51479    62541.0   101455.019    83377.5   125001.0
    ##     BizarroPipe_2()      291     1554.0     3889.358     1800.0     2259.5
    ##        DotArrow_2()     6784    11079.5    17550.867    13644.0    19006.0
    ##        magrittr_2()    75767    90329.0   142984.105   117321.0   158600.0
    ##     BizarroPipe_5()      403     1780.5     5212.444     2058.5     2503.0
    ##        DotArrow_5()    17151    24345.5    35926.211    28151.0    34415.5
    ##        magrittr_5()   137932   162278.0   239053.456   197304.5   251206.5
    ##    BizarroPipe_10()      591     1885.0     8852.037     2201.0     2738.5
    ##       DotArrow_10()    33964    44462.0    64763.019    49750.0    59228.0
    ##       magrittr_10()   242076   278208.5   368409.124   311659.0   380460.0
    ##    BizarroPipe_15()      757     2233.5    13420.592     2538.0     2981.0
    ##       DotArrow_15()    52322    66608.0    94327.757    74315.5    84371.5
    ##       magrittr_15()   343500   395121.5   523396.727   439235.0   533167.5
    ##    BizarroPipe_20()      924     2286.0    16449.091     2604.0     3091.5
    ##       DotArrow_20()    69623    86461.0   124867.739    95763.5   110531.0
    ##       magrittr_20()   455972   514032.5   668856.030   564054.5   680252.0
    ##    BizarroPipe_25()     1103     2582.5    14843.743     2973.5     3507.5
    ##       DotArrow_25()    86934   107146.0   157891.106   116907.5   136437.5
    ##       magrittr_25()   554952   632000.0   873522.579   697587.5   853869.0
    ##    BizarroPipe_50()     1969     3430.5    34512.485     3780.5     4322.5
    ##       DotArrow_50()   173835   212084.5   324047.065   231036.5   274433.0
    ##       magrittr_50()  1084363  1220441.0  1567185.864  1303327.5  1677804.5
    ##   BizarroPipe_100()     3726     5522.5    65718.201     5938.5     6686.0
    ##      DotArrow_100()   347799   420497.0   604768.874   458170.0   559596.0
    ##      magrittr_100()  2128653  2386251.0  3066232.741  2592979.5  3296936.5
    ##   BizarroPipe_200()     7209     9221.5   109945.228     9731.5    10550.0
    ##      DotArrow_200()   716868   874016.0  1259461.428   954484.0  1272027.5
    ##      magrittr_200()  4209192  4755430.0  6063837.117  5184378.5  6476041.0
    ##   BizarroPipe_500()    17649    20783.0   276567.545    21342.0    22718.5
    ##      DotArrow_500()  2142531  2419576.5  3148125.854  2582553.5  3423126.0
    ##      magrittr_500() 10764596 12092576.0 14363287.174 13653379.0 15512648.5
    ##  BizarroPipe_1000()    35108    39645.0   629563.380    40491.5    42683.0
    ##     DotArrow_1000()  4751244  5193870.5  6484469.055  5628264.0  7324629.0
    ##     magrittr_1000() 22340141 25896045.5 29198092.538 27575497.0 29908192.5
    ##        max neval
    ##    1312662  1000
    ##    1282194  1000
    ##    1815532  1000
    ##    1959195  1000
    ##    1955750  1000
    ##    2009213  1000
    ##    3082939  1000
    ##    3902866  1000
    ##    7921555  1000
    ##    6507407  1000
    ##    8972479  1000
    ##    7590817  1000
    ##   10614314  1000
    ##   10458728  1000
    ##   12260598  1000
    ##   13669700  1000
    ##   14501473  1000
    ##   20160148  1000
    ##   11771805  1000
    ##   23659671  1000
    ##   61862316  1000
    ##   30578136  1000
    ##   40459797  1000
    ##   37373252  1000
    ##   59400940  1000
    ##   72311249  1000
    ##   70872219  1000
    ##   99764489  1000
    ##  136890724  1000
    ##  174935489  1000
    ##  249443844  1000
    ##   65575058  1000
    ##   84600522  1000
    ##  586765055  1000
    ##   64848392  1000
    ##   92018863  1000

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

    ##        method        coef     Estimate  Std. Error       t value
    ## 1 BizarroPipe (Intercept)     35.78478 61925.88670  5.778647e-04
    ## 2 BizarroPipe        size    612.85658   187.86421  3.262232e+00
    ## 3    DotArrow (Intercept)  -9399.41395 18604.47148 -5.052234e-01
    ## 4    DotArrow        size   6453.01708    56.44028  1.143335e+02
    ## 5    magrittr (Intercept) 103716.33958 31861.11817  3.255264e+00
    ## 6    magrittr        size  29010.22635    96.65689  3.001361e+02
    ##      Pr(>|t|)
    ## 1 0.999538940
    ## 2 0.001108476
    ## 3 0.613411231
    ## 4 0.000000000
    ## 5 0.001136012
    ## 6 0.000000000
