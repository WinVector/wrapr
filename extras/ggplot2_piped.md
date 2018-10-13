ggplot2\_piped.Rmd
================
John Mount, Win-Vector LLC
1/29/2018

Using [`wrapr`](https://winvector.github.io/wrapr/)'s pipe with `ggplot2` (as in [Why can’t ggplot2 use %&gt;%?](https://community.rstudio.com/t/why-cant-ggplot2-use/4372)).

``` r
library("ggplot2")
library("wrapr")
suppressPackageStartupMessages(library("dplyr"))
```

    ## Warning: package 'dplyr' was built under R version 3.5.1

``` r
apply_left.gg <- function(pipe_left_arg,
                          pipe_right_arg,
                          pipe_environment,
                          left_arg_name,
                          pipe_string,
                          right_arg_name) {
  pipe_right_arg <- eval(pipe_right_arg,
                         envir = pipe_environment,
                         enclos = pipe_environment)
  pipe_left_arg + pipe_right_arg 
}


apply_right.gg <- function(pipe_left_arg,
                           pipe_right_arg,
                           pipe_environment,
                           left_arg_name,
                           pipe_string,
                           right_arg_name) {
  pipe_left_arg + pipe_right_arg 
}

apply_right.labels <- function(pipe_left_arg,
                               pipe_right_arg,
                               pipe_environment,
                               left_arg_name,
                               pipe_string,
                               right_arg_name) {
  pipe_left_arg + pipe_right_arg 
}


line <- geom_line()
title <- ggtitle("piped ggplot2",
                 subtitle = "wrapr")

data.frame(x = 1:20) %.>%
  mutate(., y = cos(3*x)) %.>%
  ggplot(., aes(x = x,  y = y)) %.>%
  geom_point() %.>%
  line %.>%
  title
```

![](ggplot2_piped_files/figure-markdown_github/unnamed-chunk-1-1.png)
