UTFArrow
================
Win-Vector LLC
12/16/2017

How to make a UTF arrow operator using [`wrapr`](https://winvector.github.io/wrapr/) and [`addinexamplesWV`](https://github.com/WinVector/addinexamplesWV) (original note [here](http://www.win-vector.com/blog/2017/12/more-pipes-in-r/#comment-66720)).

``` r
library("addinexamplesWV")
op <- paste0('%', intToUtf8(8226), intToUtf8(10132), '%')
assign(op, wrapr::`%.>%`)
options("addinexamplesWV.usrFn1" =
          function() {
            rstudioapi::insertText(paste0(' ', op, ' '))
          })


7 %•➔% sqrt(.)
```

    ## [1] 2.645751
