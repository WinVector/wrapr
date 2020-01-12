Locum
================

The [`wrapr dot arrow
pipe`](https://journal.r-project.org/archive/2018/RJ-2018-042/index.html)
was designed to both be an effective [`R`](https://www.r-project.org)
function application pipe *and* also an experimental test-bed for pipe
effects.

Let’s take a look at implementing a new effect from a user perspective.
The idea we want to implement is delayed evaluation through a collecting
object we call a “locum” or stand-in. The locum is intended to collect
operations without executing them, for later use. This is similar to a
lambda or function abstraction. The [code is now in the `wrapr`
package](https://github.com/WinVector/wrapr/blob/master/R/locum.R), but
could be implemented by any user as it uses only public or semi-public
`wrapr` interfaces.

Let’s start loading the `wrapr` package.

``` r
library(wrapr)
```

We can use the `locum` to collect the operations, and then print them.

``` r
y <- 4
p <- locum() %.>% 
  sin(.) %.>% 
  cos(.) %.>% 
  atan2(., y)

print(p)
```

    ## locum() %.>%
    ##    sin(.) %.>%
    ##    cos(.) %.>%
    ##    atan2(., y)

We can now replace the `locum` with the value we want to apply the
pipeline to.

``` r
5 %.>% p
```

    ## [1] 0.1426252

This yields the same answer as the following function application.

``` r
atan2(cos(sin(5)), 4)
```

    ## [1] 0.1426252

We can also add later intended arguments to the pipeline formatting.

``` r
print(p, 'start' = 4)
```

    ## 4 %.>%
    ##    sin(.) %.>%
    ##    cos(.) %.>%
    ##    atan2(., y)

We can do some fun things, such as combining `locum` pipelines.

``` r
p1 <- locum() %.>% sin(.)
p2 <- locum() %.>% cos(.)

p12 <- p1 %.>% p2
p12
```

    ## locum() %.>%
    ##    sin(.) %.>%
    ##    cos(.)

``` r
4 %.>% p12
```

    ## [1] 0.7270351

``` r
cos(sin(4))
```

    ## [1] 0.7270351

The idea is: `wrapr` dot arrow pipe is designed for expansion through
the `apply_right`, `apply_left` `S3` interfaces, and the
`apply_right_S4` `S4` interface. And users can access the implementation
through the `pipe_impl` function. [This
example](https://github.com/WinVector/wrapr/blob/master/Examples/Locum/Locum.md)
and [the formal
article](https://journal.r-project.org/archive/2018/RJ-2018-042/index.html)
should give users a good place to start.
[`rquery`](https://github.com/WinVector/rquery),
[`rqdatatable`](https://github.com/WinVector/rqdatatable), and
[`cdata`](https://github.com/WinVector/cdata) already use the extension
interfaces to implement interesting features.