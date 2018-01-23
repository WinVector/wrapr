
<!-- README.md is generated from README.Rmd. Please edit that file -->
[`wrapr`](https://winvector.github.io/wrapr/), is an [`R`](https://cran.r-project.org) package that supplies powerful tools for writing and debugging `R` code.

![](https://github.com/WinVector/wrapr/raw/master/tools/wraprs.png)

Introduction
------------

Primary `wrapr` services include:

-   `let()` (let block)
-   `%.>%` (dot arrow pipe)
-   `:=` (named map builder)
-   `λ()` (anonymous function builder)
-   `DebugFnW()` (function debug wrappers)

[`let()`](https://winvector.github.io/wrapr/articles/let.html)
--------------------------------------------------------------

`let()` allows execution of arbitrary code with substituted variable names (note this is subtly different than binding values for names as with `base::substitute()` or `base::with()`).

The function is simple and powerful. It treats strings as variable names and re-writes expressions as if you had used the denoted variables. For example the following block of code is equivalent to having written "`a + a`".

``` r
library("wrapr")

a <- 7

let(
  c(VAR = 'a'),
  
  VAR + VAR
)
 #  [1] 14
```

This is useful in re-adapting non-standard evaluation interfaces (NSE interfaces) so one can script or program over them.

We are trying to make `let()` self teaching and self documenting (to the extent that makes sense). For example try the arguments "`eval=FALSE`" prevent execution and see what *would* have been executed, or `debug=TRUE` to have the replaced code printed in addition to being executed:

``` r
let(
  c(VAR = 'a'),
  eval = FALSE,
  {
    VAR + VAR
  }
)
 #  {
 #      a + a
 #  }

let(
  c(VAR = 'a'),
  debugPrint = TRUE,
  {
    VAR + VAR
  }
)
 #  $VAR
 #  [1] "a"
 #  
 #  {
 #      a + a
 #  }
 #  [1] 14
```

Please see `vignette('let', package='wrapr')` for more examples. For working with `dplyr` `0.7.*` we suggest also taking a look at an alternate approach called [`seplyr`](https://github.com/WinVector/seplyr/blob/master/README.md).

[`%.>%` (dot arrow pipe)](https://winvector.github.io/wrapr/articles/dot_pipe.html)
-----------------------------------------------------------------------------------

`%.>%` dot arrow pipe is a strict pipe with intended semantics:

> "`a %.>% b`" is to be treated as if the user had written "`{ . <- a; b };`" with "`%.>%`" being treated as left-associative.

That is: `%.>%` does not alter any function arguments that are not explicitly named. `%.>%` is designed to be explicit and simple.

The effect looks is show below.

The following two expressions should be equivalent:

``` r
cos(exp(sin(4)))
 #  [1] 0.8919465

4 %.>% sin(.) %.>% exp(.) %.>% cos(.)
 #  [1] 0.8919465
```

The notation is quite powerful as it treats pipe stages as expression parameterized over the variable "`.`". This means you do not need to introduce functions to express stages. The following is a valid dot-pipe:

``` r
1:4 %.>% .^2 
 #  [1]  1  4  9 16
```

The notation is also very regular in that expressions have the same iterpretation be then surrounded by parenthesis, braces, or as-is:

``` r
1:4 %.>% { .^2 } 
 #  [1]  1  4  9 16

1:4 %.>% ( .^2 )
 #  [1]  1  4  9 16
```

Regularity can be a *big* advantage in teaching and comprehension. Please see ["In Praise of Syntactic Sugar"](http://www.win-vector.com/blog/2017/07/in-praise-of-syntactic-sugar/) for more details.

[`:=` (named map builder)](https://winvector.github.io/seplyr/articles/named_map_builder.html)
----------------------------------------------------------------------------------------------

`:=` is the "named map builder". It allows code such as the following:

``` r
'a' := 'x'
 #    a 
 #  "x"
```

The important property of named map builder is it accepts values on the left-hand side allowing the following:

``` r
name <- 'variableNameFromElsewhere'
name := 'newBinding'
 #  variableNameFromElsewhere 
 #               "newBinding"
```

A nice property is `:=` commutes (in the sense of algebra or category theory) with `R`'s concatenation function `c()`. That is the following two statements are equivalent:

``` r
c('a', 'b') := c('x', 'y')
 #    a   b 
 #  "x" "y"

c('a' := 'x', 'b' := 'y')
 #    a   b 
 #  "x" "y"
```

The named map builder is [used to make `seplyr` notation much more manageable](https://winvector.github.io/seplyr/articles/named_map_builder.html).

[`λ()` (anonymous function builder)](https://winvector.github.io/wrapr/articles/lambda.html)
--------------------------------------------------------------------------------------------

`λ()` is a concise abstract function creator or "[lambda abstraction](https://en.wikipedia.org/wiki/Lambda_calculus)". It is a placeholder that allows the use of the λ-character for very concise function abstraction.

Example:

``` r
# Make sure lambda function builder is in our enironment.
wrapr::defineLambda()

# square numbers 1 through 4
sapply(1:4, λ(x, x^2))
 #  [1]  1  4  9 16
```

[`DebugFnW()`](https://winvector.github.io/wrapr/articles/DebugFnW.html)
------------------------------------------------------------------------

`DebugFnW()` wraps a function for debugging. If the function throws an exception the execution context (function arguments, function name, and more) is captured and stored for the user. The function call can then be reconstituted, inspected and even re-run with a step-debugger. Please see our [free debugging video series](https://youtu.be/-P9UzQuJSH8?list=PLAKBwakacHbQT51nPHex1on3YNCCmggZA) and `vignette('DebugFnW', package='wrapr')` for examples.

Installing
----------

Install with either:

``` r
install.packages("wrapr")
```

or

``` r
# install.packages("devtools")
devtools::install_github("WinVector/wrapr")
```

Note
----

Note: `wrapr` is meant only for "tame names", that is: variables and column names that are also valid *simple* (without quotes) `R` variables names.
