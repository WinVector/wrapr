Differences between magrittr and wrapr pipes
================
John Mount, Win-Vector LLC
4/2/2018

In [`R`](https://www.r-project.org) a non-expert [`magrittr`](https://CRAN.R-project.org/package=magrittr)/[`dplyr`](https://CRAN.R-project.org/package=dplyr) user might expect all examples we are about to discuss to evaluate to `sin(5)` = -0.9589243.

This is not the case, but it is not a bug in `magrittr`. The deviations are things are not as simple or regular as one tends to imagine or remember them being.

`magrittr` examples
-------------------

`magrittr` is described as ["x %&gt;% f is equivalent to f(x)"](https://cran.r-project.org/web/packages/magrittr/README.html). That is a necessary simplification. As we will see below some forms are a bit different (for example `5 %>% {sin}` is not equivalent to `{sin}(5)`).

``` r
library("magrittr")

5 %>% sin
```

    ## [1] -0.9589243

``` r
5 %>% sin()
```

    ## [1] -0.9589243

``` r
5 %>% sin(.)
```

    ## [1] -0.9589243

``` r
5 %>% base::sin
```

    ## Error in .::base: unused argument (sin)

``` r
5 %>% base::sin()
```

    ## [1] -0.9589243

``` r
5 %>% base::sin(.)
```

    ## [1] -0.9589243

``` r
5 %>% ( sin )
```

    ## [1] -0.9589243

``` r
5 %>% ( sin() )
```

    ## Error in sin(): 0 arguments passed to 'sin' which requires 1

``` r
5 %>% ( sin(.) )
```

    ## Error in eval(rhs, env, env): object '.' not found

``` r
5 %>% { sin }
```

    ## function (x)  .Primitive("sin")

``` r
5 %>% { sin() }
```

    ## Error in sin(): 0 arguments passed to 'sin' which requires 1

``` r
5 %>% { sin(.) }
```

    ## [1] -0.9589243

``` r
5 %>% function(x) { sin(x) }
```

    ## Error: Anonymous functions myst be parenthesized

``` r
5 %>% ( function(x) { sin(x) } )
```

    ## [1] -0.9589243

``` r
5 %>% { function(x) { sin(x) } }
```

    ## function(x) { sin(x) }
    ## <environment: 0x7fd1a7b169f8>

``` r
f <-  function(x) { sin(x) }
5 %>% f
```

    ## [1] -0.9589243

``` r
5 %>% ( substitute(f(), list(f = sin)) )
```

    ## [1] -0.9589243

``` r
5 %>% substitute(f(), list(f = sin))
```

    ## Error in substitute(., f(), list(f = sin)): unused argument (list(f = sin))

``` r
5 %>% { substitute(f(), list(f = sin)) }
```

    ## .Primitive("sin")()

As you see some statements did were not roughly equivalent to `sin(5)`. The issues include the following.

-   `::` is a function, as so many things are in `R`. So `base::sin` is not really the package qualified name for `sin()`, it is actually shorthand for `` `::`("base", "sin") `` which is a function evaluation that performs a look-up. So `5 %>% base::sin` expands to an analogue of `` . <- 5; `::`(., "base", "sin") ``, leading to the observed error message.
-   `()` is `magrittr`'s "evaluate before piping into" notation, so `5 %>% ( sin() )` and `5 %>% ( sin(.) )` both throw an error. However, if there had been a value of "`.`" in our environment then for `5 %>% ( sin(.) )` we would get a different error message or outcome.
-   `{}` is `magrittr`'s "treat the contents as an expression" notation (which is not in fact `magrittr`'s default behavior). Thus `magrittr`'s function evaluation signature alteration transforms are not applied to `5 %>% { sin }` or `5 %>% { sin() }`. And the special convert language object into a function evaluation that powers the `5 %>% ( substitute(f(), list(f = sin)) )` example.

`5 %>% ( substitute(f(), list(f = sin)) )` (adapted from [here](https://cran.r-project.org/web/packages/magrittr/vignettes/magrittr.html)) is a bit of a strange bird. Notice the following presumably related code does not work.

``` r
g <- substitute(f(), list(f = sin))
5 %>% g
```

    ## Error in g(.): could not find function "g"

But the following does work.

``` r
g <- substitute(f(), list(f = sin))
5 %>% ( g )
```

    ## [1] -0.9589243

Again, the above are not `magrittr` bugs, they are just how `magrittr`'s behavior differs from a very regular or naive internalization of `magrittr` rules. However, regularity matters. Regularity is especially important for new users, as you want reasonable variations of what is taught to work so that experimentation is positive and not an exercise in learned helplessness.

`wrapr` examples
----------------

The [`wrapr`](https://CRAN.R-project.org/package=wrapr) package has its own, also not completely regular, rules. Below we show the `wrapr` pipe behaviors, and how they differ both from `magritttr` and simple expectations.

We think `wrapr` piping is very teachable with a few rules and additional admonitions:

-   Think of `a %.>% b` as *approximately* syntactic sugar for `{. <- a; b }`. Insisting that piping merely be such sugar presents a fairly irregular experience to the `R` user as `R`'s control structures and data structures are already fairly irregular.
-   Use explicit dots, i.e. write `5 %.>% sin(.)` and not `5 %.>% sin`.
-   You get some free de-referencing such as in `5 %.>% sin(.)` and function application as in `5 %.>% function(x) { sin(x) }`.
-   Outer parentheses do not change meaning (as is commonly the case outside pipelines, modulo `R`'s visibility controls).
-   Outer braces turn off convenience transforms and safety checking.
-   `wrapr` is grammar in the sense some statements are deliberately not part of the accepted notation.

Here is the same set of examples worked with `wrapr` piping.

``` r
library("wrapr")

5 %.>% sin
```

    ## [1] -0.9589243

``` r
5 %.>% sin()
```

    ## Error in pipe_step.default(pipe_left_arg, pipe_right_arg, pipe_environment, : wrapr::pipe_step.default does not allow direct piping into a no-argument function call expression (such as "sin()", please use sin(.)).

``` r
5 %.>% sin(.)
```

    ## [1] -0.9589243

``` r
5 %.>% base::sin
```

    ## [1] -0.9589243

``` r
5 %.>% base::sin()
```

    ## Error in pipe_step.default(pipe_left_arg, pipe_right_arg, pipe_environment, : wrapr::pipe_step.default does not allow direct piping into a no-argument function call expression (such as "base::sin()", please use base::sin(.)).

``` r
5 %.>% base::sin(.)
```

    ## [1] -0.9589243

``` r
5 %.>% ( sin )
```

    ## [1] -0.9589243

``` r
5 %.>% ( sin() )
```

    ## Error in pipe_step.default(pipe_left_arg, pipe_right_arg, pipe_environment, : wrapr::pipe_step.default does not allow direct piping into a no-argument function call expression (such as "sin()", please use sin(.)).

``` r
5 %.>% ( sin(.) )
```

    ## [1] -0.9589243

``` r
5 %.>% { sin }
```

    ## function (x)  .Primitive("sin")

``` r
5 %.>% { sin() }
```

    ## Error in sin(): 0 arguments passed to 'sin' which requires 1

``` r
5 %.>% { sin(.) }
```

    ## [1] -0.9589243

``` r
5 %.>% function(x) { sin(x) }
```

    ## [1] -0.9589243

``` r
5 %.>% ( function(x) { sin(x) } )
```

    ## [1] -0.9589243

``` r
5 %.>% { function(x) { sin(x) } }
```

    ## function(x) { sin(x) }

``` r
f <-  function(x) { sin(x) }
5 %.>% f
```

    ## [1] -0.9589243

``` r
5 %.>% ( substitute(f(), list(f = sin)) )
```

    ## Error in pipe_impl(pipe_left_arg, pipe_right_arg, pipe_environment, pipe_name): wrapr::pipe does not allow direct piping into certain reserved words or control structures (such as "substitute").

``` r
5 %.>% substitute(f(), list(f = sin))
```

    ## Error in pipe_impl(pipe_left_arg, pipe_right_arg, pipe_environment, pipe_name): wrapr::pipe does not allow direct piping into certain reserved words or control structures (such as "substitute").

``` r
5 %.>% { substitute(f(), list(f = sin)) }
```

    ## .Primitive("sin")()

The error messages are driven by the following:

-   `5 %.>% sin()` is not an allowed `wrapr` notation. The `wrapr` philosophy is not to alter evaluation signatures. If the user declares arguments `wrapr` takes the arguments as specified. The error message is signalling that the statement is not valid `wrapr` grammar (not well formed in terms of `wrapr` rules). Notice the error message suggests the alternate notation `sin(.)`. Similar rules apply for `base::sin()`. Then intent is that outer parenthesis are non-semantic, they do not change change `wrapr` pipe behavior.
-   `5 %.>% { sin }` returns just the `sin` function. This is because `{}` triggers `wrapr`'s "leave the contents alone" behavior. Note that `5 %.>% { base::sin }` returns a function as `wrapr`'s transform rules are not active for code surrounded by braces.
-   Notice `wrapr` does not work with any of the `substitute()` examples. `substitue()` is a meta-programming tool, and we think good advice is not to try to meta-program over it. In the first two cases `wrapr` sees the substitute (that it refuses to work with) and in the third `{}` case `wrapr` does look into the right-hand side as the don't look into `{}` contents rule applies.

`wrapr` is hoping to stay close the principle of least surprise.

The hope is that `wrapr` piping is powerful, useful, and not *too* different than `a %.>% b` being treated as almost syntactic sugar for `{. <- a; b }`.

Strictness
----------

For some operations that are unlikely to work close to reasonable user intent `wrapr` includes checks to warn-off the user. The following shows a few more examples of this "defense of grammar."

``` r
5 %.>% 7
```

    ## Error in pipe_impl(pipe_left_arg, pipe_right_arg, pipe_environment, pipe_name): wrapr::pipe does not allow direct piping into simple values such as class:numeric,  type:double.

``` r
5 %.>% .
```

    ## Error in pipe_impl(pipe_left_arg, pipe_right_arg, pipe_environment, pipe_name): wrapr::pipe does not allow direct piping into "."

``` r
5 %.>% return(.)
```

    ## Error in pipe_impl(pipe_left_arg, pipe_right_arg, pipe_environment, pipe_name): wrapr::pipe does not allow direct piping into certain reserved words or control structures (such as "return").

Throwing errors in these situations is based on the principle that non-signalling errors (often leading to result corruption) are much worse than signalling errors. The return example is an interesting case in point.

Let's first take a look at the effect with `magrittr`. Suppose we were writing a simple function to find the smallest non-trivial positive integer division of an integer. Such a function might work like the following.

``` r
f_base <- function(x) {
  for(i in (1L+seq_len(ceiling(sqrt(x))))) {
    if((x %% i)==0) {
      return(i)
    }
  }
  NA_integer_
}

f_base(37)
```

    ## [1] NA

``` r
f_base(35)
```

    ## [1] 5

Now suppose we try to get fancy and use `i %>% return` instead of `return(i)`. This produces a function that thinks all integer are prime. The reason is: `magrittr` can call the `return()` function, but in this situation `return()` can't manage the control path of the original function.

``` r
f_magrittr <- function(x) {
  for(i in (1L+seq_len(ceiling(sqrt(x))))) {
    if((x %% i)==0) {
      i %>% return
    }
  }
  return(NA_integer_)
}

f_magrittr(37)
```

    ## [1] NA

``` r
f_magrittr(35)
```

    ## [1] NA

Now suppose we tried the same thing with `wrapr` pipe and write `i %>% return(.)`.

``` r
f_wrapr <- function(x) {
  for(i in (1L+seq_len(ceiling(sqrt(x))))) {
    if((x %% i)==0) {
      i %.>% return(.)
    }
  }
  return(NA_integer_)
}

f_wrapr(37)
```

    ## [1] NA

``` r
f_wrapr(35)
```

    ## Error in pipe_impl(pipe_left_arg, pipe_right_arg, pipe_environment, pipe_name): wrapr::pipe does not allow direct piping into certain reserved words or control structures (such as "return").

`wrapr` also can not handle `return()` correctly, but throwing the exception at least shows us when there was an issue.

Conclusion
----------

An obvious down-side of `wrapr` is the excess dots both in the operator and in the evaluation arguments. We strongly feel the extra dots in the evaluation arguments is actually [a good trade in losing some conciseness in exchange for useful explicitness](http://www.win-vector.com/blog/2018/03/r-tip-make-arguments-explicit-in-magrittr-dplyr-pipelines/). We do not consider the extra dot in the pipe operator to be a problem (especially if you [bind the operator to a keyboard shortcut](http://www.win-vector.com/blog/2017/11/rstudio-keyboard-shortcuts-for-pipes/)). If the extra dot in the pipe operator is such a deal-breaker, consider that it could be gotten rid of by copying the pipe operator to your notation of choice (such as executing `` `%>%` <- wrapr::`%.>%` `` or `` `%.%` <- wrapr::`%.>%` `` at the top of your work). However such re-mappings are needlessly confusing and it is best to use the operator glyph that `wrapr` directly supplies.

Package authors have near total authority on package semantics. However package users have power as they can choose which packages they prefer.

We feel `wrapr` piping has many upsides including being fairly regular, and having user configurable `S3` dispatch capabilities (detailed in this formal article [here](https://github.com/WinVector/wrapr/blob/master/extras/wrapr_pipe.pdf)).
