Differences between magrittr and wrapr pipes
================
John Mount, Win-Vector LLC
4/3/2018

Let's consider piping in [`R`](https://www.r-project.org) both using the [`magrittr`](https://CRAN.R-project.org/package=magrittr) package and using the [`wrapr`](https://CRAN.R-project.org/package=wrapr) package.

`magrittr`
----------

`magrittr` describes itself as: ["`x %>% f` is equivalent to `f(x)`"](https://cran.r-project.org/web/packages/magrittr/README.html). That is a bit of simplification. As we will see below some forms are a bit different (for example `5 %>% {sin}` is not equivalent to `{sin}(5)`).

`wrapr`
-------

The [`wrapr`](https://CRAN.R-project.org/package=wrapr) package's ["dot pipe" "`%.>%`"](https://winvector.github.io/wrapr/reference/grapes-.-greater-than-grapes.html) has its own, also not completely regular, rules. Below we show the `wrapr` pipe behaviors, and how they differ both from `magritttr` and simple expectations.

We think `wrapr` piping is very teachable expression oriented pipe with a few rules and additional admonitions:

-   Think of `a %.>% b` as *approximately* syntactic sugar for `{. <- a; b }`. Insisting that piping merely be such sugar presents a fairly irregular experience to the `R` user as `R`'s control structures and data structures are already fairly irregular. Notice also we say think in terms of `{. <- a; b }` (value chaining or sequencing), not in terms of `b(a)` (function composition). They are related concepts, but sequencing is closer to what is actually implemented in this pipeline construct. Also note: `{. <- a; b }` is only the default implementation of the `wrapr` `pipe_step()` generic `S3` function. Users can override this function to specify their own composition of objects under their own rules (such as building [pipable `ggplot2` code](https://github.com/WinVector/wrapr/blob/master/extras/ggplot2_piped.md)).
-   Use explicit dots, i.e. write `5 %.>% sin(.)` and not `5 %.>% sin()`. It [good to make it obvious to the reader that "`.`" is a free-name in the right-hand side expression](http://www.win-vector.com/blog/2018/03/r-tip-make-arguments-explicit-in-magrittr-dplyr-pipelines/), allowing the easy application of the convention of treating the right-hand side expression as an implicit function of "`.`".
-   You get some free de-referencing such as in `5 %.>% sin` and function application as in `5 %.>% function(x) { sin(x) }`. This works under the rubric that these expressions are obviously "dot free", so don't make a lot of sense in a pipeline unless we do something additional with them. The additional steps are usually name look-up, function construction/application, or right-argument `S3` dispatch through a function named "`wrapr_function()`" (which asks: which function would you like to stand in for this object). One design principle is while we need exceptions, we try to only apply them where the non-exceptional case does not make sense. For `5 %.>% base::sin` directly is read in `R` would say "apply the `::` look-up function to `sin` after assigning `5` to the variable `.`." As this reading is not useful we trigger a helper rule "de-reference an evaluation of the form `::` before applying pipe rules." In this case `base::sin` returns a function which then can be applied to the value stored in "`.`", simulating `sin(5)`.
-   Outer parentheses do not change meaning (as is commonly the case outside pipelines, modulo `R`'s visibility controls).
-   Outer braces turn off convenience transforms and safety checking. This is compatible with the subtle `R` convention that brace-blocks `{}` are considered more opaque and not as eagerly looked into as parenthesized expressions (one such example can be found [here](https://radfordneal.wordpress.com/2010/08/19/speeding-up-parentheses-and-lots-more-in-r/)).
-   `wrapr` is grammar in the sense some statements are deliberately not part of the accepted notation. Some of the "errors" in the next set of examples are in fact `wrapr` refusing certain pipelines.

Examples
--------

Let's consider the following 16 attempts of writing piped variations of `sin(5)`.
In [`R`](https://www.r-project.org) a non-expert [`magrittr`](https://CRAN.R-project.org/package=magrittr)/[`dplyr`](https://CRAN.R-project.org/package=dplyr) user might expect all the pipe examples we are about to discuss to evaluate to `sin(5)` = -0.9589243. For comparison will work these examples using both `` magrittr::`%>%` `` and [`` wrapr::`%.>%` ``](https://winvector.github.io/wrapr/reference/grapes-.-greater-than-grapes.html).

``` r
library("magrittr")
library("wrapr")
packageVersion("wrapr")
```

    ## [1] '1.4.0'

``` r
library("seplyr")
library("kableExtra")


exprs = c(
  "5 PIPE_GLYPH sin",
  "5 PIPE_GLYPH sin()",
  "5 PIPE_GLYPH sin(.)",
  "5 PIPE_GLYPH base::sin",
  "5 PIPE_GLYPH base::sin()",
  "5 PIPE_GLYPH base::sin(.)",
  "5 PIPE_GLYPH ( sin )",
  "5 PIPE_GLYPH ( sin() )",
  "5 PIPE_GLYPH ( sin(.) )",
  "5 PIPE_GLYPH { sin }",
  "5 PIPE_GLYPH { sin() }",
  "5 PIPE_GLYPH { sin(.) }",
  "5 PIPE_GLYPH function(x) { sin(x) }",
  "5 PIPE_GLYPH ( function(x) { sin(x) } )",
  "5 PIPE_GLYPH { function(x) { sin(x) } }",
  "f <- function(x) { sin(x) } ; 5 PIPE_GLYPH f" )

evals <- data.frame(
  magrittr_expr = gsub("PIPE_GLYPH", 
                            "%>%", 
                            exprs, 
                            fixed = TRUE),
  magrittr_res = NA,
  wrapr_expr = gsub("PIPE_GLYPH", 
                         "%.>%", 
                         exprs, 
                         fixed = TRUE),
  wrapr_res = NA,
  stringsAsFactors = FALSE)

f <- function(expr) {
  r <- tryCatch( 
    eval(parse(text = expr)),
    error = function(e) { 
      e$message
    }
  )
  if((!is.numeric(r)) || (length(r)!=1)) {
    r <- paste(format(r), collapse = " ")
    r <- htmltools::htmlEscape(r)
  }
  r
}

checcol <- function(col) {
  vapply(col, 
         function(vi) {
           is.numeric(vi) && 
             (length(vi)==1) && 
             (abs(vi-sin(5))<1.0e-5)
         }, logical(1))
}

evals$magrittr_res <- lapply(evals$magrittr_expr, f)
evals$magrittr_good <- checcol(evals$magrittr_res)
evals$wrapr_res <- lapply(evals$wrapr_expr, f)
evals$wrapr_good <- checcol(evals$wrapr_res)

table(wrapr_eq_sin5 = evals$wrapr_good)
```

    ## wrapr_eq_sin5
    ## FALSE  TRUE 
    ##     6    10

``` r
table(magrittr_eq_sin5 = evals$magrittr_good)
```

    ## magrittr_eq_sin5
    ## FALSE  TRUE 
    ##     7     9

``` r
table(wrapr_eq_sin5 = evals$wrapr_good, 
      magrittr_eq_sin5 = evals$magrittr_good)
```

    ##              magrittr_eq_sin5
    ## wrapr_eq_sin5 FALSE TRUE
    ##         FALSE     4    2
    ##         TRUE      3    7

``` r
evals %.>%
  mutate_nse(.,
             magrittr_expr =  htmltools::htmlEscape(magrittr_expr),
             magrittr_res = cell_spec(magrittr_res, "html", color = ifelse(magrittr_good, "blue", "red"),
                                      bold = magrittr_good),
             wrapr_expr =  htmltools::htmlEscape(wrapr_expr),
             wrapr_res = cell_spec(wrapr_res, "html", color = ifelse(wrapr_good, "blue", "red"),
                                   bold = wrapr_good)) %.>%
  select_se(., qc(magrittr_expr, magrittr_res,
                  wrapr_expr, wrapr_res)) %.>%
  knitr::kable(., "html", escape = FALSE) %.>%
  kable_styling(., "striped", full_width = FALSE)
```

<table class="table table-striped" style="width: auto !important; margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
magrittr\_expr
</th>
<th style="text-align:left;">
magrittr\_res
</th>
<th style="text-align:left;">
wrapr\_expr
</th>
<th style="text-align:left;">
wrapr\_res
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
5 %&gt;% sin
</td>
<td style="text-align:left;">
<span style=" font-weight: bold;    color: blue;">-0.958924274663138</span>
</td>
<td style="text-align:left;">
5 %.&gt;% sin
</td>
<td style="text-align:left;">
<span style=" font-weight: bold;    color: blue;">-0.958924274663138</span>
</td>
</tr>
<tr>
<td style="text-align:left;">
5 %&gt;% sin()
</td>
<td style="text-align:left;">
<span style=" font-weight: bold;    color: blue;">-0.958924274663138</span>
</td>
<td style="text-align:left;">
5 %.&gt;% sin()
</td>
<td style="text-align:left;">
<span style="     color: red;">wrapr::pipe\_step.default does not allow direct piping into a no-argument function call expression (such as "sin()", please use sin(.)).</span>
</td>
</tr>
<tr>
<td style="text-align:left;">
5 %&gt;% sin(.)
</td>
<td style="text-align:left;">
<span style=" font-weight: bold;    color: blue;">-0.958924274663138</span>
</td>
<td style="text-align:left;">
5 %.&gt;% sin(.)
</td>
<td style="text-align:left;">
<span style=" font-weight: bold;    color: blue;">-0.958924274663138</span>
</td>
</tr>
<tr>
<td style="text-align:left;">
5 %&gt;% base::sin
</td>
<td style="text-align:left;">
<span style="     color: red;">unused argument (sin)</span>
</td>
<td style="text-align:left;">
5 %.&gt;% base::sin
</td>
<td style="text-align:left;">
<span style=" font-weight: bold;    color: blue;">-0.958924274663138</span>
</td>
</tr>
<tr>
<td style="text-align:left;">
5 %&gt;% base::sin()
</td>
<td style="text-align:left;">
<span style=" font-weight: bold;    color: blue;">-0.958924274663138</span>
</td>
<td style="text-align:left;">
5 %.&gt;% base::sin()
</td>
<td style="text-align:left;">
<span style="     color: red;">wrapr::pipe\_step.default does not allow direct piping into a no-argument function call expression (such as "base::sin()", please use base::sin(.)).</span>
</td>
</tr>
<tr>
<td style="text-align:left;">
5 %&gt;% base::sin(.)
</td>
<td style="text-align:left;">
<span style=" font-weight: bold;    color: blue;">-0.958924274663138</span>
</td>
<td style="text-align:left;">
5 %.&gt;% base::sin(.)
</td>
<td style="text-align:left;">
<span style=" font-weight: bold;    color: blue;">-0.958924274663138</span>
</td>
</tr>
<tr>
<td style="text-align:left;">
5 %&gt;% ( sin )
</td>
<td style="text-align:left;">
<span style=" font-weight: bold;    color: blue;">-0.958924274663138</span>
</td>
<td style="text-align:left;">
5 %.&gt;% ( sin )
</td>
<td style="text-align:left;">
<span style=" font-weight: bold;    color: blue;">-0.958924274663138</span>
</td>
</tr>
<tr>
<td style="text-align:left;">
5 %&gt;% ( sin() )
</td>
<td style="text-align:left;">
<span style="     color: red;">0 arguments passed to 'sin' which requires 1</span>
</td>
<td style="text-align:left;">
5 %.&gt;% ( sin() )
</td>
<td style="text-align:left;">
<span style="     color: red;">wrapr::pipe\_step.default does not allow direct piping into a no-argument function call expression (such as "sin()", please use sin(.)).</span>
</td>
</tr>
<tr>
<td style="text-align:left;">
5 %&gt;% ( sin(.) )
</td>
<td style="text-align:left;">
<span style="     color: red;">object '.' not found</span>
</td>
<td style="text-align:left;">
5 %.&gt;% ( sin(.) )
</td>
<td style="text-align:left;">
<span style=" font-weight: bold;    color: blue;">-0.958924274663138</span>
</td>
</tr>
<tr>
<td style="text-align:left;">
5 %&gt;% { sin }
</td>
<td style="text-align:left;">
<span style="     color: red;">.Primitive("sin")</span>
</td>
<td style="text-align:left;">
5 %.&gt;% { sin }
</td>
<td style="text-align:left;">
<span style="     color: red;">.Primitive("sin")</span>
</td>
</tr>
<tr>
<td style="text-align:left;">
5 %&gt;% { sin() }
</td>
<td style="text-align:left;">
<span style="     color: red;">0 arguments passed to 'sin' which requires 1</span>
</td>
<td style="text-align:left;">
5 %.&gt;% { sin() }
</td>
<td style="text-align:left;">
<span style="     color: red;">0 arguments passed to 'sin' which requires 1</span>
</td>
</tr>
<tr>
<td style="text-align:left;">
5 %&gt;% { sin(.) }
</td>
<td style="text-align:left;">
<span style=" font-weight: bold;    color: blue;">-0.958924274663138</span>
</td>
<td style="text-align:left;">
5 %.&gt;% { sin(.) }
</td>
<td style="text-align:left;">
<span style=" font-weight: bold;    color: blue;">-0.958924274663138</span>
</td>
</tr>
<tr>
<td style="text-align:left;">
5 %&gt;% function(x) { sin(x) }
</td>
<td style="text-align:left;">
<span style="     color: red;">Anonymous functions myst be parenthesized</span>
</td>
<td style="text-align:left;">
5 %.&gt;% function(x) { sin(x) }
</td>
<td style="text-align:left;">
<span style=" font-weight: bold;    color: blue;">-0.958924274663138</span>
</td>
</tr>
<tr>
<td style="text-align:left;">
5 %&gt;% ( function(x) { sin(x) } )
</td>
<td style="text-align:left;">
<span style=" font-weight: bold;    color: blue;">-0.958924274663138</span>
</td>
<td style="text-align:left;">
5 %.&gt;% ( function(x) { sin(x) } )
</td>
<td style="text-align:left;">
<span style=" font-weight: bold;    color: blue;">-0.958924274663138</span>
</td>
</tr>
<tr>
<td style="text-align:left;">
5 %&gt;% { function(x) { sin(x) } }
</td>
<td style="text-align:left;">
<span style="     color: red;">function (x) { sin(x) }</span>
</td>
<td style="text-align:left;">
5 %.&gt;% { function(x) { sin(x) } }
</td>
<td style="text-align:left;">
<span style="     color: red;">function (x) { sin(x) }</span>
</td>
</tr>
<tr>
<td style="text-align:left;">
f &lt;- function(x) { sin(x) } ; 5 %&gt;% f
</td>
<td style="text-align:left;">
<span style=" font-weight: bold;    color: blue;">-0.958924274663138</span>
</td>
<td style="text-align:left;">
f &lt;- function(x) { sin(x) } ; 5 %.&gt;% f
</td>
<td style="text-align:left;">
<span style=" font-weight: bold;    color: blue;">-0.958924274663138</span>
</td>
</tr>
</tbody>
</table>
As you see some statements were not roughly equivalent to `sin(5)`.

Analysis
--------

### `magrittr` Results

The `magrittr` issues include the following.

-   `::` is a function, as so many things are in `R`. So `base::sin` is not really the package qualified name for `sin()`, it is actually shorthand for `` `::`("base", "sin") `` which is a function evaluation that performs a look-up. So `5 %>% base::sin` expands to an analogue of `` . <- 5; `::`(., "base", "sin") ``, leading to the observed error message.
-   `()` is `magrittr`'s "evaluate before piping into" notation, so `5 %>% ( sin() )` and `5 %>% ( sin(.) )` both throw an error. However, if there had been a value of "`.`" in our environment then for `5 %>% ( sin(.) )` we would get a different error message or outcome.
-   `{}` is `magrittr`'s "treat the contents as an expression" notation (which is not in fact `magrittr`'s default behavior). Thus `magrittr`'s function evaluation signature alteration transforms are not applied to `5 %>% { sin }` or `5 %>% { sin() }`.

Again, the above are not `magrittr` bugs, they are just how `magrittr`'s behavior differs from a very regular or naive internalization of `magrittr` rules. However, regularity matters. Regularity is especially important for new users, as you want reasonable variations of what is taught to work so that experimentation is positive and not an exercise in learned helplessness. It is convenient when your tools happen to work the way you might remember.

`wrapr` Results
---------------

The `wrapr` error messages and non-numeric returns are driven by the following:

-   `5 %.>% sin()` is not an allowed `wrapr` notation. The `wrapr` philosophy is not to alter evaluation signatures. If the user declares arguments `wrapr` takes the arguments as specified. The error message is signalling that the statement is not valid `wrapr` grammar (not well formed in terms of `wrapr` rules). Notice the error message suggests the alternate notation `sin(.)`. Similar rules apply for `base::sin()`. Then intent is that outer parenthesis are non-semantic, they do not change change `wrapr` pipe behavior.
-   `5 %.>% { sin }` returns just the `sin` function. This is because `{}` triggers `wrapr`'s "leave the contents alone" behavior. Note that `5 %.>% { base::sin }` returns a function as `wrapr`'s transform rules are not active for code surrounded by braces.

`wrapr` is hoping to stay close the principle of least surprise.

The hope is that `wrapr` piping is easy, powerful, useful, and not *too* different than `a %.>% b` being treated as almost syntactic sugar for `{. <- a; b }`.

The Importance of Strictness
----------------------------

For some operations that are unlikely to work close to reasonable user intent `wrapr` includes checks to warn-off the user. The following shows a few more examples of this "defense of grammar."

``` r
5 %.>% 7
```

    ## Error in pipe_step.default(pipe_left_arg, pipe_right_arg, pipe_environment, : wrapr::pipe_step.default does not allow direct piping into simple values such as class:numeric,  type:double.

``` r
# magrittr's error message for the above is something of the form:
# "Error in function_list[[k]](value) : attempt to apply non-function"

5 %.>% .
```

    ## Error in pipe_step.default(pipe_left_arg, pipe_right_arg, pipe_environment = pipe_environment, : wrapr::pipe_step.default does not allow direct piping into simple values such as class:numeric,  type:double.

``` r
# note: the above error message is improved to:
# "wrapr::pipe does not allow direct piping into '.'"
# in wrapr 1.4.1

5 %.>% return(.)
```

    ## Error in pipe_step.default(pipe_left_arg, pipe_right_arg, pipe_environment, : wrapr::pipe_step.default does not allow direct piping into certain reserved words or control structures (such as "return").

Throwing errors in these situations is based on the principle that non-signalling errors (often leading to result corruption) are much worse than signalling errors. The "`return`" example is an interesting case in point.

Let's first take a look at the effect with `magrittr`. Suppose we were writing a simple function to find for a positive integer returns the smallest non-trivial (greater than `1` *and* less than the value in question) positive integer divisor of the value in question (returning `NA` if there is no such). Such a function might work like the following.

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

Now suppose we try to get fancy and use "`i %>% return`"" instead of "`return(i)`". This produces a function that thinks all integer are prime. The reason is: `magrittr` can call the `return()` function, but in this situation `return()` can't manage the control path of the original function.

``` r
f_magrittr <- function(x) {
  for(i in (1L+seq_len(ceiling(sqrt(x))))) {
    if((x %% i)==0) {
      i %>% return
    }
  }
  NA_integer_
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
  NA_integer_
}

f_wrapr(37)
```

    ## [1] NA

``` r
f_wrapr(35)
```

    ## Error in pipe_step.default(pipe_left_arg, pipe_right_arg, pipe_environment, : wrapr::pipe_step.default does not allow direct piping into certain reserved words or control structures (such as "return").

`wrapr` also can not handle `return()` control flow correctly, and throws an exception to indicate the problem.

### Aesthetics

An obvious down-side of `wrapr` is the excess dots both in the operator and in the evaluation arguments. We strongly feel the extra dots in the evaluation arguments is actually [a good trade in losing some conciseness in exchange for useful explicitness](http://www.win-vector.com/blog/2018/03/r-tip-make-arguments-explicit-in-magrittr-dplyr-pipelines/). We do not consider the extra dot in the pipe operator to be a problem (especially if you [bind the operator to a keyboard shortcut](http://www.win-vector.com/blog/2017/11/rstudio-keyboard-shortcuts-for-pipes/)). If the extra dot in the pipe operator is such a deal-breaker, consider that it could be gotten rid of by copying the pipe operator to your notation of choice (such as executing `` `%>%` <- wrapr::`%.>%` `` or `` `%.%` <- wrapr::`%.>%` `` at the top of your work). However such re-mappings are needlessly confusing and it is best to use the operator glyph that `wrapr` directly supplies.

Conclusion
----------

We feel `wrapr` piping has many upsides including being fairly regular, stricter checking, more user friendly error messages, and having user configurable `S3` dispatch capabilities (detailed in this formal article [here](https://github.com/WinVector/wrapr/blob/master/extras/wrapr_pipe.pdf)).
