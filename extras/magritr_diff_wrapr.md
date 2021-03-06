magrittr and wrapr Pipes in R, an Examination
================
John Mount, Win-Vector LLC
4/6/2018

Let's consider piping in [`R`](https://www.r-project.org) both using the [`magrittr`](https://CRAN.R-project.org/package=magrittr) package and using the [`wrapr`](https://CRAN.R-project.org/package=wrapr) package.

`magrittr` pipelines
--------------------

The `magittr` pipe glyph "`%>%`" is the most popular piping symbol in `R`.

[`magrittr` documentation](https://cran.r-project.org/web/packages/magrittr/README.html) describes `%>%` as follow.

<blockquote>
<b>Basic piping:</b>
<ul>
<li>
<code>x %&gt;% f</code> is equivalent to <code>f(x)</code>
</li>
<li>
<code>x %&gt;% f(y)</code> is equivalent to <code>f(x, y)</code>
</li>
<li>
<code>x %&gt;% f %&gt;% g %&gt;% h</code> is equivalent to <code>h(g(f(x)))</code>
</li>
</ul>
<b>The argument placeholder</b>
<ul>
<li>
<code>x %&gt;% f(y, .)</code> is equivalent to <code>f(y, x)</code>
</li>
<li>
<code>x %&gt;% f(y, z = .)</code> is equivalent to <code>f(y, z = x)</code>
</li>
</ul>
<b>Re-using the placeholder for attributes</b>
<p>
It is straight-forward to use the placeholder several times in a right-hand side expression. However, when the placeholder only appears in a nested expressions magrittr will still apply the first-argument rule. The reason is that in most cases this results more clean code.
</p>
<p>
<code>x %&gt;% f(y = nrow(.), z = ncol(.))</code> is equivalent to <code>f(x, y = nrow(x), z = nrow(x))</code>
</p>
<p>
The behavior can be overruled by enclosing the right-hand side in braces:
</p>
<p>
<code>x %&gt;% {f(y = nrow(.), z = ncol(.))}</code> is equivalent to <code>f(y = nrow(x), z = nrow(x))</code>
</p>
</blockquote>
That is a bit of simplification, but is the taught mental model.

[Grolemund, Wickham, *R for Data Science*, O'Reilly Media, 2017; "Pipes"](http://r4ds.had.co.nz/pipes.html) describes the `magrittr` pipe as follows.

<blockquote>
<pre>
 foo_foo %&gt;%
   hop(through = forest) %&gt;%
   scoop(up = field_mouse) %&gt;%
   bop(on = head)
</pre>
<p/>
\[...\]
<p/>
The pipe works by performing a “lexical transformation”: behind the scenes, magrittr reassembles the code in the pipe to a form that works by overwriting an intermediate object. When you run a pipe like the one above, magrittr does something like this:
<p/>
<pre>
 my_pipe &lt;- function(.) {
   . &lt;- hop(., through = forest)
   . &lt;- scoop(., up = field_mice)
   bop(., on = head)
 }
 my_pipe(foo_foo)
</pre>
</blockquote>
Roughly they are saying `x %>% f(ARGS)` can be considered shorthand for `{ . <- x; f(., ARGS) }` where the evaluation in question happens in a temporary environment.

A Mental Model for Planning Pipelines
-------------------------------------

To safely and confidently use piping one must eventually know what all of the commonly used related notations mean. For example it is important to know what each of the following evaluate to:

-   `5 %>% sin`: the notation demonstrated in the `magrittr` excerpt.
-   `5 %>% sin()`: possibly the notation one would abstract from the *R for Data Science* excerpt.
-   `5 %>% sin(.)`: the notation [we recommend](http://www.win-vector.com/blog/2018/03/r-tip-make-arguments-explicit-in-magrittr-dplyr-pipelines/) (especially for [the part time `R` user](http://www.win-vector.com/blog/2017/08/more-on-the-part-time-r-user/)).

Also, there are questions of how one pipes into general expressions (instead of names, functions, or partially specified function evaluation signatures).

These may seem like details: but they are the steps required to move from copying code from examples and hoping it works (a state of learned helplessness, especially when simple variations fail) or having an effective (even if approximate) mental model for the operators one has decided to work with and plan over.

`wrapr` pipelines
-----------------

`wrapr` supplies its own piping glyph: ["dot pipe" `%.>%`](https://winvector.github.io/wrapr/reference/grapes-.-greater-than-grapes.html). `wrapr`'s goal is to supply an operator that is a regular and safe with `a %.>% b` being *approximately* [syntactic sugar](https://en.wikipedia.org/wiki/Syntactic_sugar) for `{ . <- a; b }` (with, visible side-effects, i.e. we can actually see the "`.`" assignment happen).

``` r
library("wrapr")

# calculate sin(5)
5 %.>% sin(.)
```

    ## [1] -0.9589243

``` r
# 5 left in dot, a visible side-effect
print(.)
```

    ## [1] 5

``` r
# clear dot, so no later failing example 
# falsely appears to work
rm(list = ".")
```

We think `wrapr` piping is very comprehensible (non-magic) expression oriented pipe with a few rules and additional admonitions:

-   Use explicit dots, i.e. write `5 %.>% sin(.)` and not `5 %.>% sin()` or `5 %.>% sin`. It [good to make it obvious to the reader that "`.`" is a free-name in the right-hand side expression](http://www.win-vector.com/blog/2018/03/r-tip-make-arguments-explicit-in-magrittr-dplyr-pipelines/), allowing the easy application of the convention of treating the right-hand side expression as an implicit function of "`.`".
-   You get some free de-referencing such as in `5 %.>% sin` and function application as in `5 %.>% function(x) { sin(x) }`.
-   Outer parentheses do not change meaning (as is commonly the case outside pipelines, modulo `R`'s visibility controls).
-   Outer braces treat contents as raw statements, turning off `wrapr` convenience transforms and safety checking. This is compatible with the subtle `R` convention that brace-blocks `{}` are considered more opaque and not as eagerly looked into as parenthesized expressions (one such example can be found [here](https://radfordneal.wordpress.com/2010/08/19/speeding-up-parentheses-and-lots-more-in-r/)).
-   `wrapr` is grammar in the sense some statements are deliberately not part of the accepted notation. Some of the "errors" in the next set of examples are in fact `wrapr` refusing certain pipelines.
-   Advanced users can extend `wrapr` by using `R` `S3` methodology to specify their own rules for various classes (such as building [pipable `ggplot2` code](https://github.com/WinVector/wrapr/blob/master/extras/ggplot2_piped.md)). Technical details can be found [here](https://github.com/WinVector/wrapr/blob/master/extras/wrapr_pipe.pdf).

Examples
--------

Let's consider the following attempts of writing piped variations of `sin(5)` in both `magritter` and `wrapr` notations.

``` r
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
  "f <- function(x) { sin(x) }; 5 PIPE_GLYPH f"
  )
```

The point is in a room full of students in a lab setting if you show them "`5 %>% sin`" some of them are going to try variations or have variations from their work that are important to them. This possibly includes: package-qualifying the function name, wrapping expressions in parenthesis, altering arguments, building functions, and retrieving functions from data structures. The pipeline (for convenience) tries to lower the distinctions between expressions, functions, and function names. However the pipeline notation does not completely eliminate the differences.

A non-expert [`magrittr`](https://CRAN.R-project.org/package=magrittr)/[`dplyr`](https://CRAN.R-project.org/package=dplyr) user might expect all the pipe examples we are about to discuss to evaluate to `sin(5)` = -0.9589243. As `R` is routinely used by self-described non-programmers (such as scientists, analysts, and statisticians) the non-expert or [part time <code>R</code> user](http://www.win-vector.com/blog/2017/08/lets-have-some-sympathy-for-the-part-time-r-user/) is a very important class of `R` users (and in fact distinct from beginning `R` users). So how a system meets or misses simplified expectations is quite important in `R`.

To run our examples we will use a fairly involved function `work_examples()` that takes the vector of examples and returns an annotated `data.frame` of evaluation results. For completeness this code is given [here](https://github.com/WinVector/wrapr/blob/master/extras/magritr_diff_wrapr.Rmd), but can be safely skipped when reading this article.

Now we can work our examples, and return the comparison in tabular format.

``` r
work_examples(exprs, sin(5)) %.>%
  knitr::kable(., format = "html", escape = FALSE) %.>%
  column_spec(., 1:4, width = "1.75in") %.>%
  kable_styling(., "striped", full_width = FALSE)
```

<table class="table table-striped" style="width: auto !important; margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
magrittr expr
</th>
<th style="text-align:left;">
magrittr res
</th>
<th style="text-align:left;">
wrapr expr
</th>
<th style="text-align:left;">
wrapr res
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;width: 1.75in; ">
5 %&gt;% sin
</td>
<td style="text-align:left;width: 1.75in; ">
<span style=" font-weight: bold;    color: darkgreen;">-0.959</span>
</td>
<td style="text-align:left;width: 1.75in; ">
5 %.&gt;% sin
</td>
<td style="text-align:left;width: 1.75in; ">
<span style=" font-weight: bold;    color: darkgreen;">-0.959</span>
</td>
</tr>
<tr>
<td style="text-align:left;width: 1.75in; ">
5 %&gt;% sin()
</td>
<td style="text-align:left;width: 1.75in; ">
<span style=" font-weight: bold;    color: darkgreen;">-0.959</span>
</td>
<td style="text-align:left;width: 1.75in; ">
5 %.&gt;% sin()
</td>
<td style="text-align:left;width: 1.75in; ">
0 arguments passed to 'sin' which requires 1
</td>
</tr>
<tr>
<td style="text-align:left;width: 1.75in; ">
5 %&gt;% sin(.)
</td>
<td style="text-align:left;width: 1.75in; ">
<span style=" font-weight: bold;    color: darkgreen;">-0.959</span>
</td>
<td style="text-align:left;width: 1.75in; ">
5 %.&gt;% sin(.)
</td>
<td style="text-align:left;width: 1.75in; ">
<span style=" font-weight: bold;    color: darkgreen;">-0.959</span>
</td>
</tr>
<tr>
<td style="text-align:left;width: 1.75in; ">
5 %&gt;% base::sin
</td>
<td style="text-align:left;width: 1.75in; ">
unused argument (sin)
</td>
<td style="text-align:left;width: 1.75in; ">
5 %.&gt;% base::sin
</td>
<td style="text-align:left;width: 1.75in; ">
<span style=" font-weight: bold;    color: darkgreen;">-0.959</span>
</td>
</tr>
<tr>
<td style="text-align:left;width: 1.75in; ">
5 %&gt;% base::sin()
</td>
<td style="text-align:left;width: 1.75in; ">
<span style=" font-weight: bold;    color: darkgreen;">-0.959</span>
</td>
<td style="text-align:left;width: 1.75in; ">
5 %.&gt;% base::sin()
</td>
<td style="text-align:left;width: 1.75in; ">
0 arguments passed to 'sin' which requires 1
</td>
</tr>
<tr>
<td style="text-align:left;width: 1.75in; ">
5 %&gt;% base::sin(.)
</td>
<td style="text-align:left;width: 1.75in; ">
<span style=" font-weight: bold;    color: darkgreen;">-0.959</span>
</td>
<td style="text-align:left;width: 1.75in; ">
5 %.&gt;% base::sin(.)
</td>
<td style="text-align:left;width: 1.75in; ">
<span style=" font-weight: bold;    color: darkgreen;">-0.959</span>
</td>
</tr>
<tr>
<td style="text-align:left;width: 1.75in; ">
5 %&gt;% ( sin )
</td>
<td style="text-align:left;width: 1.75in; ">
<span style=" font-weight: bold;    color: darkgreen;">-0.959</span>
</td>
<td style="text-align:left;width: 1.75in; ">
5 %.&gt;% ( sin )
</td>
<td style="text-align:left;width: 1.75in; ">
<span style=" font-weight: bold;    color: darkgreen;">-0.959</span>
</td>
</tr>
<tr>
<td style="text-align:left;width: 1.75in; ">
5 %&gt;% ( sin() )
</td>
<td style="text-align:left;width: 1.75in; ">
0 arguments passed to 'sin' which requires 1
</td>
<td style="text-align:left;width: 1.75in; ">
5 %.&gt;% ( sin() )
</td>
<td style="text-align:left;width: 1.75in; ">
0 arguments passed to 'sin' which requires 1
</td>
</tr>
<tr>
<td style="text-align:left;width: 1.75in; ">
5 %&gt;% ( sin(.) )
</td>
<td style="text-align:left;width: 1.75in; ">
object '.' not found
</td>
<td style="text-align:left;width: 1.75in; ">
5 %.&gt;% ( sin(.) )
</td>
<td style="text-align:left;width: 1.75in; ">
<span style=" font-weight: bold;    color: darkgreen;">-0.959</span>
</td>
</tr>
<tr>
<td style="text-align:left;width: 1.75in; ">
5 %&gt;% { sin }
</td>
<td style="text-align:left;width: 1.75in; ">
.Primitive("sin")
</td>
<td style="text-align:left;width: 1.75in; ">
5 %.&gt;% { sin }
</td>
<td style="text-align:left;width: 1.75in; ">
.Primitive("sin")
</td>
</tr>
<tr>
<td style="text-align:left;width: 1.75in; ">
5 %&gt;% { sin() }
</td>
<td style="text-align:left;width: 1.75in; ">
0 arguments passed to 'sin' which requires 1
</td>
<td style="text-align:left;width: 1.75in; ">
5 %.&gt;% { sin() }
</td>
<td style="text-align:left;width: 1.75in; ">
0 arguments passed to 'sin' which requires 1
</td>
</tr>
<tr>
<td style="text-align:left;width: 1.75in; ">
5 %&gt;% { sin(.) }
</td>
<td style="text-align:left;width: 1.75in; ">
<span style=" font-weight: bold;    color: darkgreen;">-0.959</span>
</td>
<td style="text-align:left;width: 1.75in; ">
5 %.&gt;% { sin(.) }
</td>
<td style="text-align:left;width: 1.75in; ">
<span style=" font-weight: bold;    color: darkgreen;">-0.959</span>
</td>
</tr>
<tr>
<td style="text-align:left;width: 1.75in; ">
5 %&gt;% function(x) { sin(x) }
</td>
<td style="text-align:left;width: 1.75in; ">
Anonymous functions myst be parenthesized
</td>
<td style="text-align:left;width: 1.75in; ">
5 %.&gt;% function(x) { sin(x) }
</td>
<td style="text-align:left;width: 1.75in; ">
<span style=" font-weight: bold;    color: darkgreen;">-0.959</span>
</td>
</tr>
<tr>
<td style="text-align:left;width: 1.75in; ">
5 %&gt;% ( function(x) { sin(x) } )
</td>
<td style="text-align:left;width: 1.75in; ">
<span style=" font-weight: bold;    color: darkgreen;">-0.959</span>
</td>
<td style="text-align:left;width: 1.75in; ">
5 %.&gt;% ( function(x) { sin(x) } )
</td>
<td style="text-align:left;width: 1.75in; ">
<span style=" font-weight: bold;    color: darkgreen;">-0.959</span>
</td>
</tr>
<tr>
<td style="text-align:left;width: 1.75in; ">
5 %&gt;% { function(x) { sin(x) } }
</td>
<td style="text-align:left;width: 1.75in; ">
function (x) { sin(x) }
</td>
<td style="text-align:left;width: 1.75in; ">
5 %.&gt;% { function(x) { sin(x) } }
</td>
<td style="text-align:left;width: 1.75in; ">
function (x) { sin(x) }
</td>
</tr>
<tr>
<td style="text-align:left;width: 1.75in; ">
f &lt;- function(x) { sin(x) }; 5 %&gt;% f
</td>
<td style="text-align:left;width: 1.75in; ">
<span style=" font-weight: bold;    color: darkgreen;">-0.959</span>
</td>
<td style="text-align:left;width: 1.75in; ">
f &lt;- function(x) { sin(x) }; 5 %.&gt;% f
</td>
<td style="text-align:left;width: 1.75in; ">
<span style=" font-weight: bold;    color: darkgreen;">-0.959</span>
</td>
</tr>
</tbody>
</table>
As you can see, some statements were not roughly equivalent to `sin(5)`.

One related case to consider is the following (which we run by hand, as it seems to default `knitr` or `kableExtra` `html` styling, note: the "'\\\['" and other formatting errors are an artifacts of `HTML` quoting/rendering, and not part of the expressions):

``` r
c("lst <- list(h = sin); 5 PIPE_GLYPH lst$h",
  "lst <- list(h = sin); 5 PIPE_GLYPH lst$h()",
  "lst <- list(h = sin); 5 PIPE_GLYPH lst$h(.)",
  "lst <- list(h = sin); 5 PIPE_GLYPH lst[['h']]",
  "lst <- list(h = sin); 5 PIPE_GLYPH lst[['h']]()",
  "lst <- list(h = sin); 5 PIPE_GLYPH lst[['h']](.)") %.>%
  work_examples(., sin(5)) %.>%
  knitr::kable(., format = "html", escape = FALSE) %.>%
  column_spec(., 1:4, width = "1.75in") %.>%
  kable_styling(., "striped", full_width = FALSE) %.>%
  fix(.)
```

<table class="table table-striped" style="width: auto !important; margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
magrittr expr
</th>
<th style="text-align:left;">
magrittr res
</th>
<th style="text-align:left;">
wrapr expr
</th>
<th style="text-align:left;">
wrapr res
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;width: 1.75in; ">
lst &lt;- list(h = sin); 5 %&gt;% lst$h
</td>
<td style="text-align:left;width: 1.75in; ">
3 arguments passed to '$' which requires 2
</td>
<td style="text-align:left;width: 1.75in; ">
lst &lt;- list(h = sin); 5 %.&gt;% lst$h
</td>
<td style="text-align:left;width: 1.75in; ">
<span style=" font-weight: bold;    color: darkgreen;">-0.959</span>
</td>
</tr>
<tr>
<td style="text-align:left;width: 1.75in; ">
lst &lt;- list(h = sin); 5 %&gt;% lst$h()
</td>
<td style="text-align:left;width: 1.75in; ">
<span style=" font-weight: bold;    color: darkgreen;">-0.959</span>
</td>
<td style="text-align:left;width: 1.75in; ">
lst &lt;- list(h = sin); 5 %.&gt;% lst$h()
</td>
<td style="text-align:left;width: 1.75in; ">
0 arguments passed to 'sin' which requires 1
</td>
</tr>
<tr>
<td style="text-align:left;width: 1.75in; ">
lst &lt;- list(h = sin); 5 %&gt;% lst$h(.)
</td>
<td style="text-align:left;width: 1.75in; ">
<span style=" font-weight: bold;    color: darkgreen;">-0.959</span>
</td>
<td style="text-align:left;width: 1.75in; ">
lst &lt;- list(h = sin); 5 %.&gt;% lst$h(.)
</td>
<td style="text-align:left;width: 1.75in; ">
<span style=" font-weight: bold;    color: darkgreen;">-0.959</span>
</td>
</tr>
<tr>
<td style="text-align:left;width: 1.75in; ">
lst &lt;- list(h = sin); 5 %&gt;% lst\[\['h'\]\]
</td>
<td style="text-align:left;width: 1.75in; ">
incorrect number of subscripts
</td>
<td style="text-align:left;width: 1.75in; ">
lst &lt;- list(h = sin); 5 %.&gt;% lst\[\['h'\]\]
</td>
<td style="text-align:left;width: 1.75in; ">
<span style=" font-weight: bold;    color: darkgreen;">-0.959</span>
</td>
</tr>
<tr>
<td style="text-align:left;width: 1.75in; ">
lst &lt;- list(h = sin); 5 %&gt;% lst\[\['h'\]\]()
</td>
<td style="text-align:left;width: 1.75in; ">
<span style=" font-weight: bold;    color: darkgreen;">-0.959</span>
</td>
<td style="text-align:left;width: 1.75in; ">
lst &lt;- list(h = sin); 5 %.&gt;% lst\[\['h'\]\]()
</td>
<td style="text-align:left;width: 1.75in; ">
0 arguments passed to 'sin' which requires 1
</td>
</tr>
<tr>
<td style="text-align:left;width: 1.75in; ">
lst &lt;- list(h = sin); 5 %&gt;% lst\[\['h'\]\](.)
</td>
<td style="text-align:left;width: 1.75in; ">
<span style=" font-weight: bold;    color: darkgreen;">-0.959</span>
</td>
<td style="text-align:left;width: 1.75in; ">
lst &lt;- list(h = sin); 5 %.&gt;% lst\[\['h'\]\](.)
</td>
<td style="text-align:left;width: 1.75in; ">
<span style=" font-weight: bold;    color: darkgreen;">-0.959</span>
</td>
</tr>
</tbody>
</table>
.

Analysis
--------

### `magrittr` Results

The `magrittr` exceptions include the following.

-   `::` is a function, as so many things are in `R`. So `base::sin` is not really the package qualified name for `sin()`, it is actually shorthand for `` `::`("base", "sin") `` which is a function evaluation that performs the look-up. So `5 %>% base::sin` expands to an analogue of `` . <- 5; `::`(., "base", "sin") ``, leading to the observed error message.
-   `()` is `magrittr`'s "evaluate before piping into" notation, so `5 %>% ( sin() )` and `5 %>% ( sin(.) )` both throw an error as evaluation is attempted before any alteration of arguments is attempted.
-   `{}` is `magrittr`'s "treat the contents as raw statements" notation (which is not in fact `magrittr`'s default behavior). Thus `magrittr`'s function evaluation signature alteration transforms are not applied to `5 %>% { sin }` or `5 %>% { sin() }`.

Again, the above are not `magrittr` bugs, they are just how `magrittr`'s behavior differs from a very regular or naive internalization of `magrittr` rules. Notice neither of "`()`" nor "`{}`" are neutral notations in `magrittr` (the first adds an extra evaluation, and second switches to an expression mode with fewer substitutions). Also note the above is an argument for preferring "`sin(.)`" to "`sin()`", or "`sin`"; as "`sin(.)`" had the most regular `magrittr` behavior (not changing with the introduction of "`()`", "`{}`", or "`base::`").

Regularity is especially important for [part time users](http://www.win-vector.com/blog/2017/08/lets-have-some-sympathy-for-the-part-time-r-user/), as you want reasonable variations of what is taught to work so that experimentation is positive and not an exercise in learned helplessness. It is convenient when your tools happen to work the way you might remember.

### `wrapr` Results

The `wrapr` error messages and non-numeric returns are driven by the following:

-   `5 %.>% sin()` is not an allowed `wrapr` notation. The `wrapr` philosophy is not to alter evaluation signatures. The error message is signalling that the statement is not valid `wrapr` grammar (not well formed in terms of `wrapr` rules). Notice the error message suggests the alternate notation `sin(.)`. Similar rules apply for `base::sin()`. Then intent is that outer parenthesis are non-semantic, they do not change change `wrapr` pipe behavior.
-   `5 %.>% { sin }` returns just the `sin` function. This is because `{}` triggers `wrapr`'s "leave the contents alone" behavior.

The user only encounters two exceptions in the above variations. The first is "don't write `sin()`", which comes with a clear error message and help ("try `sin(.)`"). The second is "outer `{}` treats its contents as raw statements, turning off transforms and checking.

`wrapr` is hoping to stay close the principle of least surprise.

The hope is that `wrapr` piping is easy, powerful, useful, and not *too* different than `a %.>% b` being treated as almost syntactic sugar for `{ . <- a; b }`.

#### Aesthetics

An obvious down-side of `wrapr` piping is the excess dots both in the operator and in the evaluation arguments. We *strongly* feel the extra dots in the evaluation arguments is actually [a good trade in losing some conciseness in exchange for useful explicitness](http://www.win-vector.com/blog/2018/03/r-tip-make-arguments-explicit-in-magrittr-dplyr-pipelines/). We do not consider the extra dot in the pipe operator to be a problem (especially if you [bind the operator to a keyboard shortcut](http://www.win-vector.com/blog/2017/11/rstudio-keyboard-shortcuts-for-pipes/)). If the extra dot in the pipe operator is such a deal-breaker, consider that it could be gotten rid of by copying the pipe operator to your notation of choice (such as executing `` `%>%` <- wrapr::`%.>%` `` or `` `%.%` <- wrapr::`%.>%` `` at the top of your work). However such re-mappings are needlessly confusing and it is best to use the operator glyph that `wrapr` directly supplies.

Non-function examples
---------------------

We can also try a few simpler expressions, that do not have an explicit function marker such as `sin(.)`.

``` r
c("5 PIPE_GLYPH 1 + .",
  "5 PIPE_GLYPH (1 + .)",
  "5 PIPE_GLYPH {1 + .}") %.>%
  work_examples(., 6) %.>%
  knitr::kable(., format = "html", escape = FALSE) %.>%
  column_spec(., 1:4, width = "1.75in") %.>%
  kable_styling(., "striped", full_width = FALSE)
```

<table class="table table-striped" style="width: auto !important; margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
magrittr expr
</th>
<th style="text-align:left;">
magrittr res
</th>
<th style="text-align:left;">
wrapr expr
</th>
<th style="text-align:left;">
wrapr res
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;width: 1.75in; ">
5 %&gt;% 1 + .
</td>
<td style="text-align:left;width: 1.75in; ">
attempt to apply non-function
</td>
<td style="text-align:left;width: 1.75in; ">
5 %.&gt;% 1 + .
</td>
<td style="text-align:left;width: 1.75in; ">
wrapr::apply\_left.default does not allow piping into obvious concrete right-argument (clearly can't depend on left argument): numeric numeric
</td>
</tr>
<tr>
<td style="text-align:left;width: 1.75in; ">
5 %&gt;% (1 + .)
</td>
<td style="text-align:left;width: 1.75in; ">
non-numeric argument to binary operator
</td>
<td style="text-align:left;width: 1.75in; ">
5 %.&gt;% (1 + .)
</td>
<td style="text-align:left;width: 1.75in; ">
<span style=" font-weight: bold;    color: darkgreen;">6</span>
</td>
</tr>
<tr>
<td style="text-align:left;width: 1.75in; ">
5 %&gt;% {1 + .}
</td>
<td style="text-align:left;width: 1.75in; ">
<span style=" font-weight: bold;    color: darkgreen;">6</span>
</td>
<td style="text-align:left;width: 1.75in; ">
5 %.&gt;% {1 + .}
</td>
<td style="text-align:left;width: 1.75in; ">
<span style=" font-weight: bold;    color: darkgreen;">6</span>
</td>
</tr>
</tbody>
</table>
Some of what caused exceptions above is "`5 %ANYTHING% 1 + .`" is parsed (due to `R`'s operator precedence rules) as "`(5 %ANYTHING% 1) + .`". So without extra grouping notations ("()" or "{}") this is not a well-formed pipeline. With `wrapr` it is safe to add in parenthesis, with `magrittr` one must use `{}` (though this can not be used with `5 %>% {sin}`).

The Importance of Strictness
----------------------------

For some operations that are unlikely to work close to reasonable user intent `wrapr` includes checks to warn-off the user. The following shows a few more examples of this "defense of grammar."

``` r
5 %.>% 7
```

    ## Error: wrapr::apply_left.default does not allow piping into obvious concrete right-argument (clearly can't depend on left argument):
    ##   numeric 
    ##   numeric

``` r
# magrittr's error message for the above is something of the form:
# "Error in function_list[[k]](value) : attempt to apply non-function"

5 %.>% .
```

    ## Error in pipe_impl(pipe_left_arg, pipe_right_arg, pipe_environment, pipe_string): to reduce surprising behavior wrapr::pipe does not allow direct piping into some names, such as .

``` r
# note: the above error message is improved to:
# "wrapr::pipe does not allow direct piping into '.'"
# in wrapr 1.4.1

5 %.>% return(.)
```

    ## Error in apply_left.default(pipe_left_arg, pipe_right_arg, pipe_environment, : to reduce surprising execution behavior wrapr::apply_left.default does not allow direct piping into some expressions (such as "return(.)").

Throwing errors in these situations is based on the principle that non-signalling errors (often leading to result corruption) are much worse than signalling errors. The "`return`" example is an interesting case in point.

Let's first take a look at the effect with `magrittr`. Suppose we were writing a simple function to find for a positive integer returns the smallest non-trivial (greater than `1` *and* less than the value in question) positive integer divisor of the value in question (returning `NA` if there is none such). Such a function might work like the following.

``` r
f_base <- function(x) {
  u <- min(ceiling(sqrt(x)), x-1L)
  i <- 2L
  while(i<=u) {
    if((x %% i)==0) {
      return(i)
    }
    i <- i + 1L
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

Now suppose we try to get fancy and use "`i %>% return`" instead of "`return(i)`". This produces a function that thinks all integer are prime. The reason is: `magrittr` can call the `return()` function, but in this situation `return()` can't manage the control path of the original function.

``` r
f_magrittr <- function(x) {
  u <- min(ceiling(sqrt(x)), x-1L)
  i <- 2L
  while(i<=u) {
    if((x %% i)==0) {
      i %>% return
    }
    i <- i + 1L
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

Now suppose we tried the same thing with `wrapr` pipe and write `i %.>% return(.)`.

``` r
f_wrapr <- function(x) {
  u <- min(ceiling(sqrt(x)), x-1L)
  i <- 2L
  while(i<=u) {
    if((x %% i)==0) {
      i %.>% return(.)
    }
    i <- i + 1L
  }
  NA_integer_
}

f_wrapr(37)
```

    ## [1] NA

``` r
f_wrapr(35)
```

    ## Error in apply_left.default(pipe_left_arg, pipe_right_arg, pipe_environment, : to reduce surprising execution behavior wrapr::apply_left.default does not allow direct piping into some expressions (such as "return(.)").

`wrapr` also can not handle `return()` control flow correctly, however it (helpfully) throws an exception to indicate the problem.

Conclusion
----------

`R` usually has more than one good way to perform tasks. In this case we talked about two methods of building pipelines in `R`: `magrittr` and `wrapr`. There are more methods (some of which are listed [here](https://github.com/WinVector/wrapr/blob/master/extras/wrapr_pipe.pdf)). Our preferred pipe is the `wrapr` dot-pipe, and in the of style academic priority we try to credit alternatives and share fair comparisons (as we have done here). Priority is important to respect (as in: `magrittr` is powerful, popular, came well before, and greatly influences `wrapr` dot-pipe), but it is not monopoly rights (for example: the [public CRAN release/announcement](http://www.win-vector.com/blog/2016/12/using-replyrlet-to-parameterize-dplyr-expressions/#comment-66361) of [`let()`](https://cran.r-project.org/web/packages/wrapr/vignettes/let.html), our popular and still preferred substitution methodology and originally part of `replyr`, predates the public [CRAN release](https://cran.r-project.org/src/contrib/Archive/rlang/)/[announcement](https://blog.rstudio.com/2017/04/13/dplyr-0-6-0-coming-soon/) of `rlang`/`tidyeval` code re-writing methods). In client work we use whatever style is most compatible with the client's work and needs, for example we feel it does not make sense to take a legacy `dplyr` project and attempt to switch the pipe notation late in the game (and one does not want to needlessly mix notations).
