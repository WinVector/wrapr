Macro Substitution in R
================
John Mount
2018-09-05

This note is a *very* cursory overview of some macro-substitution facilities available in [`R`](https://www.r-project.org). I am going to try to put a few of them in context (there are likely more I am missing) and explain why I wrote yet another one ([`replyr::let()`](http://www.win-vector.com/blog/2016/12/parametric-variable-names-and-dplyr/)/[`wrapr::let()`](https://cran.r-project.org/web/packages/wrapr/vignettes/let.html)).

The `R` macro (or code control) facilities we will discuss include (in time order):

-   `base::do.call()` (part of `R` since [at least 1997](https://github.com/wch/r-source/blob/a625016dc706a58d11c75664f6c60d07e5d07b0c/src/main/names.c)).
-   `defmacro()` [Lumley T. "Programmer's Niche: Macros in R", R News, 2001, Vol 1, No. 3, pp 11–13](https://www.r-project.org/doc/Rnews/Rnews_2001-3.pdf).
-   `base::bquote()` ([released in R 1.8.1, November 2003](https://github.com/wch/r-source/blob/5a156a0865362bb8381dcd69ac335f5174a4f60c/doc/NEWS.1)).
-   `gtools::defmacro()` ([gtools 2.0.9, September 2, 2005](https://cran.r-project.org/src/contrib/Archive/gtools/)) from the [`gtools`](https://CRAN.R-project.org/package=gtools) package.
-   `strmacro()` ([gtools 2.1.1, September 23, 2005](https://cran.r-project.org/src/contrib/Archive/gtools/)) from the [`gtools`](https://CRAN.R-project.org/package=gtools) package.
-   `lazyeval` package ([released October 1, 2014](https://cran.r-project.org/src/contrib/Archive/lazyeval/)).
-   `replyr::let()`/`wrapr::let()` ([released December 8th, 2016](https://github.com/WinVector/wrapr/blob/master/extras/wrapr_let.pdf)).
-   `rlang::!!` ([released May 5th, 2017](https://cran.r-project.org/src/contrib/Archive/rlang/)).

Why macros and metaprogramming?
-------------------------------

Many `R` users never need any of `R`'s macro or metaprogramming capabilities. That is because the core language and packages are designed such that such capabilities are not immediately necessary.

For example. A common way to extract a column from a `data.frame` is to use the "`$`" operator:

``` r
d <- data.frame(x = 1:2, y = 3:4)
d$y
```

    ## [1] 3 4

In the above the name "`y`" is captured from the source-code. For another program to use this notation we might need a *macro facility* to substitute in a name coming form another variable. However, `R` supplies the `[[]]` notation which is a *value-oriented* interface, meaning we do not need to resort to macros.

``` r
COLUMNNAME <- "y"
d[[COLUMNNAME]]
```

    ## [1] 3 4

However, not all `R` functions and packages have this discipline of design. In particular some commands such as `base::order()` can be hard to program over (due to its heavy leaning on the "`...`" argument, unlike the [`data.table`](https://CRAN.R-project.org/package=data.table) design of having both a `data.table::setorder()` and a `data.table::setorderv()`).

So roughly in `R` macros and meta-programming are not urgent user-facing problems, until the user attempts to program over an interface that is *only* designed for interactive use (i.e., doesn't accept or have an alternative that accepts values stored in additional variables).

Macros are a bit technical, but when you are painted into a programming corner, you want macros. So let's talk more about macros and metaprogramming.

### Technical defintions

Some technical definitions (which we will expand on and use later).

-   **Macro**: In computer science a macro is "a rule or pattern that specifies how a certain input sequence (often a sequence of characters) should be mapped to a replacement output sequence (also often a sequence of characters) according to a defined procedure" ([source Wikipedia](https://en.wikipedia.org/wiki/Macro_(computer_science))). Macros are most interesting when the input they are working over is program source code (either parsed or not-parsed).
-   **Metaprogramming**: "Metaprogramming is a programming technique in which computer programs have the ability to treat programs as their data" ([source Wikipedia](https://en.wikipedia.org/wiki/Metaprogramming)).
-   **Quasiquotation**: "Quasiquotation is a parameterized version of ordinary quotation where instead of specifying a value exactly some holes are left to be filled in later. A quasiquotation is a template." ["Quasiquotation in lisp", Alan Bawden, 1999](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.309.227).
-   **Referential transparency**: "One of the most useful properties of expressions is that called by Quine referential transparency. In essence this means that if we wish to find the value of an expression which contains a sub-expression, the only thing we need to know about the sub-expression is its value." [Christopher Strachey, "Fundamental Concepts in Programming Languages", Higher-Order and Symbolic Computation, 13, 1149, 2000, Kluwer Academic Publishers](https://www.itu.dk/courses/BPRD/E2009/fundamental-1967.pdf) ([lecture notes written by Christopher Strachey for the International Summer School in Computer Programming at Copenhagen in August, 1967](https://en.wikipedia.org/wiki/Fundamental_Concepts_in_Programming_Languages)).
-   **Non-standard evaluation**: Evaluation that is not referentially transparent, as it may include inspecting and capturing names from code and direct control of both execution and look-up environments. Discussed (but not defined) in [Thomas Lumley, "Standard nonstandard evaluation rules", March 19, 2003](http://developer.r-project.org/nonstandard-eval.pdf).

Macros and metaprogramming are related concepts. Each has variations. For example <code>C</code>-macros are very strict text substitutions performed by a pre-processor in some of the code compilation stages. Whereas <code>Lisp</code> macros operate on <code>Lisp</code> data structures and language objects. When applied to programs both concepts converge to "code that writes code."

Macros and metaprogramming in `R`
---------------------------------

### `base::do.call()`

Sometimes you don't need the full power of macros. The function you are using may already have a powerful enough interface. Or you may be only trying to control the execution of a single function, a task for which `R` already has an excellent tool: `base::do.call()`.

For example if you want to control the call-presentation of a formula passed to `lm()` (a problem discussed [here](http://www.win-vector.com/blog/2018/09/r-tip-how-to-pass-a-formula-to-lm/)) the following code is sufficient.

``` r
# specifications of how to model,
# coming from somewhere else
outcome <- "mpg"
variables <- c("cyl", "disp", "hp", "carb")
dataf <- mtcars

# our modeling effort, 
# fully parameterized!
f <- as.formula(
  paste(outcome, 
        paste(variables, collapse = " + "), 
        sep = " ~ "))
print(f)
```

    ## mpg ~ cyl + disp + hp + carb

``` r
model <- do.call("lm", list(f, data = as.name("dataf")))
print(model)
```

    ## 
    ## Call:
    ## lm(formula = mpg ~ cyl + disp + hp + carb, data = dataf)
    ## 
    ## Coefficients:
    ## (Intercept)          cyl         disp           hp         carb  
    ##   34.021595    -1.048523    -0.026906     0.009349    -0.926863

### `defmacro()`/`gtools::defmacro()`

`defmacro()` was introduced to the `R` community in [a fantastic article by Thomas Lumley in 2001](https://www.r-project.org/doc/Rnews/Rnews_2001-3.pdf), and then later enhanced and distributed as part of the [`gtools` package](https://CRAN.R-project.org/package=gtools) by Thomas Lumley and Gregory R. Warnes.

Macros superficially look a bit like functions: they encapsulate some code. However, instead of running code in an isolated environment (the function evaluation environment) they instead perform as if the user had typed the code in directly. Let's jump ahead in our timeline and use `gtools::defmacro()` to define a macro called "`set_variable`" as follows.

``` r
library("gtools")

set_variable <- defmacro(VARNAME, VARVALUE, 
                         expr = { VARNAME <- VARVALUE })
```

Using such a macro is easy.

``` r
set_variable(x, 7)
print(x)
```

    ## [1] 7

Notice that a macro can interact with the original environment without using escape systems such as `<<-` or direct manipulations of environments. The macro's lack of isolation is both its benefit (it works as if the user had typed the code in the macro) and detriment (in most cases you should prefer the safe isolation of functions). `R` is a bit odd in that what it calls "functions" are closer to [<code>fexpr</code>](https://en.wikipedia.org/wiki/Fexpr)s, which lost popularity as <code>Lisp</code>s moved towards a cleaner differentiation between [applicative order](https://en.wikipedia.org/wiki/Lambda_calculus) functions (functions that are only executed after their arguments are resolved) and macros (arbitrary language structures).

I strongly suggest reading [the original article](https://www.r-project.org/doc/Rnews/Rnews_2001-3.pdf), as it shares a great deal of wisdom and humor. I can't resist stealing a bit of the conclusion from the article.

> While defmacro() has many (ok, one or two) practical uses, its main purpose is to show off the powers of substitute(). Manipulating expressions directly with substitute() can often let you avoid messing around with pasting and parsing strings, assigning into strange places with &lt;&lt;- or using other functions too evil to mention.

We couldn't use `defmacro()` for our particular application (programming over `dplyr 0.5.0`) as the `substitute` function will not substitute the left-hand sides of argument bindings (which is how `dplyr::mutate()` specifies assignment). We demonstrate this below by trying to replace symbols with "<code>x</code>":

``` r
# notice right hand side "B" can be substituted
substitute(
  expr = c(A = B), 
  env = list("B" = "x"))
```

    ## c(A = "x")

``` r
# notice left hand side "A" can't be substituted
substitute(
  expr = c(A = B), 
  env = list("A" = "x"))
```

    ## c(A = B)

The unwillingness of `substitute()` to replace left-hand sides of argument bindings is likely intentional. For standard function arguments (those that are not "`...`") such as the `x` in `sin(x)`, it does not make sense to substitute names. In such cases the programmer would usually know the name of the argument as they wrote the code. The following example, where only the right-hand side of bindings is replaced, is convenient and cuts down on some of the confusion sowed by the "<code>x = x</code>" notation.

``` r
substitute(sin(x = x), env = list(x = 7))
```

    ## sin(x = 7)

``` r
eval(substitute(sin(x = x), env = list(x = 7)))
```

    ## [1] 0.6569866

As `dplyr::mutate()` uses argument binding in "`...`" to denote assignments, we want control of both sides of such sub-expressions. So we need to look forward a bit more for solutions.

### `base::bquote()`

On August 15, 2003 Thomas Lumley contributed a fairly complete version of <code>Lisp</code>'s quasi-quotation (please see ["Quasiquotation in lisp", Alan Bawden, 1999](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.309.227)) system or ["the backquote"](http://cl-cookbook.sourceforge.net/macros.html) to `R` itself.

    commit 6af5ee3fab4a347accc61a96185921320547c1e6   refs/remotes/origin/R-grid
    Author: tlumley <tlumley@00db46b3-68df-0310-9c12-caf00c1e9a41>
    Date:   Fri Aug 15 18:46:06 2003 +0000

        add bquote for partial substitution
        
        
        git-svn-id: https://svn.r-project.org/R/trunk@25744 00db46b3-68df-0310-9c12-caf00c1e9a41

`R`'s syntax is different, so `R` uses the notation "<code>.(NAME)</code>" instead of "<code>\`NAME</code>", but the concepts are related to the back-tick or quasi-quotation ideas discussed in the Bawden paper.

`bquote()` has a fairly concise description (from `help(bquote)`):

> An analogue of the <code>Lisp</code> backquote macro. bquote quotes its argument except that terms wrapped in .() are evaluated in the specified <code>where</code> environment.

We can demonstrate the idea as follows. First we can use `bquote()` to quote a user supplied expression, preventing its execution.

``` r
x <- 7
A <- as.name("x")
bquote( A == 5 )
```

    ## A == 5

Now we can use the "`.()`" notation to turn off-quoting for portions of the expression.

``` r
bquote( .(A) == 5 )
```

    ## x == 5

And finally we can execute the altered expression with `eval()`.

``` r
eval(bquote( .(A) == 5 ))
```

    ## [1] FALSE

We used the above pattern in our article ["R tip: How to Pass A formula to lm"](http://www.win-vector.com/blog/2018/09/r-tip-how-to-pass-a-formula-to-lm/).

There is one limitation: due to `R`'s syntax rules `bquote()` style notation can not substitute on the left-hand side of an "`=`" expression. This means we can not use it to control the following sort of expression.

``` r
bquote( list(.(A) = 5) )
```

    ## Error: <text>:1:19: unexpected '='
    ## 1: bquote( list(.(A) =
    ##                       ^

Note the above isn't `bquote()`'s fault, the wrapping syntax just isn't legal on the left-side of an "<code>=</code>" in this case. Notice even an `R` function that does not use its argument runs into the same issue.

``` r
fnull <- function(x) {}

fnull( list(.(A) = 5) )
```

    ## Error: <text>:3:18: unexpected '='
    ## 2: 
    ## 3: fnull( list(.(A) =
    ##                     ^

The problem is this sort of notation is used a lot for argument binding, and in the popular [`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html) function (though in `dplyr 0.7.0` and beyond there is a substitute notation "`:=`" available, a notation idea that the [<code>data.table</code>](https://CRAN.R-project.org/package=data.table) package has used for quite some time.). One can work around the issue as we show below.

``` r
suppressPackageStartupMessages(library("dplyr"))

NEWVAR <- as.name("y")
OLDVAR <- as.name("x")

eval(bquote(
  
  data.frame(x = 1) %>%
    mutate(.(NEWVAR) := .(OLDVAR) + 1)
  
))
```

    ##   x y
    ## 1 1 2

One can even try to get clever about block and function notation, yielding notations such as the following (working the last example again).

``` r
# special code to make string to name maps
map_names <- function(...) {
  lapply(substitute(list(...))[-1], as.name)
}

# special block executing operator
# "A %in_block% { B }" executes B under bquote rules taking name substitutions from A
`%in_block%` <- function(a, b) {
  env = parent.frame()
  eval(do.call(bquote,
               list(substitute(b), 
                    where = a),
               envir = env),
       envir = env)
}

suppressPackageStartupMessages(library("dplyr"))

# old example in new block notation
map_names(
  NEWVAR = "y",
  OLDVAR = "x"
) %in_block% {
  data.frame(x = 1) %>%
    mutate(.(NEWVAR) := .(OLDVAR) + 1)
}
```

    ##   x y
    ## 1 1 2

In the above example mappings possibilities are set in the `map_names()` portion of the command and realized in the "`{}`"-block following `%in_block%`. This double denotation is something we will eliminate later using `wrapr::let()`.

`bquote()` is indeed a remarkable demonstration of the power of `R`'s <code>substitute</code> facility. The code for `bquote()` (accessible by executing `print(bquote)`) is amazingly compact. It is an example of where, with the right abstraction, an essentially 10-line function can do the job of an entire package (it is also hard to imagine a 10-line program having over 100 open issues, which can happen with large package solutions). This is a common situation in functional programming languages. This is part of why design, criticism, and iteration of design are so important in functional programming. Also this compactness is fairly common when you stay out of the way of <code>eval</code>/<code>apply</code> and let them do the heavy lifting. The relations between <code>eval</code> and <code>apply</code> are so fundamental that Alan Kay called it "Maxwell's Equations of Software": ["A conversation with Alan Kay", ACMqueue, Volume 2, Issue 9, December 27, 2004](https://queue.acm.org/detail.cfm?id=1039523), see also ["Lisp as the Maxwell’s equations of software", Michael Nielson April 11, 2012](http://www.michaelnielsen.org/ddi/lisp-as-the-maxwells-equations-of-software/).

### An aside

At this point notice how generous Thomas Lumley has been to the `R` community. He took the time to teach macros, ported a variation of the concept into base-`R`, and then added variations of the macro facility to a public open-source package. This is the kind of non-empire building (for "empire" please see [Bill Venables, "<code>R</code>: *Quo Vadis?*", *UseR! 2012*, Nashville, Tn, USA](http://biostat.mc.vanderbilt.edu/wiki/pub/Main/UseR-2012/VenablesRQuoVadis.pdf)) and giving back to the larger community that keeps `R` vital. Another example is [Matt Dowle and Arun Srinivasan doing so much to improve sorting in base-`R`](http://www.win-vector.com/blog/2018/08/timings-of-a-grouped-rank-filter-task/).

### `gtools::strmacro()`

Gregory R. Warnes added `strmacro()` to the `gtools` package to supply an additional macro facility. Let's demonstrate `strmacro()`.

``` r
library("gtools")

mul <- strmacro(A, B, expr={A*B})
x <- 7
y <- 2
mul("x", "y")
```

    ## [1] 14

Notice the macro arguments are now passed in as strings *and* substitution is performed by matching target names, not by annotations.

`strmacro` can in fact replace left-hand sides of argument bindings.

``` r
library("gtools")

A <- "AAA"
B <- "BBB"
x <- "xxx"

# notice right hand side "B" can be substituted 
# with name x, which evaluates to value "xxx".
strmacro(
  B,  # define substitution target
  expr = c(A = B))("x")
```

    ##     A 
    ## "xxx"

``` r
# notice left hand side "A" can be substituted
# with name x, which is used as a name in the vector expression.
strmacro(
  A,  # define substitution target
  expr = c(A = B))("x")
```

    ##     x 
    ## "BBB"

`strmacro` happens to be inconvenient for our particular application (executing a `dplyr 0.5.0` pipeline) due to the need to explicitly execute the macro and the quoting. However, `strmacro` supplied good ideas for the initial implementation of `let()` (which has since evolved quite a bit) and we found studying it and `defmacro()` to be very rewarding.

### `lazyeval`

`dplyr 0.5.0`'s parametric programming interface was a package called [`lazyeval`](https://CRAN.R-project.org/package=lazyeval) by Hadley Wickham which describes itself as:

> An alternative approach to non-standard evaluation using formulas. Provides a full implementation of LISP style 'quasiquotation', making it easier to generate code with other code.

The `lazyeval` vignettes give some details ([here](https://cran.r-project.org/web/packages/lazyeval/vignettes/lazyeval-old.html) and [here](https://cran.r-project.org/web/packages/lazyeval/vignettes/lazyeval.html)). However, contrary to these documents I do not find the method as flexible as `bquote()`, and did not find `lazyeval::lazy()` to cover nearly as many situations as `base::substitute()`. Trying to use the `lazyeval` methodology to re-work earlier `bquote()` examples of this article was not rewarding.

`lazyeval` seems to incorporate a design principle that there is merit in carrying around a variable name plus an environment where that name is resolved to a value. That is at best a point of view. In my opinion, especially in the context of quasiquotation, it is *much* better to convert bound names to values. The basic principle of computation is a bunch of code and data eventually becomes a result value. `quasiquotation` is the partial substitution of values into otherwise quoted structures; it represents forward progress in turning a mixture of code and values into a final value. Obviously things are a bit more delicate in `R` as names are observable at various points. However, designing centering on the detail of names is, in my opinion, back-porting complexity from interfaces instead of forward porting clarity from applications.

It is my impression that `lazyeval` isn't currently recommended by its authors, so we will not belabor the point.

### `wrapr::let()`

For some code-rewriting tasks we found both `substitute()` and `bquote()` a bit limiting. For this reason we (John Mount and Nina Zumel) developed `wrapr::let()`, taking inspiration from `gtools::strmacro()`. `wrapr::let()` was designed to have syntax similar to `substitute()` and also to classic <code>Lisp</code> "<code>let</code>" style value-binding blocks (informal example: "<code>(let X be 7 in (sin X))</code>").

Our first application of `let()` (at the time in the [`replyr`](https://CRAN.R-project.org/package=replyr) package) was to perform parametric programming over `dplyr 0.5.0` (the version of `dplyr` current at the time).

We have shared a number of articles and gave public talks on the topic.

-   [Using replyr::let to Parameterize dplyr Expressions](http://www.win-vector.com/blog/2016/12/using-replyrlet-to-parameterize-dplyr-expressions/).
-   [help(let, package=’replyr’)](http://www.win-vector.com/blog/2016/12/helplet-packagereplyr/).
-   [My recent BARUG talk: Parametric Programming in R with replyr](http://www.win-vector.com/blog/2017/02/my-recent-barug-talk-parametric-programming-in-r-with-replyr/).
-   [Does replyr::let work with data.table?](http://www.win-vector.com/blog/2016/12/does-replyrlet-work-with-data-table/)
-   [Comparative examples using replyr::let](Comparative%20examples%20using%20replyr::let)
-   [Evolving R Tools and Practices](http://www.win-vector.com/blog/2017/02/evolving-r-tools-and-practices/)

Other contributors worked out additional applications for `let()` including using it to control parameterized `R`-markdown.

We can work one of our `bquote()` section examples as follows.

``` r
library("wrapr")
suppressPackageStartupMessages(library("dplyr"))

NEWVAR <- as.name("y")
OLDVAR <- as.name("x")

let(
  alias = c(NEWVAR = "y",
            OLDVAR = "x"),
  
  data.frame(x = 1) %>%
    mutate(NEWVAR = OLDVAR + 1)
  
)
```

    ##   x y
    ## 1 1 2

Notice `wrapr::let()` specifies substitution targets by name (not by any decorating notation such as backtick or `.()`), and also can use the original "="-notation without problem. `let()` only allows name for name substitutions, the theory is substituting names for values is the job of `R`s `environment`s and attempting to duplicate or replace core language functionality is not desirable.

`let()` allows the user to [choose the substitution engine](https://cran.r-project.org/web/packages/wrapr/vignettes/SubstitutionModes.html) and has a debug-print option.

``` r
library("wrapr")
suppressPackageStartupMessages(library("dplyr"))

NEWVAR <- as.name("y")
OLDVAR <- as.name("x")

let(
  alias = c(NEWVAR = "y",
            OLDVAR = "x"),
  eval = FALSE, # return code instead of evaluating it
  
  data.frame(x = 1) %>%
    mutate(NEWVAR = OLDVAR + 1)
  
)
```

    ## data.frame(x = 1) %>% mutate(y = x + 1)

This works because (like `strmacro()`) `let()` specifies replacements by target names, and not as inline annotation. `let()` can also work left-hand sides of argument bindings. We can re-work one of our `strmacro()` examples to demonstrate this.

``` r
library("wrapr")

A <- "AAA"
B <- "BBB"
x <- "xxx"

# notice right hand side "B" can be substituted 
# with name x, which evaluates to value "xxx".
let(
  alias = c("B" = "x"),
  expr = { c(A = B) }
)
```

    ##     A 
    ## "xxx"

``` r
# notice left hand side "A" can be substituted
# with name x, which is used as a name in the vector expression.
let(
  alias = c("A" = "x"),
  expr = { c(A = B) }
)
```

    ##     x 
    ## "BBB"

We emphasize that potential ambiguity between replacement targets and non-replacement is easily avoidable as both the full source code and replacement plan are written in the same code block.

People who try `let()` tend to like it.

<center>
<img src="http://www.win-vector.com/blog/wp-content/uploads/2017/02/C1v_VNBXUAA8c7M.jpg-large.jpg" />
</center>
### `rlang:!!`

`rlang` was written by Lionel Henry and Hadley Wickham and was incorporated into `dplyr` on June 6, 2017. From <code>help(`!!`)</code>:

> The rlang package provides tools to work with core language features of R and the tidyverse:
>
> -   The tidy eval framework, which is a well-founded system for non-standard evaluation built on quasiquotation (!!) and quosures (quo()).

Quasiquotation is something we have discussed a few times by now. The phrase "non-standard"" in this case is going to likely mean both capturing of names from source code (breaking referential transparency) and direct manipulation of environments (including carrying names plus the environments that they are bound in, instead of simply carrying values). Both the `lazyeval` and `rlang` packages seem to follow `R`-formula style semantics (and indeed seemed to have been based on formulas at one point, trace from [here](https://github.com/r-lib/rlang/commit/cc0c497155a8da6adc43a38ac4020c2cc9bb9491#diff-04c6e90faac2675aa89e2176d2eec7d8) for details), so a bit of criticism of `R`-formula design itself is relevant here.

<blockquote>
Many modelling and graphical functions have a formula argument and a data argument. If variables in the formula were required to be in the data argument life would be a lot simpler, but this requirement was not made when formulas were introduced. Authors of modelling and graphics functions are thus required to implement a limited form of dynamic scope, which they have not done in an entirely consistent way. <small>
<center>
<a href="http://developer.r-project.org/nonstandard-eval.pdf">Thomas Lumley, "Standard nonstandard evaluation rules", March 19, 2003</a>
</center>
</small>
</blockquote>
For simple effects such as [the linear modeling example](http://www.win-vector.com/blog/2018/09/r-tip-how-to-pass-a-formula-to-lm/) `rlang` can work with packages that it has not been integrated with. For the next example `rlang` can only be used with packages that have been re-built using `rlang` (such as `dplyr` has been).

`rlang` (parts of which are brought in by the `dplyr` package) can be demonstrated on one of the examples we already exhibited in the `bquote()` section of this note as follows.

``` r
suppressPackageStartupMessages(library("dplyr"))

NEWVAR <- as.name("y")
OLDVAR <- as.name("x")

  data.frame(x = 1) %>%
    mutate(!!NEWVAR := !!OLDVAR + 1)
```

    ##   x y
    ## 1 1 2

As with `bquote()` the `:=` notation is required. Substitution targets are marked with the "`!!`" notation. Notice how syntactically this differs from the `bquote()` solution only in tick-notation and not requiring an outer function (the benefit of `rlang` being integrated into the `dplyr` package).

There are of course some semantic differences, such as how environments are handled. However, in our opinion, most of the earlier solutions already have sound ways of dealing with environments and ambiguity of references, which are merely different (not fundamentally inferior).

Unlike `strmacro()` or `wrapr::let()`, `rlang` substitution will not work on left-sides of argument binding. For example we can not successfully write the above block as follows (with <code>=</code> instead of <code>:=</code>).

``` r
suppressPackageStartupMessages(library("dplyr"))

NEWVAR <- as.name("y")
OLDVAR <- as.name("x")

  data.frame(x = 1) %>%
    mutate(!!NEWVAR = !!OLDVAR + 1)
```

    ## Error: <text>:7:21: unexpected '='
    ## 6:   data.frame(x = 1) %>%
    ## 7:     mutate(!!NEWVAR =
    ##                        ^

So the change in macro capabilities in `dplyr 0.7.0` is essentially from the introduction of `:=` (which is already enough to allow `bquote()` to program over `dplyr`), and *not* from details of `rlang`. Once `:=` was added (part of `dplyr 0.7.0`'s `rlang` integration) `bquote()` was able to work seamlessly with `dplyr`. The `:=` was the missing piece. A `dplyr` that allows `:=` to denote assignment (as `data.table` has for a *very* long time) does not need an extension package such as `gtools`, `lazyeval`, `wrapr`, or `rlang`, as `base::bquote()` can in such circumstances be made to work.

`rlang` also emphasizes the ability to capture environments along with expressions (not demonstrated here). We find users with the discipline to follow John M. Chambers' advice do not typically need this additional facility (some notes [here](http://www.win-vector.com/blog/2018/08/r-tip-put-your-values-in-columns/)) and that collecting unexpected references to environments (as is often the case with `formula`, `closure`, and now `quosure`) [is a source of reference leaks](http://www.win-vector.com/blog/2014/05/trimming-the-fat-from-glm-models-in-r/).

There may be some fine distinctions as to whether `rlang` controlled execution is to be considered metaprogramming, macro facilities, or something else (due to the limited visibility of some `rlang` effects). It is likely that `rlang` is producing derived intermediate data structures as it guides code interpretation (so it likely re-writing code representations). Also, in some examples supplied by the `rlang` documentation we see the exact [`bquote()` capture/alter/execute pattern](http://www.win-vector.com/blog/2018/09/r-tip-how-to-pass-a-formula-to-lm/) we have seen before. For example:

``` r
library("rlang")

dataf <- mtcars
f <- disp ~ drat

eval(expr(   lm(!!f, data = dataf)   ))
```

    ## 
    ## Call:
    ## lm(formula = disp ~ drat, data = dataf)
    ## 
    ## Coefficients:
    ## (Intercept)         drat  
    ##       822.8       -164.6

The above is nearly identical to how `bquote()` would have dealt with the issue.

``` r
dataf <- mtcars
f <- disp ~ drat

eval(bquote(    lm(.(f), data = dataf)    ))
```

    ## 
    ## Call:
    ## lm(formula = disp ~ drat, data = dataf)
    ## 
    ## Coefficients:
    ## (Intercept)         drat  
    ##       822.8       -164.6

`rlang` documentation does sometimes mention `bquote()`, but never seems to actually *try* `bquote()` as an alternate solution in a post-`dplyr 0.5.0` world (i.e., one where "`:=`" is part of `dplyr` and where `bquote()` is a good solution). So new readers can be forgiven for having the (false) impression that `rlang` substitution is a unique and unprecedented capability for `R`.

Conclusion
----------

`R` has a number of useful macro facilities. Not all `R` users know about them. This is because, due to a number of good `R` design decisions, not all `R` users regularly *need* macro facilities. If you do need macros (or to "program over programs", which is always a bit harder than the more desirable programming over data) I suggest reading a few of the references and picking a system that works well for your tasks.

Some of our take-aways include:

-   Always consider using functions before resorting to macros. Functions have more isolation and are generally safer to work with and compose. When you use macros you are usurping standard evaluation rules and taking direct control, so after that things become somewhat your own fault.
-   For fine control of the arguments of a single function call I recommend using `base::do.call()`.
-   `gtools::defmacro()` is a great tool for building macros, especially parameterized code-snippets that are intended to have visible side effects (such as writing back values). `gtools::strmacro()` is a bit more wild, but definitely has uses.
-   `base::bquote()` is a great choice for programming over other systems and uses clear quasiquotation semantics. It is able to easily program over `dplyr 0.7.0` and later versions and is part of the core `R` language (or "base `R`", which *should* be a *huge* plus).
-   `lazyeval` is possibly in maintenance mode, and possibly no longer recommended by the package authors.
-   `wrapr::let()` (full disclosure: our own package). I feel `wrapr::let()` is sufficiently specialized (combining re-writing and execution into one function, and being restricted only to name for name substitutions) and sufficiently general (working with any package without pre-arrangement) that it is a good comprehensible, safe, convenient, and powerful option for interested `R` users. For more on `wrapr::let()` I suggest our [formal writeup](https://github.com/WinVector/wrapr/blob/master/extras/wrapr_let.pdf).
-   `rlang` is a package being promoted by the `dplyr` package authors, however I do not recommend it for general use.
