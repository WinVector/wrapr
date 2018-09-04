Macro Substitution in R
================

This note is a *very* cursory overview of some macro-substitution
facilities available in [`R`](https://www.r-project.org). We are going
to try to put a few of them in context (there are likely more we are
missing) and explain why we wrote yet another one
([`wrapr::let()`](https://cran.r-project.org/web/packages/wrapr/vignettes/let.html)).

The `R` macro (or code control) facilities we will discuss include (in
time order):

  - `base::do.call()` (part of `R` since at least 1997).
  - `defmacro()` [Lumley T. “Programmer’s Niche: Macros in R”, R
    News, 2001, Vol 1, No. 3,
    pp 11–13](https://www.r-project.org/doc/Rnews/Rnews_2001-3.pdf).
  - `base::bquote()` ([released in R 1.8.1,
    November 2003](https://github.com/wch/r-source/blob/5a156a0865362bb8381dcd69ac335f5174a4f60c/doc/NEWS.1)).
  - `gtools::defmacro()` ([gtools 2.0.9,
    September 2, 2005](https://cran.r-project.org/src/contrib/Archive/gtools/))
    from the [`gtools`](https://CRAN.R-project.org/package=gtools)
    package.
  - `strmacro()` ([gtools 2.1.1,
    September 23, 2005](https://cran.r-project.org/src/contrib/Archive/gtools/))
    from the [`gtools`](https://CRAN.R-project.org/package=gtools)
    package.
  - `lazyeval` package ([released
    October 1, 2014](https://cran.r-project.org/src/contrib/Archive/lazyeval/)).
  - `wrapr::let()` ([announced/released
    December 8th, 2016](https://github.com/WinVector/wrapr/blob/master/extras/wrapr_let.pdf)).
  - `rlang::!!` ([released
    May 5th, 2017](https://cran.r-project.org/src/contrib/Archive/rlang/)).

## Why macros and metaprogramming

Many `R` users never need any of `R`’s macro or metaprogramming
capabilities. That is because the core language and packages are
designed such that such capabilities are not immediately necessary.

For example. A common way to extract a column from a `data.frame` is to
use the “`$`” operator:

``` r
d <- data.frame(x = 1:2, y = 3:4)
d$y
```

    ## [1] 3 4

In the above the name “`y`” is captured from the source-code. For
another program to use this notation we might need a macro facility to
substitute in a name coming form another variable. However, `R` supplies
the `[[]]` notation which is a value-oriented interface, meaning we do
not need to resort to macros.

``` r
COLUMNNAME <- "y"
d[[COLUMNNAME]]
```

    ## [1] 3 4

However, not all `R` functions and packages have this discipline of
design. In particular some commands such as `base::order()` can be hard
to program over (due to its heavy leaning on the “`...`” argument unlike
the [`data.table`](https://CRAN.R-project.org/package=data.table) design
of having both a `data.table::setorder()` and a
`data.table::setorderv()`).

So roughly in `R` macros and meta-programming are not urgent user-facing
problems, until the user attempts to program over an interface that
*only* is designed for interactive use (i.e., doesn’t accept or have an
alternative that accepts values stored in additional variables).

Macros are a bit technical, but when you are painted into a programming
corner: you want macros. So let’s talk more about macros and
metaprogramming.

### Technical defintions

In computer science a macro is “a rule or pattern that specifies how a
certain input sequence (often a sequence of characters) should be mapped
to a replacement output sequence (also often a sequence of characters)
according to a defined procedure” ([source
Wikipedia](https://en.wikipedia.org/wiki/Macro_\(computer_science\))).
Macros are most interesting when the input they are working over is
program source code (either parsed or not-parsed). Metaprogramming
“Metaprogramming is a programming technique in which computer programs
have the ability to treat programs as their data” ([source
Wikipedia](https://en.wikipedia.org/wiki/Metaprogramming)).

The two concepts are related. Each has variations. For example
<code>C</code>-macros are very strict text substitutions performed by a
pre-processor in some of the code compilation stages. Whereas
<code>Lisp</code> macros operate on <code>Lisp</code> data structures
and language objects.

When applied to programs both concepts converge to “code that writes
code.”

## Macros and metaprogramming in `R`

### `base::do.call()`

Sometimes you don’t need macros or metaprogramming. The function you are
using may have a powerful enough interface. Or you may be only trying to
control the execution of a single function, a task for which `R` already
has an excellent tool: `base::do.call()`.

For example if you want to control the call-presentation of a formula
passed to `lm()` (a problem discussed
[here](http://www.win-vector.com/blog/2018/09/r-tip-how-to-pass-a-formula-to-lm/))
the following code is sufficient.

``` r
# specifications of how to model,
# coming from somewhere else
outcome <- "mpg"
variables <- c("cyl", "disp", "hp", "carb")
data <- mtcars

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
model <- do.call("lm", list(f, data = as.name("data")))
print(model)
```

    ## 
    ## Call:
    ## lm(formula = mpg ~ cyl + disp + hp + carb, data = data)
    ## 
    ## Coefficients:
    ## (Intercept)          cyl         disp           hp         carb  
    ##   34.021595    -1.048523    -0.026906     0.009349    -0.926863

### `defmacro()`/`gtools::defmacro()`

`defmacro()` was introduced to the `R` community by [a fantastic article
by Thomas Lumley
in 2001](https://www.r-project.org/doc/Rnews/Rnews_2001-3.pdf), and then
later enhanced and distributed as part of the [`gtools`
package](https://CRAN.R-project.org/package=gtools) by Thomas Lumley and
Gregory R. Warnes.

Macros superficially look a bit like functions: the encapsulate some
code. However, instead of running code in an isolated environment (the
function evaluation environment) they instead perform as if the user had
type the code in directly. Let’s jump ahead in our timeline and use
`gtools::defmacro()` to define a macro called “`set_variable`” as
follows.

``` r
library("gtools")

set_variable <- defmacro(VARNAME, VARVALUE, 
                         expr = { VARNAME <- VARVALUE })
```

And using such a macro is easy.

``` r
set_variable(x, 7)
print(x)
```

    ## [1] 7

Notice that a macro can interact with the global environment without
using escape systems such as `<<-` or direct manipulations of
environments. The macro’s lack of isolation is both its benefit (it
works as if the user had typed the code in the macro) and detriment (in
most cases you should prefer the safe isolation of functions). `R` is a
bit odd in that what it calls “functions” are closer to
[<code>fexpr</code>](https://en.wikipedia.org/wiki/Fexpr)s, which lost
popularity as <code>Lisp</code>s moved towards a cleaner differentiation
between [applicative
order](https://en.wikipedia.org/wiki/Lambda_calculus) functions (that is
functions that are only executed after their arguments are resolved) and
macros (arbitrary language structures).

I strongly suggest reading [the original
article](https://www.r-project.org/doc/Rnews/Rnews_2001-3.pdf), as it
shares a great deal of wisdom and humor. I can’t resist stealing a bit
of the conclusion from the article.

> While defmacro() has many (ok, one or two) practical uses, its main
> purpose is to show off the powers of substitute(). Manipulating
> expressions directly with substitute() can often let you avoid messing
> around with pasting and parsing strings, assigning into strange places
> with \<\<- or using other functions too evil to mention.

We couldn’t use `defmacro()` for our particular application (programming
over `dplyr 0.5.0`) as the `substitute` function will not substitute the
left-hand sides of argument bindings (which is how `dplyr::mutate()`
specifies assignment).

``` r
substitute(list(A = A), list(A = "x"))
```

    ## list(A = "x")

``` r
defmacro(A, expr = list(A = A))("x")
```

    ## $A
    ## [1] "x"

The unwillingness of `substitute()` to replace left-hand sides of
argument bindings is likely intentional. For standard function arguments
(those that are not “`...`”) such as the `x` in `sin(x)` it does not
make sense to substitute names, the programmer would usually know the
name of the argument as they wrote the code. So the following is in fact
sensible and a good choice for `R` (there is also the issue the variable
and argument names are much more visible in `R` than in many other
programming languages, so again one wants to be careful with them).

``` r
substitute(sin(x = x), list(x = 7))
```

    ## sin(x = 7)

As `dplyr::mutate()` uses argument binding in “`...`” to denote
assignments, we want control of both sides of such sub-expressions. So
we need to look forward a bit more for solutions.

### `base::bquote()`

On August 15, 2003 Thomas Lumley contributed a fairly complete version
of <code>Lisp</code>’s quasi-quotation (please see [“Quasiquotation in
lisp”, Alan
Bawden, 1999](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.309.227))
system or [“the
backquote”](http://cl-cookbook.sourceforge.net/macros.html) to `R`
itself.

    commit 6af5ee3fab4a347accc61a96185921320547c1e6   refs/remotes/origin/R-grid
    Author: tlumley <tlumley@00db46b3-68df-0310-9c12-caf00c1e9a41>
    Date:   Fri Aug 15 18:46:06 2003 +0000
    
        add bquote for partial substitution
        
        
        git-svn-id: https://svn.r-project.org/R/trunk@25744 00db46b3-68df-0310-9c12-caf00c1e9a41

`R`’s syntax is different, so `R` uses the notation
“<code>.(NAME)</code>” instead of “<code>\`NAME</code>”, but the
concepts are related to the back-tick or quasi-quotation ideas discussed
in the Bawden paper.

`bquote()` has a fairly concise description (from `help(bquote)`):

> An analogue of the <code>Lisp</code> backquote macro. bquote quotes
> its argument except that terms wrapped in .() are evaluated in the
> specified where environment.

We can demonstrate the idea as follows. First we can use `bquote()` to
quote a user supplied expression, preventing its execution.

``` r
x <- 7
A <- as.name("x")
bquote( A == 5 )
```

    ## A == 5

Now we can use the “`.()`” notation to turn off-quoting for portions of
the expression.

``` r
bquote( .(A) == 5 )
```

    ## x == 5

And finally we can execute the altered expression with `eval()`.

``` r
eval(bquote( .(A) == 5 ))
```

    ## [1] FALSE

We used the above pattern in our article [“R tip: How to Pass A formula
to
lm”](http://www.win-vector.com/blog/2018/09/r-tip-how-to-pass-a-formula-to-lm/).

There is one limitation: due to `R`’s syntax rules `bquote()` style
notation can not substitute on the left-hand side of an “`=`”
expression. This means we can not use it to control the following sort
of expression.

``` r
bquote( list(.(A) = 5) )
```

    ## Error: <text>:1:19: unexpected '='
    ## 1: bquote( list(.(A) =
    ##                       ^

Note the above isn’t `bquote()`’s fault, the wrapping syntax just isn’t
legal on the left-side of an “<code>=</code>” in this case.

``` r
fnull <- function(x) {}

fnull( list(.(A) = 5) )
```

    ## Error: <text>:3:18: unexpected '='
    ## 2: 
    ## 3: fnull( list(.(A) =
    ##                     ^

The problem is this sort of notation is used a lot for argument binding,
and in the popular
[`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html)
function (though there is a substitute notation “`:=`” available, a
notation idea that the
[<code>data.table</code>](https://CRAN.R-project.org/package=data.table)
package has used for quite some time.). One can work around the issue as
we show below.

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

One can get (despite its unpopularity) “`=`”-style assignment working by
introducing braces and a “false mutator” style function.

``` r
`.<-` <- function(x, value) {invisible(x)}
eval(bquote({ .(NEWVAR) = 17 }))
print(y)
```

    ## [1] 17

However, the rules for “`=`” as argument binding don’t seem so easy to
circumvent.

`bquote()` is indeed a remarkable demonstration of the power of `R`’s
<code>substitute</code> facility. The code for `bquote()` (accessible by
executing `print(bquote)`) is amazingly compact. It is an example of
where, with the right abstraction, an essentially 10-line function can
do the job of an entire package. This is a common situation in
functional programming languages. This is part of why design, criticism,
and iteration of design are so important in functional programming. Also
this compactness is fairly common when you stay out of the way of
<code>eval</code>/<code>apply</code> and let them do the heavy lifting
(as they are “Maxwell’s Equations of Software”: [“A conversation with
Alan Kay”, ACMqueue, Volume 2, Issue 9,
December 27, 2004](https://queue.acm.org/detail.cfm?id=1039523), see
also [“Lisp as the Maxwell’s equations of software”, Michael Nielson
April 11, 2012](http://www.michaelnielsen.org/ddi/lisp-as-the-maxwells-equations-of-software/)).

### `gtools::strmacro()`

Gregory R. Warnes added `strmacro()` to the `gtools` package to supply
an additional macro facility. The stated purpose of `strmacro()` is to
accept argument names as quoted text. Let’s demonstrate it below.

``` r
library("gtools")

mul <- strmacro(A, B, expr={A*B})
x <- 7
y <- 2
mul("x", "y")
```

    ## [1] 14

Notice the macro arguments are can now be passed in as strings *and*
substitution is performed by matching target names, not by annotations.
This can be an advantage when the macro is going to be used by other `R`
code such as a `for`-loop.

`strmacro` can replace left-hand sides of argument bindings.

``` r
library("gtools")

strmacro(A, expr = list(A = A))('"x"')
```

    ## $x
    ## [1] "x"

`strmacro` happens to be inconvenient for our particular application
(executing a `dplry 0.5.0` pipeline) due to the need to explicitly
execute the macro and the quoting. However, `strmacro` supplied good
ideas for the initial implementation of `let()` (which has since evolved
quite a bit) and we found studying it and the `defmacro()` to be very
rewarding.

### An aside

At this point notice how generous Thomas Lumley has been to the `R`
community. He took the time to teach macros, ported a variation of the
concept into base-`R`, and then added variations of the macro facility
to a public open-source package. This is the kind of non-empire building
(for “empire” please see [Bill Venables, “<code>R</code>: *Quo
Vaidis?*”, *UseR\! 2012*, Nashville, Tn,
USA](http://biostat.mc.vanderbilt.edu/wiki/pub/Main/UseR-2012/VenablesRQuoVadis.pdf))
and giving back to the larger community that keeps `R` vital (another
example is [Matt Dowle and Arun Srinivasan doing so much to improve
sorting in
base-`R`](http://www.win-vector.com/blog/2018/08/timings-of-a-grouped-rank-filter-task/)).

### `lazyeval`

`dplyr 0.5.0`’s parametric programming interface was a package called
[`lazyeval`](https://CRAN.R-project.org/package=lazyeval) which
describes itself as:

> An alternative approach to non-standard evaluation using formulas.
> Provides a full implementation of LISP style ‘quasiquotation’, making
> it easier to generate code with other code.

`lazyeval` does not seem to be currently recommended, and is not
fundamentally more powerful than `base::bquote()`. The primary reason
`bquote()` at one time did not seem like a good macro tool for working
with `dplyr` is: `dplyr 0.5.0` did not include the `:=` operator. Once
`:=` was added (part of `dplyr 0.7.0`’s `rlang` integration) `bquote()`
was able to work seamlessly with `dplyr`.

The `:=` was the missing piece. A `dplyr` that allows `:=` to denote
assignment (as `data.table` has for a *very* long time) does not need an
extension package such as `gtools`, `lazyeval`, `wrapr`, or `rlang` as
`base::quote()` can in such circumstances be made to work.

### `wrapr::let()`

For some code-rewriting tasks we found both `substitute()` and
`bquote()` a bit limiting. For this reason we developed (taking
inspiration from `gtools::strmacro()`) `wrapr::let()`. `wrapr::let()`
was designed to have syntax similar to `substitute()` and to classic
<code>Lisp</code> “<code>let</code>” style value-binding blocks
(informal example: “<code>(let X be 7 in sin(X)</code>)”).

Our first application of `let()` (at the time in the
[`replyr`](https://CRAN.R-project.org/package=replyr) package) was to
perform parametric programming over `dplyr 0.5.0` (the version of
`dplyr` current at the time).

We (me plus my college Dr. Nina Zumel) shared a number of articles and
gave public talks on the topic.

  - [Using replyr::let to Parameterize dplyr
    Expressions](http://www.win-vector.com/blog/2016/12/using-replyrlet-to-parameterize-dplyr-expressions/).
  - [help(let,
    package=’replyr’)](http://www.win-vector.com/blog/2016/12/helplet-packagereplyr/).
  - [My recent BARUG talk: Parametric Programming in R with
    replyr](http://www.win-vector.com/blog/2017/02/my-recent-barug-talk-parametric-programming-in-r-with-replyr/).
  - [Does replyr::let work with
    data.table?](http://www.win-vector.com/blog/2016/12/does-replyrlet-work-with-data-table/)
  - [Comparative examples using
    replyr::let](Comparative%20examples%20using%20replyr::let)
  - [Evolving R Tools and
    Practices](http://www.win-vector.com/blog/2017/02/evolving-r-tools-and-practices/)

Other contributors worked out additional applications for `let()`
including using it to control parameterized `R`-markdown.

We can work one of our `bquote()` section examples as follows.

``` r
library("wrapr")
suppressPackageStartupMessages(library("dplyr"))

NEWVAR <- as.name("y")
OLDVAR <- as.name("x")

let(c(NEWVAR = "y",
      OLDVAR = "x"),
    
  data.frame(x = 1) %>%
    mutate(NEWVAR = OLDVAR + 1)
  
)
```

    ##   x y
    ## 1 1 2

Notice `wrapr::let()` specifies substitution targets by name (not by any
decorating notation such as backtick or `.()`), and also can use the
original “=”-notation without problem.

`let()` allows the user to [choose the substitution
engine](https://cran.r-project.org/web/packages/wrapr/vignettes/SubstitutionModes.html)
and has debug-print option.

``` r
library("wrapr")
suppressPackageStartupMessages(library("dplyr"))

NEWVAR <- as.name("y")
OLDVAR <- as.name("x")

let(c(NEWVAR = "y",
      OLDVAR = "x"),
    eval = FALSE,
    
  data.frame(x = 1) %>%
    mutate(NEWVAR = OLDVAR + 1)
  
)
```

    ## data.frame(x = 1) %>% mutate(y = x + 1)

This works, as (like `strmacro()`) `wrapr` specifies replacements by
target names (not inline annotation) and can work left-hand sides of
argument bindings. We can re-work one of or `strmacro()` examples to
demonstrate this.

``` r
library("wrapr")

let(c("A" = "x"), eval = FALSE,
    list(A = A))
```

    ## list(x = x)

People who try `let()` tend to like
it.

<center>

<img src="http://www.win-vector.com/blog/wp-content/uploads/2017/02/C1v_VNBXUAA8c7M.jpg-large.jpg" />

</center>

In our [formal
writeup](https://github.com/WinVector/wrapr/blob/master/extras/wrapr_let.pdf)
we take some time to point out other systems (Lumley, `gtools()`,
`bquote()`, `lazyeval`, and even `rlang` even though it came *after*
`let()`), and our debt to <code>gtools</code>:

> <code>strmacro()</code> was the inspiration for the original
> string-based <code>let()</code> implementation, although
> <code>let()</code> now uses the more powerful and safer language based
> method described above.

### `rlang:!!`

`rlang` was written by Lionel Henry and Hadley Wickham. `rlang` was
incorporated into `dplyr` on June 6, 2017, giving it an incredible
leg-up on promotion and acceptance (it doesn’t really have to compete on
merits). From <code>help(`!!`)</code>:

> Quasiquotation is the mechanism that makes it possible to program
> flexibly with tidy evaluation grammars like dplyr. It is enabled in
> all tidyeval quoting functions, the most fundamental of which are
> quo() and expr().
> 
> Quasiquotation is the combination of quoting an expression while
> allowing immediate evaluation (unquoting) of part of that expression.
> We provide both syntactic operators and functional forms for
> unquoting.

For simple effects such as [our linear modeling
example](http://www.win-vector.com/blog/2018/09/r-tip-how-to-pass-a-formula-to-lm/)
`rlang` can work with packages that it has not been integrated with. For
the next example `rlang` can only be used with packages that have been
re-built using `rlang` (such as `dplyr` has been).

`rlang` (parts of which are brought in by the `dplyr` package) can be
demonstrated on one of the examples we already exhibited in the
`bquote()` section of this note as follows.

``` r
suppressPackageStartupMessages(library("dplyr"))

NEWVAR <- as.name("y")
OLDVAR <- as.name("x")

  data.frame(x = 1) %>%
    mutate(!!NEWVAR := !!OLDVAR + 1)
```

    ##   x y
    ## 1 1 2

As with `bquote()` the `:=` notation is required. Substitution targets
are marked with the “`!!`” notation. Notice how syntactically this
differs from the `bquote()` solution only in tick-notation and not
requiring an outer function (the benefit of `rlang` being integrated
into the `dplyr` package).

We can even define our own unquote function (<code>UQ()</code>, we can’t
use <code>.()</code> due to some `dplyr` details).

``` r
suppressPackageStartupMessages(library("dplyr"))

NEWVAR <- as.name("y")
OLDVAR <- as.name("x")

UQ <- function(s) {!!s}

  data.frame(x = 1) %>%
    mutate(UQ(NEWVAR) := UQ(OLDVAR) + 1)
```

    ##   x y
    ## 1 1 2

There are of course some semantic differences, such as how environments
are handled. However, in our opinion, most of the earlier solutions
already have sound ways of dealing with environments and ambiguity of
references, which are merely different (not fundamentally inferior).

Unlike `strmacro()` or `wrapr::let()` `rlang` substitution will not work
on left-sides of argument binding. For example we can not successfully
write the above block as follows (with <code>=</code> instead of
<code>:=</code>).

``` r
suppressPackageStartupMessages(library("dplyr"))

NEWVAR <- as.name("y")
OLDVAR <- as.name("x")

UQ <- function(s) {!!s}

  data.frame(x = 1) %>%
    mutate(UQ(NEWVAR) = UQ(OLDVAR) + 1)
```

    ## Error: <text>:9:23: unexpected '='
    ## 8:   data.frame(x = 1) %>%
    ## 9:     mutate(UQ(NEWVAR) =
    ##                          ^

So this change in capabilities in `dplyr 0.7.0` is from the introduction
of `:=` (which is already enough to allow `bquote()` to program over
`dplyr`), and *not* from `rlang`.

`rlang` also emphasizes the ability to capture environments along with
expressions (not demonstrated here). We find users with the discipline
to follow John M. Chambers’ advice do not typically need this additional
facility (some notes
[here](http://www.win-vector.com/blog/2018/08/r-tip-put-your-values-in-columns/))
and that collecting unexpected references to environments (as is often
the case with `formula`, `closueres`, and now `quosures`) [is a source
of reference
leaks](http://www.win-vector.com/blog/2014/05/trimming-the-fat-from-glm-models-in-r/).

There may be some fine distinctions as to if `rlang` controlled
execution is be considered metaprogramming, macro facilities, or
something else (due to the limited visibility of some `rlang` effects).
However in some examples supplied by the `rlang` we see the exact
[`bquote()` capture/alter/execute
pattern](http://www.win-vector.com/blog/2018/09/r-tip-how-to-pass-a-formula-to-lm/):

``` r
suppressPackageStartupMessages(library("dplyr"))

data <- mtcars
f <- disp ~ drat

eval(expr(lm(!!f, data = data)))
```

    ## 
    ## Call:
    ## lm(formula = disp ~ drat, data = data)
    ## 
    ## Coefficients:
    ## (Intercept)         drat  
    ##       822.8       -164.6

Again the above is nearly identical to how `bquote()` would have dealt
with the issue.

``` r
data <- mtcars
f <- disp ~ drat

eval(bquote(    lm(.(f), data = data)    ))
```

    ## 
    ## Call:
    ## lm(formula = disp ~ drat, data = data)
    ## 
    ## Coefficients:
    ## (Intercept)         drat  
    ##       822.8       -164.6

`rlang` documentation tends not to mention or credit earlier `R` work
such as `defmacro()` or `let()`. It does sometimes mention `bquote()`,
but never seems to actually *try* `bquote()` as an alternate solution in
a post-`dplyr 0.5.0` world (i.e., one where “`:=`” is part of `dplyr`
and where `bquote()` is a good solution). So new readers can be forgiven
for having the (false) impression that `rlang` substitution is a unique
and unprecedented capability for `R`.

## Conclusion

`R` has a number of useful macro facilities. Not all `R` users know
about them. This is because, due to a number of good `R` design
decisions, not all `R` users regularly *need* macro facilities. If you
do need to macros (or to “program over programs”, which is always a bit
harder than the more desirable programming over data) I suggest reading
a few of the references and picking a system that works well for your
tasks.

For fine control of the arguments of a single function call we recommend
`base::do.call()`.

`gtools::defmacro()` is a great tool for building macros, especially
parameterized code-snippets that are intended to have visible side
effects (such as writing back values). `gtools::strmacro()` is a bit
more wild, but definitely has uses.

`base::bquote()` is able to easily program over `dplyr 0.7.0` and later
versions and is part of the core `R` language (or “base `R`”, which
*should* be a *huge* plus).

`rlang` is the method being promoted by the `dplyr` package authors,
however we do not recommend it for general use.

As for our method (`wrapr::let()`). We feel `wrapr::let()` is
sufficiently specialized (combining re-writing and execution into one
function, and being restricted only to name for name substitutions) and
sufficiently general (working with any package without pre-arrangement)
that it is a good comprehensible, safe, convenient, and powerful choice
for interested `R` users. For more on `wrapr::let()` we suggest of
[formal
writeup](https://github.com/WinVector/wrapr/blob/master/extras/wrapr_let.pdf).
