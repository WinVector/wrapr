Macro Substitution in R
================
John Mount
2018-09-14

This note is a long, but cursory, overview of some macro-substitution facilities available in [`R`](https://www.r-project.org). I am going to try to put a few of them in context (there are likely more I am missing) and explain why our group wrote yet another one ([`replyr::let()`](http://www.win-vector.com/blog/2016/12/parametric-variable-names-and-dplyr/)/[`wrapr::let()`](https://cran.r-project.org/web/packages/wrapr/vignettes/let.html)).

The `R` macro (or code control) facilities we will discuss include over 20 years of contributions (in time order):

-   `base::substitute()`/`base::eval()`: Likely part of `R` from the start (1993), as they were known in `S` (please see John M. Chambers, *Programming with Data: A Guide to the S Language*, Springer, 1998; and see also [here](https://github.com/wch/r-source/tree/fa44a2cd83a81ecbccd6545895955cff7f719156/src/library/base/R)).
-   `base::do.call()`: Part of `R` since [at least 1997](https://github.com/wch/r-source/blob/a625016dc706a58d11c75664f6c60d07e5d07b0c/src/main/names.c), probably earlier.
-   `defmacro()`: [Lumley T. "Programmer's Niche: Macros in R", R News, 2001, Vol 1, No. 3, pp 11–13](https://www.r-project.org/doc/Rnews/Rnews_2001-3.pdf).
-   `base::bquote()`: [Released in R 1.8.1, November 2003](https://github.com/wch/r-source/blob/5a156a0865362bb8381dcd69ac335f5174a4f60c/doc/NEWS.1).
-   `gtools::defmacro()`: [gtools 2.0.9, September 2, 2005](https://cran.r-project.org/src/contrib/Archive/gtools/).
-   `gtools::strmacro()`: [gtools 2.1.1, September 23, 2005](https://cran.r-project.org/src/contrib/Archive/gtools/).
-   `lazyeval` package: [Released October 1, 2014](https://cran.r-project.org/src/contrib/Archive/lazyeval/).
-   `replyr::let()`/`wrapr::let()` [Released December 8th, 2016](https://github.com/WinVector/wrapr/blob/master/extras/wrapr_let.pdf) in the [`replyr`](https://github.com/WinVector/replyr) package, later extended and moved to the low-dependency [`wrapr`](https://github.com/WinVector/wrapr) package.
-   `rlang::!!`: [Released May 5th, 2017](https://cran.r-project.org/src/contrib/Archive/rlang/).

One of the goals of this note is to document differences between our own method `wrapr::let()` and the *later* system `rlang:!!` (as we recently had [a paper](https://github.com/WinVector/wrapr/blob/master/extras/wrapr_let.pdf) rejected for not having enough such comparisons, and therefore would benefit from a reference that does so).

I have worked hard to include concrete and clear examples, so I think working through this note will be rewarding tutorial for `R` users interested in potentially lightening their programming workload through macros or metaprogramming (we will define metaprogramming and macros shortly). So I will re-phrase that: if you are interested in improving your `R` programming and not already expert in using macros methods, you may want to read on. We also advise readers who's primary experience with metapgrogramming is the `rlang` package to also read on.

Why macros and metaprogramming?
-------------------------------

Many `R` users never need any of `R`'s macro or metaprogramming capabilities. That is because the core language and packages are designed such that such capabilities are not immediately necessary.

For example. A common way to extract a column from a `data.frame` is to use the "`$`" operator:

``` r
d <- data.frame(x = 1:2, y = 3:4)
d$y
```

    ## [1] 3 4

In the above the name "`y`" is captured from the source-code. We thus say "`$`" is a *code capturing interface*, it works by looking at our source code! For another program to use this notation we might need a *macro facility* to substitute in a name coming form another variable. However, `R` supplies the `[[]]` notation which is a *value-oriented* interface, meaning what `[[]]` does is purely a function of the values made available to it (independent of what the code that supplies those values looks like).

``` r
COLUMNNAME <- "y"
d[[COLUMNNAME]]
```

    ## [1] 3 4

Value oriented interfaces are easier to program over. However, not all `R` functions and packages have the discipline of design to always supply value oriented alternatives to any code capturing interfaces. For example some commands such as `base::order()` can be hard to program over (due to its heavy leaning on the "`...`" argument, unlike the [`data.table`](https://CRAN.R-project.org/package=data.table) design of having both a `data.table::setorder()` and a `data.table::setorderv()`; we now have thin [`orderv()`](https://winvector.github.io/wrapr/reference/orderv.html) and [`sortv`()](https://winvector.github.io/wrapr/reference/sortv.html) adapters to directly address this issue).

In `R` macros and metaprogramming are *not* urgent user-facing problems, until the user attempts to program over an interface that is *only* designed for interactive use (i.e., doesn't accept or have an alternative that accepts values stored in additional variables). In our case we researched macros because we found the interfaces of `dplyr 0.5.0` to be hard to program over, even with the included `lazyeval` package; this will be our motivating problem throughout. `dplyr 0.7.*`/`dplyr 0.8.*` now uses a new metaprogramming facility called `rlang`, but this was not available until after we published `let()` and (in my opinion) is not a good solution (especially when considered in context).

Macros are a bit technical, but when you are painted into a programming corner (such as not having an interface you want), you look to macros. So let's talk more about macros and metaprogramming from the concrete point of view of trying to code capturing interfaces (such as "`$`") into value oriented interfaces (such as "`[[]]`"). Again: the idea is code capturing interfaces may be convenient for interactive work (when we are typing in the code while looking at data), but are inconvenient for programming work (where we can't always see the data and must take details from other variables, such as `COLUMNNAME`).

### Technical defintions

Some technical definitions (which we will expand on and use later).

-   **Macro**: In computer science a macro is "a rule or pattern that specifies how a certain input sequence (often a sequence of characters) should be mapped to a replacement output sequence (also often a sequence of characters) according to a defined procedure" ([source Wikipedia](https://en.wikipedia.org/wiki/Macro_(computer_science))). Macros are most interesting when the input they are working over is program source code (either parsed or not-parsed).
-   **Metaprogramming**: "Metaprogramming is a programming technique in which computer programs have the ability to treat programs as their data" ([source Wikipedia](https://en.wikipedia.org/wiki/Metaprogramming)).
-   **Quasiquotation**: "Quasiquotation is a parameterized version of ordinary quotation where instead of specifying a value exactly some holes are left to be filled in later. A quasiquotation is a template." ["Quasiquotation in lisp", Alan Bawden, 1999](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.309.227).
-   **Referential transparency**: "One of the most useful properties of expressions is that called by Quine referential transparency. In essence this means that if we wish to find the value of an expression which contains a sub-expression, the only thing we need to know about the sub-expression is its value." [Christopher Strachey, "Fundamental Concepts in Programming Languages", Higher-Order and Symbolic Computation, 13, 1149, 2000, Kluwer Academic Publishers](https://www.itu.dk/courses/BPRD/E2009/fundamental-1967.pdf) ([lecture notes written by Christopher Strachey for the International Summer School in Computer Programming at Copenhagen in August, 1967](https://en.wikipedia.org/wiki/Fundamental_Concepts_in_Programming_Languages)). This is the idea are attempting to capture in the informal phrase "value oriented interfaces."
-   **Non-standard evaluation**/**NSE**: Evaluation that is not referentially transparent, as it may include inspecting and capturing names from code and direct control of both execution and look-up environments. Discussed (but not defined) in [Thomas Lumley, "Standard nonstandard evaluation rules", March 19, 2003](http://developer.r-project.org/nonstandard-eval.pdf). "Code capturing interfaces" a style of **NSE** interfaces. **NSE** is convenient in interactive sessions, but is so at the cost of losing referential transparency (which in turn means we lose a lot of power to program and reason about programs).

Macros and metaprogramming are related concepts. Each has variations. For example <code>C</code>-macros are mere text substitutions performed by a pre-processor in some of the code compilation stages. Whereas <code>Lisp</code> macros operate directly on <code>Lisp</code> data structures and language objects (not mere text). When applied to programs both concepts converge to "code that writes code" or "programming over programs." There are games one can play to exclude or include various programming methods from these definitions. Let's take a broad (non pedantic) and general view that tools that achieve similar effects are similar enough to compare independent of how you categorize the internal implementation (an issue that does remain relevant when characterizing result quality and reliability).

Macros and metaprogramming in `R`
---------------------------------

We are going to run through the code control methods we listed earlier in roughly chronological or priority order. For each method will comment on its suitability for our example goal (converting code capturing interfaces into reusable referentially transparent interfaces, especially programming over `dplyr 0.5.0`) *and* how the capabilities of each method relate to the methods available before them (i.e. are they actually solving open problems). Obviously there is the important possibility that newer solutions *can* be better solutions, but just because a solution is newer doesn't mean it is in fact better. We prefer a mix of solutions: taking good methods from various sources instead of commit all fealty to single package (or package family) as this relieves later packages of the having to re-implement already available methods.

It is, of course, an untenable burden to expect users to ingest even a partial history of methodology before they are allowed to try a new method. However, for method developers a forgetful view of history rapidly becomes a mendacious view of history; and is counter-productive, as [a lot of expensive and valuable lessons are lost in each re-invention](https://www.joelonsoftware.com/2000/04/06/things-you-should-never-do-part-i/).

The `wrapr` package will get the (slightly unfair) presentation advantage: that as the package author, I can attempt to explain the intent and motivation of `wrapr` the package here.

### `base::do.call()`

We are going to discuss `base::do.call()` before `base::substitute()` and `base::eval()`, regardless of the order they may have entered into the `R` language.

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

`do.call()` is also an excellent way to build a value-oriented interface for order, as we show here.

``` r
orderv <- function(columns) {
  do.call(base::order, as.list(columns))
}
# Note: The package wrapr 1.6.2 supplies a more complete orderv()
# https://winvector.github.io/wrapr/reference/orderv.html

d <- data.frame(x = c(2, 2, 3, 3, 1, 1), y = 6:1, z = 1)

# code-capturing verison of the order() call, 
# concise but hard to program over
d[order(d$x, d$y), , drop = FALSE]
```

    ##   x y z
    ## 6 1 1 1
    ## 5 1 2 1
    ## 2 2 5 1
    ## 1 2 6 1
    ## 4 3 3 1
    ## 3 3 4 1

``` r
# value oriented verion of the call, 
# a bit longer but easier to program over
d[orderv(list(d$x, d$y)), , drop = FALSE]
```

    ##   x y z
    ## 6 1 1 1
    ## 5 1 2 1
    ## 2 2 5 1
    ## 1 2 6 1
    ## 4 3 3 1
    ## 3 3 4 1

``` r
# above interface is value oriented in the sense we 
# can take the specification from an outside source in a 
# variable.
order_spec <- list(d$x, d$y)
d[orderv(order_spec), , drop = FALSE]
```

    ##   x y z
    ## 6 1 1 1
    ## 5 1 2 1
    ## 2 2 5 1
    ## 1 2 6 1
    ## 4 3 3 1
    ## 3 3 4 1

Now the above [varargs](https://en.wikipedia.org/wiki/Variadic_function) issue is not quite the same issue as macro name-replacement, but they are similar in the sense variadic interfaces and name-capturing interfaces and optimized for interactive use (making them shorter than value oriented interfaces, but also a bit harder to program over).

`base::substitute()`/`base::eval()`
===================================

We are not going to show much direct use of `base::substitute()`, `base::eval()`. They are fundamental tools used both directly and to build other tools. Since we are taking a user-view of coding we will spend more time on more specialized tools build on top of these fundamental operators. So we will try to move on after merely a brief orientation or description.

`base::substitute()` captures un-evaluated code or arguments of functions. It also can, in some contexts, perform symbol substitutions (hence the name). `base::quote()` also captures its input, and [started as an function that called `substitue()`](https://github.com/wch/r-source/blob/fa44a2cd83a81ecbccd6545895955cff7f719156/src/library/base/R/eval) (which may still have different meaning, as `substitute()` behaves differently depending on the execution environment). `base::eval()` evaluates code (undoes the quoting of `substitute()` or `quote()`). This is easy to see with an example.

``` r
substitute(1 + 1)
```

    ## 1 + 1

``` r
eval(substitute(1 + 1))
```

    ## [1] 2

``` r
quote(1 + 1)
```

    ## 1 + 1

``` r
eval(quote(1 + 1))
```

    ## [1] 2

At this point we see how capturing un-evaluated code is possible, and how captured code can be later executed. Beyond delaying and printing things we haven't yet shown anything very deep.

In fact we now have enough power to directly imitate Lisp's <code>eval</code> and <code>apply</code>. This is already a lot of power, in fact the relations between such functions can be used to define the entire semantics of functional languages. The relations between Lisp <code>eval</code> and Lisp <code>apply</code> are so fundamental that Alan Kay called them "Maxwell's Equations of Software": ["A conversation with Alan Kay", ACMqueue, Volume 2, Issue 9, December 27, 2004](https://queue.acm.org/detail.cfm?id=1039523), see also ["Lisp as the Maxwell’s equations of software", Michael Nielson April 11, 2012](http://www.michaelnielsen.org/ddi/lisp-as-the-maxwells-equations-of-software/).

The above functions are "power tools", able to do just about anything for those that study them but a bit bulky and dangerous for casual use. Often user facing tools are built in terms of these operators. A great example of this is `defmacro()`, which we will discuss next.

### `defmacro()`/`gtools::defmacro()`

`defmacro()` is a function that lets the user define their own macros. `R` doesn't need a special macro definition sub-language, because the `R` language is already powerful enough to build macros.

`defmacro()` was introduced to the `R` community in [a fantastic article by Thomas Lumley in 2001](https://www.r-project.org/doc/Rnews/Rnews_2001-3.pdf), and then later enhanced and distributed as part of the [`gtools` package](https://CRAN.R-project.org/package=gtools) by Thomas Lumley and Gregory R. Warnes.

Macros superficially look a bit like functions: they encapsulate some code. However, instead of running code in an isolated environment (the function evaluation environment) they instead perform as if the user had typed the code in directly. Let's jump ahead in our timeline and use `gtools::defmacro()` to define a macro called "`set_variable`" as follows.

``` r
library("gtools")

set_variable <- defmacro(
  VARNAME,  # sustitution target/placeholder
  VARVALUE, # sustitution target/placeholder
  expr = { VARNAME <- VARVALUE } # expression to manipulate
)
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

We couldn't use `defmacro()` for our particular application (programming over `dplyr 0.5.0`) as the `substitute()` function (which `defmacro()` is based on) will not substitute the left-hand sides of argument bindings (which is how `dplyr::mutate()` specifies assignment). We demonstrate this below by trying to replace symbols with "<code>x</code>":

``` r
# notice right hand side "B" can be substituted
substitute(
  expr = c(A = B),      # expression to manipulate
  env = list("B" = "x") # mapping targets to replacements
  )
```

    ## c(A = "x")

``` r
# notice left hand side "A" can't be substituted
substitute(
  expr = c(A = B), 
  env = list("A" = "x"))
```

    ## c(A = B)

``` r
# notice $ notations are re-mapped
d <- data.frame(x = 1)
eval(substitute(
  expr = d$A,
  env = list("A" = "x")))
```

    ## [1] 1

The unwillingness of `substitute()` to replace left-hand sides of argument bindings is likely intentional. For standard function arguments (those that are not "`...`") such as the `x` in `sin(x)`, it does not make sense to substitute names. In such cases the programmer would usually know the name of the argument as they wrote the code. The following example, where only the right-hand side of bindings is replaced, is convenient and cuts down on some of the confusion sowed by the "<code>x = x</code>" notation.

``` r
substitute(sin(x = x), env = list(x = 7))
```

    ## sin(x = 7)

``` r
eval(substitute(sin(x = x), env = list(x = 7)))
```

    ## [1] 0.6569866

As `dplyr::mutate()` uses named argument binding in "`...`" to denote assignments, we will want control of both sides of such sub-expressions when trying to program over `dplyr`.

As an aside (and return to our earlier comments on "`$`"), notice substitute can replace items near a "`$`". `defmacro()` also does this, but produces yet another code capturing interface (not a value oriented interface).

``` r
d <- data.frame(x = 1)
column_i_want <- "x"

# notice $ is re-mapped, allowing us to use $ to simulate [[]]
eval(substitute(d$COLUMNNAME,
                env = list(COLUMNNAME = column_i_want)
))
```

    ## [1] 1

``` r
# notice COLUMNNAME is rempapped to name of incoming
# argument, not value of incoming argument.
read_column <- defmacro(DATA, COLUMNNAME,
                        expr = { DATA$COLUMNNAME})

# wrong
read_column(d, column_i_want)
```

    ## NULL

``` r
# correct, but not what we wanted, behaves like $ not like [[]]
read_column(d, x)
```

    ## [1] 1

To resolve our issue we want direct control of what gets substituted as a name and what is treated as a value. It turns out `base::bquote()` gives us exactly that.

### `base::bquote()`

On August 15, 2003 Thomas Lumley contributed a implementation of <code>Lisp</code>'s quasiquotation (please see ["Quasiquotation in lisp", Alan Bawden, 1999](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.309.227)) system or ["the backquote"](http://cl-cookbook.sourceforge.net/macros.html) to `R` itself.

    commit 6af5ee3fab4a347accc61a96185921320547c1e6   refs/remotes/origin/R-grid
    Author: tlumley <tlumley@00db46b3-68df-0310-9c12-caf00c1e9a41>
    Date:   Fri Aug 15 18:46:06 2003 +0000

        add bquote for partial substitution
        
        git-svn-id: https://svn.r-project.org/R/trunk@25744 00db46b3-68df-0310-9c12-caf00c1e9a41

`R`'s syntax is different than `Lisp`'s, so `R` uses the notation "<code>.(NAME)</code>" instead of "<code>\`NAME</code>". But this is in fact the back-tick and quasiquotation ideas discussed in the Bawden paper. `bquote()` is an under-appreciated tool. We can jump forward a bit to one of our conclusions: `bquote()` is a great solution for most "convert a name-capturing interface to a value oriented interface" tasks.

`bquote()` has a fairly concise description (from `help(bquote)`):

> An analogue of the <code>Lisp</code> backquote macro. bquote quotes its argument except that terms wrapped in .() are evaluated in the specified <code>where</code> environment.

We can demonstrate the idea as follows. Suppose we want to build up a simple expression that checks if a given variable is `5` or not, but we want to take the variable name in from another variable. This is easy to solve, but a good example for `bquote()`. First we use `bquote()` to quote a user supplied expression, preventing its execution.

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

We have built up the specific expression we want (from code working over an abstract template or expression), and we are now ready to execute the altered expression with `eval()`.

``` r
eval(bquote( .(A) == 5 ))
```

    ## [1] FALSE

And we are done! We used the above pattern in our article ["R tip: How to Pass A formula to lm"](http://www.win-vector.com/blog/2018/09/r-tip-how-to-pass-a-formula-to-lm/).

There is one limitation: due to `R`'s syntax rules `bquote()` style notation can not substitute on the left-hand side of an "`=`" expression. This means we can not use it to control the following sort of expression (as see below).

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

The problem is this sort of notation is used in the popular [`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html) function to denote assignment.

In `dplyr 0.7.0` and beyond there is a substitute notation "`:=`" (a notation already made popular by `data.table`) which lets us avoid the issue with "`=`":

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

However, for `dplyr 0.5.0` (the version of `dplyr` available when we were working on this issue) one must use "`=`", and therefore can not use directly `bquote()` to control `dplyr::mutate()` results.

The code for `bquote()` (accessible by executing `print(bquote)`) is amazingly compact. This is a common situation in functional programming languages.

### `gtools::strmacro()`

Gregory R. Warnes added `strmacro()` to the `gtools` package to supply an additional macro facility. Let's demonstrate `strmacro()`.

``` r
library("gtools")

mul <- strmacro(A, # substitution target 
                B, # substitution target 
                expr={A*B} # expression to manipulate
                )
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

For example notice `strmacro` can successful substitute "`a`" for "`X`" on the left and right-hand sides of "`$`" and even over function definitions.

``` r
library("gtools")

d <- data.frame(X = "XCOL", a = "ACOL")
a <- list(X = "XITEM", a = "AITEM")

macro <- strmacro(
  X, # define substitution target
  expr = list(
    d$X,
    X$a,
    f <- function(X) { X + 1 }
  )
)
macro("a")
```

    ## [[1]]
    ## [1] ACOL
    ## Levels: ACOL
    ## 
    ## [[2]]
    ## [1] "AITEM"
    ## 
    ## [[3]]
    ## function (a) 
    ## {
    ##     a + 1
    ## }

This *is* the kind of power we want.

Please keep in mind that when writing a macro both the substitution target names and code being transformed are known to the author and near each other (so easy to manage/alter together). It is only the replacement values that are not fully known or controlled. That means most danger or ambiguity seen in an example such as above can be completely eliminated by merely choosing longer substitution targets:

``` r
library("gtools")

d <- data.frame(X = "XCOL", a = "ACOL")
a <- list(X = "XITEM", a = "AITEM")

macro <- strmacro(
  STR_MACRO_TARGET_X, # define substitution target
  expr = list(
    d$STR_MACRO_TARGET_X,
    STR_MACRO_TARGET_X$a,
    f <- function(STR_MACRO_TARGET_X) { STR_MACRO_TARGET_X + 1 }
  )
)
macro("a")
```

    ## [[1]]
    ## [1] ACOL
    ## Levels: ACOL
    ## 
    ## [[2]]
    ## [1] "AITEM"
    ## 
    ## [[3]]
    ## function (a) 
    ## {
    ##     a + 1
    ## }

`strmacro` is powerful enough to program over `dplyr 0.5.0`:

``` r
library("gtools")
suppressPackageStartupMessages(library("dplyr"))

macro <- strmacro(
  NEWVAR, # define substitution target
  OLDVAR, 
  expr = {
    data.frame(x = 1) %>%
      mutate(NEWVAR = OLDVAR + 1)
  }
)
macro("y", "x")
```

    ##   x y
    ## 1 1 2

And one can even write a version of `let()` quickly in terms of `strmacro`:

``` r
library("gtools")
suppressPackageStartupMessages(library("dplyr"))

lets <- function(alias, expr, env = parent.frame()) {
  expr <- base::substitute(expr)
  args <- lapply(names(alias), as.name)
  macro <- do.call(gtools::strmacro, c(args, list(expr = expr)))
  do.call(macro, as.list(as.character(alias)), envir = env)
}

lets(
  alias = c(NEWVAR = "y",
            OLDVAR = "x"),
  
  data.frame(x = 1) %>%
    mutate(NEWVAR = OLDVAR + 1)
)
```

    ##   x y
    ## 1 1 2

The first version of `replyr::let()` was [an attributed adaption of `strmacro()` code](https://github.com/WinVector/replyr/blob/edbbbc1bc8ca2f9f5f5ec753e53ef00c6aaaca81/R/let.R), but the method is now language based. Honestly we probably should have thought harder about building `let()` *on top of* `strmacro` early on. With our own `wrapr` implementation of `let()` we do get a bit finer control, and escape the common criticism of string based solutions (using instead a language based solution). A small example of differences is given here.

``` r
lst <- list(A = 1)

lets( 
  alias = c(X = "SUBSTITUTED"),
  expr = list(
    c(X = "X.X"),
    .X = c("X" = "X")
  )
)
```

    ## [[1]]
    ##               SUBSTITUTED 
    ## "SUBSTITUTED.SUBSTITUTED" 
    ## 
    ## $.SUBSTITUTED
    ##   SUBSTITUTED 
    ## "SUBSTITUTED"

``` r
wrapr::let(
  alias = c(X = "SUBSTITUTED",
            Y = "lst"),
  expr = list(
    c(X = "X.X"),
    .X = c("X" = "X")
  )
)
```

    ## [[1]]
    ## SUBSTITUTED 
    ##       "X.X" 
    ## 
    ## $.X
    ## SUBSTITUTED 
    ##         "X"

The difference is `wrapr::let()` is substituting on language objects, not on mere strings. Notice `wrapr::let()` does not substitute into all string situations the same, but can use language context to make good choices. However, as we have said before, users can completely avoid such ambiguity by choosing appropriate substitution targets when they write their alias and code.

### `lazyeval`

`dplyr 0.5.0`'s parametric programming interface was a package called [`lazyeval`](https://CRAN.R-project.org/package=lazyeval) by Hadley Wickham which describes itself as:

> An alternative approach to non-standard evaluation using formulas. Provides a full implementation of LISP style 'quasiquotation', making it easier to generate code with other code.

The `lazyeval` vignettes give some details ([here](https://cran.r-project.org/web/packages/lazyeval/vignettes/lazyeval-old.html) and [here](https://cran.r-project.org/web/packages/lazyeval/vignettes/lazyeval.html)).

`lazyeval` seems to incorporate a design principle that there is of merit in carrying around a variable name plus an environment where that name is resolved to a value. That is at best a point of view. In my opinion, especially in the context of quasiquotation, [it is *much* better to design workflows that allow one to convert bound names to values and reserve unbound names to refer to `data.frame` columns](http://www.win-vector.com/blog/2018/08/r-tip-put-your-values-in-columns/).

It is my impression that `lazyeval` isn't currently recommended by its authors, so examples teaching it or example further criticizing it are not germane to the current discussion. We place `lazyeval` in its chronological place here to respect its priority.

### `wrapr::let()`

For some code-rewriting tasks we (John Mount and Nina Zumel) found both `substitute()` and `bquote()` a bit limiting, and `strmacro()` a bit wild. For this reason we developed `wrapr::let()`, taking inspiration from `gtools::strmacro()`. `wrapr::let()` was designed to have syntax similar to `substitute()` and also to classic <code>Lisp</code> "<code>let</code>" style value-binding blocks (informal example: "<code>(let X be 7 in (sin X))</code>"). Informally we think of `let()` as "`base::with()`, but for names, instead of values." We wanted `let()` to be a direct user facing tool, not a tool that builds tools (such as `strmacro()`). Early implementations used string-based methods, later we switched to more powerful language object based substitutions.

Our first application of `let()` (at the time in the [`replyr`](https://CRAN.R-project.org/package=replyr) package) was to perform parametric programming over `dplyr 0.5.0` (the version of `dplyr` current at the time). That is to convert `dplyr 0.5.0` name/code capturing interfaces into standard referentially transparent interfaces.

We have shared a number of articles and gave public talks on the topic.

-   [Using replyr::let to Parameterize dplyr Expressions](http://www.win-vector.com/blog/2016/12/using-replyrlet-to-parameterize-dplyr-expressions/).
-   [help(let, package=’replyr’)](http://www.win-vector.com/blog/2016/12/helplet-packagereplyr/).
-   [My recent BARUG talk: Parametric Programming in R with replyr](http://www.win-vector.com/blog/2017/02/my-recent-barug-talk-parametric-programming-in-r-with-replyr/).
-   [Does replyr::let work with data.table?](http://www.win-vector.com/blog/2016/12/does-replyrlet-work-with-data-table/)
-   [Comparative examples using replyr::let](http://www.win-vector.com/blog/2016/12/comparative-examples-using-replyrlet/)
-   [Evolving R Tools and Practices](http://www.win-vector.com/blog/2017/02/evolving-r-tools-and-practices/)

Other contributors worked out additional applications for `let()` including using it to control parameterized `R`-markdown.

`let()` is good for situations where we are forced to use an interactive interface. For example if we were forced to use "`$`" as the only way to access `data.frame` columns (i.e., if `[[]]` did not exist) we could use `let()` to work around it as follows.

``` r
library("wrapr")

d <- data.frame(x = 1:2, y = 3:4)

# iteractive way to read the data
d$y
```

    ## [1] 3 4

``` r
# programming friendly method
column_i_want <- "y"
d[[column_i_want]]
```

    ## [1] 3 4

``` r
# adapted method (if [[]] did not exist)
let(
  c(COLUMNNAME = column_i_want), # mapping of subsitution targets to replacement names
    d$COLUMNNAME                 # expression to manipulate
  )
```

    ## [1] 3 4

We can also work one of our `bquote()` section examples as follows.

``` r
library("wrapr")
suppressPackageStartupMessages(library("dplyr"))

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

For clarity and safety `wrapr::let()` is deliberately limited to replacing names with names (which for convenience can be represented as strings), leaving value substitution to `R` itself. For example to following is deliberately not supported:

``` r
library("wrapr")

let(
  alias = c(X = 5),
  
  X
)
```

    ## Error in prepareAlias(alias, strict = strict): wrapr:let alias values must all be strings or names ( X is class: numeric )

The above is a bit of the `wrapr::let()` design philosophy: leave already well solved tasks to base `R` and packages that already do them well. Value substitution is left to `R` itself (that is what `environment`s already do) and `bquote()`, splicing and vararg issues are left to `do.call()`.

This also means `wrapr::let()` can *not* work the earlier [linear model example](http://www.win-vector.com/blog/2018/09/r-tip-how-to-pass-a-formula-to-lm/). Users should not consider that a problem, as we have already shown how to solve that problem with `do.call()`. I feel add-on packages should strive to solve problems that *do not* have satisfactory base-`R` solutions, and *not* attempt to supplant `R` itself for mere stylistic concerns.

`let()` specifies replacements by target names (like `strmacro()`), and not as inline annotation (as with `bquote()).`let()\` can also work left-hand sides of argument bindings. We can re-work one of our earlier examples to demonstrate this.

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

We emphasize that potential ambiguity between replacement targets and non-replacement is easily avoidable as both the full source code and replacement plan are written in the same code block, and thus near each other and under a single author's control.

People who try `let()` tend to like it.

<center>
<img src="http://www.win-vector.com/blog/wp-content/uploads/2017/02/C1v_VNBXUAA8c7M.jpg-large.jpg" />
</center>
<p/>
.

### <code>rlang::`!!`</code>

`rlang` was written by Lionel Henry and Hadley Wickham and was incorporated into `dplyr` on June 6, 2017. From <code>help(`!!`, package = "rlang")</code>:

> The rlang package provides tools to work with core language features of R and the tidyverse:
>
> -   The tidy eval framework, which is a well-founded system for non-standard evaluation built on quasiquotation (!!) and quosures (quo()).

There is a lot packed into the above statement. However, at this point of our note we have enough shared vocabulary to work through the terms.

-   "Tidyverse" is a marketing brand name.
-   "The tidy eval framework" means the substitution and evaluation portions of `rlang` (`!!`, `rlang::expr()`, `rlang::sym()`, and other components).
-   "non-standard evaluation" refers to both capturing of names from source code and carrying of environments references (essentially pointers to environments).
-   Quasiquotation is something we have discussed a few times by now.
-   "quosures" are described as follows in the ["Advanced R"](https://github.com/hadley/adv-r/blob/master/Quotation.Rmd) (authored by Hadley Wickham): "<code>~</code>, the formula, is a quoting function that also captures the environment. It's the inspiration for quosures ...".

What I want to call out is: the phrase "which is a well-founded system", and the repeated use of the word "tidy." Alone this may not seem like much, but this is fairly typical of `rlang`/`tidyeval`/`tidyverse` promotion: a merely implied but strongly repeated assertion that earlier <code>R</code> systems are not well founded and are "un-tidy." For example "Advanced R"'s current discussion of <code>bquote()</code> is [now almost entirely the following two statements.](https://github.com/hadley/adv-r/search?l=RMarkdown&q=bquote):

-   "<code>bquote()</code> provides a limited form of quasiquotation" (what the deficiency is does not appear to be discussed in the text).
-   "<code>bquote()</code> is a neat function, but is not used by any other function in base R."

Let's directly establish some context before working `rlang` examples.

The `rlang` package is an evolution of `R`-formula style semantics (as is also [the `lazyeval` package](https://github.com/r-lib/rlang/commit/cc0c497155a8da6adc43a38ac4020c2cc9bb9491#diff-04c6e90faac2675aa89e2176d2eec7d8)). So some known issues with `formula` remain relevant:

<blockquote>
Many modelling and graphical functions have a formula argument and a data argument. If variables in the formula were required to be in the data argument life would be a lot simpler, but this requirement was not made when formulas were introduced. Authors of modelling and graphics functions are thus required to implement a limited form of dynamic scope, which they have not done in an entirely consistent way.
<p/>
<small>
<center>
<a href="http://developer.r-project.org/nonstandard-eval.pdf">Thomas Lumley, "Standard nonstandard evaluation rules", March 19, 2003</a>
</center>
</small>
</blockquote>
<p/>
.

`rlang` emphasizes the ability to capture environments along with expressions. John M. Chambers already criticized the over-use of taking non-trivial values from the environment carried by `R` `formula` objects:

<blockquote>
Where clear and trustworthy software is a priority, I would personally avoid such tricks. Ideally, all the variables in the model frame should come from an explicit, verifiable data source, typically a data frame object that is archived for future inspection (or equivalently, some other equally well-defined source of data, either inside or outside <code>R</code>, that is used explicitly to construct the data for the model).
<p/>
<center>
<small><em>Software for Data Analysis</em> (Springer 2008), John M. Chambers, Chapter 6, section 9, page 221.</small>
</center>
</blockquote>
<p/>
.

Chambers is saying "spaghetti data" (non-trivial values being taken from various carried environments) is worth avoiding (as [spaghetti code](https://en.wikipedia.org/wiki/Spaghetti_code) is worth avoiding). The principle is: one should restrict oneself to *structured data* (taking non-trivial values from <code>data.frame</code> columns, a data-version of [structured programming](https://en.wikipedia.org/wiki/Structured_programming), as we discuss [here](http://www.win-vector.com/blog/2018/08/r-tip-put-your-values-in-columns/)).

Basically `rlang` `quosure`s are sufficiently like `formula`s to run into the same issues (including [reference leaks](http://www.win-vector.com/blog/2014/05/trimming-the-fat-from-glm-models-in-r/)). Our advice is: [prefer avoiding complexity to getting better and working within complexities](http://www.win-vector.com/blog/2011/04/do-your-tools-support-production-or-complexity/).

But enough theory, let's see `rlang` in action.

<code>rlang</code> (parts of which are brought in by the <code>dplyr</code> package) can be demonstrated on one of the examples we already exhibited in the <code>bquote()</code> section of this note as follows.

``` r
suppressPackageStartupMessages(library("dplyr"))

NEWVAR <- as.name("y")
OLDVAR <- as.name("x")

  data.frame(x = 1) %>%
    mutate(!!NEWVAR := !!OLDVAR + 1)
```

    ##   x y
    ## 1 1 2

As with `bquote()` the `:=` notation is required. Substitution targets are marked with the "`!!`" notation. Syntactically this differs from the `bquote()` solution only in tick-notation and not requiring an outer function wrapper (a simple benefit of `rlang` being integrated into the `dplyr` package).

For comparison we will write out the `bquote()` solution again.

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

However, the reason `rlang` could avoid the wrapping is: `rlang` is integrated into the `dplyr` package (essentially the `mutate()` step itself incorporates such a wrapper). Such integration is easy to achieve, as we show below.

``` r
suppressPackageStartupMessages(library("dplyr"))

# define a bquote() enabled variation of dplyr::mutate()
mutate_bq <- function(.data, ...) {
  env = parent.frame()
  mc <- substitute(dplyr::mutate(.data = .data, ...))
  mc <- do.call(bquote, list(mc, where = env), envir = env)
  eval(mc, envir = env)
}

NEWVAR <- as.name("y")
OLDVAR <- as.name("x")

  data.frame(x = 1) %>%
    mutate_bq(.(NEWVAR) := .(OLDVAR) + 1)
```

    ##   x y
    ## 1 1 2

We just demonstrated a `bquote()`-enhanced version of `mutate()` in about 4 lines of code. Enhancing `mutate()` with `strmacro()` or `let()` would be similarly easy.

There are, of course, some semantic differences, such as how to unambiguously choose values from the <code>data.frame</code> or the environment. However, in my opinion, most of the earlier solutions already have sound ways of dealing with these issues. `rlang` does supply some additional services such as [vararg](https://en.wikipedia.org/wiki/Variadic_function) splicing through `!!!`, however as we have seen in our `order()` example `do.call()` already is an excellent tools for vararg splicing. The other systems don't need to solve splicing, as it isn't an unsolved problem.

Similar to `bquote()` (and unlike `strmacro()` and `let()`) `rlang` substitution will not work on left-sides of argument binding. For example we can not successfully write the above block as follows (with <code>=</code> instead of <code>:=</code>).

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

A `dplyr` that allows `:=` to denote assignment does not need an extension package such as `gtools`, `lazyeval`, `wrapr`, or `rlang`; `base::bquote()` is already able to do the work.

`rlang` can work with packages that it has not been integrated with, as [the linear modeling example](http://www.win-vector.com/blog/2018/09/r-tip-how-to-pass-a-formula-to-lm/) demonstrates. We repeat a simplified such example here (example volunteered by an `rlang` package author recently):

``` r
library("rlang")

dataf <- mtcars
f <- disp ~ drat

eval(expr(        lm(!!f, data = dataf)   ))
```

    ## 
    ## Call:
    ## lm(formula = disp ~ drat, data = dataf)
    ## 
    ## Coefficients:
    ## (Intercept)         drat  
    ##       822.8       -164.6

Or possibly the following form is preferred:

``` r
eval_tidy(quo(    lm(!!f, data = dataf)   ))
```

    ## 
    ## Call:
    ## lm(formula = disp ~ drat, data = dataf)
    ## 
    ## Coefficients:
    ## (Intercept)         drat  
    ##       822.8       -164.6

However, the above are nearly identical to how `bquote()` would have dealt with the issue.

``` r
dataf <- mtcars
f <- disp ~ drat

eval(bquote(      lm(.(f), data = dataf)  ))
```

    ## 
    ## Call:
    ## lm(formula = disp ~ drat, data = dataf)
    ## 
    ## Coefficients:
    ## (Intercept)         drat  
    ##       822.8       -164.6

We are assuming the difference is much finer control of environments (the `expr()` object being of `class` `call` and not carrying an `environment`, and `quo()` object being of class `quosuer` and carrying an `environment`). However the `bquote()` object is of `class` `call` (without an environment) and the function `eval()` does take an environment argument, so with some care it is quite possible to reasonably control environments when using `bquote()`.

`rlang` environment control is powerful, allowing an expression of the form "`a - a`" that evaluates to a non-zero value.

``` r
library("rlang")

mk_term <- function(name, value) {
  v <- sym(name)
  env <- environment()
  assign(name, value, envir = env)
  quo(!!v)
}
terms <- expr(!!mk_term("a", 5) - !!mk_term("a", 12))

print(terms)
```

    ## (~a) - ~a

``` r
eval_tidy(terms)
```

    ## [1] -7

`rlang` documentation and promotion does sometimes mention `bquote()`, but never seems to actually *try* `bquote()` as an alternate solution in a post-`dplyr 0.5.0` world (i.e., one where "`:=`" is part of `dplyr`). So new readers can be forgiven for having the (false) impression that `rlang` substitution is a unique and unprecedented capability for `R`.

`rlang` can not substitute into a number of common target positions due to syntax restrictions. For example the we could try to repeat the `wrapr::let()` replace the item on the right of the "`$`" example, but that does not work.

``` r
library("rlang")

X <- sym("y")
expr(d$!!X)
```

    ## Error: <text>:4:8: unexpected '!'
    ## 3: X <- sym("y")
    ## 4: expr(d$!
    ##           ^

``` r
expr(f <- function(!!X) { !!X + 1 })
```

    ## Error: <text>:1:20: unexpected '!'
    ## 1: expr(f <- function(!
    ##                        ^

Similar issues likely exist for the right-hand sides of "`$`", and function arguments ([cases `wrapr::let()` handles with ease](https://winvector.github.io/wrapr/articles/SubstitutionModes.html)).

``` r
library("wrapr")

let(
  c(X = "y"),
  {
    d$X
    X$a
    f <- function(X) { X + 1 }
  },
  eval = FALSE
)
```

    ## {
    ##     d$y
    ##     y$a
    ##     f <- function(y) {
    ##         y + 1
    ##     }
    ## }

The `rlang` replacement notation is just not as powerful as name-based substitution (as used by `strmacro()` and `wrapr::let()`).

#### `rlang` and `dplyr`

A huge marketing advantage for `rlang` is its privileged place in the so-called `tidyverse` and its integration into `dplyr`. So it is relevant to examine this combination of packages.

The combined `rlang`/`dplyr` interface surface is large and complicated. A lot varies depending if the user is attempting to specify a column using an integer index, a string, a `name`/`symbol`, a `quosure`/`formula`, an expression, or un-evaluated source code (all of which seem to be allowed); plus variations depending on if the execution is a function context or not; plus variations the "semantics" of the `dplyr` verb ([there are at least two styles: "`select`" and "`eval`", and possibly more](https://github.com/tidyverse/dplyr/issues/3316)). This creates a large user responsibility to know which combination of adapters and which access patterns are correct. We give an example below (contrived, but the kind of experimentation a new user often uses to learn):

<small>

``` r
suppressPackageStartupMessages(library("dplyr"))
library("rlang")

x <- "Species"

# wrong
iris %>% group_by(x) %>% summarize(n = n())
```

    ## Error in grouped_df_impl(data, unname(vars), drop): Column `x` is unknown

``` r
# wrong
iris %>% group_by(!!x) %>% summarize(n = n())
```

    ## # A tibble: 1 x 2
    ##   `"Species"`     n
    ##   <chr>       <int>
    ## 1 Species       150

``` r
# wrong
iris %>% group_by(!!quo(x)) %>% summarize(n = n())
```

    ## Error in grouped_df_impl(data, unname(vars), drop): Column `x` is unknown

``` r
# wrong
iris %>% group_by(!!enquo(x)) %>% summarize(n = n())
```

    ## # A tibble: 1 x 2
    ##   `"Species"`     n
    ##   <chr>       <int>
    ## 1 Species       150

``` r
# wrong
iris %>% group_by(!!expr(x)) %>% summarize(n = n())
```

    ## Error in grouped_df_impl(data, unname(vars), drop): Column `x` is unknown

``` r
# wrong
iris %>% group_by(!!enexpr(x)) %>% summarize(n = n())
```

    ## # A tibble: 1 x 2
    ##   `"Species"`     n
    ##   <chr>       <int>
    ## 1 Species       150

``` r
# wrong (syntax error)
iris %>% group_by(.data$!!x) %>% summarize(n = n())
```

    ## Error: <text>:2:25: unexpected '!'
    ## 1: # wrong (syntax error)
    ## 2: iris %>% group_by(.data$!
    ##                            ^

``` r
# works, package authors call patterns like this "quoting and unquoting"
# https://github.com/tidyverse/dplyr/issues/3801#issuecomment-419137995
iris %>% group_by(!!sym(x)) %>% summarize(n = n())
```

    ## # A tibble: 3 x 2
    ##   Species        n
    ##   <fct>      <int>
    ## 1 setosa        50
    ## 2 versicolor    50
    ## 3 virginica     50

``` r
# works (base-R solution using dplyr's .data pronoun)
iris %>% group_by(.data[[x]]) %>% summarize(n = n())
```

    ## # A tibble: 3 x 2
    ##   x              n
    ##   <fct>      <int>
    ## 1 setosa        50
    ## 2 versicolor    50
    ## 3 virginica     50

</small>
<p/>
To be fair: a lot of the above issues were driven by our insistence on starting from a string column name instead of a symbol or captured un-evaluated code. Though it is disappointing that "`x`" does not work as a synonym for "`!!sym(x)`" (one would like quoting plus unquoting to look like a no-op). The `rlang` preference appears to be strongly for capturing un-evaluated code or arguments. However, [prefering non-trivial variables to be in columns](http://www.win-vector.com/blog/2018/08/r-tip-put-your-values-in-columns/) and considering column names to be strings is a valid point of view and a useful when when programming over modeling tasks (where one may supply the set of dependent variables as a vector of column names). I feel there is a subtle difference between the problems `rlang` apparently wants to solve (composing NSE interfaces) and the problems analysts/data-scientists actually have (wanting to propagate controlling values, such as column names, into analyses).

In contrast to the above examples the `base::bquote()` and `wrapr::let()` patterns are fairly regular.

<small>

``` r
suppressPackageStartupMessages(library("dplyr"))

# works
x <- as.name("Species")
eval(bquote( 
  iris %>% group_by(.(x)) %>% summarize(n = n()) 
))
```

    ## # A tibble: 3 x 2
    ##   Species        n
    ##   <fct>      <int>
    ## 1 setosa        50
    ## 2 versicolor    50
    ## 3 virginica     50

</small>

<small>

``` r
suppressPackageStartupMessages(library("dplyr"))
library("wrapr")

# works
x <- "Species"
let(
  c(X = x),
  iris %>% group_by(X) %>% summarize(n = n())
)
```

    ## # A tibble: 3 x 2
    ##   Species        n
    ##   <fct>      <int>
    ## 1 setosa        50
    ## 2 versicolor    50
    ## 3 virginica     50

``` r
# works
x <- "Species"
let(
  c(X = x),
  iris %>% group_by(.data$X) %>% summarize(n = n())
)
```

    ## # A tibble: 3 x 2
    ##   Species        n
    ##   <fct>      <int>
    ## 1 setosa        50
    ## 2 versicolor    50
    ## 3 virginica     50

</small>
<p/>
.

Conclusion
----------

`R` has a number of useful macro and metaprogramming facilities. Not all `R` users know about them. This is because, due to a number of good `R` design decisions, not all `R` users regularly *need* macro facilities. If you do need macros (or to "program over programs", which is always a bit harder than the more desirable programming over data) I suggest reading a few of the references and picking a system that works well for your tasks. The job of metaprogramming is to reduce programmer burden, so these tools should only be applied when they are less work than the obvious alternatives (such as repeating code).

Some of our take-aways include:

-   Always consider using functions before resorting to macros. Functions have more isolation and are generally safer to work with and compose. When you use macros you are usurping standard evaluation rules and taking direct control, so after that things become somewhat your own fault.
-   For fine control of the arguments of a single function call I recommend using `base::do.call()`.
-   For composing macro-like entities with each other `substitute()`, `quote()`, `eval()`, and `unquote()` are the base-`R` fundamental tools.
-   `gtools::defmacro()` is a great tool for building macros, especially parameterized code-snippets that are intended to have visible side effects (such as writing back values).
-   `base::bquote()` is a great choice for programming over other systems and uses clear quasiquotation semantics. It is able to easily program over `dplyr 0.7.0` and later versions and is part of the core `R` language (or "base `R`", which *should* be a *huge* plus).
-   `gtools::strmacro()` is a very powerful tool, fully capable of programming over `dplyr 0.5.0`.
-   `lazyeval` is possibly in maintenance mode, and possibly no longer recommended by the package authors.
-   `wrapr::let()` (full disclosure: our own package). My opinion is: `wrapr::let()` is sufficiently specialized (combining re-writing and execution into one function, and being restricted only to name for name substitutions) and sufficiently general (working with any package without pre-arrangement) so that it is a good comprehensible, safe, convenient, and powerful option for interested `R` users. I recommend using `wrapr::let()` in conjunction with the other tools we have mentioned earlier, in particular `base::do.call()` for multi-argument splicing. It is not reasonable to insist that all metaprogramming or macro effects come from a single package, as this would require later packages attempting to contribute additional metaprogramming tools to also needlessly attempt to re-implement additional tools that already work well. For more on `wrapr::let()` I suggest our [formal writeup](https://github.com/WinVector/wrapr/blob/master/extras/wrapr_let.pdf).
-   `rlang` is a package being promoted by the `dplyr` package authors. My opinion and experience is: I do not recommend `rlang` for general use.
