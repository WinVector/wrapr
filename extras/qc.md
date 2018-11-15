Quoting c
================

Many [`R`](https://github.com/r-lib/rlang/issues/686) users appear to be big fans of "code capturing" or ["non standard evaluation"](http://developer.r-project.org/nonstandard-eval.pdf) (NSE) interfaces. These are simply interfaces where a name to be used is captured from the source code the user typed, and thus does not need quote marks. For example:

``` r
d <- data.frame(x = 1)

d$x
```

    ## [1] 1

Notice both during `data.frame` creation and column access: the column name is given without quotes and also accessed without quotes.

This differs from using a standard value oriented interface as in the following:

``` r
d[["x"]]
```

    ## [1] 1

A natural reason for `R` users to look for automatic quoting is: it helps make working with columns in `data.frame`s (`R`'s primary data analysis structure) look much like working with variables in the environment. Without the quotes a column name looks very much like a variable name. And thinking of columns as variables is a useful mindset.

Another place implicit quoting shows up is with `R`'s "combine" operator where one can write either of the following.

``` r
c(a = "b")
```

    ##   a 
    ## "b"

``` r
c("a" = "b")
```

    ##   a 
    ## "b"

The [`wrapr`](https://CRAN.R-project.org/package=wrapr) package brings in a new function: [`qc()`](https://winvector.github.io/wrapr/reference/qc.html) or "quoting `c()`" that gives a very powerful and convenient way to elide quotes.

``` r
library(wrapr)

qc(a = b)
```

    ##   a 
    ## "b"

Notice quotes are not required on either side of the name assignment. Again, eliding quotes is not that big a deal, and not to everyone's taste. For example I have never seen a Python user feel they are missing anything because they write "`{"a" : "b"}`" to construct their own named dictionary structure.

That being said, `qc()` is a very convenient and consistent notation if you do want to work in an NSE style.

For example, if it ever bothered you that `dplyr` join takes the join column names as a character vector you can use `qc()` to instead write:

``` r
dplyr::full_join(
  iris, iris, 
  by = qc(Sepal.Length, Sepal.Width, 
          Petal.Length, Petal.Width, 
          Species))
```

(Actually I very much like that the join takes the columns as a vector, as it is much easier to program over.) I feel the `qc()` grouping of the columns makes it easier for a reader to see which arguments are the column set than a use of `...` would. Please take, as an example, the following `dplyr::group_by()`:

``` r
library(dplyr)

starwars %>%
  group_by(homeworld, species, add = FALSE) %>%
  summarize(mass = mean(mass, na.rm = TRUE))
```

    ## # A tibble: 58 x 3
    ## # Groups:   homeworld [?]
    ##    homeworld      species    mass
    ##    <chr>          <chr>     <dbl>
    ##  1 Alderaan       Human      64  
    ##  2 Aleen Minor    Aleena     15  
    ##  3 Bespin         Human      79  
    ##  4 Bestine IV     Human     110  
    ##  5 Cato Neimoidia Neimodian  90  
    ##  6 Cerea          Cerean     82  
    ##  7 Champala       Chagrian  NaN  
    ##  8 Chandrila      Human     NaN  
    ##  9 Concord Dawn   Human      79  
    ## 10 Corellia       Human      78.5
    ## # ... with 48 more rows

When coming back to such code later, I find the following notation to be easier to read:

``` r
library(seplyr)

starwars %>%
  group_by_se(qc(homeworld, species), add = FALSE) %>%
  summarize(mass = mean(mass, na.rm = TRUE))
```

    ## # A tibble: 58 x 3
    ## # Groups:   homeworld [?]
    ##    homeworld      species    mass
    ##    <chr>          <chr>     <dbl>
    ##  1 Alderaan       Human      64  
    ##  2 Aleen Minor    Aleena     15  
    ##  3 Bespin         Human      79  
    ##  4 Bestine IV     Human     110  
    ##  5 Cato Neimoidia Neimodian  90  
    ##  6 Cerea          Cerean     82  
    ##  7 Champala       Chagrian  NaN  
    ##  8 Chandrila      Human     NaN  
    ##  9 Concord Dawn   Human      79  
    ## 10 Corellia       Human      78.5
    ## # ... with 48 more rows

In the above we can clearly see which arguments to the grouping command are intended to be column names, and which are not.

`qc()` is a powerful NSE tool that annotates and contains where we are expecting quoting behavior. Some possible applications include examples such as the following.

``` r
# install many packages
install.packages(qc(testthat, knitr, rmarkdown, R.rsp))

# select columns
iris[, qc(Petal.Length, Petal.Width, Species)]

# control a for-loop
for(col in qc(Petal.Length, Petal.Width)) {
  iris[[col]] <- sqrt(iris[[col]])
}

# control a vapply
vapply(qc(Petal.Length, Petal.Width), 
       function(col) {
         sum(is.na(iris[[col]]))
       }, numeric(1))
```

The idea is: with `qc()` the user can switch name capturing notation at will, with no prior-arrangement needed in the functions or packages used. Also the parenthesis in `qc()` make for more legible code: a reader can see which arguments are being quoted and taken as a group.

As of `wrapr 1.7.0` `qc()` incorporates [`bquote()`](https://stat.ethz.ch/R-manual/R-devel/library/base/html/bquote.html) functionality. `bquote()` is `R`'s built-in quasi-quotation facility. It was added to `R` in August of 2003 by Thomas Lumley, and doesn't get as much attention as it deserves.

A quoting tool such as `qc()` becomes a quasi-quoting tool if we add a notation that signals we do not wish to quote. In `R` the standard notation for this is "`.()`" (Lisp uses a back-tick, and the `rlang` package uses "`!!`"). The `bquote()`-enabled version of `qc()` lets us write code such as the following.

``` r
extra_column = "Species"

qc(Petal.Length, Petal.Width, extra_column)
```

    ## [1] "Petal.Length" "Petal.Width"  "extra_column"

``` r
qc(Petal.Length, Petal.Width, .(extra_column))
```

    ## [1] "Petal.Length" "Petal.Width"  "Species"

Notice it is un-ambiguous what is going on above. The first `qc()` quotes all of its arguments into strings. The second works much the same, with the exception of names marked with `.()`. This ability to "break out" or turn off quoting is convenient if we are working with a combination of values we wish to type in directly and others we wish to take from variables.

`qc()` allows substitution on the left-hand sides of assignments, if we use the alternate `:=` notation for assignment (a convention put forward by `data.table`, and later adopted by `dplyr`).

``` r
library(wrapr)

left_name = "a"
right_value = "b"

qc(.(left_name) := .(right_value))
```

    ##   a 
    ## "b"

The `wrapr` package also exports an implementation for `:=`. So one could also write:

``` r
library(wrapr)

left_name := right_value
```

    ##   a 
    ## "b"

The hope is that the `qc()` and `:=` operators are well behaved enough to [commute](https://en.wikipedia.org/wiki/Commutative_property) in the sense the following two statements should return the same value.

``` r
library(wrapr)

qc(a := b, c := d)
```

    ##   a   c 
    ## "b" "d"

``` r
qc(a, c) := qc(b, d)
```

    ##   a   c 
    ## "b" "d"

The idea is: when there is a symmetry it is often evidence you are using the right concepts.

In conclusion: the goal of `wrapr::qc()` is to put a very regular and controllable quoting facility directly into the hands of the `R` user. This allows the `R` user to treat just about any `R` function or package *as if* the function or package itself implemented argument quoting and quasi-quotation capabilities.
