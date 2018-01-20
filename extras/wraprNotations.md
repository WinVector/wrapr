Some wrapr Notations
================
Win-Vector LLC
1/20/2018

I would like to demonstrate some helpful [`wrapr`](https://CRAN.R-project.org/package=wrapr) [`R`](https://www.r-project.org) notation tools that really neaten up your `R` code.

First we demonstrate `wrapr`'s ["named map builder": `:=`](https://winvector.github.io/wrapr/reference/named_map_builder.html).
The named name builder adds names to vectors and lists by nice "names on the left and values on the right" notation.

For example to assign the names "`a`" and "`b`" to the vector `c(1, 2)` we would write the following code.

``` r
library("data.table")
suppressPackageStartupMessages(library("dplyr"))
library("wrapr")
```

    ## 
    ## Attaching package: 'wrapr'

    ## The following object is masked from 'package:data.table':
    ## 
    ##     :=

``` r
c("a" := 1, "b" := 2)
```

    ## a b 
    ## 1 2

This is read as "`a` is 1 and `b` is 2".

We can perform this as a sequence (as above), or even vectorized (as shown below):

``` r
c("a", "b") := c(1, 2)
```

    ## a b 
    ## 1 2

One can think of this as an operator version of `setNames(nm = c("a", "b"), c(1,2))` (from `stats`).

Another wrapr function speeds up the notation even more: [`qc()` (quoting combine/concatenate)](https://winvector.github.io/wrapr/reference/qc.html). This work by analogy to `R`'s `c()` function, except it quotes its arguments. This lets us write the previous vector naming as:

``` r
qc(a, b) := c(1, 2)
```

    ## a b 
    ## 1 2

These notations work particularly well with `wrapr::let()`.

`wrapr::let()` evaluates an expression with a number of symbolic substitutions, and the named map builder is a great way to specify such substitutions. A quick examples is given below.

``` r
x <- 1
let("VARNAME" := "x",
    VARNAME + 1)
```

    ## [1] 2

`let()` is useful in writing re-usable (or parametric) functions (often a challenge in `R`).

``` r
d <- data.frame(x = c(1, 2))

incrementColumn <- function(data, COLUMNNAME) {
  let(c("COLUMNNAME" = COLUMNNAME),
      mutate(data, COLUMNNAME = COLUMNNAME + 1)
  )
}

incrementColumn(d, "x")
```

    ##   x
    ## 1 2
    ## 2 3

The idea is we use the stand-in symbol `COLUMNNAME` in our code (no matter how complicated) and `let()` substitutes name (represented as a string) stored into the expression before execution. What we just executed was equivalent to the following:

``` r
COLUMNNAME = "x"
let(c("COLUMNNAME" = COLUMNNAME), eval = FALSE,
      mutate(d, COLUMNNAME = COLUMNNAME + 1)
  )
```

    ## mutate(d, x = x + 1)

For non-trivial code, the above rapidly becomes more legible than notations such as the following:

``` r
COLUMNSYM <- rlang::sym("x")
mutate(d, !!COLUMNSYM := (!!COLUMNSYM) + 1)
```

    ##   x
    ## 1 2
    ## 2 3

Also, there is a helper function makes function creation even more convenient: [`wrapr::mapsyms()`](https://winvector.github.io/wrapr/reference/mapsyms.html). A `mapsyms` expression of the form `mapsyms(COLUMNNAME)` is equivalent to the code `c("COLUMNNAME" = COLUMNNAME)`. In our example that means it builds the name to name mapping: c('COLUMNNAME' := 'x') (here we used [`wrapr::map_to_char()`](https://winvector.github.io/wrapr/reference/map_to_char.html) to present the result). With `mapsyms()` we can write the earlier function as:

``` r
incrementColumn <- function(data, COLUMNNAME) {
  let(mapsyms(COLUMNNAME),
      mutate(data, COLUMNNAME = COLUMNNAME + 1)
  )
}

incrementColumn(d, "x")
```

    ##   x
    ## 1 2
    ## 2 3

We have more `mapsyms()` examples in our article ["Let X=X in R"](http://www.win-vector.com/blog/2017/11/let-xx-in-r/).

The `let()` method of building functions works well with [`dplyr`](https://CRAN.R-project.org/package=dplyr) and [`data.table`](https://CRAN.R-project.org/package=data.table). For each of these let's show code for the "by hand logistic scoring" example from ["Let’s Have Some Sympathy For The Part-time R User"](http://www.win-vector.com/blog/2017/08/lets-have-some-sympathy-for-the-part-time-r-user/).

`dplyr` function example:

``` r
# scoring function wrapping code where the 
# columns of interest were originally
# subjectID, surveyCategory, and assessmentTotal.
# function lets us re-map columns later.
logistic_score <- function(data, scale, 
                           subjectID = "subjectID",
                           surveyCategory = "surveyCategory",
                           assessmentTotal = "assessmentTotal") {
  let(mapsyms(subjectID, surveyCategory, assessmentTotal),
      data %>%
        group_by(subjectID) %>%
        mutate(probability =
                 exp(assessmentTotal * scale)/
                 sum(exp(assessmentTotal * scale))) %>%
        arrange(probability, surveyCategory) %>%
        mutate(isDiagnosis = row_number() == n()) %>%
        filter(isDiagnosis) %>%
        ungroup() %>%
        select(subjectID, surveyCategory, probability) %>%
        rename(diagnosis = surveyCategory) %>%
        arrange(subjectID)
      )
}

# new data where columns of interest are
# student, surveyCategory, and points.
d <- data.frame(
  student = c(1,                   
              1,
              2,                   
              2),
  surveyCategory = c(
    'withdrawal behavior',
    'positive re-framing',
    'withdrawal behavior',
    'positive re-framing'
  ),
  points = c(5,                 
             2,
             3,                  
             4),
  stringsAsFactors = FALSE
)

# apply the function to new data
# re-specifying columns to the names we need
d %>%
  logistic_score(., 
                 scale = 0.237,
                 subjectID = "student",
                 assessmentTotal = "points") %>%
  knitr::kable(.)
```

|  student| diagnosis           |  probability|
|--------:|:--------------------|------------:|
|        1| withdrawal behavior |    0.6706221|
|        2| positive re-framing |    0.5589742|

Hers is a `data.table` function example.

``` r
logistic_score <- function(data, scale, 
                           subjectID = "subjectID",
                           surveyCategory = "surveyCategory",
                           assessmentTotal = "assessmentTotal") {
  let(qc(SUBJECTID, SURVEYCATEGORY, ASSESSMENTTOTAL) :=
        c(subjectID, surveyCategory, assessmentTotal),
      { 
        dDT <- data.table::data.table(data)
        setnames(dDT, surveyCategory, "diagnosis")
        dDT[,expaTs:=exp(ASSESSMENTTOTAL*scale)]
        # precalculate -> this uses gsum internally
        dDT[,sum_expaTs:=sum(expaTs),SUBJECTID] 
        dDT[,probability := expaTs / sum_expaTs]
        dDT[,c(assessmentTotal,"expaTs","sum_expaTs"):=NULL]
        setorder(dDT, SUBJECTID, -probability, diagnosis)
        dDT[,.SD[1],SUBJECTID]
      })
}

d %>%
  logistic_score(., 
                 scale = 0.237,
                 subjectID = "student",
                 assessmentTotal = "points") %>%
  knitr::kable(.)
```

|  student| diagnosis           |  probability|
|--------:|:--------------------|------------:|
|        1| withdrawal behavior |    0.6706221|
|        2| positive re-framing |    0.5589742|

Notice in the above we did not user `mapsyms()`. This is because the `data.table` code ([from here](http://www.win-vector.com/blog/2018/01/base-r-can-be-fast/#comment-66751)) used a mixture of standard (column names as string values) and non-standard (column names captured from free-symbol names) notations. In this case we suggest using the convention we used here:

-   Build a name map using the `qc(...) := c(..)` notation. Reserve uppercase symbols for symbols to be replaced and lower case symbols for variable holding names of columns.
-   Given the above use the uppercase version of each symbol in non-standard contexts, and lower-case in standard contexts.

The above convention is *very* powerful.

Note for `:=` to work we must have `wrapr`'s definition active, which we achieved by loading the `wrapr` package after loading the `data.table` package. `data.table`'s use of `:=` should continue to be correct as that is always performed by `data.table` itself, where `wrapr`'s definition can not interfere.

And that is our lesson on some of the `wrapr` notations. I hope you can incorporate `wrapr` into your work, and please do check out some of our additional training materials.
