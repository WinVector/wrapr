Some wrapr Notations
================
Win-Vector LLC
1/20/2018

I would like to demonstrate some helpful [`wrapr`](https://CRAN.R-project.org/package=wrapr) [`R`](https://www.r-project.org) notation tools that really neaten up your `R` code.

First we demonstrate `wrapr`'s ["named map builder": `:=`](https://winvector.github.io/wrapr/reference/named_map_builder.html).
The named name builder adds names to vectors and lists by nice "names on the left and values on the right" notation.

For example to assign the names "`a`" and "`b`" to the vector `c(1, 2)` we would write the following code.

``` r
library("wrapr")

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
suppressPackageStartupMessages(library("dplyr"))

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

`data.table` function example (notice we do have to build some string constants to for `wrapr` to keep out of its own way during substitution in the presence of mixed standard and non-standard notation; this is a `wrapr` limitation, not at `data.table` issue).

``` r
library("data.table")
```

    ## 
    ## Attaching package: 'data.table'

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     between, first, last

    ## The following object is masked from 'package:wrapr':
    ## 
    ##     :=

``` r
logistic_score <- function(data, scale, 
                           subjectID = "subjectID",
                           surveyCategory = "surveyCategory",
                           assessmentTotal = "assessmentTotal") {
  sc <- surveyCategory
  at <- assessmentTotal
  let(mapsyms(subjectID, surveyCategory, assessmentTotal),
      { 
        dDT <- data.table::data.table(data)
        setnames(dDT, sc, "diagnosis")
        dDT[,expaTs:=exp(assessmentTotal*scale)]
        # precalculate -> this uses gsum internally
        dDT[,sum_expaTs:=sum(expaTs),subjectID] 
        dDT[,probability := expaTs / sum_expaTs]
        dDT[,c(at,"expaTs","sum_expaTs"):=NULL]
        setorder(dDT, subjectID, -probability, diagnosis)
        dDT[,.SD[1],subjectID]
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
