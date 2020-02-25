How `wrapr::unpack` differs from `zeallot::%<-%`
================

Here is an example of how
[`wrapr::unpack`](https://CRAN.R-project.org/package=wrapr) differs from
[`zeallot::%<-%`](https://CRAN.R-project.org/package=zeallot). Let’s
take splitting data into 3 parts: `train`, `calibration`, and `test`.

We can do this with `wrapr::unpack` as follows (using either the `<-`
assignment notation or the `:=` pseudo-assignment notation).

``` r
library(wrapr)

d <- data.frame(
  x = 1:100)

d$group <- sample(
  c('train', 'calibration', 'test'), 
  size = nrow(d),
  replace = TRUE, 
  prob = c(0.7, 0.2, 0.1))

unpack[train, calibration, test] <- split(d, d$group)
```

As `unpack` works by name we always have (as long as all the groups are
non-empty): `train$group == "train"`, `calibration$group ==
"calibration"`, and ’test$group == “test”\`.

``` r
unique(train$group)
```

    ## [1] "train"

``` r
unique(calibration$group)
```

    ## [1] "calibration"

``` r
unique(test$group)
```

    ## [1] "test"

When one of the sets is empty, `unpack` catches and reports it with a
signaling error.

``` r
d$group <- sample(
  c('train', 'calibration', 'test'), 
  size = nrow(d),
  replace = TRUE, 
  prob = c(0.7, 0.2, 0))

unpack[train, calibration, test] <- split(d, d$group)
```

    ## Error in write_values_into_env(unpack_environment = unpack_environment, : wrapr::unpack all source names must be in value, missing: 'test'.

We can see this behavior is stable.

``` r
m <- matrix(0, nrow = 3, ncol = 3)
rownames(m) <- c('train', 'calibration', 'test')
colnames(m) <- c('train', 'calibration', 'test')

for(i in 1:100) {
  d$group <- sample(
  c('train', 'calibration', 'test'), 
  size = nrow(d),
  replace = TRUE, 
  prob = c(0.7, 0.2, 0.1))

  unpack[train, calibration, test] <- split(d, d$group)
  
  for(nm in c('train', 'calibration', 'test')) {
    found = unique(get(nm)$group)
    m[nm, found] = m[nm, found] + 1
  }
}

print(m)
```

    ##             train calibration test
    ## train         100           0    0
    ## calibration     0         100    0
    ## test            0           0  100

`zeallot`, on the other hand, unpacks by position. The first item found
is assigned to the first position. Name matching is not enforced.

``` r
library(zeallot)

d$group <- sample(
  c('train', 'calibration', 'test'), 
  size = nrow(d),
  replace = TRUE, 
  prob = c(0.7, 0.2, 0.1))

c(train, calibration, test) %<-% split(d, d$group)
```

Notice the groups were not unpacked into the desired target names.

``` r
unique(train$group)
```

    ## [1] "calibration"

``` r
unique(calibration$group)
```

    ## [1] "test"

``` r
unique(test$group)
```

    ## [1] "train"

To unpack correctly we have to successfully guess the order of the
results of `split()`. For character vectors this appears to be
alphabetic order (likely due to string to factor conversion). However
this order can vary.

Here we show getting the order correctly by specifying the order to
match our `zeallot` unpacking (we could also get the unpack to work by
writing `c(calibration, test, train) %<-% split(d, d$group)`).

``` r
d$group <- sample(
  factor(c('train', 'calibration', 'test'), 
         levels = c('train', 'calibration', 'test')),
  size = nrow(d),
  replace = TRUE, 
  prob = c(0.7, 0.2, 0.1))

c(train, calibration, test) %<-% split(d, d$group)
```

The groups are now unpacked into the desired target names.

``` r
unique(train$group)
```

    ## [1] train
    ## Levels: train calibration test

``` r
unique(calibration$group)
```

    ## [1] calibration
    ## Levels: train calibration test

``` r
unique(test$group)
```

    ## [1] test
    ## Levels: train calibration test

Our concern is: there is no guarantee in `R` that functions that return
named lists always return the fields in the same order (especially as a
package our function evolves over time). In `Python`, where positional
unpacking is the standard, functions tend to return `tuples`, not named
lists, so it is guaranteed positions are stable. We feel it is more
reliable (and more `R`-like) to unpack from named lists using names.
Some related work (including `zeallot` and a package the precedes
`zeallot`) can be found
[here](https://winvector.github.io/wrapr/reference/unpack.html).
