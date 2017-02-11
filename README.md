<!-- README.md is generated from README.Rmd. Please edit that file -->
This document describes `wrapr`, an [R](https://cran.r-project.org) package available from [Github](https://github.com/WinVector/wrapr) and [CRAN](https://CRAN.R-project.org/).

Introduction
------------

Primary `wrapr` services include:

-   `wrapr::let()`
-   `wrapr::DebugFnW()`

`wrapr::let()`
--------------

`wrapr::let()` allows execution of arbitrary code with substituted variable names (note this is subtly different than binding values for names as with `base::substitute()` or `base::with()`). Please see `vignette('let', package='wrapr')` for examples.

`wrapr::DebugFnW()`
-------------------

`wrapr::DebugFnW()` wraps a function for debugging. If the function throws an exception the execution context (function arguments, function name, and more) is captured and stored for the user. The function call can then be reconstituted, inspected and even re-run with a step-debugger. Please see `vignette('DebugFnW', package='wrapr')` for examples.
