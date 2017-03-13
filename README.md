<!-- README.md is generated from README.Rmd. Please edit that file -->
This document describes `wrapr`, an [R](https://cran.r-project.org) package available from [Github](https://github.com/WinVector/wrapr) and [CRAN](https://CRAN.R-project.org/).

Introduction
------------

`wrapr` wraps `R` functions debugging and better standard evaluation.

![](tools/wraprs.png)

Primary `wrapr` services include:

-   `wrapr::let()`
-   `wrapr::DebugFnW()`

`wrapr::let()`
--------------

`wrapr::let()` allows execution of arbitrary code with substituted variable names (note this is subtly different than binding values for names as with `base::substitute()` or `base::with()`). Please see `vignette('let', package='wrapr')` for examples.

`wrapr::DebugFnW()`
-------------------

`wrapr::DebugFnW()` wraps a function for debugging. If the function throws an exception the execution context (function arguments, function name, and more) is captured and stored for the user. The function call can then be reconstituted, inspected and even re-run with a step-debugger. Please see `vignette('DebugFnW', package='wrapr')` for examples.

`dump.frames`
=============

Note: `wrapr` debug functionality rehashes some of the capabilities of `dump.frames` (see `help(dump.frames)`). Roughly `dump.frames` catches the exception (so trying to step or continue re-throws, and arguments may have moved from their starting values) and `wrapr` catches the call causing the exception in a state *prior* to starting the calculation (so arguments should be at their starting values). We have found some cases where `wrapr` is a bit more convenient in how it interacts with the `RStudio` visual debugger (please see this [screencast](https://youtu.be/2NCj4Hacm8E?list=PLAKBwakacHbQT51nPHex1on3YNCCmggZA) for some comparison). Also, please see [this article](http://www.win-vector.com/blog/2012/10/error-handling-in-r/) for use of <code>tryCatch</code> and <code>withRestarts</code>.
