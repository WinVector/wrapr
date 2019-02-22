
I am collecting here some notes on testing in `R`.

There seems to be a general (false) impression among non R-core developers that to run tests, `R` package developers need a test management system such as `RUnit` or `testthat`. And a further false impression that `testthat` is the only `R` test management system. This is in fact not true, as `R` itself has a capable testing facility in "`R CMD check`" (a command triggering `R` from outside of any given integrated development environment).

By a combination of skimming the `R`-manuals ( [https://cran.r-project.org/manuals.html](https://cran.r-project.org/manuals.html) ) and running a few experiments I came up with a description of how `R`-testing actually works. And I have adapted the available tools to fit my current preferred workflow.  This may not be your preferred workflow, but I have and give my reasons below.



### A glimpse of the `R` test ecosystem

  1) During "`R CMD check`", `R` runs all `.R` files (and `.r` files) in the `tests` directory. It counts tests as failures if the test raises an exception (for example calls `stop()`), or if the text output does not match what is already in a `.Rout.save` file in the same directory.
  2) The contents of the `tests` directory are written into source-distribution packages, but not written into binary-distribution packages.
  3) The contents of the `inst` directory are copied into the root-level of package distributions.
  4) [`RUnit`](https://CRAN.R-project.org/package=RUnit) (released June 2004) itself collects test suites from directories and then runs them, recording user assertions in a JUnit-inspired report.  The idea is that once you have a bunch of tests you really want to track them some way.
  5) [`testthat`](https://CRAN.R-project.org/package=testthat) (released November 2009) self-describes as integrating into a workflow. It runs tests found in the `tests/testthat` sub-directory and tracks user assertions. The related `devtools/usethis` package both writes a canonical test controlling file into the `tests` directory (allowing `testthat` to be triggered by "`R CMD check`"), and can also directly run tests.
  6) [`unitizer`](https://CRAN.R-project.org/package=unitizer) (released April 2017) bases its tests on comparisons of objects, rather than comparing text or requiring user assertions. It also aids in producing and updating reference objects.
  7) [`tinytest`](https://github.com/markvanderloo/tinytest) (pre-release) decouples the ideas of test failures from exceptions.


### The different types of tests

There are many reasons for testing, and different ways that tests are used. Much confusion stems from a failure to separate the different motivations behind testing. Some categories of tests include:

  1) **Acceptance unit tests.** These are tests that must succeed for a package to be considered usable.  Failing these tests can cause the package to be rejected by CRAN or by end-users.
  2) **Weak integration tests.** Integration tests are different than unit tests, but there is some overlap. For packages that are tightly coupled a wrong version of one package can cause a related package to fail. In this case it make a lot of sense to expose the tests to the users, so they can check the compatibility of their installed package suites.
  3) **Tests that represent development goals.** These tests can be from test-driven development, or reproducible errors incorporated from submitted issues. These tests may be in a failing state for some time. They are more private to the package developer and should not be distributed to CRAN or to the end users.
  
Confusion between these (and additional) categories of use, or assuming there is only one use of tests, are the sources of many arguments over proper testing procedures and/or appropriate test systems. In fact, it is useful to discuss the currently available test systems in R in light of (at least) the testing scenarios we've just described:

The "`R CMD check`" mechanism seems optimized to support case 1. `RUnit` directly supports case 3; the `wrapr` adapter for `RUnit` (which we will discuss below) is designed to support cases 1 and 2. `testthat::test_check()` supports case 1, and `testthat::test_dir()` supports case 3. `unitizer` and `tinytest` seem to emphasize cases 3 and 1.

### Observations (based on above)

  1) `R` package developers do not need to use a test system such as `RUnit` or `testthat` to run tests.  The `data.table` package is a great example of this: a core package running thousands of tests, without needing an external testing package.  
  2) If you wish to allow end-users to run tests for binary distributed packages, the package developer must place them somewhere other than in `tests`.  My suggestion is put them in `inst/unit_tests`, which will get installed at the top-level of packages and is findable with the `system.file()` command.  
  3) Package developers need the ability to run tests from both their sources (which `RUnit` and `testthat` both supply) and also from installed copies of their package (which `RUnit` supplies, as `RUnit` is path oriented rather than package oriented, and `testthat` supplies through the `test_dir()` command).  
  4) The same package may be distributed to users either in binary or source fashion.  A user may receive a binary package from CRAN if they are a non-Unix using a current (or near-current) version of `R`.  They will receive a source version if they are running an obsolete version of `R`, or if the package has not yet been built by CRAN for their version of `R`. Because tests in the `tests` directory are present in source versions of packages and not in binary versions of packages this means the user may or may not get tests.  This seems like needless variation. Any needless variation is a possible source of confusion and errors. In my opinion the user should never get tests, or always get tests.  Since there is no way to strip CRAN acceptance tests out after CRAN submission, I suggest the user always get tests.
  5) Tests need to be in a canonical place that can be found both by the user and by the test runner.  Relative paths can cause problems, so to reliably run tests we need an anchor point other that the `R`'s current working directory. One such anchor point is the installed package directory structure, which can be searched with `system.file()`.  This argues for tests being part of a the package distribution.  (Another way is to find paths relative to test runner source code, though [the solutions can be problematic](https://stackoverflow.com/questions/1815606/rscript-determine-path-of-the-executing-script), possibly the "`getSrcDirectory(function(){})`" solution is a good fit.)


### Critique

  * `RUnit` doesn't seem to suggest a canonical test-launcher file: a "do things exactly this way, and this is how you signal an error to `R CMD check`" file.  This isn't a big deal, as it is just a few lines of code: scan directories for tests, run tests, and trigger an externally observable failure if there are any test failures. But there is a huge difference between a task being 99% done and actually done. To this end I am now prototyping this complete test-running advice here: [https://github.com/WinVector/wrapr/blob/master/tests/package_test_runner.R](https://github.com/WinVector/wrapr/blob/master/tests/package_test_runner.R).
  * `testthat` is a popular testing facility as it is installed by `devtools/usethis`.  However, note that `devtools/usethis` registers `testthat` as a "Suggests" package; so technically the test control file written by `devtools/usethis` is incorrect, as it has an unqualified "`library(testthat)`" in it. This means there is in fact no guarantee that `testthat` is going to be installed on the machine testing the package. Correct behavior would be to check for the presence of the `testthat` package and skip testing if not present.  The Rcpp `RUnit` tests ( [https://github.com/RcppCore/Rcpp/blob/master/tests/doRUnit.R](https://github.com/RcppCore/Rcpp/blob/master/tests/doRUnit.R) ) are an example of correctly checking if the testrunner is present before loading the test runner package.  Likely to avoid pain most test infrastructures have `RUnit` and `testthat` pre-loaded, but it is incorrect to count on this.


### My advice

For your package think a bit on what you want from testing, instead of uncritically following popular procedures. As in all cases: how `R` actually works is described in the manuals ([https://cran.r-project.org/manuals.html](https://cran.r-project.org/manuals.html)), and may not be what you heard on the street.

  * If you want absolute minimal dependencies, do not use any test runner, but instead use `R`'s built-in testing capabilities.
  * It is a good development practice to minimize the unnecessary differences between the developer's environment and the user's environment. To my mind this is an argument for distributing your basic tests.  `RUnit` makes distributing tests easy, `testthat` by default does not.
  * Keep in mind end-users have different needs than package developers, so different test-entry points may be better for each.
  * If you want to emphasize test reporting you may want to consider `unitizer` or `tinytest` (when released).
  * I repeat, in current `R` many user-visible failures are due to environment and undeclared simultaneous package version details. Many users like to test a package before investing time coding against it.  Also, a failing user-run test is in fact a reproducible failure example which can be very valuable in filing issues.  Having tests present allows a package developer to ask "have you tried the tests?"
  * Consider: why should a user start work with a package without an easy "start by running the standard tests" introduction?

### My current R test setup

To conveniently provide test interfaces both to `R CMD check` and to end-users simultaneously, I now do the following.

  * Add `RUnit` and `wrapr` to the package `Suggests` fields in the package `DESCRIPTION`.
  * Place all tests in `inst/unit_tests` with files names of the form `test_.*\\.R` and zero argument test-functions with names 
  of the form `test_.*` (example [here](https://github.com/WinVector/wrapr/blob/master/inst/unit_tests/test_c.R)).
  * To integrate with `R CMD check`: include code such as [the following](https://github.com/WinVector/wrapr/blob/master/tests/package_test_runner.R) in the tests directory: `tests` (changing the package name to be that of your own package). Note for package developers using RStudio this also integrate the tests with the "Check" button in the Build Pane.
  * To provide convenient user acceptance tests: add a function similar to [`run_sigr_tests()`](https://github.com/WinVector/sigr/blob/master/R/run_sigr_tests.R) to your package.  Then tell your users that to accept your package all they have to do is install it (plus any dependencies) and then, from `R`, run `run_PKGNAME_tests()`. Notice the example we give here is for the [`sigr`]( https://CRAN.R-project.org/package=sigr) package (not the [`wrapr`](https://CRAN.R-project.org/package=wrapr) package).


  
