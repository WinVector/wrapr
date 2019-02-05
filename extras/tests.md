
There seems to be a general (false) impression among non R-core developers that to run tests, `R` package developers need a test management system such as `RUnit` or `testthat`. And a further false impression that `testthat` is the only `R` test management system. This is in fact not true, as `R` itself has a capable testing facility in "`R CMD check`" (a command triggering `R` from outside of any given integrated development environment).

By a combination of skimming the `R`-manuals ( [https://cran.r-project.org/manuals.html](https://cran.r-project.org/manuals.html) ) and running a few experiments I came up with a description of how `R`-testing actually works.



### A glimpse of the `R` test ecosystem:

  1) During "`R CMD check`", `R` runs all `.R` files (and `.r` files) in the `tests` directory. It counts tests as failures if the test raises an exception (for example calls `stop()`), or if the text output does not match what is already in a .Rout.save file in the same directory.
  2) The contents of the `tests` directory are written into source-distribution packages, but not written into binary-distribution packages.
  3) The contents of the inst directory are copied into the root-level of package distributions.
  4) [`RUnit`](https://CRAN.R-project.org/package=RUnit) (released June 2004) itself collects test suites from directories and then runs them, recording user assertions in a JUnit-inspired report.  The idea is: once you have a bunch of tests you really want to track them some way.
  5) [`testthat`](https://CRAN.R-project.org/package=testthat) (released November 2009) self-describes as integrating into a workflow. It runs tests found in the `tests/testthat` sub-directory and tracks user assertions. The related devtools/usethis package both writes a canonical test controlling file into the `tests` directory (allowing `testthat` to be triggered by "`R CMD check`"), and can also directly run tests.
  6) [`unitizer`](https://CRAN.R-project.org/package=unitizer) (released April 2017) bases its tests on comparisons of objects (not comparison text or requiring user assertions) with aids in producing and updating reference objects.
  7) [`tinytest`](https://github.com/markvanderloo/tinytest) (pre-release) decouples the ideas of test failures from exceptions.


### Uses of tests

Tests are used in different ways.  A lot of consusion is a failure to separate these ways. Some common ways tests are used include:

  1) Acceptance unit tests. These are test that must succeed for a package to be considered usable.  These are tests that cause the package to be rejected by CRAN or by end-users if they fail.
  2) Weak integration tests. Integration tests are different than unit tests, but there is some overlap. For packages that are tightly coupled a wrong version of one package can cause a related package to fail. In this case it make a lot of sense to expose the tests to the users, so they can run these tests.
  3) Tests that represent development goals. These tests can be from test-drive development, or reproducible errors incorporated from submitted issues. These tests may be in a failing state for some time These tests are more private to the package developer and should not be distributed to CRAN or to the end users.
  
Confusion between these (and additional) categories of use, or assuming there is only one use of tests is a source of many testing arguments.


### Observations (based on above):

  1) `R` packaged developers do not need to use a test system such as `RUnit` or `testthat` to run tests.  The data.table package is a great example of this: a core package running thousands of tests, without needing an external testing package.  The "`R CMD check`" mechanism seems optimized to support case 1 from the "Uses of tests" section.
  2) If you wish to allow end-users to run tests for binary distributed packages, the package developer must place them somewhere other than in `tests`.  My suggestion is in `inst/unit_tests` which will get installed at the top-level of packages and is find-able with the system.file() command.  The `wrapr` adapter for `RUnit` is designed to support cases 1 and 2.
  3) Package developers need the ability to run tests from both their sources (which `RUnit` and `testthat` both supply) and also from installed copies of their package (which `RUnit` supplies as `RUnit` is path oriented not package oriented, and `testthat` supplies through the test_dir() command).  `RUnit` and `testthat::test_dir()` seem to support case 3.
  4) The same package may be distributed to users either in binary or source fasion.  A user may recieve a binary package from CRAN if they are a non-unix using a current (or near-current) version of `R`.  They will recieve a source version if they are running an obsolete version of `R`, or if the package has not yet been built by CRAN for their version of `R`. Because tests in the `tests` directory are present in source versions of packages and not in binary versions of packages this means the user may or may not get tests.  This seems like needless variation. Any needless variation is a possible source of confusion and errors. In my opinion the user should never get tests, or always get tests.  Since there is no way to strip tests out after CRAN submission I suggest the user always get tests. 


### Criticisms:

  * `RUnit` doesn't seem to suggest a canonical test-launcher file.  A "do things exactly this way, and this is how you signal an error to `R CMD check`."  This isn't a big deal as it is just a few lines of code (scan directories for tests, run tests, and trigger a externally observable failure if there are any test failures). But there is a huge difference between a task being 99% done and actually done. To this end I am now prototyping this complete test-running advice here: [https://github.com/WinVector/wrapr/blob/master/tests/package_test_runner.R](https://github.com/WinVector/wrapr/blob/master/tests/package_test_runner.R) .
  * `testthat` is a popular testing facility as it is installed by devtools/usethis.  However, as it is recommended devtools by a "Suggests" package, the test control file written by devtools/usethis is incorrect, as it has an unqualified "`library(testthat)`" in it. So there is in fact no guarantee that `testthat` is in fact going to be installed on the machine testing the package. Correct behavior would be to check for the presence of the `testthat` package and skip testing if not present.  The Rcpp `RUnit` tests ( [https://github.com/RcppCore/Rcpp/blob/master/tests/doRUnit.R](https://github.com/RcppCore/Rcpp/blob/master/tests/doRUnit.R) ) are an example of correctly checking if the testrunner is present before loading the test runner package.  Likely to avoid pain most test infrastructures have `RUnit` and `testthat` pre-loaded, but it is incorrect to count on this.



### My advice.

For your package think a bit on what you want from testing, instead of uncritically following popular procedures. As in all cases: how `R` actually works is described in the manuals ( [https://cran.r-project.org/manuals.html](https://cran.r-project.org/manuals.html) ), and may not be what you heard on the street.

  * If you want absolute minimal dependencies: do not use any test runner, but instead user `R`'s in-built testing capabilities.
  * It is a good development practice to minimize the unnecessary differences between the developer's environment and the user's environment. To my mind this is an argument for distributing your basic tests.  `RUnit` makes distributing tests easy, `testthat` by default does not.
  * `RUnit` does not supply a suggested canonical single step way to integrate with "`R CMD check`", `testthat` does supply such advice (via devtools::use_testthat()). However it is easy to develop a canonical `RUnit` use pattern. I advise including code such as the following [https://github.com/WinVector/wrapr/blob/master/tests/package_test_runner.R](https://github.com/WinVector/wrapr/blob/master/tests/package_test_runner.R) (plus the matching function when it is released) and telling users they can run test by running the lines in this file.
  * If you want to emphasize test reporting you may want to consider `unitizer` or `tinytest` (when released).
  * I repeat, in current `R` many user-visible failures are due to environment and undeclared simultaneous package version details. Many users like to test a package before investing time coding against it.  Also, a failing user-run test is in fact a reproducible failure example which can be very valuable in filing issues.  Having tests present allows a package developer to ask "have you tried the tests?"


