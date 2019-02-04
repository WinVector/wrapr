
There seems to be a general (false) impression that to run tests R package developers need a test management system such as RUnit or testthat. And a further false impression that testthat is the only R test management system. This is in fact not true, R itself has a capable testing facility in "R CMD check" (a command triggering R from outside of any given integrated development environment).

By a combination of skimming the R-manuals ( [https://cran.r-project.org/manuals.html](https://cran.r-project.org/manuals.html) ) and running a few experiments I came up with a description of how R-testing actually works.



The facts:

  1) During "R CMD check", R runs all .R files (and .r files) in the tests directory. It counts tests as failures if the test raises an exception (for example calls stop()), or if the output does not match what is already in a .Rout.save file in the same directory.
  2) The contents of the tests directory are written into source-distribution packages, but not written into binary-distribution packages.
  3) The contents of the inst directory are copied into the root-level of package distributions.
  4) RUnit (released June 2004) itself collects test suites from directories and then runs them, recording results in a JUnit report.  The idea is: one you have a bunch of tests you really want to track them some way.
  5) testthat (released November 2009) self-describes as integrating into a workflow. It  runs tests found in the testts/testthat sub-directory and tracks results. The related devtools package both writes a canonical test controlling file into the tests directory (allowing testthat to be triggered by "R CMD check"), and can also directly run tests.



The observations/conclusions (based on above):

  1) R packaged developers do not need to use a test system such as RUnit or testthat to  run tests.  The data.table package is a great example of this: a core package running thousands of tests, without needing an external testing package.
  2) The file that devtools writes into tests is incorrect for most packages, as it has an unqualified "library(testthat)" in it. As the official advice is to put your testthat as a package "Suggests", there is in fact no guarantee that testthat is in fact going to be installed on the machine testing the package. Correct behavior would be to check for the presence of the testthat package and skip testing if not present. The Rcpp tests ( [https://github.com/RcppCore/Rcpp/blob/master/tests/doRUnit.R](https://github.com/RcppCore/Rcpp/blob/master/tests/doRUnit.R) ) are an example of correctly checking if the testrunner is present before loading the test runner package.  Likely to avoid pain most test infrastructures have RUnit and testthat pre-loaded, but it is incorrect to count on this.
  3) Unlike testthat, RUnit doesn't seem to suggest a canonical test-launcher file.  This isn't a big deal as it is just a few lines of code (scan directories for tests, run tests, and trigger a externally observable failure if there are any test failures). But there is a huge difference between a task being 99% done and actually done. To this end I am now prototyping this complete test-running advice here: [https://github.com/WinVector/wrapr/blob/master/tests/package_test_runner.R](https://github.com/WinVector/wrapr/blob/master/tests/package_test_runner.R) .
  4) If you wish to allow end-users to run tests for binary distributed packages, the package developer must place them somewhere other than in tests.  My suggestion is in inst/unit_tests which will get installed at the top-level of packages and is findable with the system.file() command.
  5) Package developers need the ability to run tests from both their sources (which RUnit and testthat both supply) and also from installed copies of their package (which RUnit supplies as RUnit is path oriented not package oriented, and testthat supplies through the test_dir() command).



My advice.

For your package think a bit on what you want from testing, instead of uncritically following popular procedures. As in all cases: how R actually works is described in the manuals ( [https://cran.r-project.org/manuals.html](https://cran.r-project.org/manuals.html) ), and may not be what you heard on the street.

  * If you want absolute minimal dependencies: do not use any test runner, but instead user R's in-built testing capabilities.
  * It is a good development practice to minimize the unnecessary differences between the developer's environment and the user's environment. To my mind this is an argument for distributing your basic tests.  RUnit makes distributing tests easy, testthat does not.
  * RUnit does not supply a suggested canonical single step way to integrate with "R CMD check", testthat does supply such advice (via devtools::use_testthat()).  However it is easy to develop a canonical RUnit user pattern. I advise including code such as the following [https://github.com/WinVector/wrapr/blob/master/tests/package_test_runner.R](https://github.com/WinVector/wrapr/blob/master/tests/package_test_runner.R) (plus the matching function when it is released) and telling users they can run test by running the lines in this file.
  * If you want reporting and integration with other testing systems, you want to stay close to *-Unit style as this is used by many other languages.
  * Many user-visible failures are due to environment and multiple package version details. Many users like to test a package before coding against it.  Also, a failing user-run test is in fact a reproducible failure example which can be very valuable in filing issues.


