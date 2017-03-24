

## Test environments

 * local OS X install x86_64-apple-darwin13.4.0 (64-bit) 10.12.3
 * R version 3.3.3
 * win-builder (devel and release) 

## R CMD check --as-cran wrapr_0.1.2.tar.gz

* using R version 3.3.3 (2017-03-06)
* using platform: x86_64-apple-darwin13.4.0 (64-bit)
* using session charset: UTF-8
* using option ‘--as-cran’
* checking for file ‘wrapr/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘wrapr’ version ‘0.1.2’
* package encoding: UTF-8
* checking CRAN incoming feasibility ...

Status: OK

## Downstream dependencies

Checked against all downstream dependencies.

  devtools::revdep('wrapr')
  [1] "replyr"  "WVPlots"
