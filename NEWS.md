
# wrapr 2.0.9 2022-01-26

 * Document qc() limitations (ref: https://github.com/WinVector/wrapr/issues/15#issue-1046032267 ).
 * Add sx() for https://github.com/WinVector/wrapr/issues/15#issue-1046032267 use case.

# wrapr 2.0.8 2021-06-10

 * Make calls to order stricter (no names, and convert data.frames), 
   see https://stat.ethz.ch/pipermail/r-devel/2020-May/079500.html 
   and https://stat.ethz.ch/pipermail/r-package-devel/2021q1/006550.html
 * Add international character tests for bc().
   
# wrapr 2.0.7 2021-02-03

 * Add bc() blank-separated parsing, suggested by Emil Erik Pula Bellamy Begtrup-Bright \url{https://github.com/WinVector/wrapr/issues/12}.
 * Fix strsplit_capture empty-return type.

# wrapr 2.0.6 2020-12-06

 * Bump version.
 * Add pack().

# wrapr 2.0.5 2020-11-13

 * More ... checks.
 * Better error message for extra comman in unpack.
 * Remove RUnit suggest / test function.

# wrapr 2.0.4 2020-10-16

 * Move to tinytest.

# wrapr 2.0.2 2020-08-10

 * dot-function (f.).
 * character-mode for mk_formula.
 * work around refs such as `\link[utils]{dump.frames}` no longer working with Ben Boker's suggestion `\link[utils:debugger]{dump.frames}` on R-pkg-devel
 * URLs to https where possible.

# wrapr 2.0.0 2020-03-27

 * Allow as_named_list to choose new names.
 * Update references.
 * Stricter tests on unpack/to.
 * Stricter wrapr-pipe value checks.
 * := for names.
 * := for to/unpack.
 * Don't use := for anonymous function construction.
 * Update evalb, and bquote uses including adding .(-) notation.
 * Remove some obsolete methods such as CapturePipeine, as_dot_fn, UnaryFunctions/ApplyTo, and locum.
 * Make pipe_impl public, and document more.
 * Vectorize string interpolation and add operator versions.
 * Fix description.

# wrapr 1.9.6 2020-01-26

 * Add dotpipe_eager_eval_*.
 * Add on unpack method.

# wrapr 1.9.5 2020-01-18

 * remove q= from sQuote to, as that feature wasn't added until newer Rs.

# wrapr 1.9.4 2020-01-16

 * tolerance on check_equiv_frames
 * remove graphics dependency
 * improve ... check message.

# wrapr 1.9.3 2019-11-03

 * := for factors and logical

# wrapr 1.9.2 2019-10-13

 * Fix dev version at request of CRAN, appears to be instability in capturing ... and passing to bquote(). https://github.com/WinVector/wrapr/blob/master/extras/wrapr_dev_CRAN_issue_2019_10_13.md

# wrapr 1.9.1 2019-10-05

 * Fix locum composite line printing.
 * Document .()-escaping in pipe.

# wrapr 1.9.0 2019-09-28

 * Add locum ( https://github.com/WinVector/wrapr/blob/master/Examples/Locum/Locum.md ).

# wrapr 1.8.9 2019-07-24

 * More unit tests.
 * Adjust license.

# wrapr 1.8.8 2019-07-06

 * More work with mk_formula().

# wrapr 1.8.7 2019-05-23

 * Better sinterp semantics (vectorize over values, not template).
 * Looser checks on qchar_frame().

# wrapr 1.8.6 2019-04-02

  * Turn off radix sort to work around https://github.com/WinVector/wrapr/issues/9
  * Make draw_frame() resistent to operator driven re-indentation.
  * Add draw_framec().

# wrapr 1.8.5 2019-03-24

  * More tests.
  * Be more careful with formals().
  * Add frame checking utils.
  * Doc fixes.

# wrapr 1.8.4 2019-02-19

  * Add run_packages_tests() and run_wrapr_tests().
  * Move to RUnit tests.
  * More args to clean_fit.

# wrapr 1.8.3 2019-01-29

 * Add string strsplit_capture() and dotsubs().
 * better bquote_function() and evalb() examples.
 * Allow comparisions in mk_formula(), and general improvements.

# wrapr 1.8.2 2019-01-04

 * Add paste(class(), collapse=" ").
 * Add seqi().
 * Add evalb().

# wrapr 1.8.1 2018-12-13

 * Fix as_fn() environment assignment.

# wrapr 1.8.0 2018-12-11

 * Add generic unary functions.
 * Add .() pipe escaping.
 * Add clean lm() and glm() fitters.
 * Add split_at_brace_pairs.
 * Documentation fixes.
 * Error msg fix.
 * Add %p%, and %.%.
 
# wrapr 1.7.0 2018-11-15

 * bquote enable qc() and other quoting methods.
 * Preserve NA types in draw_frame.
 * Add VectorizeM, vapplym, lapplym.
 * Add bquote_function().
 * Export underbar version of apply_left_default to get non-S3 version of code.
 * Better string concat example.
 * Add %c% and %qc%.
 * Allow qc() to call c().

# wrapr 1.6.3 2018-10-03

 * Make sure parent.frame() is unambiguosly resovled (force()).
 * qe(), qae(), qs() now return character vectors instead of lists.
 * Add psagg().
 * Add grepv().
 * More examples in SubstitutionModes vignette (show we don't need special pairlist case).
 * Allow no-intercept version of formula.
 
# wrapr 1.6.2 2018-09-10

 * Add mk_formula().
 * Documentation fixes.
 * Add %in_block% operator notation.
 * Add orderv().
 
# wrapr 1.6.1 2018-08-21

 * More restrictive apply_right_S4.
 * Clear methods note.
 
# wrapr 1.6.0 2018-08-01
 
 * S4 dispatch apply_right_S4.
 * split based partition_tables().
 * Allow drawing of empty data.frames.
 * Documentation fixes.

# wrapr 1.5.1 2018-07-07

 * make pipe_impl public (but keyworld internal).
 * fix draw_frame quoting.
 * drop old pipe fns.

# wrapr 1.5.0 2018-06-13

 * Rationalize and re-name pipe interfaces to apply_right and apply_left.
 * Fix qc() eval environment and recursion.
 * Add `%?%` coalescing operator.
 * Add reduce/expand operators.
 * Dot pipe tries to preserve names in function calls.
 * Improve error checking and reporting.
 * Add uniques().
 * Add partition_tables() and execute_parallel().

# wrapr 1.4.1 2018-05-17

 * Move dot assignment into S3 step.
 * Refine error checking.
 * Minor documentation fixes.
 * Fix print/visibility treatment.
 * Remove special 1-key case from := .
 * draw_frame NA handling.
 * Add view().
 
# wrapr 1.4.0 2018-04-03

 * More tests and checks.
 * Allow lookups at the top level ($, [], [[]], ::, :::, @).
 * Starting enforcing strict piping rules (with usable error messages).
 * Move base_fns to https://gist.github.com/JohnMount/1982127318654c8631203e5b5d4946ac and seplyr.
 * Minor documentation fixes/improvements.
 * Extend map_to_char() to work without names.
 * wrapr_function right-dispatch falls back to pipe_step left-disptach as default.

# wrapr 1.3.0 2018-03-12

 * base_fns- slight stregnthenings of base operators for piping.
 * draw_frame et. al.- functions for building example data.frames.

# wrapr 1.2.0 2018-02-21
 
 * Add map_upper.
 * Accept x=y in qae().
 * Strengthen :=.
 * Add left S3 dispatch through pipe_step.
 * Add right S3 dispatch through wrapr_function.
 * Add match_order.
 * Remove deprecated methods.
 * Move mk_tmp_name_source to this package.
 * Add stop_if_dot_args.

# wrapr 1.1.1 2018-01-20
 
 * Fix some null/blank substitution issues.
 * Minor documentation improvements.
 
# wrapr 1.1.0 2018-01-03

 * minor doc improvements.
 * fix deparse in qae() and qe().
 * Deprecate ateval(), seval(), beval(), and "stringsubs".
 * add qs().
 * add dereference and class-supplied function to pipes.

# wrapr 1.0.2 2017-12-13

 * add "to dot" pipe.
 * harden := a bit.
 * let qc() have names.
 * add qae().
 * add map_to_char().

# wrapr 1.0.1 2017-11-17

 * Better error msgs.
 * Bit more debugging info.
 * Add grepdf() and qc().

# wrapr 1.0.0 2017-10-04

 * Add mapsyms() function.
 
# wrapr 0.4.2 2017-08-31

 * Work on location of lambda-definition (do NOT write into environment until asked).
 * Minor check fixes.
 
# wrapr 0.4.1 2017-08-24

 * Do not insist let-mapping be invertible.
 * Migrate named map builder and lambda from seplyr.
 
# wrapr 0.4.0 2017-07-22

 * Allow non-strict names.
 * Insist let-mapping be invertible.
 
# wrapr 0.3.0 2017-07-08

 * Introduce "dot arrow" pipe `%.>%`

# wrapr 0.2.0 2017-07-05

 * Switch to abstract syntax tree substitution.
 * Allow variable swaps.

# wrapr 0.1.3 2017-06-13

 * More flexible treatment of `NULL`.
 * Add non string based version of let() replacement.

# wrapr 0.1.2 2017-04-13
 
 * add `ateval()`

# wrapr 0.1.1 2017-03-13

 * Allow names in `let`.

# wrapr 0.1.0 2017-02-10
 
 * First version (some fns, from replyr package).
