

#' Double apply pipe.
#'
#' Defined as roughly : \code{a \%!\% b} ~ \code{a \%.>\% (a \%.>\% b)}.
#' Useful for systems like \code{rquery} where first appearance of an object
#' captures a description and second appearance applies the operation.
#'
#'
#' @param pipe_left_arg left argument expression (substituted into .)
#' @param pipe_right_arg right argument expession (presumably including .)
#' @return \code{pipe_left_arg \%.>\% (pipe_left_arg \%.>\% pipe_right_arg)}
#'
#' @examples
#'
#' # An example function that captures a desciption on
#' # first pass and value on the second.
#' # The meaninful application of this is rquery pipelines.
#' b <- function(a1) {
#'   a_name <- substitute(a1)
#'   function(a2) {
#'     paste0(a_name, ": ", a2)
#'   }
#' }
#'
#' x <- 7
#'
#' x %!% b # "x: 7"
#' 5 %!% b # "5: 5"
#'
#' @name doubleapply
#'
#' @export
`%!%` <- function(pipe_left_arg, pipe_right_arg) {
  pipe_left_arg_s <- substitute(pipe_left_arg)
  pipe_right_arg <- substitute(pipe_right_arg)
  pipe_environment <- parent.frame()
  pipe_string <- as.character(sys.call()[[1]])
  b <- pipe_impl(pipe_left_arg_s, pipe_right_arg,
            pipe_environment, pipe_string)
  pipe_left_arg %.>% b
}
