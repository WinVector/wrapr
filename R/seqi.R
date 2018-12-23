
#' Increasing whole-number sequence.
#'
#' Return an in increaing whole-number sequence from a to b inclusive (return integer(0) if none such). Allows for safe iteraton.
#'
#' @param a scalar lower bound
#' @param b scalar upper bound
#' @return whole number sequence
#'
#' @examples
#'
#' # print 3, 4, and then 5
#' for(i in seqi(3, 5)) {
#'    print(i)
#' }
#'
#' # empty
#' for(i in seqi(5, 2)) {
#'    print(i)
#' }
#'
#' @export
#'
seqi <- function(a, b) {
  a = ceiling(a)
  b = floor(b)
  if(a>b) {
    return(integer(0))
  }
  base::seq(a, b, by = 1L)
}