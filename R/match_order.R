

#' Invert a permuation.
#'
#' For a permutation p build q such that p[q] == q[p] == seq_len(length(p)).
#' See \url{http://www.win-vector.com/blog/2017/05/on-indexing-operators-and-composition/}.
#'
#' @param p vector of lenth n containing each of seq_len(n) exactly once.
#' @return vector q such that p[q] == q[p] == seq_len(length(p))
#'
#' @examples
#'
#' p <- c(4, 5, 7, 8, 9, 6, 1, 3, 2, 10)
#' q <- invert_perm(p)
#' p[q]
#' all.equal(p[q], seq_len(length(p)))
#' q[p]
#' all.equal(q[p], seq_len(length(p)))
#'
#' @export
#'
invert_perm <- function(p) {
  # see: http://www.win-vector.com/blog/2017/05/on-indexing-operators-and-composition/
  pinv <- seq_len(length(p))
  pinv[p] <- seq_len(length(p))
  pinv
}


#' Match one order to another.
#'
#' Build a pemutation p such that ids1[p] == ids2.  See \url{http://www.win-vector.com/blog/2017/09/permutation-theory-in-action/}.
#'
#' @param ids1 unique vector of ids.
#' @param ids2 unique vector of ids with sort(ids1)==sort(ids2).
#' @return p integers such that ids1[p] == ids2
#'
#' @examples
#'
#' ids1 <- c(4, 5, 7, 8, 9, 6, 1, 3, 2, 10)
#' ids2 <- c(3, 6, 4, 8, 5, 7, 1, 9,10, 2)
#' p <- match_order(ids1, ids2)
#' ids1[p]
#' all.equal(ids1[p], ids2)
#'
#' @export
#'
match_order <- function(ids1, ids2) {
  p1 <- order(ids1)
  p2 <- order(ids2)
  # invert p2
  p2inv <- invert_perm(p2)
  # composition rule: (o1[p1])[p2inv] == o1[p1[p2inv]]
  # see: http://www.win-vector.com/blog/2017/05/on-indexing-operators-and-composition/
  p <- p1[p2inv]
  p
}
