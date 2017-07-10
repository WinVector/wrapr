


#' Add list name as a column to a list of data.frames.
#'
#' @param dlist named list of data.frames
#' @param destinationColumn character, name of new column to add
#' @return list of data frames, each of which as the new destinationColumn.
#'
#' @examples
#'
#' dlist <- list(a = data.frame(x = 1), b = data.frame(x = 2))
#' add_name_column(dlist, 'name')
#'
#' @export
#'
add_name_column <- function(dlist, destinationColumn) {
  res <- dlist
  # since res list is pre-allocated, re-assigning into it
  # should be fast.
  # Please see: http://www.win-vector.com/blog/2015/07/efficient-accumulation-in-r/
  for(ni in names(dlist)) {
    vi <- dlist[[ni]]
    vi[[destinationColumn]] <- ni
    res[[ni]] <- vi
  }
  res
}
