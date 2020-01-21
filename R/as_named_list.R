
#' Capture named objects as a named list.
#'
#' Build a named list from a sequence of named argumetn of the form NAME = VALUE.
#'
#' @param ... argument names (must be names, not strings or values).
#' @return a named list mapping argument names to argument values
#'
#' @examples
#'
#' a <- data.frame(x = 1)
#' b <- 2
#' str(as_named_list(a, b))
#'
#' # an example application for this function is managing saving and
#' # loading values into the workspace.
#' if(FALSE) {
#'   # remotes::install_github("WinVector/wrapr")
#'   library(wrapr)
#'
#'   a <- 5
#'   b <- 7
#'   do_not_want <- 13
#'
#'   # save the elements of our workspace we want
#'   saveRDS(as_named_list(a, b), 'example_data.RDS')
#'
#'   # clear values out of our workspace for the example
#'   rm(list = ls())
#'   ls()
#'   # notice workspace environemnt now empty
#'
#'   # read back while documenting what we expect to
#'   # read in
#'   unpack[a, b] <- readRDS('example_data.RDS')
#'
#'   # confirm what we have, the extra unpack is a side
#'   # effect of the []<- notation. To avoid this instead
#'   # use one of:
#'   #   unpack(readRDS('example_data.RDS'), a, b)
#'   #   readRDS('example_data.RDS') %.>% unpack(., a, b)
#'   #   readRDS('example_data.RDS') %.>% unpack[a, b]
#'   ls()
#'   # notice do_not_want is not present
#'
#'   print(a)
#'
#'   print(b)
#' }
#'
#' @export
#'
as_named_list <- function(...) {
  # get environment to work in
  unpack_environment <- parent.frame(n = 1)
  # capture ... args
  name_args <- as.list(do.call(bquote, list(substitute(list(...)), where = unpack_environment)))[-1]
  n_args <- length(name_args)
  if(n_args <= 0) {
    stop("wrapr::as_named_list expected arguments")
  }
  if(length(names(name_args)) > 0) {
    stop("wrapr::as_named_list did not expect any argument to be bound by name")
  }
  str_args <- character(n_args)
  for(i in seq_len(n_args)) {
    arg_i <- name_args[[i]]
    if(is.null(arg_i)) {
      stop("wrapr::as_named_list expected all arguments to not be NULL")
    }
    if(!(is.name(arg_i) || is.character(arg_i))) {
      stop("wrapr::as_named_list expected all arguments to be names or character")
    }
    carg_i <- as.character(arg_i)
    if(length(carg_i) != 1) {
      stop("wrapr::as_named_list expect all arguments to be length 1")
    }
    if(is.na(carg_i)) {
      stop("wrapr::as_named_list expect all arguments to not be NA")
    }
    if(nchar(carg_i) <= 0) {
      stop("wrapr::as_named_list empty argument (often this means there is an extra comma in the function call)")
    }
    str_args[[i]] <- carg_i
  }
  if(length(unique(str_args)) != n_args) {
    stop("wrapr::as_named_list, expected all argument names to be unique")
  }
  values <- vector(mode = 'list', n_args)
  for(i in seq_len(n_args)) {
    values[[i]] <- get(str_args[[i]], envir = unpack_environment, mode = 'any', inherits = TRUE)
  }
  names(values) <- str_args
  return(values)
}
