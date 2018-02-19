
#' Produce a temp name generator with a given prefix.
#'
#' Returns a function f where: f() returns a new temporary name,
#' f(remove=vector) removes names in vector and returns what was removed,
#' f(dumpList=TRUE) returns the list of names generated and clears the list,
#' f(peek=TRUE) returns the list without altering anything.
#'
#' @param prefix character, string to prefix temp names with.
#' @param ... force later argument to be bound by name.
#' @param alphabet character, characters to choose from in building ids.
#' @param size character, number of characters to build id portion of names from.
#' @param sep character, separator between temp name fields.
#' @return name generator function.
#'
#' @examples
#'
#' f <- mk_tmp_name_source('ex')
#' print(f())
#' nm2 <- f()
#' print(nm2)
#' f(remove=nm2)
#' print(f(dumpList=TRUE))
#'
#' @export
mk_tmp_name_source <- function(prefix = "tmpnam",
                               ...,
                               alphabet = as.character(0:9),
                               size = 20,
                               sep = "_") {
  stop_if_dot_args(substitute(list(...)),
                          "wrapr::mk_tmp_name_source")
  force(prefix)
  force(alphabet)
  force(size)
  force(sep)
  if((length(prefix)!=1)||(!is.character(prefix))) {
    stop("wrapr::mk_tmp_name_source prefix must be a string")
  }
  idstr <- paste(base::sample(alphabet, size=size, replace= TRUE),
                 collapse = '')
  count <- 0
  nameList <- list()
  function(..., peek=FALSE, dumpList=FALSE, remove=NULL) {
    stop_if_dot_args(substitute(list(...)),
                            "wrapr tmp name source")
    if(peek) {
      return(names(nameList))
    }
    if(dumpList) {
      v <- names(nameList)
      nameList <<- list()
      return(v)
    }
    if(!is.null(remove)) {
      victims <- intersect(remove, names(nameList))
      # this removes from lists
      nameList[victims] <<- NULL
      return(victims)
    }
    nm <- paste(prefix, idstr, sprintf('%010d',count), sep = sep)
    nameList[[nm]] <<- 1
    count <<- count + 1
    nm
  }
}
