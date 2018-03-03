

# Re-expose basic R data manipulation tools in more pipe friendly wrappers.

#' Evaluate an expression with a data frame acting as the inner environment.
#'
#' References can be forced to the environment with a .e$ prefix
#' and forced to the data frame with a .d$ prefix (failure to
#' lookup returns null). Only works on in-memory data.frames.
#' Part piping with base R series: \url{http://www.win-vector.com/blog/tag/piping-with-base-r/}.
#'
#' @param x data.frame to work with
#' @param expr logical expression to compute per-row
#' @param env environment to work in
#' @return evaluated result
#'
#' @seealso \code{\link[base]{with}}, \code{\link[base]{within}}
#'
#' @examples
#'
#' Temp <- 90
#' Ozone_bound <- 100
#' summary(with_eval(airquality,
#'             (.d$Temp > .e$Temp) &
#'               (!is.na(Ozone)) & (Ozone < Ozone_bound)))
#'
#' @export
#'
with_eval <- function(x, expr, env = parent.frame()) {
  if(!is.data.frame(x)) {
    stop("with_eval expected x to be a data.frame")
  }
  e <- substitute(expr) # capture expression
  eval_env <- new.env(parent = env)
  assign(".d", x, eval_env)   # data prefix .d$
  assign(".e", env, eval_env) # environment prefix .e$
  r <- eval(e, envir = x, enclos = eval_env)
  r
}

#' Pick a subset of rows, evaluating the subset expression
#' as if the columns of x were in the evaluation environment.
#'
#' References can be forced to the environment with a .e$ prefix
#' and forced to the data frame with a .d$ prefix (failure to
#' lookup returns null). Only works on in-memory data.frames.
#' Part piping with base R series: \url{http://www.win-vector.com/blog/tag/piping-with-base-r/}.
#' See also \url{http://www.win-vector.com/blog/2018/02/is-r-basesubset-really-that-bad/}.
#'
#' @param x data.frame to work with
#' @param subset logical expression to compute per-row
#' @param env environment to work in
#' @return data.frame that is the specified subset of the rows of x.
#'
#' @seealso \code{\link[base]{subset}}
#'
#' @examples
#'
#' Temp <- 90
#' Ozone_bound <- 100
#' subset_rows(airquality,
#'             (.d$Temp > .e$Temp) &
#'               (!is.na(Ozone)) & (Ozone < Ozone_bound))
#'
#' @export
#'
subset_rows <- function(x, subset, env = parent.frame()) {
  if(!is.data.frame(x)) {
    stop("subset_rows expected x to be a data.frame")
  }
  if(missing(subset) || (nrow(x)<=0)) {
    return(x)
  }
  e <- substitute(subset) # capture expression
  eval_env <- new.env(parent = env)
  assign(".d", x, eval_env)   # data prefix .d$
  assign(".e", env, eval_env) # environment prefix .e$
  r <- eval(e, envir = x, enclos = eval_env)
  if(!is.logical(r)) {
    stop("subset_rows predicate must evaluate to logical")
  }
  r <- r & !is.na(r)
  if(length(r)!=nrow(x)) {
    stop("subset_rows predicate must have one entry per row")
  }
  x[r, , drop = FALSE]
}

#' Pick a sequence of columns.
#'
#' Only works on in-memory data.frames.
#' Part piping with base R series: \url{http://www.win-vector.com/blog/tag/piping-with-base-r/}.
#'
#' @param x data.frame to work with
#' @param columns character, names of columns
#' @return data.frame that is the specified subset of the columns of x.
#'
#' @examples
#'
#' head(sel_columns(mtcars, c("mpg", "cyl", "disp")))
#'
#' @export
#'
sel_columns <- function(x, columns) {
  if(!is.data.frame(x)) {
    stop("sel_columns expected x to be a data.frame")
  }
  if(missing(columns)) {
    return(x)
  }
  x[ , columns, drop = FALSE]
}

#' Pick a selection of rows (can be used to order).
#'
#' Only works on in-memory data.frames.
#' Part piping with base R series: \url{http://www.win-vector.com/blog/tag/piping-with-base-r/}.
#'
#' @param x data.frame to work with
#' @param rows numeric, row indexes.
#' @return data.frame that is the specified row selection.
#'
#' @examples
#'
#' d <- data.frame(x = c('b', 'a', 'c'))
#' sel_rows(d, d$x)
#'
#' @export
#'
sel_rows <- function(x, rows) {
  if(!is.data.frame(x)) {
    stop("sel_columns expected x to be a data.frame")
  }
  if(missing(rows)) {
    return(x)
  }
  x[rows, , drop = FALSE]
}


#' Evaluate an expression with a data frame acting as the inner environment
#' and assigning back to the data.frame.  Statements are executed sequentially.
#'
#' References can be forced to the environment with a .e$ prefix
#' and forced to the data frame with a .d$ prefix (failure to
#' lookup returns null). Only works on in-memory data.frames.
#' Part piping with base R series: \url{http://www.win-vector.com/blog/tag/piping-with-base-r/}.
#'
#' @param transform_columns_data_frame data.frame to work with
#' @param ...  named exprssions to add to data frame
#' @param transform_columns_env environment to work in
#' @return data frame with additional or altered columns
#'
#' @seealso \code{\link[base]{transform}}, \code{\link[base]{within}}
#'
#' @examples
#'
#' d <- data.frame(x = c(1,2))
#' transform_columns(d, y = x*x, d = 0, z = x + y, d = 1, q = x + d)
#'
#' @export
#'
transform_columns <- function(transform_columns_data_frame,
                              ...,
                              transform_columns_env = parent.frame()) {
  terms <- substitute(list(...))[-1]
  if(!is.data.frame(transform_columns_data_frame)) {
    stop("transform_columns: transform_columns_data_frame must be a data.frame")
  }
  if(length(terms)<=0) {
    stop("transform_columns: expected transforms")
  }
  for(i in seq_len(length(terms))) {
    ni <- names(terms)[[i]]
    if(length(ni)<=0) {
      stop("transform_columns: empty name")
    }
    if( (!is.character(ni)) && (!is.character(ni)) ) {
      stop(paste("transform_columns: odd class for name: ", class(ni)))
    }
    ni <- as.character(ni)
    if(nchar(ni)<=0) {
      stop("transform_columns: empty name")
    }
    ti <- terms[[i]]
    eval_env <- new.env(parent = transform_columns_env)
    assign(".d", transform_columns_data_frame, eval_env)   # data prefix .d$
    assign(".e", transform_columns_env, eval_env) # environment prefix .e$
    transform_columns_data_frame[[ni]] <- eval(ti,
                                               envir = transform_columns_data_frame,
                                               enclos = eval_env)
  }
  transform_columns_data_frame
}



