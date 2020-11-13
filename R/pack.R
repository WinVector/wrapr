
#' Pack values into a named list.
#'
#' This function packs values given by name into a named list.
#'
#' @param ... values to pack, these should be specified by name (not as constants).
#' @param .wrapr_private_var_env environment to evaluate in
#' @return named list of values
#'
#' @seealso \code{\link{unpack}}
#'
#' @examples
#'
#' x <- 1
#' y <- 2
#' pack(x, y)  # list(x = 1, y = 2)
#'
#' pack(a = x, y)  # list(a = 1, y = 2)
#'
#' pack(a = 5, y)  # list(a = 5, y = 2)
#'
#' pack(1, 2)  # list('1' = 1, '2' = 2)
#'
#' v <- pack(x = 8, y = 9)  # list(x = 8, y = 9)
#' v -> unpack[x, y]
#' print(x)  # 8
#' print(y)  # 9
#'
#' @export
#'
pack <- function(..., .wrapr_private_var_env = parent.frame()) {
  force(.wrapr_private_var_env)
  values <- list(...)
  quoted <- qc(..., .wrapr_private_var_env = .wrapr_private_var_env)
  names <- names(quoted)
  if(is.null(names)) {
    names <- character(length(quoted))
  }
  not_named <- nchar(names) <= 0
  names[not_named] <- as.character(quoted[not_named])
  if(length(names) != length(unique(names))) {
    stop("names were not unique")
  }
  names(values) <- names
  values
}
