
# From: https://gist.github.com/lionel-/10cd649b31f11512e4aea3b7a98fe381
# renamed piple to `%t>%` to avoid name-collisions.

library("rlang")


#' @import rlang
`%t>%` <- function(x, y) {
  lhs <- rlang:::captureArg(x)
  lhs_value <- eval_bare(lhs$expr, lhs$env)


  rhs <- rlang:::captureArg(y)

  # Rebind the `.` symbol temporarily
  env <- caller_env()
  if (env_has(env, ".")) {
    dot <- env$dot
    on.exit(env_bind(.env = env, `.` = dot))
  } else {
    on.exit(env_unbind(env, "."))
  }
  env$. <- lhs_value

  # Transform symbols to function calls
  if (!is_lang(rhs$expr)) {
    rhs$expr <- new_language(rhs$expr, NULL)
  }

  # If call does not mention the pronoun, we splice it at the front of
  # the argument list
  if (!has_pronoun(rhs$expr)) {
    rhs$expr <- splice_dot(rhs$expr)
  }

  # Propagate visibility. Note that withVisible() doesn't work with
  # rlang::eval_bare()
  rhs_result <- withVisible(eval(rhs$expr, rhs$env))
  if (rhs_result$visible) {
    rhs_result$value
  } else {
    invisible(rhs_result$value)
  }
}


sym_dot <- quote(.)
has_pronoun <- function(node) {
  while(!is_null(node)) {
    if (identical(node_car(node), sym_dot)) {
      return(TRUE)
    }
    node <- node_cdr(node)
  }
  FALSE
}
splice_dot <- function(node) {
  new_cdr <- node(sym_dot, node_cdr(node))
  mut_node_cdr(node, new_cdr)
}
