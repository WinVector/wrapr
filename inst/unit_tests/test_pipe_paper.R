
# For testing user S3 functions see:
# https://github.com/r-lib/testthat/issues/266
# https://github.com/r-lib/testthat/issues/720
# https://stackoverflow.com/questions/28099185/how-do-i-re-register-s3-method-inside-r-package
# But assign in namespace is not to be used in packages, so probably not in tests
# So, instead: assign("apply_left.formula", apply_left.formula, envir = .GlobalEnv)

test_pipe_paper <- function() {
  ###################################################
  ### code chunk number 2: wpipe1
  ###################################################
  5 %.>% sin(.)


  ###################################################
  ### code chunk number 3: wpipe1e
  ###################################################
  5 %.>% {1 + .}
  5 %.>% (1 + .)



  ###################################################
  ### code chunk number 11: extq1
  ###################################################
  d <- data.frame(x=1:5, y = c(1, 1, 0, 1, 0))
  model <- glm(y~x, family = binomial, data = d)
  apply_right.glm <- function(pipe_left_arg,
                              pipe_right_arg,
                              pipe_environment,
                              left_arg_name,
                              pipe_string,
                              right_arg_name) {
    predict(pipe_right_arg,
            newdata = pipe_left_arg,
            type = 'response')
  }
  if("apply_right.glm" %in% ls(.GlobalEnv)) {
    warning("not testing apply_right.glm as it already has definition")
  } else {
    assign("apply_right.glm", apply_right.glm, envir = .GlobalEnv)
    d %.>% model
    rm(list = "apply_right.glm",  envir = .GlobalEnv)
  }


  ###################################################
  ### code chunk number 13: extq3
  ###################################################
  apply_left.character <- function(pipe_left_arg,
                                   pipe_right_arg,
                                   pipe_environment,
                                   left_arg_name,
                                   pipe_string,
                                   right_arg_name) {
    pipe_right_arg <- eval(pipe_right_arg,
                           envir = pipe_environment,
                           enclos = pipe_environment)
    paste0(pipe_left_arg, pipe_right_arg)
  }
  if("apply_left.character" %in% ls(.GlobalEnv)) {
    warning("not testing apply_left.character as it already has definition")
  } else {
    assign("apply_left.character", apply_left.character, envir = .GlobalEnv)
    `%+%` <- wrapr::`%.>%`
    res <- "a" %+% "b" %+% "c"
    RUnit::checkEquals("abc", res)
    rm(list = "apply_left.character",  envir = .GlobalEnv)
  }


  ###################################################
  ### code chunk number 14: extq4
  ###################################################
  apply_left.formula <- function(pipe_left_arg,
                                 pipe_right_arg,
                                 pipe_environment,
                                 left_arg_name,
                                 pipe_string,
                                 right_arg_name) {
    pipe_right_arg <- eval(pipe_right_arg,
                           envir = pipe_environment,
                           enclos = pipe_environment)
    pipe_right_arg <- paste(pipe_right_arg, collapse = " + ")
    update(pipe_left_arg, paste(" ~ . +", pipe_right_arg))
  }
  if("apply_left.formula" %in% ls(.GlobalEnv)) {
    warning("not testing apply_left.formula as it already has definition")
  } else {
    assign("apply_left.formula", apply_left.formula, envir = .GlobalEnv)
    `%+%` <- wrapr::`%.>%`
    (y~a) %+% c("b", "c", "d") %+% "e"
    rm(list = "apply_left.formula",  envir = .GlobalEnv)
  }

  invisible(NULL)
}
