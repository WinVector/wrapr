library('wrapr')

context("pipe_paper")

# For testing user S3 functions see:
# https://github.com/r-lib/testthat/issues/266
# https://github.com/r-lib/testthat/issues/720
# https://stackoverflow.com/questions/28099185/how-do-i-re-register-s3-method-inside-r-package
# But assign in namespace is not to be used in packages, so probably not in tests
# So, instead: assign("pipe_step.formula", pipe_step.formula, envir = .GlobalEnv)

test_that("test_pipe_paper.R", {
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
  wrapr_function.glm <- function(pipe_left_arg,
                                 pipe_right_arg,
                                 pipe_environment,
                                 pipe_name) {
    predict(pipe_right_arg,
            newdata = pipe_left_arg,
            type = 'response')
  }
  assign("wrapr_function.glm", wrapr_function.glm, envir = .GlobalEnv)
  d %.>% model
  rm(list = "wrapr_function.glm",  envir = .GlobalEnv)


  ###################################################
  ### code chunk number 13: extq3
  ###################################################
  pipe_step.character <- function(pipe_left_arg,
                                  pipe_right_arg,
                                  pipe_environment,
                                  pipe_name) {
    pipe_right_arg <- eval(pipe_right_arg,
                           envir = pipe_environment,
                           enclos = pipe_environment)
    paste0(pipe_left_arg, pipe_right_arg)
  }
  assign("pipe_step.character", pipe_step.character, envir = .GlobalEnv)
  `%+%` <- wrapr::`%.>%`
  res <- "a" %+% "b" %+% "c"
  expect_equal("abc", res)
  rm(list = "pipe_step.character",  envir = .GlobalEnv)


  ###################################################
  ### code chunk number 14: extq4
  ###################################################
  pipe_step.formula <- function(pipe_left_arg,
                                pipe_right_arg,
                                pipe_environment,
                                pipe_name) {
    pipe_right_arg <- eval(pipe_right_arg,
                           envir = pipe_environment,
                           enclos = pipe_environment)
    pipe_right_arg <- paste(pipe_right_arg, collapse = " + ")
    update(pipe_left_arg, paste(" ~ . +", pipe_right_arg))
  }
  assign("pipe_step.formula", pipe_step.formula, envir = .GlobalEnv)
  `%+%` <- wrapr::`%.>%`
  (y~a) %+% c("b", "c", "d") %+% "e"
  rm(list = "pipe_step.formula",  envir = .GlobalEnv)


})
