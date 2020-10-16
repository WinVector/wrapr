

test_unpack_unpack <- function() {
  # named unpacking
  # looks like assignment: DESTINATION = NAME_VALUE_USING
  d <- data.frame(x = 1:2,
                  g=c('test', 'train'),
                  stringsAsFactors = FALSE)

  unpack[train_set = train, test_set = test] <- split(d, d$g)
  # train_set and test_set now correctly split
  expect_true(test_set$g[[1]] == 'test')
  expect_true(train_set$g[[1]] == 'train')
  rm(list = c('train_set', 'test_set'))

  # again with self in local environment
  # named unpacking
  # looks like assignment: DESTINATION = NAME_VALUE_USING
  d <- data.frame(x = 1:2,
                  g=c('test', 'train'),
                  stringsAsFactors = FALSE)
  unpack[train_set = train, test_set = test] <- split(d, d$g)
  # train_set and test_set now correctly split
  expect_true(test_set$g[[1]] == 'test')
  expect_true(train_set$g[[1]] == 'train')
  rm(list = c('train_set', 'test_set'))

  split(d, d$g) %.>% unpack[train_set = train, test_set = test]
  # train_set and test_set now correctly split
  expect_true(test_set$g[[1]] == 'test')
  expect_true(train_set$g[[1]] == 'train')
  rm(list = c('train_set', 'test_set'))

  # named unpacking NEWNAME = OLDNAME implicit form
  # values are matched by name, not index
  unpack[train, test] <- split(d, d$g)
  expect_true(test$g[[1]] == 'test')
  expect_true(train$g[[1]] == 'train')
  rm(list = c('train', 'test'))

  # function version
  unpack(split(d, d$g), train, test)
  expect_true(test$g[[1]] == 'test')
  expect_true(train$g[[1]] == 'train')
  rm(list = c('train', 'test'))

  # pipe version
  split(d, d$g) %.>% unpack(., train, test)
  expect_true(test$g[[1]] == 'test')
  expect_true(train$g[[1]] == 'train')
  rm(list = c('train', 'test'))
}

test_unpack_unpack()



test_unpack_to <- function() {
  # named unpacking
  # looks like assignment: DESTINATION = NAME_VALUE_USING
  d <- data.frame(x = 1:2,
                  g=c('test', 'train'),
                  stringsAsFactors = FALSE)

  to[train_set = train, test_set = test] <- split(d, d$g)
  # train_set and test_set now correctly split
  expect_true(test_set$g[[1]] == 'test')
  expect_true(train_set$g[[1]] == 'train')
  rm(list = c('train_set', 'test_set'))

  # again with self in local environment
  # named unpacking
  # looks like assignment: DESTINATION = NAME_VALUE_USING
  d <- data.frame(x = 1:2,
                  g=c('test', 'train'),
                  stringsAsFactors = FALSE)
  to[train_set = train, test_set = test] <- split(d, d$g)
  # train_set and test_set now correctly split
  expect_true(test_set$g[[1]] == 'test')
  expect_true(train_set$g[[1]] == 'train')
  rm(list = c('train_set', 'test_set'))

  split(d, d$g) %.>% to[train_set = train, test_set = test]
  # train_set and test_set now correctly split
  expect_true(test_set$g[[1]] == 'test')
  expect_true(train_set$g[[1]] == 'train')
  rm(list = c('train_set', 'test_set'))

  # named unpacking NEWNAME = OLDNAME implicit form
  # values are matched by name, not index
  to[train, test] <- split(d, d$g)
  expect_true(test$g[[1]] == 'test')
  expect_true(train$g[[1]] == 'train')
  rm(list = c('train', 'test'))

  # pipe version (no dot)
  split(d, d$g) %.>% to(train, test)
  expect_true(test$g[[1]] == 'test')
  expect_true(train$g[[1]] == 'train')
  rm(list = c('train', 'test'))
}

test_unpack_to()




test_partial_unpack_specification <- function() {
  list(a = 1, b = 2) -> to[e = a, b]
  expect_equal(e, 1)
  expect_equal(b, 2)
  invisible(NULL)
}

test_partial_unpack_specification()



test_grab_rewrite <- function() {
  f <- function(...) {
    unpack_environment <- parent.frame(n = 1)
    args <- substitute(list(...))
    grab_assignments_from_dots(args)
  }
  v <- f(a, c = d, e := f, g <- h, i -> j)
  expect_true(identical(v, c('a', 'c' = 'd', 'e' = 'f', 'g' = 'h', 'j' = 'i')))
}

test_grab_rewrite()


test_partial_unpack_specification2 <- function() {
  list(a = 1, b = 2) -> to[e <- a, b]
  expect_equal(e, 1)
  expect_equal(b, 2)
  invisible(NULL)
}

test_partial_unpack_specification2()



test_unpack_bquote_position <- function() {
  aname <- 'a'
  bname <- 'b'
  # allowed
  unpack(data.frame(a = 1, b = 2), a, b = b)
  # allowed
  unpack(data.frame(a = 1, b = 2), a = .(aname), b)
  # not allowed
  expect_error(unpack(data.frame(a = 1, b = 2), .(aname), b))
  # not allowed
  expect_error(unpack(data.frame(a = 1, b = 2), .(aname) := a, b))
  # not allowed
  expect_error(unpack(data.frame(a = 1, b = 2), x = .(aname) := a, b))
}

test_unpack_bquote_position()



