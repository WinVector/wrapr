

test_unpack_unpack <- function() {
  # named unpacking
  # looks like assignment: DESTINATION = NAME_VALUE_USING
  d <- data.frame(x = 1:2,
                  g=c('test', 'train'),
                  stringsAsFactors = FALSE)

  unpack[train_set = train, test_set = test] <- split(d, d$g)
  # train_set and test_set now correctly split
  RUnit::checkTrue(test_set$g[[1]] == 'test')
  RUnit::checkTrue(train_set$g[[1]] == 'train')
  rm(list = c('train_set', 'test_set'))

  # again with self in local environment
  # named unpacking
  # looks like assignment: DESTINATION = NAME_VALUE_USING
  d <- data.frame(x = 1:2,
                  g=c('test', 'train'),
                  stringsAsFactors = FALSE)
  unpack[train_set = train, test_set = test] <- split(d, d$g)
  # train_set and test_set now correctly split
  RUnit::checkTrue(test_set$g[[1]] == 'test')
  RUnit::checkTrue(train_set$g[[1]] == 'train')
  rm(list = c('train_set', 'test_set'))

  split(d, d$g) %.>% unpack[train_set = train, test_set = test]
  # train_set and test_set now correctly split
  RUnit::checkTrue(test_set$g[[1]] == 'test')
  RUnit::checkTrue(train_set$g[[1]] == 'train')
  rm(list = c('train_set', 'test_set'))

  # named unpacking NEWNAME = OLDNAME implicit form
  # values are matched by name, not index
  unpack[train, test] <- split(d, d$g)
  RUnit::checkTrue(test$g[[1]] == 'test')
  RUnit::checkTrue(train$g[[1]] == 'train')
  rm(list = c('train', 'test'))

  # function version
  unpack(split(d, d$g), train, test)
  RUnit::checkTrue(test$g[[1]] == 'test')
  RUnit::checkTrue(train$g[[1]] == 'train')
  rm(list = c('train', 'test'))

  # pipe version
  split(d, d$g) %.>% unpack(., train, test)
  RUnit::checkTrue(test$g[[1]] == 'test')
  RUnit::checkTrue(train$g[[1]] == 'train')
  rm(list = c('train', 'test'))
}

test_unpack_into <- function() {
  # named unpacking
  # looks like assignment: DESTINATION = NAME_VALUE_USING
  d <- data.frame(x = 1:2,
                  g=c('test', 'train'),
                  stringsAsFactors = FALSE)

  into[train_set = train, test_set = test] <- split(d, d$g)
  # train_set and test_set now correctly split
  RUnit::checkTrue(test_set$g[[1]] == 'test')
  RUnit::checkTrue(train_set$g[[1]] == 'train')
  rm(list = c('train_set', 'test_set'))

  # again with self in local environment
  # named unpacking
  # looks like assignment: DESTINATION = NAME_VALUE_USING
  d <- data.frame(x = 1:2,
                  g=c('test', 'train'),
                  stringsAsFactors = FALSE)
  into[train_set = train, test_set = test] <- split(d, d$g)
  # train_set and test_set now correctly split
  RUnit::checkTrue(test_set$g[[1]] == 'test')
  RUnit::checkTrue(train_set$g[[1]] == 'train')
  rm(list = c('train_set', 'test_set'))

  split(d, d$g) %.>% into[train_set = train, test_set = test]
  # train_set and test_set now correctly split
  RUnit::checkTrue(test_set$g[[1]] == 'test')
  RUnit::checkTrue(train_set$g[[1]] == 'train')
  rm(list = c('train_set', 'test_set'))

  # named unpacking NEWNAME = OLDNAME implicit form
  # values are matched by name, not index
  into[train, test] <- split(d, d$g)
  RUnit::checkTrue(test$g[[1]] == 'test')
  RUnit::checkTrue(train$g[[1]] == 'train')
  rm(list = c('train', 'test'))

  # function version
  into(split(d, d$g), train, test)
  RUnit::checkTrue(test$g[[1]] == 'test')
  RUnit::checkTrue(train$g[[1]] == 'train')
  rm(list = c('train', 'test'))

  # pipe version
  split(d, d$g) %.>% into(., train, test)
  RUnit::checkTrue(test$g[[1]] == 'test')
  RUnit::checkTrue(train$g[[1]] == 'train')
  rm(list = c('train', 'test'))
}


test_unpack_to <- function() {
  # named unpacking
  # looks like assignment: DESTINATION = NAME_VALUE_USING
  d <- data.frame(x = 1:2,
                  g=c('test', 'train'),
                  stringsAsFactors = FALSE)

  to[train_set = train, test_set = test] <- split(d, d$g)
  # train_set and test_set now correctly split
  RUnit::checkTrue(test_set$g[[1]] == 'test')
  RUnit::checkTrue(train_set$g[[1]] == 'train')
  rm(list = c('train_set', 'test_set'))

  # again with self in local environment
  # named unpacking
  # looks like assignment: DESTINATION = NAME_VALUE_USING
  d <- data.frame(x = 1:2,
                  g=c('test', 'train'),
                  stringsAsFactors = FALSE)
  to[train_set = train, test_set = test] <- split(d, d$g)
  # train_set and test_set now correctly split
  RUnit::checkTrue(test_set$g[[1]] == 'test')
  RUnit::checkTrue(train_set$g[[1]] == 'train')
  rm(list = c('train_set', 'test_set'))

  split(d, d$g) %.>% to[train_set = train, test_set = test]
  # train_set and test_set now correctly split
  RUnit::checkTrue(test_set$g[[1]] == 'test')
  RUnit::checkTrue(train_set$g[[1]] == 'train')
  rm(list = c('train_set', 'test_set'))

  # named unpacking NEWNAME = OLDNAME implicit form
  # values are matched by name, not index
  to[train, test] <- split(d, d$g)
  RUnit::checkTrue(test$g[[1]] == 'test')
  RUnit::checkTrue(train$g[[1]] == 'train')
  rm(list = c('train', 'test'))

  # function version
  to(split(d, d$g), train, test)
  RUnit::checkTrue(test$g[[1]] == 'test')
  RUnit::checkTrue(train$g[[1]] == 'train')
  rm(list = c('train', 'test'))

  # pipe version
  split(d, d$g) %.>% to(., train, test)
  RUnit::checkTrue(test$g[[1]] == 'test')
  RUnit::checkTrue(train$g[[1]] == 'train')
  rm(list = c('train', 'test'))
}


test_unpack_unpack_i <- function() {
  a <- 'x'
  # name capture version
  unpack_i[a, b] <- list(5, 10)
  RUnit::checkEquals(a, 5)
  RUnit::checkEquals(b, 10)

  # bquote re-direct to value in variable
  # plus quotes are allowed
  a <- 'x'
  unpack_i[.(a), 'b'] <- list(20, 40)
  RUnit::checkEquals(a, 'x')
  RUnit::checkEquals(x, 20)
  RUnit::checkEquals(b, 40)

  list(256, 2106) %.>% unpack_i(., a, b)
  RUnit::checkEquals(a, 256)
  RUnit::checkEquals(b, 2106)

  list(2567, 21067) %.>% wrapr::unpack_i(., a, b)
  RUnit::checkEquals(a, 2567)
  RUnit::checkEquals(b, 21067)

  invisible(NULL)
}

test_partial_unpack_specification <- function() {
  list(a = 1, b = 2) -> to[e = a, b]
  RUnit::checkEquals(e, 1)
  RUnit::checkEquals(b, 2)
  invisible(NULL)
}

test_grab_rewrite <- function() {
  f <- function(...) {
    unpack_environment <- parent.frame(n = 1)
    captured_dots <- as.list(do.call(bquote,
                                     list(substitute(list(...)),
                                          where = unpack_environment),
                                     envir = unpack_environment))[-1]
    grab_assignments_from_dots(captured_dots)
  }
  v <- f(a, c = d, e := f, g <- h, i -> j)
  RUnit::checkTrue(identical(v, c('a', 'c' = 'd', 'e' = 'f', 'g' = 'h', 'j' = 'i')))
}


test_partial_unpack_specification2 <- function() {
  list(a = 1, b = 2) -> to[e <- a, b]
  RUnit::checkEquals(e, 1)
  RUnit::checkEquals(b, 2)
  invisible(NULL)
}

