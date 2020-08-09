
#' Build a custom writeback function that writes state into a user named variable.
#'
#'
#' @param varName character where to write captured state
#' @return writeback function for use with functions such as \code{\link{DebugFnW}}
#'
#' @examples
#'
#' # user function
#' f <- function(i) { (1:10)[[i]] }
#' # capture last error in variable called "lastError"
#' writeBack <- buildNameCallback('lastError')
#' # wrap function with writeBack
#' df <- DebugFnW(writeBack,f)
#' # capture error (Note: tryCatch not needed for user code!)
#' tryCatch(
#'   df(12),
#'    error = function(e) { print(e) })
#' # examine error
#' str(lastError)
#' # redo call, perhaps debugging
#' tryCatch(
#'  do.call(lastError$fn_name, lastError$args),
#'    error = function(e) { print(e) })
#'
#' @export
buildNameCallback <- function(varName) {
  curEnv <- parent.frame()
  writeBack <- function(sit) {
    assign('lastError', sit, envir=curEnv)
  }
  attr(writeBack,'name') <- paste0('writing to variable: "', varName, '"')
  writeBack
}


#' Return an error to a file, environment (no names) or callback
#'
#' @param e caught exception
#' @param saveDest where to save
#' @param cap saved arguments
#' @param wrapperName name of wrapper
#' @param recallString how to call function again
#'
#' @keywords internal
#'
#' @export
#'
returnCapture <- function(e, saveDest, cap, wrapperName,
                          recallString = 'do.call(p$fn, p$args)') {
  es <- trimws(paste(as.character(e), collapse = ' '))
  if(is.null(saveDest)) {
    saveDest <- paste0(tempfile('debug'),'.RDS')
  }
  if(is.name(saveDest)) {
    curEnv <- globalenv()
    assign(as.character(saveDest), cap, envir=curEnv)
    return(paste0("wrapr::", wrapperName,
                  ": wrote error to globalenv() variable '",
                  as.character(saveDest), "'",
                  "\n You can reproduce the error with:",
                  "\n '", recallString,
                  "' (replace 'p' with actual variable name)"))
  }
  if(is.function(saveDest)) {
    saveDest(cap)
    fName <- attr(saveDest, 'name')
    if(!is.null(fName)) {
      return(paste0("wrapr::", wrapperName,
                    ": wrote error to user function: '",
                    fName, "' on catching '", es, "'",
                    "\n You can reproduce the error with:",
                    "\n '", recallString,
                    "' (replace 'p' with actual variable name)"))
    }
    return(paste0("wrapr::", wrapperName,
                  ": wrote error to user function on catching '",
                  es, "'",
                  "\n You can reproduce the error with:",
                  "\n '", recallString,
                  "' (replace 'p' with actual variable name)"))
  }
  if(is.character(saveDest)) {
    saveRDS(object=cap,
            file=saveDest)
    return(paste0("wrapr::", wrapperName, ": wrote '",saveDest,
                  "' on catching '",es,"'",
                  "\n You can reproduce the error with:",
                  "\n'p <- readRDS('",saveDest,
                  "'); ", recallString, "'"))
  }
  return(paste0("wrapr::", wrapperName,
                ": don't know how to write error to '",
                paste(class(saveDest), collapse= " "),
                "' on catching '", es, "'"))
}


#' Capture arguments of exception throwing function call for later debugging.
#'
#' Run fn, save arguments on failure.
#' Please see: \code{vignette("DebugFnW", package="wrapr")}.
#' @seealso \code{\link[utils:debugger]{dump.frames}}, \code{\link{DebugFn}}, \code{\link{DebugFnW}},  \code{\link{DebugFnWE}}, \code{\link{DebugPrintFn}}, \code{\link{DebugFnE}}, \code{\link{DebugPrintFnE}}
#'
#' @param saveDest where to write captured state (determined by type): NULL random temp file, character temp file, name globalenv() variable, and function triggers callback.
#' @param fn function to call
#' @param ... arguments for fn
#' @return fn(...) normally, but if fn(...) throws an exception save to saveDest RDS of list r such that do.call(r$fn,r$args) repeats the call to fn with args.
#'
#' @examples
#'
#' saveDest <- paste0(tempfile('debug'),'.RDS')
#' f <- function(i) { (1:10)[[i]] }
#' # correct run
#' DebugFn(saveDest, f, 5)
#' # now re-run
#' # capture error on incorrect run
#' tryCatch(
#'    DebugFn(saveDest, f, 12),
#'    error = function(e) { print(e) })
#' # examine details
#' situation <- readRDS(saveDest)
#' str(situation)
#' # fix and re-run
#' situation$args[[1]] <- 6
#' do.call(situation$fn_name,situation$args)
#' # clean up
#' file.remove(saveDest)
#'
#' @export
DebugFn <- function(saveDest,fn,...) {
  args <- list(...)
  envir <- parent.frame()
  namedargs <- match.call()
  fn_name <- as.character(namedargs[['fn']])
  force(saveDest)
  force(fn)
  tryCatch({
    do.call(fn, args, envir=envir)
  },
  error = function(e) {
    cap <- list(fn=fn,
                args=args,
                fn_name=fn_name)
    es <- wrapr::returnCapture(e, saveDest, cap, "DebugFn")
    stop(es)
  })
}

#' Wrap a function for debugging.
#'
#' Wrap fn, so it will save arguments on failure.
#' @seealso \code{\link[utils:debugger]{dump.frames}}, \code{\link{DebugFn}}, \code{\link{DebugFnW}},  \code{\link{DebugFnWE}}, \code{\link{DebugPrintFn}}, \code{\link{DebugFnE}}, \code{\link{DebugPrintFnE}}
#' Operator idea from: https://gist.github.com/nassimhaddad/c9c327d10a91dcf9a3370d30dff8ac3d .
#' Please see: \code{vignette("DebugFnW", package="wrapr")}.
#'
#' @param saveDest where to write captured state (determined by type): NULL random temp file, character temp file, name globalenv() variable, and function triggers callback.
#' @param fn function to call
#' @return wrapped function that saves state on error.
#'
#' @examples
#'
#' saveDest <- paste0(tempfile('debug'),'.RDS')
#' f <- function(i) { (1:10)[[i]] }
#' df <- DebugFnW(saveDest,f)
#' # correct run
#' df(5)
#' # now re-run
#' # capture error on incorrect run
#' tryCatch(
#'    df(12),
#'    error = function(e) { print(e) })
#' # examine details
#' situation <- readRDS(saveDest)
#' str(situation)
#' # fix and re-run
#' situation$args[[1]] <- 6
#' do.call(situation$fn,situation$args)
#' # clean up
#' file.remove(saveDest)
#'
#'
#' f <- function(i) { (1:10)[[i]] }
#' curEnv <- environment()
#' writeBack <- function(sit) {
#'    assign('lastError', sit, envir=curEnv)
#' }
#' attr(writeBack,'name') <- 'writeBack'
#' df <- DebugFnW(writeBack,f)
#' tryCatch(
#'    df(12),
#'    error = function(e) { print(e) })
#' str(lastError)
#'
#'
#' @export
DebugFnW <- function(saveDest,fn) {
  namedargs <- match.call()
  fn_name <- as.character(namedargs[['fn']])
  force(saveDest)
  force(fn)
  f2 <- function(...) {
    args <- list(...)
    envir <- parent.frame()
    namedargs <- match.call()
    tryCatch({
      do.call(fn, args, envir=envir)
    },
    error = function(e) {
      cap <- list(fn=fn,
                  args=args,
                  namedargs=namedargs,
                  fn_name=fn_name)
      es <- wrapr::returnCapture(e, saveDest, cap, "DebugFnW")
      stop(es)
    })
  }
  newenv <- new.env(parent = environment(fn))
  assign('fn', fn, envir = newenv)
  assign('fn_name', fn_name, envir = newenv)
  assign('saveDest', saveDest, envir = newenv)
  environment(f2) <- newenv
  f2
}


#' Capture arguments of exception throwing function call for later debugging.
#'
#' Run fn and print result, save arguments on failure.  Use on systems like \code{ggplot()}
#' where some calculation is delayed until \code{print()}.
#' Please see: \code{vignette("DebugFnW", package="wrapr")}.
#'
#' @seealso \code{\link[utils:debugger]{dump.frames}}, \code{\link{DebugFn}}, \code{\link{DebugFnW}},  \code{\link{DebugFnWE}}, \code{\link{DebugPrintFn}}, \code{\link{DebugFnE}}, \code{\link{DebugPrintFnE}}
#'
#' @param saveDest where to write captured state (determined by type): NULL random temp file, character temp file, name globalenv() variable, and function triggers callback.
#' @param fn function to call
#' @param ... arguments for fn
#' @return fn(...) normally, but if fn(...) throws an exception save to saveDest RDS of list r such that do.call(r$fn,r$args) repeats the call to fn with args.
#'
#' @examples
#'
#' saveDest <- paste0(tempfile('debug'),'.RDS')
#' f <- function(i) { (1:10)[[i]] }
#' # correct run
#' DebugPrintFn(saveDest, f, 5)
#' # now re-run
#' # capture error on incorrect run
#' tryCatch(
#'    DebugPrintFn(saveDest, f, 12),
#'    error = function(e) { print(e) })
#' # examine details
#' situation <- readRDS(saveDest)
#' str(situation)
#' # fix and re-run
#' situation$args[[1]] <- 6
#' do.call(situation$fn,situation$args)
#' # clean up
#' file.remove(saveDest)
#'
#' @export
DebugPrintFn <- function(saveDest,fn,...) {
  args <- list(...)
  namedargs <- match.call()
  fn_name <- as.character(namedargs[['fn']])
  envir <- parent.frame()
  force(saveDest)
  force(fn)
  tryCatch({
    res = do.call(fn, args, envir=envir)
    print(res)
    res
  },
  error = function(e) {
    cap <- list(fn=fn,
                args=args,
                fn_name=fn_name)
    es <- wrapr::returnCapture(e, saveDest, cap, "DebugPrintFn")
    stop(es)
  })
}

#' Capture arguments and environment of exception throwing function call for later debugging.
#'
#' Run fn, save arguments, and environment on failure.
#' Please see: \code{vignette("DebugFnW", package="wrapr")}.
#' @seealso \code{\link[utils:debugger]{dump.frames}}, \code{\link{DebugFn}}, \code{\link{DebugFnW}},  \code{\link{DebugFnWE}}, \code{\link{DebugPrintFn}}, \code{\link{DebugFnE}}, \code{\link{DebugPrintFnE}}
#'
#' @param saveDest where to write captured state (determined by type): NULL random temp file, character temp file, name globalenv() variable, and function triggers callback.
#' @param fn function to call
#' @param ... arguments for fn
#' @return fn(...) normally, but if fn(...) throws an exception save to saveDest RDS of list r such that do.call(r$fn,r$args) repeats the call to fn with args.
#'
#' @examples
#'
#' saveDest <- paste0(tempfile('debug'),'.RDS')
#' f <- function(i) { (1:10)[[i]] }
#' # correct run
#' DebugFnE(saveDest, f, 5)
#' # now re-run
#' # capture error on incorrect run
#' tryCatch(
#'    DebugFnE(saveDest, f, 12),
#'    error = function(e) { print(e) })
#' # examine details
#' situation <- readRDS(saveDest)
#' str(situation)
#' # fix and re-run
#' situation$args[[1]] <- 6
#' do.call(situation$fn, situation$args, envir=situation$env)
#' # clean up
#' file.remove(saveDest)
#'
#' @export
DebugFnE <- function(saveDest,fn,...) {
  args <- list(...)
  envir <- parent.frame()
  namedargs <- match.call()
  fn_name <- as.character(namedargs[['fn']])
  force(saveDest)
  force(fn)
  tryCatch({
    do.call(fn, args, envir=envir)
  },
  error = function(e) {
    cap <- list(fn=fn,
                args=args,
                env=envir,
                fn_name=fn_name)
    es <- wrapr::returnCapture(e, saveDest, cap, "DebugFnE",
                               recallString = 'do.call(p$fn, p$args, envir= p$env)')
    stop(es)
  })
}


#' Wrap function to capture arguments and environment of exception throwing function call for later debugging.
#'
#' Wrap fn, so it will save arguments and environment on failure.
#' Please see: \code{vignette("DebugFnW", package="wrapr")}.
#' @seealso \code{\link[utils:debugger]{dump.frames}}, \code{\link{DebugFn}}, \code{\link{DebugFnW}},  \code{\link{DebugFnWE}}, \code{\link{DebugPrintFn}}, \code{\link{DebugFnE}}, \code{\link{DebugPrintFnE}}
#'
#' Idea from: https://gist.github.com/nassimhaddad/c9c327d10a91dcf9a3370d30dff8ac3d
#'
#' @param saveDest where to write captured state (determined by type): NULL random temp file, character temp file, name globalenv() variable, and function triggers callback.
#' @param fn function to call
#' @param ... arguments for fn
#' @return wrapped function that captures state on error.
#'
#' @examples
#'
#' saveDest <- paste0(tempfile('debug'),'.RDS')
#' f <- function(i) { (1:10)[[i]] }
#' df <- DebugFnWE(saveDest, f)
#' # correct run
#' df(5)
#' # now re-run
#' # capture error on incorrect run
#' tryCatch(
#'    df(12),
#'    error = function(e) { print(e) })
#' # examine details
#' situation <- readRDS(saveDest)
#' str(situation)
#' # fix and re-run
#' situation$args[[1]] <- 6
#' do.call(situation$fn, situation$args, envir=situation$env)
#' # clean up
#' file.remove(saveDest)
#'
#' @export
DebugFnWE <- function(saveDest,fn,...) {
  namedargs <- match.call()
  fn_name <- as.character(namedargs[['fn']])
  force(saveDest)
  force(fn)
  f2 <- function(...) {
    args <- list(...)
    envir <- parent.frame()
    namedargs <- match.call()
    tryCatch({
      do.call(fn, args, envir=envir)
    },
    error = function(e) {
      cap <- list(fn=fn,
                  args=args,
                  namedargs=namedargs,
                  fn_name=fn_name,
                  env=envir)
      es <- wrapr::returnCapture(e, saveDest, cap, "DebugFnWE",
                                 recallString = 'do.call(p$fn, p$args, envir= p$env)')
      stop(es)
    })
  }
  newenv <- new.env(parent = environment(fn))
  assign('fn', fn, envir = newenv)
  assign('fn_name', fn_name, envir = newenv)
  assign('saveDest', saveDest, envir = newenv)
  environment(f2) <- newenv
  f2
}

#' Capture arguments and environment of exception throwing function call for later debugging.
#'
#' Run fn and print result, save arguments and environment on failure.  Use on systems like ggplot()
#' where some calculation is delayed until print().
#' Please see: \code{vignette("DebugFnW", package="wrapr")}.
#' @seealso \code{\link[utils:debugger]{dump.frames}}, \code{\link{DebugFn}}, \code{\link{DebugFnW}},  \code{\link{DebugFnWE}}, \code{\link{DebugPrintFn}}, \code{\link{DebugFnE}}, \code{\link{DebugPrintFnE}}
#'
#' @param saveDest where to write captured state (determined by type): NULL random temp file, character temp file, name globalenv() variable, and function triggers callback.
#' @param fn function to call
#' @param ... arguments for fn
#' @return fn(...) normally, but if fn(...) throws an exception save to saveDest RDS of list r such that do.call(r$fn,r$args) repeats the call to fn with args.
#'
#' @examples
#'
#' saveDest <- paste0(tempfile('debug'),'.RDS')
#' f <- function(i) { (1:10)[[i]] }
#' # correct run
#' DebugPrintFnE(saveDest, f, 5)
#' # now re-run
#' # capture error on incorrect run
#' tryCatch(
#'    DebugPrintFnE(saveDest, f, 12),
#'    error = function(e) { print(e) })
#' # examine details
#' situation <- readRDS(saveDest)
#' str(situation)
#' # fix and re-run
#' situation$args[[1]] <- 6
#' do.call(situation$fn, situation$args, envir=situation$env)
#' # clean up
#' file.remove(saveDest)
#'
#' @export
DebugPrintFnE <- function(saveDest,fn,...) {
  args <- list(...)
  envir <- parent.frame()
  namedargs <- match.call()
  fn_name <- as.character(namedargs[['fn']])
  force(saveDest)
  force(fn)
  tryCatch({
    res = do.call(fn, args, envir=envir)
    print(res)
    res
  },
  error = function(e) {
    cap <- list(fn=fn,
                args=args,
                env=envir,
                fn_name=fn_name)
    es <- wrapr::returnCapture(e, saveDest, cap, "DebugPrintFnE",
                               recallString = 'do.call(p$fn, p$args, envir= p$env)')
    stop(es)
  })
}

