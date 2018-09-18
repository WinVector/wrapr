
is_infix <- function(vi) {
  vi <- as.character(vi)
  if(nchar(vi)<=0) {
    return(FALSE)
  }
  if(substr(vi,1,1)=="`") {
    vi <- substr(vi,2,nchar(vi)-1)
  }
  if(nchar(vi)<=0) {
    return(FALSE)
  }
  if(substr(vi,1,1)=="%") {
    return(TRUE)
  }
  syms <- c("::", "$", "@", "^", ":",
            "*", "/", "+", "-",
            ">", ">=", "<", "<=",  "==", "!=",
            "&", "&&",
            "|", "||",
            "~",
            "->",  "->>",
            "=",
            "<-", "<<-")
  if(vi %in% syms) {
    return(TRUE)
  }
  return(FALSE)
}

#' Build a data.frame from the user's description.
#'
#' A convenient way to build a data.frame in legible transposed form.  Position of
#' first "|" (or other infix operator) determines number of columns
#' (all other infix operators are aliases for ",").
#' Names are de-referenced.
#'
#' @param ... cell names, first infix operator denotes end of header row of column names.
#' @param cf_eval_environment environment to evaluate names in.
#' @return character data.frame
#'
#' @seealso \code{\link{draw_frame}}, \code{\link{qchar_frame}}
#'
#' @examples
#'
#' tc_name <- "training"
#' x <- build_frame(
#'    "measure",                   tc_name, "validation" |
#'    "minus binary cross entropy",      5, -7           |
#'    "accuracy",                      0.8, 0.6          )
#' print(x)
#' str(x)
#' cat(draw_frame(x))
#'
#' build_frame(
#'   "x" |
#'   -1  |
#'   2   )
#'
#' @export
#'
build_frame <- function(..., cf_eval_environment = parent.frame()) {
  v <- as.list(substitute(list(...))[-1])
  force(cf_eval_environment)
  lv <- length(v)
  # inspect input
  if(lv<1) {
    return(data.frame())
  }
  # unpack
  unpack_val <- function(vi) {
    if(length(vi)<=0) {
      stop("wrapr::build_frame unexpected NULL/empty element")
    }
    if(is.name(vi)) {
      viv <- cf_eval_environment[[as.character(vi)]]
      if(is.name(viv)) {
        stop(paste("wrapr::build_frame name",
                   vi,
                   "resolved to another name:",
                   viv))
      }
      if(is.call(viv)) {
        stop(paste("wrapr::build_frame name",
                   vi,
                   "resolved to call",
                   viv))
      }
      if(length(viv)<=0) {
        stop(paste("wrapr::build_frame name",
                   vi,
                   "resolved to NULL"))
      }
      vi <- viv
    }
    if(is.call(vi)) {
      if((length(vi)==3) && (is_infix(vi[[1]]))) {
        vi <- list(unpack_val(vi[[2]]),
                   as.name("sep"),
                   unpack_val(vi[[3]]))
      } else {
        viv <- eval(vi,
                   envir = cf_eval_environment,
                   enclos = cf_eval_environment)
        if(is.name(viv)) {
          stop(paste("wrapr::build_frame eval",
                     vi,
                     "resolved to another name:",
                     viv))
        }
        if(length(viv)<=0) {
          stop(paste("wrapr::build_frame eval",
                     vi,
                     "resolved to NULL"))
        }
        vi <- viv
      }
    }
    Reduce(c, lapply(vi, as.list))
  }
  vu <- lapply(v, unpack_val)
  vu <- Reduce(c, lapply(vu, as.list))
  ncol <- length(vu)
  if(ncol<1) {
    stop("wrapr::build_frame() zero columns")
  }
  is_name <- vapply(vu, is.name, logical(1))
  if(any(is_name)) {
    ncol <- which(is_name)[[1]]-1
    vu <- vu[!is_name] # filter out names
  }
  nrow <- (length(vu)/ncol) - 1
  if(abs(nrow - round(nrow))>0.1) {
    stop("wrapr::build_frame confused as to cell count")
  }
  if(nrow<=0) {
    fr <- data.frame(x = logical(0))
    colnames(fr) <- as.character(vu[[1]])
    if(ncol>1) {
      for(i in 2:ncol) {
        ci <- as.character(vu[[i]])
        fr[[ci]] <- logical(0)
      }
    }
  } else {
    seq <- seq_len(nrow)*ncol
    fr <- data.frame(x = unlist(vu[seq + 1],
                                recursive = FALSE,
                                use.names = FALSE),
                     stringsAsFactors = FALSE)
    colnames(fr) <- as.character(vu[[1]])
    if(ncol>1) {
      for(i in 2:ncol) {
        ci <- as.character(vu[[i]])
        fr[[ci]] <-  unlist(vu[seq + i],
                            recursive = FALSE,
                            use.names = FALSE)
      }
    }
  }
  rownames(fr) <- NULL
  fr
}


#' Render a simple data.frame in build_frame format.
#'
#' @param x data.frame (with atomic types).
#' @param ... not used for values, forces later arguments to bind by name.
#' @param time_format character, format for "POSIXt" classes.
#' @param formatC_options named list, options for formatC()- used on numerics.
#' @return character
#'
#' @seealso \code{\link{build_frame}},  \code{\link{qchar_frame}}
#'
#' @examples
#'
#' tc_name <- "training"
#' x <- build_frame(
#'   "measure"                   , tc_name, "validation", "idx" |
#'   "minus binary cross entropy", 5      , 7           , 1L    |
#'   "accuracy"                  , 0.8    , 0.6         , 2L    )
#' print(x)
#' cat(draw_frame(x))
#'
#'
#' @export
#'
draw_frame <- function(x,
                       ...,
                       time_format = "%Y-%m-%d %H:%M:%S",
                       formatC_options = list()) {
  wrapr::stop_if_dot_args(substitute(list(...)), "wrapr::draw_frame")
  formatC_args = list(digits = NULL,
                      width = NULL,
                      format = NULL,
                      flag = "",
                      mode = NULL,
                      big.mark = "",
                      big.interval = 3L,
                      small.mark = "",
                      small.interval = 5L,
                      decimal.mark = getOption("OutDec"),
                      preserve.width = "individual",
                      zero.print = NULL,
                      drop0trailing = FALSE)
  x_s <- substitute(x)
  for(oi in names(formatC_options)) {
    formatC_args[[oi]] <- formatC_options[[oi]]
  }
  if(!is.data.frame(x)) {
    stop("draw_frame x needs to be a data.frame")
  }
  res <- "wrapr::build_frame()"
  nrow <- nrow(x)
  ncol <- ncol(x)
  if((nrow>=1) && (ncol<1)) {
    stop("wrapr::draw_frame bad input: no columns, but has rows")
  }
  qts <- function(v) {
    # wayts to quote: dput(), shQuote(), deparse()
    vapply(as.character(v),
           function(vi) {
             deparse(vi)
           },
           character(1))
  }
  if((nrow<1) || (ncol<1)) {
    if(ncol>=1) {
      res <- paste(qts(colnames(x)), collapse = ", ")
      res <- paste0("wrapr::build_frame(", res, ")")
    }
  } else {
    # convert to character matrix
    xq <- x

    for(ci in colnames(x)) {
      if("POSIXt" %in% class(x[[ci]])) {
        xq[[ci]] <- paste0("\"",
                           format(x[[ci]], time_format),
                           "\"")
      } else if(is.character(x[[ci]]) || is.factor(x[[ci]])) {
        xq[[ci]] <- qts(as.character(x[[ci]]))
      } else if(is.integer(x[[ci]])) {
        xq[[ci]] <- paste0(format(x[[ci]], scientific = FALSE), "L")
      } else if(is.numeric(x[[ci]])) {
        xq[[ci]] <- formatC(x[[ci]],
                            digits = formatC_args$digits,
                            width =  formatC_args$width,
                            format =  formatC_args$format,
                            flag =  formatC_args$flag,
                            mode =  formatC_args$mode,
                            big.mark =  formatC_args$big.mark,
                            big.interval =  formatC_args$big.interval,
                            small.mark =  formatC_args$small.mark,
                            small.interval =  formatC_args$small.interval,
                            decimal.mark =  formatC_args$decimal.mark,
                            preserve.width =  formatC_args$preserve.width,
                            zero.print =  formatC_args$zero.print,
                            drop0trailing =  formatC_args$drop0trailing)
      } else {
        xq[[ci]] <- as.character(x[[ci]])
      }
      xq[[ci]][is.na(x[[ci]])] <- NA
    }
    xm <- as.matrix(xq)
    xm <- matrix(data = as.character(xm),
                 nrow = nrow, ncol = ncol)
    # convert header to values
    xm <- rbind(matrix(data = qts(colnames(x)),
                       nrow = 1, ncol = ncol),
                xm)
    # compute padding
    widths <- nchar(xm)
    widths[is.na(as.numeric(widths))] <- 2
    colmaxes <- matrix(data = apply(widths, 2, max),
                       nrow = nrow+1, ncol = ncol,
                       byrow = TRUE)
    padlens <- colmaxes - widths
    pads <- matrix(data = vapply(padlens,
                                 function(vi) {
                                   paste(rep(' ', vi), collapse = '')
                                 }, character(1)),
                   nrow = nrow+1, ncol = ncol)
    # get intermediates
    seps <- matrix(data = ", ",
                   nrow = nrow+1, ncol = ncol)
    seps[, ncol] <- " |"
    seps[nrow+1, ncol] <- " )"
    # format
    fmt <- matrix(data = paste0(xm, pads, seps),
                  nrow = nrow+1, ncol = ncol)
    rlist <- vapply(seq_len(nrow+1),
                    function(i) {
                      paste(fmt[i, , drop=TRUE], collapse = '')
                    }, character(1))
    rlist <- paste0("   ", rlist)
    res <- paste(rlist, collapse = "\n")
    res <- paste0("wrapr::build_frame(\n", res, "\n")
  }
  if(is.name(x_s)) {
    res <- paste0(as.character(x_s), " <- ", res)
  }
  res
}



#' Build a quoted data.frame.
#'
#' A convenient way to build a character data.frame in legible transposed form.  Position of
#' first "|" (or other infix operator) determines number of columns
#' (all other infix operators are aliases for ",").
#' Names are treated as character types.
#'
#' @param ... cell names, first infix operator denotes end of header row of column names.
#' @return character data.frame
#'
#' @seealso \code{\link{draw_frame}}, \code{\link{build_frame}}
#'
#' @examples
#'
#' x <- qchar_frame(
#'    measure,                      training, validation |
#'    "minus binary cross entropy", loss,     val_loss   |
#'    accuracy,                     acc,      val_acc    )
#' print(x)
#' str(x)
#' cat(draw_frame(x))
#'
#' qchar_frame(
#'   x |
#'   1 |
#'   2 )
#'
#' @export
#'
qchar_frame <- function(...) {
  v <- as.list(substitute(list(...))[-1])
  lv <- length(v)
  env <- parent.frame()
  if(lv<1) {
    return(data.frame())
  }
  # inspect input
  cls <- vapply(v, class, character(1))
  if(length(setdiff(cls, c("character", "call", "name")))>0) {
    stop("wrapr::qchar_frame expect only strings, names, +, and commas")
  }
  if(sum(cls=="call") < 1) {
    # no rows case
    fr <- data.frame(x = character(0),
                     stringsAsFactors = FALSE)
    colnames(fr) <- as.character(v[[1]])
    if(lv>1) {
      for(i in 2:lv) {
        fr[[as.character(v[[i]])]] <- character(0)
      }
    }
    rownames(fr) <- NULL
    return(fr)
  }
  ncol <- match("call", cls)
  # unpack
  unpack_val <- function(vi) {
    if(length(vi)<=0) {
      stop("wrapr::qchar_frame unexpected NULL/empty element")
    }
    if(is.call(vi)) {
      if((length(vi)!=3) || (!is_infix(vi[[1]]))) {
        stop(paste("wrapr::qchar_frame unexpected operator", vi[[1]]))
      }
      vi <- lapply(as.list(vi)[-1], unpack_val)
    }
    as.character(unlist(vi))
  }
  vu <- lapply(v, unpack_val)
  vu <- unlist(vu)
  ncell <- length(vu)
  nrow <- ncell/ncol
  if(abs(nrow - round(nrow))>0.1) {
    stop("wrapr::qchar_frame confused as to cell count")
  }
  fr <- as.data.frame(matrix(data = vu[-seq_len(ncol)],
                             ncol=ncol,
                             byrow = TRUE),
                      stringsAsFactors = FALSE)
  colnames(fr) <- vu[seq_len(ncol)]
  rownames(fr) <- NULL
  fr
}

