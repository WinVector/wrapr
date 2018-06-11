
#' Partition as set of talbes into a list.
#'
#' Partition a set of tables into a list of sets of tables.  Note: removes rownames.
#'
#' @param tables_used charater, names of tables to look for.
#' @param partition_column character, name of column to partition by (tables should not have NAs in this column).
#' @param ... force later arguments to bind by name.
#' @param source_usage optional named map from tables_used names to sets of columns used.
#' @param source_limit optional numeric scalar limit on rows wanted every source.
#' @param tables named map from tables_used names to data.frames.
#' @param env environment to also look for tables named by tables_used
#' @return list of names maps of data.frames partitioned by partition_column.
#'
#' @seealso \code{\link{execute_parallel}}
#'
#' @examples
#'
#' d <- data.frame(a = 1:5, g = c(1, 1, 2, 2, 2))
#' partition_tables("d", "g", tables = list(d = d))
#'
#' @export
#'
partition_tables <- function(tables_used,
                             partition_column,
                             ...,
                             source_usage = NULL,
                             source_limit = NULL,
                             tables = NULL,
                             env = NULL) {
  wrapr::stop_if_dot_args(substitute(list(...)), "rqdatatable::partition_tables")
  if((!is.character(partition_column)) || (length(partition_column)!=1)) {
    stop("rqdatatable::partition_tables partition column must be a single string")
  }
  # make sure all needed tables are in the ntables list
  ntables <- list()
  for(ni in tables_used) {
    if(!is.null(tables)) {
      ti <- tables[[ni]]
    }
    if(is.null(ti) && (!is.null(env))) {
      ti <- get(ni, envir = env) # should throw if not found
    }
    if(is.null(ti)) {
      stop(paste("rqdatatable::partition_tables could not find table:", ni))
    }
    # front-load some error checking
    if(!is.data.frame(ti)) {
      stop(paste("rqdatatable::partition_tables all arguments must resolve to data.frames",
                 ni, paste(class(ti), collapse = " ")))
    }
    nti <- colnames(ti)
    if(!is.null(source_usage)) {
      nsi <- source_usage[[ni]]
      missing <- setdiff(nsi, nti)
      if(length(missing)>0) {
        stop(paste("rqdatatable::ex_data_table_parallel missing required columns",
                   ni, paste(missing, collapse = " ")))
      }
      if((partition_column %in% nti) && (!(partition_column %in% nsi))) {
        # preserve partition column
        nsi <- c(nsi, partition_column)
      }
    } else {
      nsi <- nti
    }
    if(is.null(source_limit) || (nrow(ti)>source_limit)) {
      ti <- ti[ , nsi, drop = FALSE]
    } else {
      ti <- ti[seq_len(source_limit), nsi, drop = FALSE]
    }
    rownames(ti) <- NULL
    ntables[[ni]] <- ti
  }
  ti <- NULL
  env <- NULL
  tables <- NULL
  # get a list of values of the partition column
  levels <- c()
  for(ni in names(ntables)) {
    ti <- ntables[[ni]]
    if(partition_column %in% colnames(ti)) {
      if(any(is.na(ti[[partition_column]]))) {
        stop(paste("rqdatatable::ex_data_table_parallel NAs in partation_column for table ",
                   ni))
      }
      levels <- unique(c(levels, as.character(ti[[partition_column]])))
    }
  }
  if(length(levels)<=0) {
    stop(paste("rqdatatable::ex_data_table_parallel no values found for partition column", partition_column))
  }
  # build a list of tablesets
  tablesets <- lapply(levels,
                      function(li) {
                        nti <- ntables
                        for(ni in names(ntables)) {
                          ti <- ntables[[ni]]
                          if(partition_column %in% colnames(ti)) {
                            ti <- ti[as.character(ti[[partition_column]])==li, , drop = FALSE]
                          }
                          rownames(ti) <- NULL
                          nti[[ni]] <- as.data.frame(ti)
                        }
                        nti
                      })
  names(tablesets) <- levels
  tablesets
}


#' Execute f in parallel partition ed by partition_column.
#'
#' Execute f in parallel partiation by \code{partition_column}, see
#' \code{\link{partition_tables}} for details.
#'
#' @param tables named map of tables to use.
#' @param partition_column character name of column to partition on
#' @param f function to apply to each tableset signature is function takes a single argument that is a named list of data.frames.
#' @param ... force later arguments to bind by name.
#' @param cl parallel cluster.
#' @param debug logical if TRUE use lapply instead of parallel::clusterApplyLB.
#' @param env environment to look for values in.
#' @return list of f evaluations.
#'
#' @seealso \code{\link{partition_tables}}
#'
#' @examples
#'
#' if(requireNamespace("parallel", quietly = TRUE)) {
#'   cl <- parallel::makeCluster(2)
#'
#'   d <- data.frame(x = 1:5, g = c(1, 1, 2, 2 ,2))
#'   f <- function(dl) {
#'     d <- dl$d
#'     d$s <- sqrt(d$x)
#'     d
#'   }
#'   r <- execute_parallel(list(d = d), f,
#'                         partition_column = "g",
#'                         cl = cl) %.>%
#'     do.call(rbind, .) %.>%
#'     print(.)
#'
#'   parallel::stopCluster(cl)
#' }
#'
#' @export
#'
execute_parallel <- function(tables,
                             f,
                             partition_column,
                             ...,
                             cl = NULL,
                             debug = FALSE,
                             env = parent.frame()) {
  tablesets <- partition_tables(names(tables),
                                partition_column = partition_column,
                                tables = tables,
                                env = env)
  if(debug || (!requireNamespace("parallel", quietly = TRUE))) {
    res <- lapply(tablesets, f)
  } else {
    # dispatch the operation in parallel
    res <- parallel::clusterApplyLB(cl, tablesets, f)
  }
  names(res) <- names(tablesets)
  res
}

