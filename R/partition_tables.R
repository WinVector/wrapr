
#' Partition as set of talbes into a list. (move to wrapr!)
#'
#' @param tables_used charater, names of tables to look for.
#' @param partition_column character, name of column to partition by.
#' @param sources_usage optional named map from tables_used names to sets of columns used.
#' @param source_limit optional numeric scalar limit on rows wanted every source.
#' @param tables named map from tables_used names to data.frames.
#' @param env environment to also look for tables named by tables_used
#' @return list of names maps of data.frames partitioned by partition_column.
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
      ntables[[ni]] <- ti[ , nsi, drop = FALSE]
    } else {
      ntables[[ni]] <- ti[seq_len(source_limit), nsi, drop = FALSE]
    }
  }
  env <- NULL
  tables <- NULL
  # get a list of values of the partition column
  levels <- c()
  for(ni in names(ntables)) {
    ti <- ntables[[ni]]
    if(partition_column %in% colnames(ti)) {
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
                          nti[[ni]] <- as.data.frame(ti)
                        }
                        nti
                      })
  tablesets
}
