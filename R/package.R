

#' @description
#' \code{wrapr}: Wrap R Functions for Debugging and Parametric Programming
#'
#'
#' Provides \code{DebugFnW()} to capture function context on error for
#' debugging, and \code{let()} which converts non-standard evaluation interfaces to
#' parametric standard evaluation interfaces.
#' \code{DebugFnW()} captures the calling function and arguments prior to the
#' call causing the exception, while
#' the classic \code{options(error=dump.frames)} form captures at the
#' moment of the exception
#' itself (thus function arguments may not be at their starting values).
#' \code{let()} rebinds (possibly unbound) names to names.
#'
#'For more information:
#' \itemize{
#'   \item \code{vignette('DebugFnW', package='wrapr')}
#'   \item \code{vignette('let', package='wrapr')}
#'   \item \code{vignette(package='wrapr')}
#'   \item Website: \url{https://github.com/WinVector/wrapr}
#'   \item \code{let} video: \url{https://youtu.be/iKLGxzzm9Hk?list=PLAKBwakacHbQp_Z66asDnjn-0qttTO-o9}
#'   \item Debug wrapper video: \url{https://youtu.be/zFEC9-1XSN8?list=PLAKBwakacHbQT51nPHex1on3YNCCmggZA}.}
#'
"_PACKAGE"
