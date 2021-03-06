% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cf.R
\name{draw_framec}
\alias{draw_framec}
\title{Render a simple data.frame in qchar_frame format.}
\usage{
draw_framec(x, ..., unquote_cols = character(0), adjust_for_auto_indent = 2)
}
\arguments{
\item{x}{data.frame (with character types).}

\item{...}{not used for values, forces later arguments to bind by name.}

\item{unquote_cols}{character, columns to elide quotes from.}

\item{adjust_for_auto_indent}{integer additional after first row padding.}
}
\value{
character
}
\description{
Render a simple data.frame in qchar_frame format.
}
\examples{

controlTable <- wrapr::qchar_frame(
  "flower_part", "Length"     , "Width"     |
    "Petal"    , Petal.Length , Petal.Width |
    "Sepal"    , Sepal.Length , Sepal.Width )
cat(draw_framec(controlTable, unquote_cols = qc(Length, Width)))


}
\seealso{
\code{\link{build_frame}},  \code{\link{qchar_frame}}
}
