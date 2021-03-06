% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lambda.R
\name{f.}
\alias{f.}
\title{Build an anonymous function of dot.}
\usage{
f.(body, env = parent.frame())
}
\arguments{
\item{body}{function body}

\item{env}{environment to work in.}
}
\value{
user defined function.
}
\description{
Build an anonymous function of dot.
}
\examples{

f <- f.(sin(.) \%.>\% cos(.))
7 \%.>\% f


}
\seealso{
\code{\link{lambda}}, \code{\link{defineLambda}}, \code{\link{named_map_builder}}, \code{\link{makeFunction_se}}
}
