% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bootstrap_measures.R
\name{bootstrap_measures}
\alias{bootstrap_measures}
\title{Bootstrap customer data from an A/B-test}
\usage{
bootstrap_measures(input_data_frame, bootstrap_iterations = 100, ...)
}
\arguments{
\item{input_data_frame}{A grouped data frame with one row per
customer/visitor. It should contain a (grouped-by) column with the test
variant and additional columns for each measure in the test for that
customer/visitor.}

\item{bootstrap_iterations}{The number of bootstrap iterations to calculate,
the default is 100.}

\item{...}{<\code{\link[dplyr:dplyr_data_masking]{data-masking}}> Name-value pairs of
summary functions. The name will be the name of the variable in the result.

The value can be:
\itemize{
\item A vector of length 1, e.g. \code{min(x)}, \code{n()}, or \code{sum(is.na(y))}.
\item A vector of length \code{n}, e.g. \code{quantile()}.
\item A data frame, to add multiple columns from a single expression.
}}
}
\value{
Returns a boot_strap object (a data frame) with
\code{bootstrap_iterations} rows containing columns as defined in \code{...}
}
\description{
Bootstrap customer data from an A/B-test
}
\examples{
example_data <- data.frame(
                x = rep(stats::runif(10),2),
                group = c("A", "B")
                ) \%>\%
                group_by(group)
bootstrap_measures(example_data, kpi1=mean(x))
}
