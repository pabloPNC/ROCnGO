% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/response.R
\name{reorder_response_factor}
\alias{reorder_response_factor}
\title{Establish condition of interest as 1 and absence as 0.}
\usage{
reorder_response_factor(response_fct, condition, absent)
}
\arguments{
\item{response_fct}{A factor with different categories (\code{levels}).}

\item{condition}{Name of category being the condition of interest.}

\item{absent}{Character vector of categories not corresponding to the
condition of interest.}
}
\value{
\code{factor}with values (0, 1) where 1 matches condition of interest.
}
\description{
Transforms levels in a \code{factor} to 1 if they match condition of interest (
\code{condition}) or 0 otherwise (\code{absent}) or 0 otherwise (\code{absent}).
}
\keyword{internal}
