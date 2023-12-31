% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/build_days.R
\name{build_days}
\alias{build_days}
\title{Calculate day-level summaries from the whenwhat dataset}
\usage{
build_days(
  data = NULL,
  verbose = TRUE,
  where_field = NULL,
  whenwhat_field = NULL,
  loca = NULL,
  participant_basis = NULL
)
}
\arguments{
\item{data}{The whenwhat data obtained from \code{\link{build_whenwhatwhere}}.}

\item{verbose}{Print progress to console. Default is \code{TRUE}.}

\item{where_field}{...}

\item{whenwhat_field}{...}

\item{loca}{Nested list with location information}

\item{participant_basis}{participant_basis
#'}
}
\value{
A table summarised by day.
}
\description{
Build a days dataset by summarising \code{whenwhat}
by day and person (\code{identifier}).
}
