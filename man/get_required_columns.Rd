% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_required_columns.R
\name{get_required_columns}
\alias{get_required_columns}
\title{Get required column names}
\usage{
get_required_columns()
}
\value{
A \code{character} vector.
}
\description{
Gets required column names (except taxa names) for the package. This
function is designed to help users to add additional columns in
\code{\link[=select_forcis_columns]{select_forcis_columns()}} (argument \code{cols}) if missing from this list.

These columns are required by some functions (\verb{compute_*()}, \verb{plot_*()},
etc.) of the package and shouldn't be deleted.
}
\examples{
# Get required column names (expect taxa names) ----
get_required_columns()
}
