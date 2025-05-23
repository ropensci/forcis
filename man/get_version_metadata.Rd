% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_version_metadata.R
\name{get_version_metadata}
\alias{get_version_metadata}
\title{Print information of a specific version of the FORCIS database}
\usage{
get_version_metadata(version = NULL)
}
\arguments{
\item{version}{a \code{character} of length 1. The label of the version. Use
\code{\link[=get_available_versions]{get_available_versions()}} to list available versions. If \code{NULL} (default)
the latest version is used.}
}
\value{
A \code{list} with all information about the version, including: \code{title},
\code{doi}, \code{publication_date}, \code{description}, \code{access_right}, \code{creators},
\code{keywords}, \code{version}, \code{resource_type}, \code{license}, and \code{files}.
}
\description{
Prints information of a specific version of the FORCIS database by querying
the Zenodo API (\url{https://developers.zenodo.org}).
}
\examples{
\donttest{
# Get information for the latest version of the FORCIS database ----
get_version_metadata()
}
}
