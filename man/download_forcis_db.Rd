% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/download_forcis_db.R
\name{download_forcis_db}
\alias{download_forcis_db}
\title{Download the FORCIS database}
\usage{
download_forcis_db(
  path,
  version = options()$forcis_version,
  check_for_update = options()$forcis_check_for_update,
  overwrite = FALSE,
  timeout = 60
)
}
\arguments{
\item{path}{a \code{character} of length 1. The folder in which the FORCIS
database will be saved. Note that a subdirectory will be created, e.g.
\verb{forcis-db/version-99/} (with \code{99} the version number).}

\item{version}{a \code{character} of length 1. The version number (with two
numbers, e.g. \code{08} instead of \code{8}) of the FORCIS database to use.
Default is the latest version. Note that this argument can be handle with
the global option \code{forcis_version}. For example, if user calls
\code{options(forcis_version = "07")}, the version \code{07} will be used by default
for the current R session. Note that it is recommended to use the latest
version of the database.}

\item{check_for_update}{a \code{logical}. If \code{TRUE} (default) the function will
check if a newer version of the FORCIS database is available on Zenodo
and will print an informative message. Note that this argument can be
handle with the global option \code{forcis_check_for_update}. For example, if
user calls \code{options(forcis_check_for_update = FALSE)}, the message to
download the latest version will be disabled for the current R session.}

\item{overwrite}{a \code{logical}. If \code{TRUE} it will override the downloaded
files of the FORCIS database. Default is \code{FALSE}.}

\item{timeout}{an \code{integer}. The timeout for downloading files from Zenodo.
Default is \code{60}. This number can be increased for low Internet connection.}
}
\value{
No return value. The FORCIS files will be saved in the \code{path} folder.
}
\description{
Downloads the entire FORCIS database as a collection of five \code{csv} files from
Zenodo (\url{https://zenodo.org/doi/10.5281/zenodo.7390791}). Additional
files will be also downloaded.
}
\details{
The FORCIS database is regularly updated. The global structure of the tables
doesn’t change between versions but some bugs can be fixed and new records
can be added. This is why it is recommended to use the latest version of the
database. The package is designed to handle the versioning of the database on
Zenodo and will inform the user if a new version is available each time
he/she uses one of the \verb{read_*_data()} functions.

For more information, please read the vignette available at
\url{https://docs.ropensci.org/forcis/articles/database-versions.html}.
}
\examples{
\donttest{
# Folder in which the database will be saved ----
# N.B. In this example we use a temporary folder but you should select an
# existing folder (for instance "data/").
path <- tempdir()

# Download the database ----
download_forcis_db(path, timeout = 300)

# Check the content of the folder ----
list.files(path, recursive = TRUE)
}
}
\references{
Chaabane S, De Garidel-Thoron T, Giraud X, \emph{et al.} (2023) The FORCIS
database: A global census of planktonic Foraminifera from ocean waters.
\emph{Scientific Data}, 10, 354.
DOI: \doi{10.1038/s41597-023-02264-2}.
}
\seealso{
\code{\link[=read_plankton_nets_data]{read_plankton_nets_data()}} to import the FORCIS database.
}
