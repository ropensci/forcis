% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filter_by_month.R
\name{filter_by_month}
\alias{filter_by_month}
\title{Filter FORCIS data by month of sampling}
\usage{
filter_by_month(data, months)
}
\arguments{
\item{data}{a \code{tibble} or a \code{data.frame}. One obtained by \verb{read_*_data()}
functions.}

\item{months}{a \code{numeric} containing one or several months.}
}
\value{
A \code{tibble} containing a subset of \code{data} for the desired months.
}
\description{
Filters FORCIS data by month of sampling.
}
\examples{
# Import example dataset ----
file_name <- system.file(file.path("extdata", "FORCIS_net_sample.csv"),
                         package = "forcis")

net_data <- read.csv(file_name)

# Dimensions of the data.frame ----
dim(net_data)

# Filter by months ----
net_data_sub <- filter_by_month(net_data, months = 1:2)

# Dimensions of the data.frame ----
dim(net_data_sub)
}
