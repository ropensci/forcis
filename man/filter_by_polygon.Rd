% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filter_by_polygon.R
\name{filter_by_polygon}
\alias{filter_by_polygon}
\title{Filter FORCIS data by a spatial polygon}
\usage{
filter_by_polygon(data, polygon)
}
\arguments{
\item{data}{a \code{tibble} or a \code{data.frame}. One obtained by \verb{read_*_data()}
functions.}

\item{polygon}{an \verb{sf POLYGON} object.}
}
\value{
A \code{tibble} containing a subset of \code{data} for the desired spatial
polygon.
}
\description{
Filters FORCIS data by a spatial polygon.
}
\examples{
# Import example dataset ----
file_name <- system.file(file.path("extdata", "FORCIS_net_sample.csv"),
                         package = "forcis")

net_data <- read.csv(file_name)

# Dimensions of the data.frame ----
dim(net_data)

# Import Indian Ocean spatial polygons ----
file_name <- system.file(file.path("extdata",
                         "IHO_Indian_ocean_polygon.gpkg"),
                         package = "forcis")

indian_ocean <- sf::st_read(file_name)

# Filter by polygon ----
net_data_sub <- filter_by_polygon(net_data, polygon = indian_ocean)

# Dimensions of the data.frame ----
dim(net_data_sub)
}
