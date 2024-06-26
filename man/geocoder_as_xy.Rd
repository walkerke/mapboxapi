% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/search.R
\name{geocoder_as_xy}
\alias{geocoder_as_xy}
\title{Convert the results of a \code{mapboxGeocoderInput()} geocoded location to XY coordinates}
\usage{
geocoder_as_xy(input)
}
\arguments{
\item{input}{The name of the Shiny input using \code{mapboxGeocoderInput()}, likely something like \code{input$geocode}}
}
\value{
A length-2 vector representing the geocoded longitude/latitude coordinates of the location.
}
\description{
Convert the results of a \code{mapboxGeocoderInput()} geocoded location to XY coordinates
}
