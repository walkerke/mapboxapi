% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/maps.R
\name{prep_overlay_markers}
\alias{prep_overlay_markers}
\title{Prepare overlay markers for use in a Mapbox static map}
\usage{
prep_overlay_markers(
  data = NULL,
  marker_type = c("pin-s", "pin-l", "url"),
  label = NA,
  color = NA,
  longitude = NULL,
  latitude = NULL,
  url = NA
)
}
\arguments{
\item{data}{An input data frame with longitude and latitude columns (X and Y
or lon and lat as names are also acceptable) or an \code{sf} object with
geometry type POINT.}

\item{marker_type}{The marker type; one of \code{"pin-s"}, for a small pin;
\code{"pin-l"}, for a large pin; and \code{"url"}, for an image path. If
marker_type is the same length as the rows in data, a mix of different
marker types are allowed.}

\item{label}{The marker label (optional). Can be a letter, number (0 through
99), or a valid Maki icon (see \url{https://labs.mapbox.com/maki-icons/})
for options).}

\item{color}{The marker color (optional). \code{color} can be specified as a color
name or as a three or six-digit hexadecimal code (with or without the
number sign).}

\item{longitude}{A vector of longitudes; inferred from the input dataset if
\code{data} is provided.}

\item{latitude}{A vector of latitudes; inferred from the input dataset if
\code{data} is provided.}

\item{url}{The URL of the image to be used for the icon if \code{marker_type = "url"}.}
}
\value{
A formatted list of marker specifications that can be passed to the
\link{static_mapbox} function.
}
\description{
Markers are prepared to match GeoJSON
\href{https://github.com/mapbox/mapbox-gl-markers#geojson-marker-spec}{marker-spec}
which is a partial implementation of the GeoJSON
\href{https://github.com/mapbox/simplestyle-spec/tree/master/1.1.0}{simplestyle-spec}
(described as a work-in-progress by Mapbox).
}
