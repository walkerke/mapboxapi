#' An R interface to Mapbox web services
#'
#' Use Mapbox web services APIs for spatial data science and visualization projects in R. Usage of the package is governed by the Mapbox Terms of Service.
#'
#' @author Kyle Walker
#' @name mapboxapi
#' @import httr
#' @import sf
#' @importFrom jsonlite fromJSON
#' @import purrr
#' @import dplyr
#' @importFrom curl curl_escape
#' @import aws.s3
#' @importFrom rlang flatten_if arg_match abort inform
#' @importFrom stringi stri_rand_strings
#' @import slippymath
#' @import protolite
#' @import geojsonsf
#' @import leaflet
#' @importFrom magick image_read
#' @importFrom tidyr unnest_wider
#' @importFrom utils read.table write.table
#' @importFrom units set_units
#' @importFrom raster raster projection brick extent cellsFromExtent setValues values crop res
#' @importFrom png readPNG
#' @importFrom jpeg readJPEG
#' @import htmltools
"_PACKAGE"
