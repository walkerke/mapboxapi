#' An R interface to Mapbox web services
#'
#' Use Mapbox web services APIs for spatial data science and visualization
#' projects in R. Usage of the package is governed by the Mapbox Terms of
#' Service.
#'
#' @author Kyle Walker
#' @name mapboxapi
#' @docType package
#' @import httr
#' @import sf
#' @import dplyr
#' @import rlang
#' @importFrom leaflet tileOptions getMapData addTiles filterNULL
#' @importFrom purrr map map2 map_int reduce slowly transpose map_chr rate_delay
#'   imap
#' @importFrom curl curl_escape
#' @importFrom stringi stri_rand_strings
#' @importFrom slippymath bbox_to_tile_grid lonlat_to_tilenum tile_bbox
#' @importFrom geojsonsf sf_geojson
#' @importFrom utils read.table write.table
#' @importFrom units set_units
#' @importFrom png readPNG
NULL
