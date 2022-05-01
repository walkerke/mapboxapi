#' Generate an .mbtiles file with tippecanoe
#'
#' [Tippecanoe](https://github.com/mapbox/tippecanoe) is tile-generation utility
#' for building vector tilesets from large (or small) collections of GeoJSON,
#' Geobuf, or CSV features. The [tippecanoe] function requires that the
#' tippecanoe utility is installed on your system; see the tippecanoe
#' documentation for [installation
#' instructions](https://github.com/mapbox/tippecanoe#installation). Once
#' installed, tippecanoe can be used in large visualization workflows in concert
#' with Mapbox Studio.
#'
#' Mapbox also offers the [Mapbox Tiling
#' Service](https://docs.mapbox.com/mapbox-tiling-service/guides/) as an
#' alternate way to transform datasets into vector tiles.
#'
#' @param input The dataset from which to generate vector tiles. Can be an sf
#'   object or GeoJSON file on disk.
#' @param output The name of the output .mbtiles file (with .mbtiles extension).
#'   Will be saved in the current working directory.
#' @param layer_name The name of the layer in the output .mbtiles file. If NULL,
#'   will either be a random string (if input is an `sf` object) or the name of
#'   the input GeoJSON file (if input is a file path).
#' @param min_zoom,max_zoom The minimum and maximum zoom levels for which to
#'   compute tiles. If both min_zoom and max_zoom are blank, tippecanoe will
#'   guess the best zoom levels for your data.
#' @param drop_rate The rate at which tippecanoe will drop features as you zoom
#'   out. If NULL, tippecanoe will drop features as needed in the densest tiles
#'   to stay within Mapbox's limits.
#' @param overwrite If `TRUE`, an existing .mbtiles file with the same name will
#'   be overwritten.
#' @param other_options A character string of other options to be passed to the
#'   tippecanoe program.
#' @param keep_geojson Whether nor not to keep the temporary CSV or GeoJSON file
#'   used to generate the tiles. Defaults to `FALSE`.
#'
#' @examples \dontrun{
#'
#' # Workflow: create a dynamic tileset for dot-density mapping
#' library(tidycensus)
#' library(sf)
#' library(mapboxapi)
#'
#' # Get population data for Census tracts in Vermont
#' vt_population <- get_decennial(
#'   geography = "tract",
#'   variables = "P001001",
#'   state = "Vermont",
#'   year = 2010,
#'   geometry = TRUE
#' )
#'
#' # Convert to representative dots - 1 per person
#' vt_dots <- st_sample(
#'   vt_population,
#'   size = vt_population$value
#' )
#'
#' # Use tippecanoe to create dynamic tiles
#' tippecanoe(
#'   input = vt_dots,
#'   output = "vt_population.mbtiles",
#'   layer_name = "vermont_population",
#'   max_zoom = 18,
#'   drop_rate = 1.5
#' )
#'
#' # Upload to your Mapbox account for visualization
#' # A Mapbox secret access token must be set with mb_access_token()
#' # to upload data to your account
#' upload_tiles(
#'   input = "vt_population.mbtiles",
#'   username = "kwalkertcu",
#'   tileset_id = "vt_population_dots",
#'   multipart = TRUE
#' )
#' }
#'
#' @export
tippecanoe <- function(input,
                       output,
                       layer_name = NULL,
                       min_zoom = NULL,
                       max_zoom = NULL,
                       drop_rate = NULL,
                       overwrite = TRUE,
                       other_options = NULL,
                       keep_geojson = FALSE) {
  check_install <- system("tippecanoe -v") == 0

  if (!check_install) {
    stop("tippecanoe is not installed. Please visit https://github.com/mapbox/tippecanoe for installation instructions.",
      call. = FALSE
    )
  }

  # Assemble the options
  opts <- c()

  if (!is.null(min_zoom)) {
    opts <- c(opts, sprintf("-Z%s", min_zoom))
  }

  if (!is.null(max_zoom)) {
    opts <- c(opts, sprintf("-z%s", max_zoom))
  }

  if (is.null(min_zoom) && is.null(max_zoom)) {
    opts <- c(opts, "-zg")
  }

  if (!is.null(drop_rate)) {
    opts <- c(opts, sprintf("-r%s", drop_rate))
  } else {
    opts <- c(opts, "-as")
  }

  if (overwrite) {
    opts <- c(opts, "-f")
  }

  collapsed_opts <- paste0(opts, collapse = " ")

  if (!is.null(other_options)) {
    extra_opts <- paste0(other_options, collapse = " ")
    collapsed_opts <- paste(collapsed_opts, extra_opts)
  }

  dir <- getwd()

  # If input is an `sf` object, it should be first converted to GeoJSON
  if (any(grepl("^sf", class(input)))) {
    input <- sf::st_transform(input, 4326)

    if (is.null(layer_name)) {
      layer_name <- stringi::stri_rand_strings(1, 6)
    }

    if (keep_geojson) {
      outfile <- paste0(layer_name, ".geojson")

      path <- file.path(dir, outfile)

      sf::st_write(input, path,
        quiet = TRUE,
        delete_dsn = TRUE, delete_layer = TRUE
      )
    } else {
      tmp <- tempdir()

      tempfile <- paste0(layer_name, ".geojson")

      path <- file.path(tmp, tempfile)

      sf::st_write(input, path,
        quiet = TRUE,
        delete_dsn = TRUE, delete_layer = TRUE
      )
    }

    call <- sprintf(
      "tippecanoe -o %s/%s %s %s",
      dir, output, collapsed_opts, path
    )

    system(call)
  } else if (inherits(input, "character")) {
    if (!is.null(layer_name)) {
      collapsed_opts <- paste0(collapsed_opts, " -l ", layer_name)
    }

    call <- sprintf(
      "tippecanoe -o %s/%s %s %s",
      dir, output, collapsed_opts, input
    )

    system(call)
  }
}
