#' Generate an .mbtiles file with tippecanoe
#'
#' @param input The dataset from which to generate vector tiles.  Can be an sf object or GeoJSON file on disk.
#' @param output The name of the output .mbtiles file (with .mbtiles extension).  Will be saved in the current working directory.
#' @param options A character vector of options to be passed to the tippecanoe program.
#' @param keep_file Whether nor not to keep the temporary CSV or GeoJSON file used to generate the tiles.  Defaults to \code{FALSE}.
#' @export
tippecanoe <- function(input,
                       output,
                       layer_name = "layer",
                       min_zoom = NULL,
                       max_zoom = NULL,
                       drop_rate = NULL,
                       overwrite = TRUE,
                       other_options = NULL,
                       keep_geojson = FALSE) {

  check_install <- system("tippecanoe -v") == 0

  if (!check_install) {
    stop("tippecanoe is not installed.  Please visit https://github.com/mapbox/tippecanoe for installation instructions.",
         call. = FALSE)
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

  # If input is an sf object, it should be first converted to GeoJSON
  if (any(grepl("^sf", class(input)))) {

    input <- sf::st_transform(input, 4326)

    if (keep_geojson) {
      outfile <- paste0(layer_name, ".geojson")

      path <- file.path(dir, outfile)

      sf::st_write(input, path, quiet = TRUE)

    } else {

      tmp <- tempdir()

      tempfile <- paste0(layer_name, ".geojson")

      path <- file.path(tmp, tempfile)

      sf::st_write(input, path, quiet = TRUE)

    }

    call <- sprintf("tippecanoe -o %s/%s %s %s",
                    dir, output, collapsed_opts, path)

    system(call)


  } else if (class(input) == "character") {
    call <- sprintf("tippecanoe -o %s/%s %s %s",
                    dir, output, collapsed_opts, input)

    system(call)
  }

}
