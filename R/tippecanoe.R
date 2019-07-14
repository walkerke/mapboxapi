#' Generate an .mbtiles file with tippecanoe
#'
#' @param input The dataset from which to generate vector tiles.  Can be an sf object or GeoJSON file on disk.
#' @param output The name of the output .mbtiles file (with .mbtiles extension).  Will be saved in the current working directory.
#' @param arguments A list of arguments and corresponding values to be passed to the tippecanoe program.
#'
#'
#' @return
#' @export
#'
#' @examples
tippecanoe <- function(input,
                       output,
                       options = NULL) {

  check_install <- system("tippecanoe -v") == 0

  if (!check_install) {
    stop("tippecanoe is not installed.  Please visit https://github.com/mapbox/tippecanoe for installation instructions",
         call. = FALSE)
  }

  # If input is an sf object, it should be first saved to GeoJSON
  if (any(grepl("^sf", class(input)))) {

  }



}
