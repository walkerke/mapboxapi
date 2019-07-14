#' Generate an .mbtiles file with tippecanoe
#'
#' @param input The dataset from which to generate vector tiles.  Can be an sf object or GeoJSON file on disk.
#' @param output The name of the output .mbtiles file (with .mbtiles extension).  Will be saved in the current working directory.
#' @param options A character vector of options to be passed to the tippecanoe program.

#' @export
tippecanoe <- function(input,
                       output,
                       options = NULL) {

  check_install <- system("tippecanoe -v") == 0

  if (!check_install) {
    stop("tippecanoe is not installed.  Please visit https://github.com/mapbox/tippecanoe for installation instructions",
         call. = FALSE)
  }

  opts <- paste0(options, collapse = " ")

  dir <- getwd()


  # If input is an sf object, it should be first converted to GeoJSON
  if (any(grepl("^sf", class(input)))) {

    tmp <- tempdir()

    tempfile <- paste0(stringi::stri_rand_strings(1, 6), ".geojson")

    path <- file.path(tmp, tempfile)

    st_write(input, path)


    call <- sprintf("tippecanoe -o %s/%s %s %s",
                    dir, output, opts, path)

    system(call)

  } else if (class(input) == "character") {
    call <- sprintf("tippecanoe -o %s/%s %s %s",
                    dir, output, opts, input)

    system(call)
  }



}
