#' Retrieve a matrix of travel times from the Mapbox Directions API
#'
#' @param input_data The input coordinates of your request.  Acceptable inputs include a vector of
#' coordinate pairs in "long, lat" format or an sf object.
#' For sf linestrings or polygons, the distance between centroids will be taken.
#' @param sources The positional indices of the source coordinates.  Defaults to "all".
#' @param destinations The positional indices of the destination coordinates.  Defaults to "all".
#' @param mode One of "driving" (the default), "driving-traffic", "walking", or "cycling".
#' @param access_token A Mapbox access token (required)
#'
#' @return An R matrix of source-destination travel times.
#' @export
#'
#' @examples
mb_matrix <- function(input_data, sources = "all", destinations = "all",
                      mode = "driving",
                      access_token = NULL,
                      duration_output = "minutes") {

  if (is.null(access_token)) {
    stop("A Mapbox access token is required.  Please locate yours from your Mapbox account.",
         call. = FALSE)
  }

  if (!mode %in% c("driving", "driving-traffic", "walking", "cycling")) {
    stop("The following travel modes are supported: 'driving', 'driving-traffic', 'walking',
         and 'cycling'.  Please modify your request accordingly", call. = FALSE)
  }

  # Parse the request

  if ("sf" %in% class(input_data)) {
    if (unique(st_geometry_type(input_data)) != "POINT") {
      input_data <- st_centroid(input_data)
    }

    coords <- st_coordinates(input_data) %>%
      as.data.frame() %>%
      transpose()

    formatted_coords <- map_chr(coords, function(x) {
      unlist(x) %>%
        paste0(collapse = ",")

    }) %>%
      paste0(collapse = ";")

  }

  if (is.vector(input_data)) {
    formatted_coords <- paste0(input_data, collapse = ";")
  }



  base_url <- paste0("https://api.mapbox.com/directions-matrix/v1/mapbox/",
                     mode,
                     "/",
                     formatted_coords)

  request <- GET(base_url,
                 query = list(
                   access_token = access_token,
                   sources = sources,
                   destinations = destinations
                 ))

  content <- content(request, as = "text") %>%
    fromJSON(flatten = TRUE)

  duration_matrix <- content$durations

  if (duration_output == "seconds") {
    return(duration_matrix)
  } else if (duration_output == "minutes") {
    return(duration_matrix / 60)
  } else {
    stop("`duration_output` must be one of 'minutes' or 'seconds'", call. = FALSE)
  }






}
