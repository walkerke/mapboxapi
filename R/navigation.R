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
mb_matrix <- function(input_data,
                      sources = "all",
                      destinations = "all",
                      mode = "driving",
                      fallback_speed = 44,
                      access_token = NULL,
                      duration_output = "minutes") {

  if (is.null(access_token)) {

    if (Sys.getenv("MAPBOX_ACCESS_TOKEN") != "") {
      access_token <- Sys.getenv("MAPBOX_ACCESS_TOKEN")
    } else {
      stop("A Mapbox access token is required.  Please locate yours from your Mapbox account.",
           call. = FALSE)
    }


  }

  if (!mode %in% c("driving", "driving-traffic", "walking", "cycling")) {
    stop("The following travel modes are supported: 'driving', 'driving-traffic', 'walking',
         and 'cycling'.  Please modify your request accordingly", call. = FALSE)
  }

  # Parse the request

  if (any(grepl("^sf", class(input_data)))) {

    if (unique(st_geometry_type(input_data)) != "POINT") {
      input_data <- st_centroid(input_data)
    }

    input_data <- st_transform(input_data, 4326)

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
                   destinations = destinations,
                   fallback_speed = fallback_speed
                 ))

  content <- content(request, as = "text") %>%
    fromJSON(flatten = TRUE)

  if (request$status_code != 200) {
    stop(content$message, call. = FALSE)
  }

  duration_matrix <- content$durations

  if (duration_output == "seconds") {
    return(duration_matrix)
  } else if (duration_output == "minutes") {
    return(duration_matrix / 60)
  } else {
    stop("`duration_output` must be one of 'minutes' or 'seconds'", call. = FALSE)
  }

}


#' Generate an isochrone using the Mapbox API
#'
#' @param location A vector of form \code{c(longitude, latitude)}, an address that can be geocoded as a character string, or an sf object.
#' @param profile One of "driving", "walking", or "cycling".  "driving" is the default.
#' @param contours A vector of isochrone contours, specified in minutes.  4 is the maximum; defaults to \code{c(5, 10, 15)}.
#' @param access_token A valid Mapbox access token.
#' @param denoise A floating-point value between 0 and 1 used to remove smaller contours.  1 is the default and returns only the largest contour for an input time.
#' @param geometry one of \code{"polygons"} (the default), which returns isochrones as polygons, or alternatively \code{"linestring"}, which returns isochrones as linestrings.
#' @param output one of \code{"sf"} (the default), which returns an sf object representing the isochrone(s), or \code{"geojson"}, which returns the GeoJSON response from the API.
#' @param rate_limit The rate limit for the API, expressed in maximum number of calls per minute.  For most users this will be 300 though this parameter can be modified based on your Mapbox plan. Used when \code{location} is \code{"sf"}.
#'
#' @return An sf object representing the isochrone(s) around the location(s).
#' @export
mb_isochrone <- function(location,
                         profile = "driving",
                         contours = c(5, 10, 15),
                         access_token = NULL,
                         denoise = 1,
                         geometry = "polygon",
                         output = "sf",
                         rate_limit = 300) {

  if (is.null(access_token)) {

    if (Sys.getenv("MAPBOX_ACCESS_TOKEN") != "") {
      access_token <- Sys.getenv("MAPBOX_ACCESS_TOKEN")
    } else {
      stop("A Mapbox access token is required.  Please locate yours from your Mapbox account.",
           call. = FALSE)
    }
  }

  # If input location is an sf object, call a rate-limited function internally
  mb_isochrone_sf <- function(sf_object) {

    # Convert to centroids if geometry is not points
    if (unique(st_geometry_type(location)) != "POINT") {
      location <- suppressMessages(st_centroid(location))
      message("Using feature centroids to compute isochrones")
    }

    input_data <- st_transform(location, 4326)

    coords <- st_coordinates(input_data) %>%
      as.data.frame() %>%
      transpose()

    mb_isochrone_limited <- purrr::slowly(mb_isochrone,
                                          rate = rate_delay(60 / rate_limit),
                                          quiet = TRUE)

    map(coords, ~{
      mb_isochrone_limited(location = .x,
                           profile = profile,
                           contours = contours,
                           access_token = access_token,
                           denoise = denoise,
                           geometry = geometry,
                           output = "sf")
    }) %>%
      data.table::rbindlist() %>%
      st_as_sf(crs = 4326)

  }

  if (any(grepl("^sf", class(location)))) {

    sf_isos <- mb_isochrone_sf(location)

    return(sf_isos)

  }

  if (geometry == "polygon") {
    polygons <- "true"
  } else if (geometry == "linestring") {
    polygons <- "false"
  } else {
    stop("The geometry must be one of 'polygon' or 'linestring'", call. = FALSE)
  }

  # If location is an address, geocode it
  if (length(location) == 1) {
    coords <- mb_geocode(location, access_token = access_token)
  } else if (length(location) == 2) {
    coords <- location
  } else {
    stop("The specified location must either be a coordinate pair or a valid address",
         call. = FALSE)
  }

  base <- sprintf(
    "https://api.mapbox.com/isochrone/v1/mapbox/%s/%s",
    profile,
    paste0(coords, collapse = ",")
  )


  request <- GET(base,
                 query = list(
                   access_token = access_token,
                   contours_minutes = paste0(contours, collapse = ","),
                   denoise = as.character(denoise),
                   polygons = polygons
                 ))

  content <- content(request, as = "text")

  if (request$status_code != 200) {
    pull <- fromJSON(content)
    stop(pull$message, call. = FALSE)
  }

  if (output == "sf") {
    isos <- read_sf(content)
    return(isos)
  } else if (output == "geojson") {
    return(content)
  }

}
