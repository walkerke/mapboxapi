#' Make a request to the Mapbox Directions API
#'
#' @param input_data An input dataset of class \code{"sf"}, or a list of coordinate pairs for format \code{c(longitude, latitude)}. Cannot be used with an origin/destination pair.
#' @param origin An address or coordinate pair that represents the origin of your requested route. Cannot be used with \code{input_data}.
#' @param destination An address or coordinate pair that represents the destination of your requested route.
#' @param profile One of "driving" (the default), "driving-traffic", "walking", or "cycling".
#' @param output One of "sf" (the default), which returns an sf LINESTRING representing the route geometry, or "full", which returns the full request from the Directions API as a list.
#' @param alternatives Whether or not to return alternative routes with your request.
#' @param annotations Must be used with overview = FULL.
#' @param bearings bearings
#' @param continue_straight continue_straight
#' @param exclude exclude
#' @param geometries geometries
#' @param overview overview
#' @param radiuses radiuses
#' @param approaches approaches
#' @param steps steps
#' @param banner_instructions banner_instructions
#' @param language language
#' @param roundabout_exits roundabout_exits
#' @param voice_instructions voice_instructions
#' @param voice_units voice_units
#' @param waypoint_names waypoint_names
#' @param waypoint_targets waypoint_targets
#' @param waypoints waypoints
#' @param walking_speed walking_speed
#' @param walkway_bias walkway_bias
#' @param alley_bias alley_bias
#' @param access_token acces_token
#'
#' @return A list or sf object
#' @export
mb_directions <- function(input_data = NULL,
                          origin = NULL,
                          destination = NULL,
                          profile = "driving",
                          output = "sf",
                          alternatives = NULL,
                          annotations = NULL,
                          bearings = NULL,
                          continue_straight = NULL,
                          exclude = NULL,
                          geometries = NULL,
                          overview = "simplified",
                          radiuses = NULL,
                          approaches = NULL,
                          steps = NULL,
                          banner_instructions = NULL,
                          language = NULL,
                          roundabout_exits = NULL,
                          voice_instructions = NULL,
                          voice_units = NULL,
                          waypoint_names = NULL,
                          waypoint_targets = NULL,
                          waypoints = NULL,
                          walking_speed = NULL,
                          walkway_bias = NULL,
                          alley_bias = NULL,
                          access_token = NULL
) {

  if (is.null(access_token)) {
    # Use public token first, then secret token
    if (Sys.getenv("MAPBOX_PUBLIC_TOKEN") != "") {
      access_token <- Sys.getenv("MAPBOX_PUBLIC_TOKEN")
    } else {
      if (Sys.getenv("MAPBOX_SECRET_TOKEN" != "")) {
        access_token <- Sys.getenv("MAPBOX_SECRET_TOKEN")
      } else {
        stop("A Mapbox access token is required.  Please locate yours from your Mapbox account.", call. = FALSE)
      }
    }
  }

  # Check for co-existence of input data and origin/destination
  if (!is.null(input_data)) {
    if (!is.null(origin) || !is.null(destination)) {
      stop("Either input data or an origin/destination pair must be specified, but not both.")
    }
  }

  if (!is.null(origin)) {
    if (!is.null(input_data)) {
      stop("Either input data or an origin/destination pair must be specified, but not both.")
    }
    if (is.null(destination)) {
      stop("An origin requires a destination to be specified.")
    }
  }

  if (!is.null(destination)) {
    if (!is.null(input_data)) {
      stop("Either input data or an origin/destination pair must be specified, but not both.")
    }
    if (is.null(origin)) {
      stop("A destination requires an origin to be specified.")
    }
  }

  # Check size of request and process limits accordingly
  if (!profile %in% c("driving", "driving-traffic", "walking", "cycling")) {
    stop("The following travel profiles are supported: 'driving', 'driving-traffic', 'walking', and 'cycling'.  Please modify your request accordingly", call. = FALSE)
  }

  if (!is.null(input_data)) {
    if (!is.null(nrow(input_data))) {
      request_length <- nrow(input_data)
    } else {
      request_length <- length(input_data)
    }
  } else {
    request_length <- 2
  }

  if (profile == "driving-traffic") {
    if (request_length > 3) {
      stop("The maximum number of input coordinates for the profile driving-traffic is 3.")
    }
  } else {
    if (request_length > 25) {
      stop("The maximum number of input coordinates is 25.")
    }
  }

  # If input_data is an sf object, process it accordingly
  if (!is.null(input_data)) {
    if (any(grepl("^sf", class(input_data)))) {

      if (unique(sf::st_geometry_type(input_data)) != "POINT") {
        input_data <- suppressWarnings(sf::st_centroid(input_data))
      }

      input_data <- sf::st_transform(input_data, 4326)

      coords <- sf::st_coordinates(input_data) %>%
        as.data.frame() %>%
        purrr::transpose()

      formatted_coords <- purrr::map(coords, ~{
        paste0(.x, collapse = ",")
      }) %>%
        unlist() %>%
        paste0(collapse = ";")
    } else if ("list" %in% class(input_data)) {

      formatted_coords <- map(input_data, ~{
        # If list element is an address, geocode it
        if (class(.x) == "character") {
          cxy <- mb_geocode(.x)
        } else {
          cxy <- .x
        }
        paste0(cxy, collapse = ",")
      }) %>%
        unlist() %>%
        paste0(collapse = ";")

    }
  }

  # If origin/destination are specified, check to see if they represent coordinate pairs
  # or addresses.  If they are addresses, geocode them then process.
  # We've already done error handling to make sure origin and destination are both supplied.
  if (!is.null(origin)) {
    if (class(origin) == "character") {
      oxy <- paste0(mb_geocode(origin), collapse = ",")
    } else {
      oxy <- paste0(origin, collapse = ",")
    }
    if (class(destination) == "character") {
      dxy <- paste0(mb_geocode(destination), collapse = ",")
    } else {
      dxy <- paste0(destination, collapse = ",")
    }

    formatted_coords <- paste(oxy, dxy, sep = ";")
  }

  # Assemble the request
  base <- sprintf("https://api.mapbox.com/directions/v5/mapbox/%s/%s",
                  profile, formatted_coords)

  # Account for boolean-to-string logic if applicable
  if (!is.null(alternatives)) {
    if (alternatives) {
      alternatives <- 'true'
    } else {
      alternatives <- 'false'
    }
  }


  if (!is.null(steps)) {
    if (steps) {
      steps <- 'true'
    }
  } else {
    steps <- 'false'
  }

  if (!is.null(banner_instructions)) {
    if (banner_instructions) {
      if (output == "sf") {
        warning("Banner instructions are being ignored; set `output = 'full'` to retrieve this content.")
      }
      banner_instructions <- 'true'
    } else {
      banner_instructions <- 'false'
    }
  }

  if (!is.null(roundabout_exits)) {
    if (roundabound_exits) {
      roundabout_exits <- 'true'
    } else {
      roundabount_exits <- 'false'
    }
  }

  if (!is.null(voice_instructions)) {
    if (voice_instructions) {
      if (output == "sf") {
        warning("Voice instructions are being ignored; set `output = 'full'` to retrieve this content.")
      }
      voice_instructions <- 'true'
    } else {
      voice_instructions <- 'false'
    }
  }

  if (!is.null(voice_units)) {
    if (voice_units) {
      if (output == "sf") {
        warning("Voice units are being ignored; set `output = 'full'` to retrieve this content.")
      }
      voice_units <- 'true'
    } else {
      voice_units <- 'false'
    }
  }

  # If the output is sf, the returned geometry should be 'geojson' so that it is easier
  # to parse without additional dependencies
  if (output == "sf") {

  }


  request <- httr::GET(url = base,
                       query = list(
                         access_token = access_token,
                         alternatives = alternatives,
                         annotations = annotations,
                         bearings = bearings,
                         continue_straight = continue_straight,
                         exclude = exclude,
                         geometries = "geojson",
                         overview = "simplified",
                         radiuses = radiuses,
                         approaches = approaches,
                         steps = steps,
                         banner_instructions = banner_instructions,
                         language = language,
                         roundabout_exits = roundabout_exits,
                         voice_instructions = voice_instructions,
                         voice_units = voice_units,
                         waypoint_names = waypoint_names,
                         waypoint_targets = waypoint_targets,
                         waypoints = waypoints,
                         walking_speed = walking_speed,
                         walkway_bias = walkway_bias,
                         alley_bias = alley_bias
                       ))

  if (request$status_code != 200) {
    pull <- jsonlite::fromJSON(content)
    stop(pull$message, call. = FALSE)
  }

  content <- httr::content(request, as = "text") %>%
    jsonlite::fromJSON()

  if (output == "sf") {

    if (steps == 'true') {

      to_return <- purrr::map(content$routes$legs, ~{
        geoms <- .x$steps[[1]]$geometry[[1]]

        route <- purrr::map(geoms, ~{
          .x %>%
            sf::st_linestring() %>%
            sf::st_sfc(crs = 4326) %>%
            sf::st_sf()
        }) %>%
          purrr::reduce(rbind)

        route$distance <- .x$steps[[1]]$distance / 1000
        route$duration <- .x$steps[[1]]$duration / 60
        route$instruction <- .x$steps[[1]]$maneuver$instruction

        return(route)
      })

      if (length(to_return) == 1) {
        return(to_return[[1]])
      } else {
        return(to_return)
      }

    } else {

      to_return <- purrr::imap(content$routes$geometry$coordinates, ~{
        route <- .x %>%
          sf::st_linestring() %>%
          sf::st_sfc(crs = 4326) %>%
          sf::st_sf()

        route$distance <- content$routes$distance[.y] / 1000
        route$duration <- content$routes$duration[.y] / 60

        return(route)
      })

      if (length(to_return) == 1) {
        return(to_return[[1]])
      } else {
        return(to_return)
      }
    }
  } else {
    return(content)
  }

}
