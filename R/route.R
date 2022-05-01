#' Make a request to the Mapbox Directions API
#'
#' See the [Mapbox Directions API
#' documentation](https://docs.mapbox.com/api/navigation/directions/) for more
#' information.
#'
#' @param input_data An input dataset of class `"sf"`, or a list of coordinate
#'   pairs for format `c(longitude, latitude)`. Cannot be used with an
#'   origin/destination pair.
#' @param origin An address or coordinate pair that represents the origin of
#'   your requested route. Cannot be used with `input_data`.
#' @param destination An address or coordinate pair that represents the
#'   destination of your requested route.
#' @param profile One of "driving" (the default), "driving-traffic", "walking",
#'   or "cycling".
#' @param output One of "sf" (the default), which returns an sf LINESTRING
#'   representing the route geometry, or "full", which returns the full request
#'   from the Directions API as a list.
#' @param depart_at (optional) For the "driving" or "driving-traffic" profiles,
#'   the departure date and time to reflect historical traffic patterns. If
#'   "driving-traffic" is used, live traffic will be mixed in with historical
#'   traffic for dates/times near to the current time. Should be specified as an
#'   ISO 8601 date/time, e.g. `"2022-03-31T09:00"`.
#' @param alternatives Whether or not to return alternative routes with your
#'   request. If TRUE, a list of up to 3 possible routes will be returned.
#' @param annotations A comma-separated string of additional route metadata,
#'   which may include duration, distance, speed, and congestion. Must be used
#'   with overview = "full".
#' @param bearings A semicolon-delimited character string of bearings
#' @param continue_straight continue_straight
#' @param exclude Road types to exclude from your route; possible choices are
#'   `'toll'`, `'motorway'`, or `'ferry'`. Defaults to `NULL`.
#' @param geometries The route geometry format. If `output = 'sf'`, you will get
#'   back an `sf` object and you should leave this blank. If `output = 'full'`,
#'   the embedded route geometries will be `polyline` with five decimal place
#'   precision. `'polyline6'` may also be specified.
#' @param overview If left blank, defaults to `'simplified'` for simplified
#'   geometry; the other option is `'full'` which provides the most detailed
#'   geometry available.
#' @param radiuses A character string with semicolon-separated radii that
#'   specify the distance (in meters) to snap each input coordinate to the road
#'   network. Defaults to `NULL`.
#' @param approaches A character string with semicolon-separated specifications
#'   for how to approach waypoints. Options include `unrestricted` and `curb`.
#'   Defaults to `NULL` which uses `unrestricted` for all waypoints.
#' @param steps If `TRUE`, returns the route object split up into route legs
#'   with step-by-step instructions included. If `FALSE` or `NULL` (the
#'   default), a single line geometry representing the full route will be
#'   returned.
#' @param banner_instructions Whether or not to return banner objects; only
#'   available when`output = 'full'` and `steps = TRUE`.
#' @param language The language of the returned instructions (defaults to
#'   English). Available language codes are found at
#'   <https://docs.mapbox.com/api/navigation/#instructions-languages>. Only
#'   available when `steps = TRUE`.
#' @param roundabout_exits If TRUE, adds instructions for roundabout entrance
#'   and exit. Only available when `steps = TRUE`.
#' @param voice_instructions,voice_units Only available when `steps = TRUE` and
#'   `output = 'full'`.
#' @param waypoint_names,waypoint_targets,waypoints Only available when `steps =
#'   TRUE` and `output = 'full'`.
#' @param walking_speed The walking speed in meters/second; available when
#'   `profile = 'walking'`.
#' @param walkway_bias Can take values between -1 and 1, where negative numbers
#'   avoid walkways and positive numbers prefer walkways. Available when
#'   `profile = 'walking'`.
#' @param alley_bias Can take values between -1 and 1, where negative numbers
#'   avoid alleys and positive numbers prefer alleys. Available when `profile =
#'   'walking'`.
#' @param access_token A Mapbox access token; which can be set with
#'   [mb_access_token()]
#'
#' @return An `sf` object (or list of `sf` objects), or full R list representing
#'   the API response.
#'
#' @examples \dontrun{
#' library(mapboxapi)
#' library(leaflet)
#'
#' my_route <- mb_directions(
#'   origin = "10 Avenue de Wagram, 75008 Paris France",
#'   destination = "59 Rue de Tocqueville, 75017 Paris France",
#'   profile = "cycling",
#'   steps = TRUE,
#'   language = "fr"
#' )
#'
#' leaflet(my_route) %>%
#'   addMapboxTiles(
#'     style_id = "light-v9",
#'     username = "mapbox"
#'   ) %>%
#'   addPolylines()
#' }
#' @export
mb_directions <- function(input_data = NULL,
                          origin = NULL,
                          destination = NULL,
                          profile = "driving",
                          output = "sf",
                          depart_at = NULL,
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
                          access_token = NULL) {
  access_token <- get_mb_access_token(access_token)

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
    stop("The following travel profiles are supported: 'driving', 'driving-traffic', 'walking', and 'cycling'. Please modify your request accordingly", call. = FALSE)
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

  # If input_data is an `sf` object, process it accordingly
  if (!is.null(input_data)) {
    if (any(grepl("^sf", class(input_data)))) {
      if (unique(sf::st_geometry_type(input_data)) != "POINT") {
        input_data <- suppressWarnings(sf::st_centroid(input_data))
      }

      input_data <- sf::st_transform(input_data, 4326)

      coords <- sf::st_coordinates(input_data) %>%
        as.data.frame() %>%
        purrr::transpose()

      formatted_coords <- purrr::map(coords, ~ {
        paste0(.x, collapse = ",")
      }) %>%
        unlist() %>%
        paste0(collapse = ";")
    } else if ("list" %in% class(input_data)) {
      formatted_coords <- map(input_data, ~ {
        # If list element is an address, geocode it
        if (inherits(.x, "character")) {
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
  # or addresses. If they are addresses, geocode them then process.
  # We've already done error handling to make sure origin and destination are both supplied.
  if (!is.null(origin)) {
    if (inherits(origin, "character")) {
      oxy <- paste0(mb_geocode(origin), collapse = ",")
    } else {
      oxy <- paste0(origin, collapse = ",")
    }
    if (inherits(destination, "character")) {
      dxy <- paste0(mb_geocode(destination), collapse = ",")
    } else {
      dxy <- paste0(destination, collapse = ",")
    }

    formatted_coords <- paste(oxy, dxy, sep = ";")
  }

  # Assemble the request
  base <- sprintf(
    "https://api.mapbox.com/directions/v5/mapbox/%s/%s",
    profile, formatted_coords
  )

  # Account for boolean-to-string logic if applicable
  if (!is.null(alternatives)) {
    if (alternatives) {
      alternatives <- "true"
    } else {
      alternatives <- "false"
    }
  }


  if (!is.null(steps)) {
    if (steps) {
      steps <- "true"
    }
  } else {
    steps <- "false"
  }

  if (!is.null(banner_instructions)) {
    if (banner_instructions) {
      if (output == "sf") {
        warning("Banner instructions are being ignored; set `output = 'full'` to retrieve this content.")
      }
      banner_instructions <- "true"
    } else {
      banner_instructions <- "false"
    }
  }

  if (!is.null(roundabout_exits)) {
    if (roundabout_exits) {
      roundabout_exits <- "true"
    } else {
      roundabout_exits <- "false"
    }
  }

  if (!is.null(voice_instructions)) {
    if (voice_instructions) {
      if (output == "sf") {
        warning("Voice instructions are being ignored; set `output = 'full'` to retrieve this content.")
      }
      voice_instructions <- "true"
    } else {
      voice_instructions <- "false"
    }
  }

  if (!is.null(voice_units)) {
    if (voice_units) {
      if (output == "sf") {
        warning("Voice units are being ignored; set `output = 'full'` to retrieve this content.")
      }
      voice_units <- "true"
    } else {
      voice_units <- "false"
    }
  }

  request <- httr::GET(
    url = base,
    query = list(
      access_token = access_token,
      depart_at = depart_at,
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
    )
  )

  if (request$status_code != 200) {
    pull <- jsonlite::fromJSON(content)
    stop(pull$message, call. = FALSE)
  }

  content <- httr::content(request, as = "text") %>%
    jsonlite::fromJSON()

  if (output == "sf") {
    if (steps == "true") {
      to_return <- purrr::map(content$routes$legs, ~ {
        geoms <- .x$steps[[1]]$geometry[[1]]

        route <- purrr::map(geoms, ~ {
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
      to_return <- purrr::imap(content$routes$geometry$coordinates, ~ {
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


#' Return an optimized route for a series of input coordinates
#'
#' @param input_data An input dataset of class `"sf"`, or a list of coordinate
#'   pairs of format `c(longitude, latitude)`. Must be between 2 and 12
#'   coordinate pairs.
#' @param profile One of "driving" (the default), "driving-traffic", "walking",
#'   or "cycling".
#' @param output One of "sf" (the default), which returns an `sf` LINESTRING
#'   representing the route geometry, or "full", which returns the full request
#'   from the Directions API as a list.
#' @param source One of `"any"` (the default) or `"first"`. If "any" is
#'   specified, any of the input coordinates may be used as the starting point.
#'   If "first" is specified, the first coordinate will be used.
#' @param destination One of `"any"` (the default) or `"last"`. If "any" is
#'   specified, any of the input coordinates may be used as the ending point. If
#'   "last" is specified, the last coordinate will be used.
#' @param roundtrip If `TRUE` (the default), the route will start and end at the
#'   same point. `roundtrip = FALSE` only works when `source` is `"first"` and
#'   `destination` is `"last"`. If `FALSE` is supplied here, the route will
#'   start at the first point in `input_data` and end at the last point.
#' @param annotations A comma-separated string of additional route metadata,
#'   which may include duration, distance, speed, and congestion. Must be used
#'   with `overview = "full"`.
#' @param approaches A character string with semicolon-separated specifications
#'   for how to approach waypoints. Options include `unrestricted` and `curb`.
#'   Defaults to NULL which uses `unrestricted` for all waypoints.
#' @param bearings A semicolon-delimited character string of bearings.
#' @param distributions A semicolon-delimited character string of number pairs
#'   that specifies pick-up and drop-off locations. The first number indicates
#'   the index of the pick-up location, and the second number represents the
#'   index of the drop-off location.
#' @param language The language of the returned instructions (defaults to
#'   English). Available language codes are found at
#'   <https://docs.mapbox.com/api/navigation/#instructions-languages>. Only
#'   available when `steps = TRUE`.
#' @param overview If left blank, defaults to `'simplified'` for simplified
#'   geometry; the other option is `'full'` which provides the most detailed
#'   geometry available.
#' @param radiuses A character string with semicolon-separated radii that
#'   specify the distance (in meters) to snap each input coordinate to the road
#'   network. Defaults to `NULL`.
#' @param steps If `TRUE`, returns the route object split up into route legs
#'   with step-by-step instructions included. If `FALSE` or `NULL` (the default), a
#'   single line geometry representing the full route will be returned.
#' @param access_token Your Mapbox access token; which can be set with
#'   [mb_access_token()]
#'
#' @return Either a list of two `sf` objects - one representing the waypoints, and one representing the route - or an R list representing the full optimization API response.
#'
#' @examples \dontrun{
#'
#' library(mapboxapi)
#' library(sf)
#'
#' to_visit <- data.frame(
#'   X = c(-0.209307, -0.185875, -0.216877, -0.233511, -0.234541),
#'   Y = c(5.556019, 5.58031, 5.582528, 5.566771, 5.550209)
#' ) %>%
#'   st_as_sf(coords = c("X", "Y"), crs = 4326)
#'
#' optimized_route <- mb_optimized_route(to_visit,
#'   profile = "driving-traffic"
#' )
#' }
#'
#' @export
mb_optimized_route <- function(input_data,
                               profile = c(
                                 "driving", "walking", "cycling",
                                 "driving-traffic"
                               ),
                               output = "sf",
                               source = c("any", "first"),
                               destination = c("any", "last"),
                               roundtrip = TRUE,
                               annotations = NULL,
                               approaches = NULL,
                               bearings = NULL,
                               distributions = NULL,
                               language = NULL,
                               overview = "simplified",
                               radiuses = NULL,
                               steps = NULL,
                               access_token = NULL) {
  profile <- match.arg(profile)
  source <- match.arg(source)
  destination <- match.arg(destination)

  access_token <- get_mb_access_token(access_token)

  # If input_data is an `sf` object, process it accordingly
  if (!is.null(input_data)) {
    if (any(grepl("^sf", class(input_data)))) {
      if (unique(sf::st_geometry_type(input_data)) != "POINT") {
        input_data <- suppressWarnings(sf::st_centroid(input_data))
      }

      input_data <- sf::st_transform(input_data, 4326)

      coords <- sf::st_coordinates(input_data) %>%
        as.data.frame() %>%
        purrr::transpose()

      formatted_coords <- purrr::map(coords, ~ {
        paste0(.x, collapse = ",")
      }) %>%
        unlist() %>%
        paste0(collapse = ";")
    } else if ("list" %in% class(input_data)) {
      formatted_coords <- map(input_data, ~ {
        # If list element is an address, geocode it
        if (inherits(.x, "character")) {
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

  # Assemble the request
  base <- sprintf(
    "https://api.mapbox.com/optimized-trips/v1/mapbox/%s/%s",
    profile, formatted_coords
  )

  # Account for boolean-to-string logic if applicable
  if (!is.null(steps)) {
    if (steps) {
      steps <- "true"
    }
  } else {
    steps <- "false"
  }


  if (roundtrip) {
    roundtrip <- "true"
  } else {
    roundtrip <- "false"
    source <- "first"
    destination <- "last"
  }


  request <- httr::GET(
    url = base,
    query = list(
      access_token = access_token,
      annotations = annotations,
      bearings = bearings,
      destination = destination,
      distributions = distributions,
      geometries = "geojson",
      overview = "simplified",
      radiuses = radiuses,
      approaches = approaches,
      steps = steps,
      language = language,
      source = source,
      roundtrip = roundtrip
    )
  )


  if (request$status_code != 200) {
    pull <- jsonlite::fromJSON(content)
    stop(pull$message, call. = FALSE)
  }

  content <- httr::content(request, as = "text") %>%
    jsonlite::fromJSON()

  if (output == "sf") {
    if (steps == "true") {
      to_return <- purrr::map(content$trips$legs[[1]]$steps, ~ {
        route <- purrr::map(.x$geometry$coordinates, function(r) {
          r %>%
            sf::st_linestring() %>%
            sf::st_sfc(crs = 4326) %>%
            sf::st_sf()
        }) %>%
          dplyr::bind_rows()

        route$distance <- .x$distance / 1000
        route$duration <- .x$duration / 60
        route$instruction <- .x$maneuver$instruction

        return(route)
      }) %>%
        dplyr::bind_rows()

      waypoints <- content$waypoints %>%
        tidyr::unnest_wider(location, names_sep = "_") %>%
        sf::st_as_sf(coords = c("location_1", "location_2"), crs = 4326) %>%
        dplyr::select(name, waypoint_index)

      waypoints$waypoint_index <- waypoints$waypoint_index + 1


      output <- list()

      output$waypoints <- waypoints

      if (length(to_return) == 1) {
        output$route <- to_return[[1]]

        return(output)
      } else {
        output$route <- to_return

        return(output)
      }
    } else {
      to_return <- purrr::imap(content$trips$geometry$coordinates, ~ {
        route <- .x %>%
          sf::st_linestring() %>%
          sf::st_sfc(crs = 4326) %>%
          sf::st_sf()

        route$distance <- content$trips$distance[.y] / 1000
        route$duration <- content$trips$duration[.y] / 60

        return(route)
      })

      waypoints <- content$waypoints %>%
        tidyr::unnest_wider(location, names_sep = "_") %>%
        sf::st_as_sf(coords = c("location_1", "location_2"), crs = 4326) %>%
        dplyr::select(name, waypoint_index)

      waypoints$waypoint_index <- waypoints$waypoint_index + 1

      output <- list()

      output$waypoints <- waypoints

      if (length(to_return) == 1) {
        output$route <- to_return[[1]]
        return(output)
      } else {
        output$route <- to_return
        return(output)
      }
    }
  } else {
    return(content)
  }
}
