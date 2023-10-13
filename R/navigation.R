#' Retrieve a matrix of travel times from the Mapbox Directions API
#'
#' @param origins The input coordinates of your request. Acceptable inputs include a list of
#' coordinate pair vectors in \code{c(x, y)} format or an `sf` object.
#' For sf linestrings or polygons, the distance between centroids will be taken.
#' @param destinations The destination coordinates of your request. If \code{NULL} (the default), a many-to-many matrix using \code{origins} will be returned.
#' @param profile One of "driving" (the default), "driving-traffic", "walking", or "cycling".
#' @param fallback_speed A value expressed in kilometers per hour used to estimate travel time when a route cannot be found between locations. The returned travel time will be based on the straight-line estimate of travel between the locations at the specified fallback speed.
#' @param output one of \code{"duration"} (the default), which will be measured in either minutes or seconds (depending on the value of \code{duration_output}), or \code{"distance"}, which will be returned in meters.
#' @param duration_output one of \code{"minutes"} (the default) or \code{"seconds"}
#' @param access_token A Mapbox access token (required)
#' @param depart_at (optional) For the "driving" or "driving-traffic" profiles, the departure date and time to reflect historical traffic patterns. If "driving-traffic" is used, live traffic will be mixed in with historical traffic for dates/times near to the current time. Should be specified as an ISO 8601 date/time, e.g. \code{"2023-03-31T09:00"}. The time must be set to the current time or in the future.
#' @param allow_large_matrix \code{mb_matrix()} will prevent the user from calculating large travel-time matrices (greater than 25x25) by default, as they may lead to unexpected charges.  If the user sets this argument to \code{TRUE}, \code{mb_matrix()} will bypass this error and calculate the large matrix for the user.  Defaults to \code{FALSE}.
#'
#' @return An R matrix of source-destination travel times.
#'
#' @examples \dontrun{
#'
#' library(mapboxapi)
#' library(tigris)
#' library(mapdeck)
#'
#' philly_tracts <- tracts("PA", "Philadelphia", cb = TRUE, class = "sf")
#' downtown_philly <- mb_geocode("Philadelphia City Hall, Philadelphia PA")
#'
#' time_to_downtown <- mb_matrix(philly_tracts, downtown_philly)
#'
#' philly_tracts$time <- time_to_downtown
#'
#' mapdeck(style = mapdeck_style("light")) %>%
#'   add_polygon(
#'     data = philly_tracts,
#'     fill_colour = "time",
#'     fill_opacity = 0.6,
#'     legend = TRUE
#'   )
#' }
#'
#' @export
mb_matrix <- function(origins,
                      destinations = NULL,
                      profile = "driving",
                      fallback_speed = NULL,
                      output = c("duration", "distance"),
                      duration_output = c("minutes", "seconds"),
                      access_token = NULL,
                      depart_at = NULL,
                      allow_large_matrix = FALSE)
                      {

  access_token <- get_mb_access_token(access_token)

  output <- rlang::arg_match(output)
  duration_output <- rlang::arg_match(duration_output)

  if (!profile %in% c("driving", "driving-traffic", "walking", "cycling")) {
    stop("The following travel profiles are supported: 'driving', 'driving-traffic', 'walking', and 'cycling'. Please modify your request accordingly", call. = FALSE)
  }

  if (is.numeric(origins)) {
    origins <- list(origins)
  }

  if (!is.null(destinations)) {
    if (is.numeric(destinations)) {
      destinations <- list(destinations)
    }
  }

  # Figure out size of request, and chunk accordingly if necessary
  # Scenario 1: origins > limit, destinations < limit
  if (!is.null(nrow(origins))) {
    origin_size <- nrow(origins)
  } else {
    origin_size <- length(origins)
  }

  if (!is.null(destinations)) {
    if (!is.null(nrow(destinations))) {
      dest_size <- nrow(destinations)
    } else {
      dest_size <- length(destinations)
    }
  } else {
    dest_size <- 0
  }

  coord_size <- origin_size + dest_size

  # Specify limits depending on profile
  if (profile == "driving-traffic") {
    rate_limit <- 30
    coord_limit <- 10
  } else {
    rate_limit <- 60
    coord_limit <- 25
  }

  # Check to see if coordinate request will exceed the limit
  if (coord_size > coord_limit) {
    chunk <- TRUE
  } else {
    chunk <- FALSE
  }

  # Specify chunking logic.
  if (chunk) {
    message("Splitting your matrix request into smaller chunks and re-assembling the result.")
    # Define slow matrix function
    mb_matrix_limited <- purrr::slowly(mb_matrix, rate = rate_delay(60 / rate_limit))

    # Scenario 1: origins exceed limit, destinations do not
    # This scenario comes up when both origins and destinations are specified.
    if (!is.null(destinations) && dest_size < coord_limit && origin_size >= coord_limit) {
      chunk_size <- coord_limit - dest_size
      if (any(grepl("^sf", class(origins)))) {
        if (sf::st_geometry_type(origins, by_geometry = FALSE) != "POINT") {
          message("Using feature centroids for origins")
        }
        matrix_output <- origins %>%
          dplyr::mutate(ix = c(0, rep(1:(nrow(origins) - 1) %/% chunk_size))) %>%
          split(.$ix) %>%
          purrr::map(., ~ {
            suppressMessages(
              mb_matrix_limited(
                origins = .x,
                destinations = destinations,
                profile = profile,
                fallback_speed = fallback_speed,
                access_token = access_token,
                output = output,
                duration_output = duration_output,
                depart_at = depart_at
              )
            )
          }, .progress = TRUE) %>%
          purrr::reduce(rbind)
        return(matrix_output)
      } else {
        ix <- c(0, rep(1:(length(origins) - 1) %/% chunk_size))
        matrix_output <- origins %>%
          split(.$ix) %>%
          purrr::map(., ~ {
            mb_matrix_limited(
              origins = .x,
              destinations = destinations,
              profile = profile,
              fallback_speed = fallback_speed,
              access_token = access_token,
              output = output,
              duration_output = duration_output,
              depart_at = depart_at
            )
          }, .progress = TRUE) %>%
          purrr::reduce(rbind)
        return(matrix_output)
      }
    }
    # Scenario 2: destinations exceed limit, origins do not
    # Again, comes up when both origins and destinations are specified
    else if (!is.null(destinations) && origin_size < coord_limit && dest_size >= coord_limit) {
      chunk_size <- coord_limit - origin_size
      if (any(grepl("^sf", class(destinations)))) {
        if (sf::st_geometry_type(origins, by_geometry = FALSE) != "POINT") {
          message("Using feature centroids for destinations")
        }
        matrix_output <- destinations %>%
          dplyr::mutate(ix = c(0, rep(1:(nrow(destinations) - 1) %/% chunk_size))) %>%
          split(.$ix) %>%
          purrr::map(., ~ {
            suppressMessages(
              mb_matrix_limited(
                origins = origins,
                destinations = .x,
                profile = profile,
                fallback_speed = fallback_speed,
                access_token = access_token,
                output = output,
                duration_output = duration_output,
                depart_at = depart_at
              )
            )
          }, .progress = TRUE) %>%
          purrr::reduce(cbind)
        return(matrix_output)
      } else {
        ix <- c(0, rep(1:(length(destinations) - 1) %/% chunk_size))
        matrix_output <- destinations %>%
          split(ix) %>%
          purrr::map(., ~ {
            mb_matrix_limited(
              origins = origins,
              destinations = .x,
              profile = profile,
              fallback_speed = fallback_speed,
              access_token = access_token,
              output = output,
              duration_output = duration_output,
              depart_at = depart_at
            )
          }, .progress = TRUE) %>%
          purrr::reduce(cbind)
        return(matrix_output)
      }
    } else if (coord_size > coord_limit && origin_size < coord_limit && dest_size < coord_limit) {
      # Scenario 3 (issue #42): the sum of origin and destination sizes are below the coordinate limit,
      # but neither are uniquely.
      if (origin_size > dest_size) {
        chunk_size <- coord_limit - dest_size
        if (any(grepl("^sf", class(origins)))) {
          if (sf::st_geometry_type(origins, by_geometry = FALSE) != "POINT") {
            message("Using feature centroids for origins")
          }
          matrix_output <- origins %>%
            dplyr::mutate(ix = c(0, rep(1:(nrow(origins) - 1) %/% chunk_size))) %>%
            split(.$ix) %>%
            purrr::map(., ~ {
              suppressMessages(
                mb_matrix_limited(
                  origins = .x,
                  destinations = destinations,
                  profile = profile,
                  fallback_speed = fallback_speed,
                  access_token = access_token,
                  output = output,
                  duration_output = duration_output,
                  depart_at = depart_at
                )
              )
            }, .progress = TRUE) %>%
            purrr::reduce(rbind)
          return(matrix_output)
        } else {
          ix <- c(0, rep(1:(length(origins) - 1) %/% chunk_size))
          matrix_output <- origins %>%
            split(.$ix) %>%
            purrr::map(., ~ {
              mb_matrix_limited(
                origins = .x,
                destinations = destinations,
                profile = profile,
                fallback_speed = fallback_speed,
                access_token = access_token,
                output = output,
                duration_output = duration_output,
                depart_at = depart_at
              )
            }, .progress = TRUE) %>%
            purrr::reduce(rbind)
          return(matrix_output)
        }
      } else if (origin_size < dest_size) {
        chunk_size <- coord_limit - origin_size
        if (any(grepl("^sf", class(destinations)))) {
          if (sf::st_geometry_type(origins, by_geometry = FALSE) != "POINT") {
            message("Using feature centroids for destinations")
          }
          matrix_output <- destinations %>%
            dplyr::mutate(ix = c(0, rep(1:(nrow(destinations) - 1) %/% chunk_size))) %>%
            split(.$ix) %>%
            purrr::map(., ~ {
              suppressMessages(
                mb_matrix_limited(
                  origins = origins,
                  destinations = .x,
                  profile = profile,
                  fallback_speed = fallback_speed,
                  access_token = access_token,
                  output = output,
                  duration_output = duration_output,
                  depart_at = depart_at
                )
              )
            }, .progress = TRUE) %>%
            purrr::reduce(cbind)
          return(matrix_output)
        } else {
          ix <- c(0, rep(1:(length(destinations) - 1) %/% chunk_size))
          matrix_output <- destinations %>%
            split(ix) %>%
            purrr::map(., ~ {
              mb_matrix_limited(
                origins = origins,
                destinations = .x,
                profile = profile,
                fallback_speed = fallback_speed,
                access_token = access_token,
                output = output,
                duration_output = duration_output,
                depart_at = depart_at
              )
            }, .progress = TRUE) %>%
            purrr::reduce(cbind)
          return(matrix_output)
        }
      }
    } else if ((origin_size > coord_limit && dest_size > coord_limit) || (origin_size > coord_limit && is.null(destinations))) {
    # Scenario 4: Both origins _and_ destinations exceed limit
    # Can be when destinations are specified, or left blank with origins as many-to-many
    # Idea: split the destinations into chunks. Then, the origin walks through the first chunk,
    # then the second, then the third, etc. until the full matrix is assembled.
    # The function will need to call itself to get this to work, so let's try it.

      if (!allow_large_matrix) {
        rlang::abort(message = c("You have requested a large travel-time matrix which may incur charges to your Mapbox account.",
                                 "i" = "To calculate this matrix, re-run `mb_matrix()` with the argument\n`allow_large_matrix = TRUE`.",
                                 "i" = "The limit for Mapbox's free tier is 100,000 matrix elements per month,\n equivalent to one 316x316 travel-time matrix.",
                                 "i" = "Please visit https://www.mapbox.com/pricing for more information."))
      } else {
        if (any(grepl("^sf", class(destinations)))) {
          matrix_output <- destinations %>%
            dplyr::mutate(ix = dplyr::ntile(n = coord_limit - 1)) %>%
            split(~ix) %>%
            purrr::map(., ~ {
              suppressMessages(
                mb_matrix(
                  origins = origins,
                  destinations = .x,
                  profile = profile,
                  fallback_speed = fallback_speed,
                  access_token = access_token,
                  output = output,
                  duration_output = duration_output,
                  depart_at = depart_at
                )
              )
            }, .progress = TRUE) %>%
            purrr::reduce(cbind)
          return(matrix_output)
        } else {
          ix <- c(0, rep(1:(length(destinations) - 1) %/% coord_limit - 1))
          matrix_output <- destinations %>%
            split(ix) %>%
            purrr::map(., ~ {
              mb_matrix(
                origins = origins,
                destinations = .x,
                profile = profile,
                fallback_speed = fallback_speed,
                access_token = access_token,
                output = output,
                duration_output = duration_output,
                depart_at = depart_at
              )
            }, .progress = TRUE) %>%
            purrr::reduce(cbind)
          return(matrix_output)
        }
      }
    }
  }

  # Specify fallback speeds based on travel profile, if fallback speed is not provided
  if (is.null(fallback_speed)) {
    if (profile %in% c("driving", "driving-traffic")) {
      fallback_speed <- "44"
    } else if (profile == "cycling") {
      fallback_speed <- "16"
    } else if (profile == "walking") {
      fallback_speed <- "5"
    }
  }

  # Parse the request
  if (any(grepl("^sf", class(origins)))) {
    if (sf::st_geometry_type(origins, by_geometry = FALSE) != "POINT") {
      origins <- suppressWarnings(sf::st_centroid(origins))
      message("Using feature centroids for origins")
    }

    origins <- sf::st_transform(origins, 4326)

    coords <- sf::st_coordinates(origins) %>%
      as.data.frame() %>%
      purrr::transpose()

    formatted_origins <- purrr::map(coords, ~ {
      paste0(.x, collapse = ",")
    }) %>%
      unlist() %>%
      paste0(collapse = ";")

    if (!is.null(destinations)) {
      if ("data.frame" %in% class(origins)) {
        origin_end <- nrow(origins) - 1
      } else {
        origin_end <- length(origins) - 1
      }
      origin_ix <- paste0(0:origin_end, collapse = ";")
      if ("data.frame" %in% class(destinations)) {
        if (length(destinations) == 1) {
          destination_ix <- origin_end + 1
        } else if (length(destinations) > 1) {
          dest_start <- origin_end + 1
          dest_end <- dest_start + (nrow(destinations) - 1)
          destination_ix <- paste0(dest_start:dest_end, collapse = ";")
        }
      } else {
        if (length(destinations) == 1) {
          destination_ix <- origin_end + 1
        } else if (length(destinations) > 1) {
          dest_start <- origin_end + 1
          dest_end <- dest_start + (length(destinations) - 1)
          destination_ix <- paste0(dest_start:dest_end, collapse = ";")
        }
      }
    } else {
      origin_ix <- "all"
      destination_ix <- "all"
    }

    formatted_coords <- formatted_origins

  }

  if ("list" %in% class(origins)) {
    if (!is.null(destinations)) {
      end <- length(origins) - 1
      origin_ix <- paste0(0:end, collapse = ";")
      if ("data.frame" %in% class(destinations)) {
        if (length(destinations) == 1) {
          destination_ix <- origin_end + 1
        } else if (length(destinations) > 1) {
          dest_start <- origin_end + 1
          dest_end <- dest_start + (nrow(destinations) - 1)
          destination_ix <- paste0(dest_start:dest_end, collapse = ";")
        }
      } else {
        if (length(destinations) == 1) {
          destination_ix <- origin_end + 1
        } else if (length(destinations) > 1) {
          dest_start <- origin_end + 1
          dest_end <- dest_start + (length(destinations) - 1)
          destination_ix <- paste0(dest_start:dest_end, collapse = ";")
        }
      }
    } else {
      origin_ix <- "all"
      destination_ix <- "all"
    }

    formatted_origins <- map(origins, ~ {
      paste0(.x, collapse = ",")
    }) %>%
      unlist() %>%
      paste0(collapse = ";")

    formatted_coords <- formatted_origins
  }

  # If destinations is supplied, process the data accordingly
  if (!is.null(destinations)) {
    if (any(grepl("^sf", class(destinations)))) {
      if (sf::st_geometry_type(destinations,
        by_geometry = FALSE
      ) != "POINT") {
        destinations <- suppressWarnings(sf::st_centroid(destinations))
        message("Using feature centroids for destinations")
      }

      destinations <- sf::st_transform(destinations, 4326)

      coords <- sf::st_coordinates(destinations) %>%
        as.data.frame() %>%
        purrr::transpose()

      formatted_destinations <- purrr::map_chr(coords, function(x) {
        unlist(x) %>%
          paste0(collapse = ",")
      }) %>%
        paste0(collapse = ";")
    } else if ("list" %in% class(destinations)) {
      formatted_destinations <- map(destinations, ~ {
        paste0(.x, collapse = ",")
      }) %>%
        unlist() %>%
        paste0(collapse = ";")
    }

    formatted_coords <- paste(formatted_origins, formatted_destinations,
      sep = ";"
    )
  }



  base_url <- paste0(
    "https://api.mapbox.com/directions-matrix/v1/mapbox/",
    profile,
    "/",
    formatted_coords
  )

  request <- httr::GET(base_url,
    query = list(
      access_token = access_token,
      sources = origin_ix,
      destinations = destination_ix,
      annotations = output,
      fallback_speed = fallback_speed,
      depart_at = depart_at
    )
  )

  content <- httr::content(request, as = "text") %>%
    jsonlite::fromJSON(flatten = TRUE)

  if (request$status_code != 200) {
    stop(print(content$message), call. = FALSE)
  }

  if (output == "distance") {
    distance_matrix <- content$distances

    return(distance_matrix)
  } else if (output == "duration") {

    duration_matrix <- content$durations

    if (duration_output == "seconds") {
      return(duration_matrix)
    } else if (duration_output == "minutes") {
      return(duration_matrix / 60)
    }

  }

}


#' Generate isochrones using the Mapbox Navigation Service Isochrone API
#'
#' This function returns isochrones from the Mapbox Navigation Service
#' [Isochrone API](https://docs.mapbox.com/api/navigation/isochrone/). Isochrones are
#' shapes that represent the reachable area around one or more locations within
#' a given travel time. Isochrones can be computed for driving, walking, or
#' cycling routing profiles, and can optionally be set to return distances
#' rather than times. [mb_isochrone()] returns isochrones as simple
#' features objects in the WGS 1984 geographic coordinate system.
#'
#' @param location A vector of form \code{c(longitude, latitude)}, an address that can be geocoded as a character string, or an `sf` object.
#' @param profile One of "driving", "walking", "cycling", or "driving-traffic".
#'                "driving" is the default.
#' @param time A vector of isochrone contours, specified in minutes. Defaults to \code{c(5, 10, 15)}. The maximum time supported is 60 minutes. Reflects traffic conditions for the date and time at which the function is called. If reproducibility of isochrones is required, supply an argument to the \code{depart_at} parameter.
#' @param distance A vector of distance contours specified in meters. If supplied, will supercede
#'                 any call to the \code{time} parameter as time and distance cannot be used
#'                 simultaneously. Defaults to \code{NULL}.
#' @param depart_at (optional) For the "driving" or "driving-traffic" profiles, the departure date and time to reflect historical traffic patterns. If "driving-traffic" is used, live traffic will be mixed in with historical traffic for dates/times near to the current time. Should be specified as an ISO 8601 date/time, e.g. \code{"2022-03-31T09:00"}. If \code{NULL} (the default), isochrones will reflect traffic conditions at the date and time when the function is called.
#' @param access_token A valid Mapbox access token.
#' @param denoise A floating-point value between 0 and 1 used to remove smaller contours. 1 is the default and returns only the largest contour for an input time.
#' @param generalize A value expressed in meters of the tolerance for the Douglas-Peucker generalization algorithm used to simplify the isochrone shapes. If \code{NULL} (the default), the Mapbox API will choose an optimal value for you.
#' @param geometry one of \code{"polygon"} (the default), which returns isochrones as polygons, or alternatively \code{"linestring"}, which returns isochrones as linestrings.
#' @param output one of \code{"sf"} (the default), which returns an `sf` object representing the isochrone(s), or \code{"list"}, which returns the GeoJSON response from the API as an R list.
#' @param rate_limit The rate limit for the API, expressed in maximum number of calls per minute. For most users this will be 300 though this parameter can be modified based on your Mapbox plan. Used when \code{location} is \code{"sf"}.
#' @param keep_color_cols Whether or not to retain the color columns that the Mapbox API generates by default (applies when the output is an `sf` object). Defaults to `FALSE`.
#' @param id_column If the input dataset is an `sf` object, the column in your dataset you want to use as the isochrone ID. Otherwise, isochrone IDs will be identified by row index or position.
#'
#' @return An `sf` object representing the isochrone(s) around the location(s).
#'
#' @examples \dontrun{
#'
#' library(mapboxapi)
#' library(mapdeck)
#' isochrones <- mb_isochrone("The Kremlin, Moscow Russia",
#'   time = c(4, 8, 12),
#'   profile = "walking"
#' )
#'
#' mapdeck(style = mapdeck_style("light")) %>%
#'   add_polygon(
#'     data = isochrones,
#'     fill_colour = "time",
#'     fill_opacity = 0.5,
#'     legend = TRUE
#'   )
#' }
#'
#' @export
mb_isochrone <- function(location,
                         profile = "driving",
                         time = c(5, 10, 15),
                         distance = NULL,
                         depart_at = NULL,
                         access_token = NULL,
                         denoise = 1,
                         generalize = NULL,
                         geometry = "polygon",
                         output = "sf",
                         rate_limit = 300,
                         keep_color_cols = FALSE,
                         id_column = NULL) {
  access_token <- get_mb_access_token(access_token)

  # if (!is.null(depart_at)) {
  #   warning("The `depart_at` parameter is no longer supported for `mb_isochrone()`; returning isochrones under typical traffic conditions.")
  # }

  # If distance is supplied, time should be set to NULL
  if (!is.null(distance)) {
    time <- NULL

    if (max(distance) > 100000) {
      stop("The maximum distance you can request is 100,000 meters (100km).",
        call. = FALSE
      )
    }
  }

  # Check to ensure the max time does not exceed the limit
  if (!is.null(time)) {
    if (max(time) > 60) {
      stop("The maximum time you can request is 60 minutes.", call. = FALSE)
    }
  }


  # If input location is an `sf` object, call a rate-limited function internally
  mb_isochrone_sf <- function(sf_object) {

    # Convert to centroids if geometry is not points
    if (sf::st_geometry_type(location, by_geometry = FALSE) != "POINT") {
      location <- suppressWarnings(sf::st_centroid(location))
      message("Using feature centroids to compute isochrones")
    }

    input_data <- st_transform(location, 4326)

    coords <- sf::st_coordinates(input_data) %>%
      as.data.frame() %>%
      transpose()

    mb_isochrone_limited <- purrr::slowly(mb_isochrone,
      rate = purrr::rate_delay(60 / rate_limit),
      quiet = TRUE
    )

    # Grab IDs to allocate to isochrones
    if ("data.frame" %in% class(location)) {
      if (!is.null(id_column)) {
        iso_ids <- as.list(location[[id_column]])
      } else {
        iso_ids <- as.list(seq(nrow(location)))
      }
    } else {
      iso_ids <- seq_len(location)
    }

    purrr::map2(coords, iso_ids, ~ {
      mb_isochrone_limited(
        location = .x,
        profile = profile,
        time = time,
        distance = distance,
        depart_at = depart_at,
        access_token = access_token,
        denoise = denoise,
        generalize = generalize,
        geometry = geometry,
        output = "sf"
      ) %>%
        dplyr::mutate(id = .y)
    }, .progress = TRUE) %>%
      dplyr::bind_rows()
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
      call. = FALSE
    )
  }

  base <- sprintf(
    "https://api.mapbox.com/isochrone/v1/mapbox/%s/%s",
    profile,
    paste0(coords, collapse = ",")
  )

  # Once assembled, we can check to see how many times have been requested
  # to handle rate-limiting internally.
  request_isochrones <- function(base, access_token, time, distance, depart_at,
                                 denoise,
                                 generalize, polygons,
                                 output, keep_color_cols) {
    if (!is.null(time)) {
      request <- GET(base,
        query = list(
          access_token = access_token,
          contours_minutes = paste0(time, collapse = ","),
          depart_at = depart_at,
          denoise = as.character(denoise),
          generalize = generalize,
          polygons = polygons
        )
      )
    } else if (!is.null(distance)) {
      request <- GET(base,
        query = list(
          access_token = access_token,
          contours_meters = paste0(distance, collapse = ","),
          depart_at = depart_at,
          denoise = as.character(denoise),
          generalize = generalize,
          polygons = polygons
        )
      )
    }



    content <- content(request, as = "text")

    if (request$status_code != 200) {
      pull <- fromJSON(content)
      stop(pull$message, call. = FALSE)
    }

    if (output == "sf") {
      if (!is.null(time)) {
        isos <- sf::read_sf(content) %>%
          dplyr::rename(time = contour)
      } else if (!is.null(distance)) {
        isos <- sf::read_sf(content) %>%
          dplyr::rename(distance = contour)
      }

      if (keep_color_cols) {
        return(isos)
      } else {
        if (!is.null(time)) {
          return(dplyr::select(isos, time))
        } else if (!is.null(distance)) {
          return(dplyr::select(isos, distance))
        }
      }
    } else if (output == "list") {
      return(content)
    }
  }

  # Handle the requested times with respect to the contour limit of 4
  if (!is.null(time)) {
    if (length(time) <= 4) {
      iso_request <- request_isochrones(
        base = base,
        access_token = access_token,
        time = time,
        distance = distance,
        depart_at = depart_at,
        denoise = denoise,
        generalize = generalize,
        polygons = polygons,
        output = output,
        keep_color_cols = keep_color_cols
      )

      return(iso_request)
    } else {
      if (output == "list") {
        stop("The maximum number of times you can request for list output is 4.", call. = FALSE)
      }

      # Chunk the times into groups of 4 or less
      times_chunked <- split(time, ceiling(seq_along(time) / 4))

      # Iterate over the times and reassemble in a rate-limited way
      request_isochrones_times <- purrr::slowly(request_isochrones,
        rate = purrr::rate_delay(60 / rate_limit),
        quiet = TRUE
      )

      iso_requests <- purrr::map(times_chunked, ~ {
        request_isochrones_times(
          base = base,
          access_token = access_token,
          time = .x,
          denoise = denoise,
          depart_at = depart_at,
          generalize = generalize,
          polygons = polygons,
          output = "sf",
          keep_color_cols = keep_color_cols
        )
      }, .progress = TRUE) %>%
        dplyr::bind_rows() %>%
        # data.table::rbindlist() %>%
        # st_as_sf(crs = 4326) %>%
        dplyr::arrange(dplyr::desc(time))

      return(iso_requests)
    }
  } else if (!is.null(distance)) {
    if (length(distance) <= 4) {
      iso_request <- request_isochrones(
        base = base,
        access_token = access_token,
        time = time,
        distance = distance,
        depart_at = depart_at,
        denoise = denoise,
        generalize = generalize,
        polygons = polygons,
        output = output,
        keep_color_cols = keep_color_cols
      )

      return(iso_request)
    } else {
      if (output == "list") {
        stop("The maximum number of distances you can request for list output is 4.",
          call. = FALSE
        )
      }

      # Chunk the distances into groups of 4 or less
      distances_chunked <- split(distance, ceiling(seq_along(distance) / 4))

      # Iterate over the distances and reassemble in a rate-limited way
      request_isochrones_distances <- purrr::slowly(request_isochrones,
        rate = purrr::rate_delay(60 / rate_limit),
        quiet = TRUE
      )

      iso_requests <- purrr::map(distances_chunked, ~ {
        request_isochrones_distances(
          base = base,
          access_token = access_token,
          distance = .x,
          time = time,
          depart_at = depart_at,
          denoise = denoise,
          generalize = generalize,
          polygons = polygons,
          output = "sf",
          keep_color_cols = keep_color_cols
        )
      }, .progress = TRUE) %>%
        dplyr::bind_rows() %>%
        # data.table::rbindlist() %>%
        # st_as_sf(crs = 4326) %>%
        dplyr::arrange(dplyr::desc(distance))

      return(iso_requests)
    }
  }
}
