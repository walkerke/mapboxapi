#' Retrieve a matrix of travel times from the Mapbox Directions API
#'
#' @param origins The input coordinates of your request.  Acceptable inputs include a list of
#' coordinate pair vectors in \code{c(x, y)} format or an sf object.
#' For sf linestrings or polygons, the distance between centroids will be taken.
#' @param destinations The destination coordinates of your request.  If \code{NULL} (the default), a many-to-many matrix using \code{origins} will be returned.
#' @param profile One of "driving" (the default), "driving-traffic", "walking", or "cycling".
#' @param fallback_speed A value expressed in kilometers per hour used to estimate travel time when a route cannot be found between locations.  The returned travel time will be based on the straight-line estimate of travel between the locations at the specified fallback speed.
#' @param access_token A Mapbox access token (required)
#' @param duration_output one of \code{"minutes"} (the default) or \code{"seconds"}
#'
#' @return An R matrix of source-destination travel times.
#' @export
mb_matrix <- function(origins,
                      destinations = NULL,
                      profile = "driving",
                      fallback_speed = NULL,
                      access_token = NULL,
                      duration_output = "minutes") {

  if (is.null(access_token)) {
    # Use public token first, then secret token
    if (Sys.getenv("MAPBOX_PUBLIC_TOKEN") != "") {
      access_token <- Sys.getenv("MAPBOX_PUBLIC_TOKEN")
    } else {
      if (Sys.getenv("MAPBOX_SECRET_TOKEN") != "") {
        access_token <- Sys.getenv("MAPBOX_SECRET_TOKEN")
      } else {
        stop("A Mapbox access token is required.  Please locate yours from your Mapbox account.", call. = FALSE)
      }

    }
  }

  if (!profile %in% c("driving", "driving-traffic", "walking", "cycling")) {
    stop("The following travel profiles are supported: 'driving', 'driving-traffic', 'walking', and 'cycling'.  Please modify your request accordingly", call. = FALSE)
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

  # Specify chunking logic.  Scenario 1: origins exceed limit, destinations do not
  # This scenario comes up when both origins and destinations are specified.
  if (chunk) {
    message("Splitting your matrix request into smaller chunks and re-assembling the result.")
    # Define slow matrix function
    mb_matrix_limited <- purrr::slowly(mb_matrix, rate = rate_delay(60 / rate_limit))

    if (!is.null(destinations) && dest_size < coord_limit && origin_size >= coord_limit) {
      chunk_size <- coord_limit - dest_size
      if (any(grepl("^sf", class(origins)))) {
        matrix_output <- origins %>%
          dplyr::mutate(ix = c(0, rep(1:(nrow(origins) - 1) %/% chunk_size))) %>%
          dplyr::group_by(ix) %>%
          dplyr::group_split(.keep = FALSE) %>%
          purrr::map(., ~{
          mb_matrix_limited(origins = .x,
                            destinations = destinations,
                            profile = profile,
                            fallback_speed = fallback_speed,
                            access_token = access_token,
                            duration_output = duration_output)
        }) %>%
          reduce(rbind)
        return(matrix_output)
      } else {
        ix <- c(0, rep(1:(length(origins) - 1) %/% chunk_size))
        matrix_output <- origins %>%
          split(ix) %>%
          purrr::map(., ~{
            mb_matrix_limited(origins = .x,
                              destinations = destinations,
                              profile = profile,
                              fallback_speed = fallback_speed,
                              access_token = access_token,
                              duration_output = duration_output)
          }) %>%
          purrr::reduce(rbind)
        return(matrix_output)
      }
    }
    # Scenario 2: destinations exceed limit, origins do not
    # Again, comes up when both origins and destinations are specified
    else if (!is.null(destinations) && origin_size < coord_limit && dest_size >= coord_limit) {
      chunk_size <- coord_limit - origin_size
      if (any(grepl("^sf", class(destinations)))) {
        matrix_output <- destinations %>%
          dplyr::mutate(ix = c(0, rep(1:(nrow(destinations) - 1) %/% chunk_size))) %>%
          dplyr::group_by(ix) %>%
          dplyr::group_split(.keep = FALSE) %>%
          purrr::map(., ~{
            mb_matrix_limited(origins = origins,
                              destinations = .x,
                              profile = profile,
                              fallback_speed = fallback_speed,
                              access_token = access_token,
                              duration_output = duration_output)
          }) %>%
          reduce(rbind)
        return(matrix_output)
      } else {
        ix <- c(0, rep(1:(length(destinations) - 1) %/% chunk_size))
        matrix_output <- destinations %>%
          split(ix) %>%
          purrr::map(., ~{
            mb_matrix_limited(origins = origins,
                              destinations = .x,
                              profile = profile,
                              fallback_speed = fallback_speed,
                              access_token = access_token,
                              duration_output = duration_output)
          }) %>%
          purrr::reduce(rbind)
        return(matrix_output)
      }
    }
    # Scenario 3: Both origins _and_ destinations exceed limit
    # Can be when destinations are specified, or left blank with origins as many-to-many
    # Idea: split the destinations into chunks.  Then, the origin walks through the first chunk,
    # then the second, then the third, etc. until the full matrix is assembled.
    # This will take a bit of work
  } else if ((origin_size > coord_limit && dest_size > coord_limit) || (origin_size > coord_limit && is.null(destinations))) {
    stop("Your matrix request is too large.  Please split up your request into smaller pieces; we plan to support this size in a future release.")
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

    formatted_origins <- purrr::map(coords, ~{
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

    formatted_origins <- map(origins, ~{
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
                               by_geometry = FALSE) != "POINT") {
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

    }

    else if ("list" %in% class(destinations)) {
      formatted_destinations <- map(destinations, ~{
        paste0(.x, collapse = ",")
      }) %>%
        unlist() %>%
        paste0(collapse = ";")
    }

    formatted_coords <- paste(formatted_origins, formatted_destinations,
                               sep = ";")

  }



  base_url <- paste0("https://api.mapbox.com/directions-matrix/v1/mapbox/",
                     profile,
                     "/",
                     formatted_coords)

  request <- httr::GET(base_url,
                 query = list(
                   access_token = access_token,
                   sources = origin_ix,
                   destinations = destination_ix,
                   fallback_speed = fallback_speed
                 ))

  content <- httr::content(request, as = "text") %>%
    jsonlite::fromJSON(flatten = TRUE)

  if (request$status_code != 200) {
    stop(print(content$message), call. = FALSE)
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
#' @param time A vector of isochrone contours, specified in minutes. Defaults to \code{c(5, 10, 15)}.  The maximum time supported is 60 minutes.
#' @param access_token A valid Mapbox access token.
#' @param denoise A floating-point value between 0 and 1 used to remove smaller contours.  1 is the default and returns only the largest contour for an input time.
#' @param geometry one of \code{"polygon"} (the default), which returns isochrones as polygons, or alternatively \code{"linestring"}, which returns isochrones as linestrings.
#' @param output one of \code{"sf"} (the default), which returns an sf object representing the isochrone(s), or \code{"list"}, which returns the GeoJSON response from the API as an R list.
#' @param rate_limit The rate limit for the API, expressed in maximum number of calls per minute.  For most users this will be 300 though this parameter can be modified based on your Mapbox plan. Used when \code{location} is \code{"sf"}.
#' @param keep_color_cols Whether or not to retain the color columns that the Mapbox API generates by default (applies when the output is an sf object).  Defaults to \code{FALSE}.
#'
#' @return An sf object representing the isochrone(s) around the location(s).
#' @export
mb_isochrone <- function(location,
                         profile = "driving",
                         time = c(5, 10, 15),
                         access_token = NULL,
                         denoise = 1,
                         geometry = "polygon",
                         output = "sf",
                         rate_limit = 300,
                         keep_color_cols = FALSE) {

  if (is.null(access_token)) {
    # Use public token first, then secret token
    if (Sys.getenv("MAPBOX_PUBLIC_TOKEN") != "") {
      access_token <- Sys.getenv("MAPBOX_PUBLIC_TOKEN")
    } else {
      if (Sys.getenv("MAPBOX_SECRET_TOKEN") != "") {
        access_token <- Sys.getenv("MAPBOX_SECRET_TOKEN")
      } else {
        stop("A Mapbox access token is required.  Please locate yours from your Mapbox account.", call. = FALSE)
      }

    }
  }

  # Check to ensure the max time does not exceed the limit
  if (max(time) > 60) {
    stop("The maximum time you can request is 60 minutes.", call. = FALSE)
  }


  # If input location is an sf object, call a rate-limited function internally
  mb_isochrone_sf <- function(sf_object) {

    # Convert to centroids if geometry is not points
    if (sf::st_geometry_type(location, by_geometry = FALSE) != "POINT") {
      location <- suppressMessages(st_centroid(location))
      message("Using feature centroids to compute isochrones")
    }

    input_data <- st_transform(location, 4326)

    coords <- st_coordinates(input_data) %>%
      as.data.frame() %>%
      transpose()

    mb_isochrone_limited <- purrr::slowly(mb_isochrone,
                                          rate = purrr::rate_delay(60 / rate_limit),
                                          quiet = TRUE)

    map(coords, ~{
      mb_isochrone_limited(location = .x,
                           profile = profile,
                           time = time,
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

  # Once assembled, we can check to see how many times have been requested
  # to handle rate-limiting internally.
  request_isochrones <- function(base, access_token, time, denoise, polygons,
                                 output, keep_color_cols) {
    request <- GET(base,
                   query = list(
                     access_token = access_token,
                     contours_minutes = paste0(time, collapse = ","),
                     denoise = as.character(denoise),
                     polygons = polygons
                   ))

    content <- content(request, as = "text")

    if (request$status_code != 200) {
      pull <- fromJSON(content)
      stop(pull$message, call. = FALSE)
    }

    if (output == "sf") {
      isos <- sf::read_sf(content) %>%
        dplyr::rename(time = contour)

      if (keep_color_cols) {
        return(isos)
      } else {
        return(dplyr::select(isos, time))
      }

      return(isos)
    } else if (output == "list") {
      return(content)
    }
  }

  # Handle the requested times with respect to the contour limit of 4
  if (length(time) <= 4) {
    iso_request <- request_isochrones(
      base = base,
      access_token = access_token,
      time = time,
      denoise = denoise,
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
                                              quiet = TRUE)

    iso_requests <- purrr::map(times_chunked, ~{
      request_isochrones_times(
        base = base,
        access_token = access_token,
        time = .x,
        denoise = denoise,
        polygons = polygons,
        output = "sf",
        keep_color_cols = keep_color_cols
      )
    }) %>%
      data.table::rbindlist() %>%
      st_as_sf(crs = 4326) %>%
      dplyr::arrange(dplyr::desc(time))

    return(iso_requests)

  }



}
