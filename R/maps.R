#' Upload dataset to your Mapbox account
#'
#' @param input The path to the dataset to upload
#' @param username Your Mapbox username
#' @param access_token Your Mapbox access token; must have secret scope
#' @param tileset_name The name of the tileset in your Mapbox account
#' @param tileset_id The ID of the tileset in your Mapbox account
#'
#' @export
upload_tiles <- function(input,
                         username,
                         access_token = NULL,
                         tileset_id = NULL,
                         tileset_name = NULL,
                         multipart = FALSE) {

  if (is.null(access_token)) {

    if (Sys.getenv("MAPBOX_SECRET_TOKEN") != "") {
      access_token <- Sys.getenv("MAPBOX_SECRET_TOKEN")
    } else {
      stop("A Mapbox secret access token is required.  Please locate yours from your Mapbox account.",
           call. = FALSE)
    }
  }

  # If tileset_id is NULL, use the basename of the input
  if (is.null(tileset_id)) {
    tileset_id <- gsub("\\.[^.]*$", "", basename(input))
  }

  # Get AWS credentials
  base1 <- sprintf("https://api.mapbox.com/uploads/v1/%s/credentials",
                   username)

  req <- httr::POST(base1, query = list(access_token = access_token))

  credentials <- httr::content(req, as = "text") %>%
    jsonlite::fromJSON()

  # Use these credentials to transfer to the staging bucket
  aws.s3::put_object(file = input,
             object = credentials$key,
             bucket = credentials$bucket,
             region = "us-east-1",
             key = credentials$accessKeyId,
             secret = credentials$secretAccessKey,
             session_token = credentials$sessionToken,
             check_region = FALSE,
             multipart = multipart)


  # Once done, generate the upload
  url <- sprintf("http://%s.s3.amazonaws.com/%s",
                 credentials$bucket,
                 credentials$key)

  if (is.null(tileset_name)) {
    upload <- sprintf('{"url": "%s", "tileset": "%s.%s"}',
                      url, username, tileset_id)
  } else {
    upload <- sprintf('{"url": "%s", "tileset": "%s.%s", "name": "%s"}',
                      url, username, tileset_id, tileset_name)
  }



  base2 <- sprintf('https://api.mapbox.com/uploads/v1/%s',
                   username)

  request <- httr::POST(base2,
                        httr::add_headers("Content-Type" = "application/json",
                                          "Cache-Control" = "no-cache"),
                        body = upload,
                        query = list(access_token = access_token))

  response <- request %>%
    httr::content(as = "text") %>%
    jsonlite::fromJSON()

  if (request$status_code != "201") {
    stop(sprintf("Upload failed: your error message is %s", response),
         call. = FALSE)
  }

  message(sprintf("Your upload ID is %s", response$id))

}


#' Check the status of a Mapbox upload
#'
#' @param username Your account's username
#' @param id The upload ID
#' @param access_token Your Mapbox access token
#'
#' @export
check_upload_status <- function(upload_id,
                                username,
                                access_token = NULL) {

  if (is.null(access_token)) {

    if (Sys.getenv("MAPBOX_SECRET_TOKEN") != "") {
      access_token <- Sys.getenv("MAPBOX_SECRET_TOKEN")
    } else {
      stop("A Mapbox secret access token is required.  Please locate yours from your Mapbox account.",
           call. = FALSE)
    }
  }

  status <- httr::GET(sprintf("https://api.mapbox.com/uploads/v1/%s/%s",
                        username, upload_id),
                query = list(access_token = access_token))

  status %>%
    httr::content(as = "text") %>%
    jsonlite::fromJSON()

}


#' Get information about features in a tileset using the Tilequery API
#'
#' @param location The location for which you'd like to query tiles, expressed as either a length-2 vector of longitude and latitude or an address you'd like to geocode.
#' @param tileset_id The tileset ID to query.
#' @param radius The radius around the point for which you'd like to query features.  For point-in-polygon queries (e.g. "what county is my point located in?") the default of 0 should be used.
#' @param limit How many features to return (defaults to 5). Can be an integer between 1 and 50.
#' @param dedupe Whether or not to return duplicate features as identified by their IDs.  The default, TRUE, will de-duplicate your dataset.
#' @param geometry The feature geometry type to query - can be \code{"point"}, \code{"linestring"}, or \code{"polygon"}. If left blank, all geometry types will be queried.
#' @param layers A vector of layer IDs you'd like to query (recommended); if left blank will query all layers, with the limitation that at most 50 features can be returned.
#' @param access_token Your Mapbox access token, which can be set with \code{mb_access_token()}.
#'
#' @return A data frame of information about the requested features.
#' @export
query_tiles <- function(location,
                        tileset_id,
                        radius = 0,
                        limit = 5,
                        dedupe = TRUE,
                        geometry = NULL,
                        layers = NULL,
                        access_token = NULL) {

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

  # If location is an address, geocode it
  if (length(location) == 1) {
    coords <- mb_geocode(location, access_token = access_token)
  } else if (length(location) == 2) {
    coords <- location
  } else {
    stop("The specified location must either be a coordinate pair or a valid address",
         call. = FALSE)
  }

  # Format the requested layers if included
  if (!is.null(layers)) {
    layers <- paste0(layers, collapse = ",")
  }

  # Format the dedupe param
  if (dedupe) {
    dedupe <- "true"
  } else {
    dedupe <- "false"
  }

  # Construct the JSON for the request
  base <- sprintf("https://api.mapbox.com/v4/%s/tilequery/%s,%s.json",
                  tileset_id, coords[1], coords[2])

  # Build the request
  query <- httr::GET(base, query = list(
    radius = radius,
    limit = limit,
    dedupe = dedupe,
    geometry = geometry,
    layers = layers,
    access_token = access_token
  ))

  content <- httr::content(query, as = "text")

  if (query$status_code != 200) {
    pull <- jsonlite::fromJSON(content)
    stop(pull$message, call. = FALSE)
  }

  result <- jsonlite::fromJSON(content)

  return(result)

}


#' Retrieve vector tiles from a given Mapbox tileset
#'
#' @param tileset_id The name of the tileset ID; names can be retrieved from your Mapbox account
#' @param location The location for which you'd like to retrieve tiles.  If the input is an sf object, the function will return data for all tiles that intersect the object's bounding box.  If the input is a coordinate pair or an address, data will be returned for the specific tile that contains the input.
#' @param zoom The zoom level of the request; larger zoom levels will return more detail but will take longer to process.
#' @param access_token Your Mapbox access token; can be set with \code{mb_access_token()}.
#'
#' @return A list of sf objects representing the different layer types found in the requested vector tiles.
#' @export
get_vector_tiles <- function(tileset_id,
                             location,
                             zoom,
                             access_token = NULL) {

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

  # If location is an sf object, get the bbox and the tiles that intersect it
  if (any(grepl("^sf", class(location)))) {
    bbox <- location %>%
      sf::st_transform(4326) %>%
      sf::st_bbox(.)

    tile_grid <- slippymath::bbox_to_tile_grid(bbox = bbox, zoom = zoom)

    tile_ids <- tile_grid$tiles

    message(sprintf("Requesting data for %s map tiles. To speed up your query, choose a smaller extent or zoom level.", nrow(tile_ids)))
    sf_list <- purrr::map2(tile_ids$x, tile_ids$y, ~{
      # Build the request to Mapbox
      url <- sprintf("https://api.mapbox.com/v4/%s/%s/%s/%s.mvt",
                     tileset_id, zoom, .x, .y)


      request <- httr::GET(url, query = list(access_token = access_token))


      if (request$status_code != 200) {
        content <- httr::content(request, as = "text")
        stop(print(content$message), call. = FALSE)
      }

      sf_output <- protolite::read_mvt_sf(request$url)

      return(sf_output)
    })

    # Now, combine the internal list elements by name
    # First, find the tile with the most elements and get the names
    max_ix <- purrr::map(sf_list, ~length(.x)) %>% which.max()

    # Next, grab the names of that list element
    layer_names <- names(sf_list[[max_ix]])

    names(layer_names) <- layer_names

    # Now, iterate over the layer names, then the lists, keeping what you need and combining
    master_list <- purrr::map(layer_names, function(name) {
      # print(name) # Leave here for de-bugging
      layer_list <- purrr::map(sf_list, ~{
        if (name %in% names(.x)) {
          layer <- .x[[name]]
          # If layer comes through as mixed geometry, we need to parse it into
          # multiple list elements and return that
          if (sf::st_geometry_type(layer, by_geometry = FALSE) == "GEOMETRY") {
            sub_geoms <- list()
            mixed_geoms <- sf::st_geometry_type(layer, by_geometry = TRUE)
            if ("POINT" %in% mixed_geoms || "MULTIPOINT" %in% mixed_geoms) {
              point_ix <- mixed_geoms %in% c("POINT", "MULTIPOINT")
              sub_geoms$points <- layer[point_ix, ] %>%
                suppressWarnings(sf::st_cast("MULTIPOINT"))
            }
            if ("LINESTRING" %in% mixed_geoms || "MULTILINESTRING" %in% mixed_geoms) {
              line_ix <- mixed_geoms %in% c("LINESTRING", "MULTILINESTRING")
              sub_geoms$lines <- layer[line_ix, ] %>%
                sf::st_cast("MULTILINESTRING")
            }
            if ("POLYGON" %in% mixed_geoms || "MULTIPOLYGON" %in% mixed_geoms) {
              polygon_ix <- mixed_geoms %in% c("POLYGON", "MULTIPOLYGON")
              sub_geoms$polygons <- layer[polygon_ix, ] %>%
                sf::st_cast("MULTIPOLYGON")
            }

            return(sub_geoms)
          } else {
            if ("POLYGON" %in% sf::st_geometry_type(layer)) {
              # Remove malformed polygons
              layer$layer_area <- sf::st_area(layer) %>% as.numeric()
              layer <- dplyr::filter(layer, layer_area > 0)
              layer <- sf::st_cast(layer, "MULTIPOLYGON")
              layer <- dplyr::select(layer, -layer_area)
            } else if ("LINESTRING" %in% sf::st_geometry_type(layer)) {
              layer <- sf::st_cast(layer, "MULTILINESTRING")
            } else if ("POINT" %in% sf::st_geometry_type(layer)) {
              layer <- sf::st_cast(layer, "MULTIPOINT")
            }
            return(layer)
          }
        }
      }) %>%
        purrr::compact()

    })

    # Within the master list, we need to separate out by geometry type
    # We'll have a mix of points, lines, and polygons for some
    parsed_list <- purrr::map(layer_names, function(name) {
      # ID the specific segment by layer name then smooth out any mixed geoms
      segment <- master_list[[name]] %>%
        rlang::flatten_if(predicate = function(x) inherits(x, "list"))
      # identify how many geometries are in the list
      geoms <- purrr::map_int(segment, ~{
        sf::st_geometry_type(.x, by_geometry = FALSE)
      })

      # If there is only one geometry, return the combined segment across the tiles
      # Otherwise, we should give back points, lines, and polygons objects as
      # appropriate
      if (length(unique(geoms)) == 1) {
        return(dplyr::bind_rows(segment))
      } else if (length(unique(geoms)) > 1) {
        output <- list()
        if (2L %in% geoms || 1L %in% geoms || 5L %in% geoms) {
          point_ix <- geoms %in% c(1L, 2L, 5L)
          output$points <- segment[point_ix] %>%
            dplyr::bind_rows() %>%
            sf::st_cast("MULTIPOINT")
        }
        if (6L %in% geoms) {
          line_ix <- geoms == 6L
          output$lines <- segment[line_ix] %>% dplyr::bind_rows()
        }
        if (7L %in% geoms) {
          polygon_ix <- geoms == 7L
          output$polygons <- segment[polygon_ix] %>% dplyr::bind_rows()
        }
        return(output)
      }
    })

    return(parsed_list)

  }

  # If location is a length-2 numeric vector of longitude/latitude, get the specific tile IDs
  # If location is an address/text description, geocode it
  if (is.vector(location)) {
    if (class(location) == "numeric") {
      if (length(location) != 2) {
        stop("Location must be a length-2 vector of format c(lon, lat) if supplying coordinates as input.")
      }
    } else if (class(location) == "character") {
      location <- mb_geocode(location)
    }

    tile_id <- slippymath::lonlat_to_tilenum(location[1], location[2], zoom = zoom)


    # Build the request to Mapbox
    url <- sprintf("https://api.mapbox.com/v4/%s/%s/%s/%s.mvt",
                   tileset_id, zoom, tile_id$x, tile_id$y)


    request <- httr::GET(url, query = list(access_token = access_token))


    if (request$status_code != 200) {
      content <- httr::content(request, as = "text")
      stop(print(content$message), call. = FALSE)
    }

    sf_output <- protolite::read_mvt_sf(request$url)

    return(sf_output)

  }



}
