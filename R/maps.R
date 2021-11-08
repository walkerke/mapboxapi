#' Upload dataset to your Mapbox account
#'
#' @param input An sf object, or the path to the dataset to upload as a character string.
#' @param username Your Mapbox username
#' @param access_token Your Mapbox access token; must have secret scope
#' @param tileset_id The ID of the tileset in your Mapbox account
#' @param tileset_name The name of the tileset in your Mapbox account
#' @param keep_geojson Whether or not to keep the temporary GeoJSON used to generate the tiles (if the input is an sf object)
#' @param multipart Whether or not to upload to the temporary AWS staging bucket as a multipart object; defaults to \code{FALSE}.
#'
#' @examples \dontrun{
#'
#' # Example: create a tileset of median age for all United States Census tracts
#' # Requires setting a Mapbox secret access token as an environment variable
#'
#' library(mapboxapi)
#' library(tidycensus)
#' options(tigris_use_cache = TRUE)
#'
#' median_age <- get_acs(
#'   geography = "tract",
#'   variables = "B01002_001",
#'   state = c(state.abb, "DC"),
#'   geometry = TRUE
#' )
#'
#' upload_tiles(
#'   input = median_age,
#'   username = "kwalkertcu", # Your username goes here
#'   tileset_id = "median_age",
#'   tileset_name = "us_median_age_2014_to_2018"
#' )
#'
#' }
#'
#' @export
upload_tiles <- function(input,
                         username,
                         access_token = NULL,
                         tileset_id = NULL,
                         tileset_name = NULL,
                         keep_geojson = FALSE,
                         multipart = FALSE) {

  if (is.null(access_token)) {

    if (Sys.getenv("MAPBOX_SECRET_TOKEN") != "") {
      access_token <- Sys.getenv("MAPBOX_SECRET_TOKEN")
    } else {
      stop("A Mapbox secret access token is required.  Please locate yours from your Mapbox account.",
           call. = FALSE)
    }
  }

  # Allow input to be an sf object
  if (any(grepl("^sf", class(input)))) {

    input <- sf::st_transform(input, 4326)

    if (is.null(tileset_name)) {
      layer_name <- stringi::stri_rand_strings(1, 6)
    } else {
      layer_name <- tileset_name
    }

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

  } else {
    path <- input
  }

  # If tileset_id is NULL, use the basename of the input
  if (is.null(tileset_id)) {
    tileset_id <- gsub("\\.[^.]*$", "", basename(path))
  }

  # Get AWS credentials
  base1 <- sprintf("https://api.mapbox.com/uploads/v1/%s/credentials",
                   username)

  req <- httr::POST(base1, query = list(access_token = access_token))

  credentials <- httr::content(req, as = "text") %>%
    jsonlite::fromJSON()

  # Use these credentials to transfer to the staging bucket
  aws.s3::put_object(file = path,
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
#' @param upload_id The upload ID
#' @param username Your account's username
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
#' @param radius The radius around the point (in meters) for which you'd like to query features.  For point-in-polygon queries (e.g. "what county is my point located in?") the default of 0 should be used.
#' @param limit How many features to return (defaults to 5). Can be an integer between 1 and 50.
#' @param dedupe Whether or not to return duplicate features as identified by their IDs.  The default, TRUE, will de-duplicate your dataset.
#' @param geometry The feature geometry type to query - can be \code{"point"}, \code{"linestring"}, or \code{"polygon"}. If left blank, all geometry types will be queried.
#' @param layers A vector of layer IDs you'd like to query (recommended); if left blank will query all layers, with the limitation that at most 50 features can be returned.
#' @param access_token Your Mapbox access token, which can be set with \code{mb_access_token()}.
#'
#' @return An R list containing the API response, which includes information about the requested features.  Parse the list to extract desired elements.
#' @seealso \url{https://docs.mapbox.com/help/tutorials/find-elevations-with-tilequery-api/}
#'
#' @examples \dontrun{
#'
#' library(mapboxapi)
#'
#' elevation <- query_tiles(
#'   location = "Breckenridge, Colorado",
#'   tileset_id = "mapbox.mapbox-terrain-v2",
#'   layer = "contour",
#'   limit = 50
#' )
#'
#' max(elevation$features$properties$ele)
#'
#' }
#'
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
      if (Sys.getenv("MAPBOX_SECRET_TOKEN") != "") {
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
#'
#' @examples \dontrun{
#'
#' library(mapboxapi)
#' library(ggplot2)
#'
#' vector_extract <- get_vector_tiles(
#'   tileset_id = "mapbox.mapbox-streets-v8",
#'   location = c(-73.99405, 40.72033),
#'   zoom = 15
#' )
#'
#' ggplot(vector_extract$building$polygons) +
#'   geom_sf() +
#'   theme_void()
#'
#' }
#'
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
      if (Sys.getenv("MAPBOX_SECRET_TOKEN") != "") {
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


      # if (request$status_code != 200) {
      #   content <- httr::content(request, as = "text")
      #   stop(print(content$message), call. = FALSE)
      # }

      # Only try to read the data if there is data available
      if (request$status_code == 200) {
        sf_output <- protolite::read_mvt_sf(request$url)
        return(sf_output)
      }

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

    sf_list <- protolite::read_mvt_sf(request$url)

    # Iterate through the elements and parse into sub-elements when geometry is mixed
    layer_names <- names(sf_list)

    names(layer_names) <- layer_names

    output_list <- purrr::map(layer_names, function(name) {
      layer <- sf_list[[name]]
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

    })

    return(output_list)

  }

}


#' Return a static Mapbox map from a specified style
#'
#' @param style_id The style ID
#' @param username Your Mapbox username
#' @param overlay_sf The overlay sf object (optional).  The function will convert the sf object to GeoJSON then plot over the basemap style.  Spatial data that are too large will trigger an error, and should be added to the style in Mapbox Studio instead.
#' @param overlay_style A named list of vectors pecifying how to style the sf overlay.  Possible names are "stroke", "stroke-width", "stroke-opacity", "fill", and "fill-opacity".  The fill and stroke color values should be specified as six-digit hex codes, and the opacity and width values should be supplied as floating-point numbers.
#' @param overlay_markers The prepared overlay markers (optional).  See the function \code{\link{prep_overlay_markers}} for more information on how to specify a marker overlay.
#' @param longitude The longitude of the map center.  If an overlay is supplied, the map will default to the extent of the overlay unless longitude, latitude, and zoom are all specified.
#' @param latitude The latitude of the map center.  If an overlay is supplied, the map will default to the extent of the overlay unless longitude, latitude, and zoom are all specified.
#' @param zoom The map zoom.  The map will infer this from the overlay unless longitude, latitude, and zoom are all specified.
#' @param bbox The bounding box for the requested map. Not needed if longitude/latitude/zoom are provided.
#' @param width The map width; defaults to 600px
#' @param height The map height; defaults to 600px
#' @param bearing The map bearing; defaults to 0
#' @param pitch The map pitch; defaults to 0
#' @param scaling_factor The scaling factor of the tiles; either \code{"1x"} (the default) or \code{"2x"}
#' @param before_layer A character string that specifies where in the hierarchy of layer elements the overlay should be inserted.  The overlay will be placed just above the specified layer in the given Mapbox styles.
#' @param access_token Your Mapbox access token; can be set with \code{mb_access_token()}.
#'
#' @return A pointer to an image of class \code{"magick-image"}.  The resulting image can be manipulated further with functions from the magick package.
#'
#' @examples \dontrun{
#'
#' library(mapboxapi)
#'
#' points_of_interest <- tibble::tibble(
#'   longitude = c(-73.99405, -74.00616, -73.99577, -74.00761),
#'   latitude = c(40.72033, 40.72182, 40.71590, 40.71428)
#' )
#'
#' prepped_pois <- prep_overlay_markers(
#'   data = points_of_interest,
#'   marker_type = "pin-l",
#'   label = 1:4,
#'   color = "fff"
#' )
#'
#' map <- static_mapbox(
#'   style_id = "streets-v11",
#'   username = "mapbox",
#'   overlay_markers = prepped_pois,
#'   width = 1200,
#'   height = 800
#' )
#'
#' map
#'
#' }
#'
#' @export
static_mapbox <- function(style_id,
                          username,
                          overlay_sf = NULL,
                          overlay_style = NULL,
                          overlay_markers = NULL,
                          longitude = NULL,
                          latitude = NULL,
                          zoom = NULL,
                          bbox = NULL,
                          width = 600,
                          height = 400,
                          bearing = NULL,
                          pitch = NULL,
                          scaling_factor = c("1x", "2x"),
                          before_layer = NULL,
                          access_token = NULL) {

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

  # Construct the request URL
  # First, do chunk 1
  base <- sprintf("https://api.mapbox.com/styles/v1/%s/%s/static",
                    username, style_id)

  # Next, figure out the overlay
  # Basically, the idea is that you can string together GeoJSON, markers, etc.
  # on a map and put it over a Mapbox style.  overlay should accept such
  # components and format them accordingly.
  #
  # `overlay_sf` converts to GeoJSON to make the request.
  # The input GeoJSON should have internal information conforming to simplestyle-spec
  # Eventually, this function could be able to do that internally but not yet
  overlay <- NULL

  if (!is.null(overlay_sf)) {
    if (any(grepl("sfc", class(overlay_sf)))) {
      overlay_sf <- st_sf(overlay_sf)
    }

    if (!is.null(overlay_style)) {
      style_names <- names(overlay_style)
      if ("stroke" %in% style_names) {
        overlay_sf$stroke <- utils::URLencode(overlay_style$stroke,
                                              reserved = TRUE)
      }
      if ("stroke-opacity" %in% style_names) {
        overlay_sf$`stroke-opacity` <- utils::URLencode(overlay_style$`stroke-opacity`,
                                                        reserved = TRUE)
      }
      if ("stroke-width" %in% style_names) {
        overlay_sf$`stroke-width` <- utils::URLencode(overlay_style$`stroke-width`,
                                                      reserved = TRUE)
      }
      if ("fill" %in% style_names) {
        overlay_sf$fill <- utils::URLencode(overlay_style$fill,
                                            reserved = TRUE)
      }
      if ("stroke-width" %in% style_names) {
        overlay_sf$`fill-opacity` <- utils::URLencode(overlay_style$`fill-opacity`,
                                                      reserved = TRUE)
      }
    }

    overlay_json <- geojsonsf::sf_geojson(overlay_sf)

    overlay <- sprintf("geojson(%s)", overlay_json)

  }

  if (!is.null(overlay_markers)) {

    if (attr(overlay_markers, "mapboxapi") != "marker_spec") {
      stop("Overlay markers should be formatted with `prep_overlay_markers() before using in a static map", call. = FALSE)
    }

    marker_spec <- overlay_markers %>%
      unlist() %>%
      paste0(collapse = ",")


    if (!is.null(overlay)) {
      overlay <- paste(overlay, marker_spec, sep = ",")
    } else {
      overlay <- marker_spec
    }
  }

  if (!is.null(overlay)) {
    base <- paste(base, overlay, sep = "/")
  }

  focus_args <- c(longitude, latitude, zoom)

  if (all(is.null(focus_args))) {

    if (is.null(bbox)) {
      focus <- "auto"
    } else {
      collapsed_bbox <- paste0(bbox, collapse = ",")
      focus <- paste0("[", collapsed_bbox, "]")
    }

  } else {
    if (is.null(bearing)) {
      bearing <- 0
    }

    if (is.null(pitch)) {
      pitch <- 0
    }

    focus_args <- c(focus_args, bearing, pitch)

    focus <- paste0(focus_args, collapse = ",")
  }

  # If a bounding box is supplied, use it instead


  base <- paste(base, focus, sep = "/")

  base1 <- sprintf("%s/%sx%s", base, width, height)

  scaling_factor <- match.arg(scaling_factor)

  if (scaling_factor == "2x") {
    base1 <- sprintf("%s@2x", base1)
  }

  if (nchar(base1) > 8192) {
    stop("Your request is too large likely due to the size of your overlay geometry. Consider simplifying your overlay geometry or adding your data to a style in Mapbox Studio to resolve this.", call. = FALSE)
  }

  request <- httr::GET(base1, query = list(access_token = access_token,
                                           before_layer = before_layer))

  if (request$status_code != 200) {
    content <- httr::content(request, as = "text")
    stop(print(jsonlite::fromJSON(content)), call. = FALSE)
  }

  img <- magick::image_read(httr::content(request))

  return(img)

}


#' Prepare overlay markers for use in a Mapbox static map
#'
#' @param data An input data frame with longitude and latitude columns (X and Y or lon and lat as names are also acceptable) or an sf object with geometry type POINT.
#' @param marker_type The marker type; one of \code{"pin-s"}, for a small pin; \code{"pin-l"}, for a large pin; and \code{"url"}, for an image path.
#' @param label The marker label (optional).  Can be a letter, number (0 through 99), or a valid Maki icon (see \url{https://labs.mapbox.com/maki-icons/}) for options).
#' @param color The marker color (optional).  Color should be specified as a three or six-digit hexadecimal code without the number sign.
#' @param longitude A vector of longitudes; inferred from the input dataset if \code{data} is provided.
#' @param latitude A vector of latitudes; inferred from the input dataset if \code{data} is provided.
#' @param url The URL of the image to be used for the icon if \code{marker_type = "url"}.
#' @rdname static_mapbox
#' @return A formatted list of marker specifications that can be passed to the \code{\link{static_mapbox}} function.
#' @export
prep_overlay_markers <- function(data = NULL,
                                 marker_type = c("pin-s", "pin-l", "url"),
                                 label = NA,
                                 color = NA,
                                 longitude = NULL,
                                 latitude = NULL,
                                 url = NA) {


  if (!is.null(data)) {
    if (any(grepl("^sf", class(data)))) {
      if (sf::st_geometry_type(data, by_geometry = FALSE) != "POINT") {
        stop("To make markers from sf objects you must use the geometry type POINT",
             call. = FALSE)
      }
      # Construct the marker data frame
      coords_df <- data %>%
        sf::st_coordinates() %>%
        as.data.frame()

      coords_df <- cbind(data, coords_df)
    } else {
      coords_df <- data
    }
    # Infer coordinates from column names
    names(coords_df) <- tolower(names(coords_df))

    if (all(c("lon", "lat") %in% names(coords_df))) {
      coords_df$longitude <- coords_df$lon
      coords_df$latitude <- coords_df$lat
      # coords_df <- dplyr::rename(coords_df, longitude = lon, latitude = lat)
    } else if (all(c("x", "y") %in% names(coords_df))) {
      coords_df$longitude <- coords_df$x
      coords_df$latitude <- coords_df$y
      # coords_df <- dplyr::rename(coords_df, longitude = x, latitude = y)
    }

    if (!all(c("longitude", "latitude") %in% names(coords_df))) {
      stop("Your input dataset must have longitude/latitude information denoted by columns x and y, lon and lat, or longitude and latitude.", call. = FALSE)
    }

    if ("marker_type" %in% names(coords_df)) {
      marker_type <- coords_df$marker_type
    } else {
      marker_type <- match.arg(marker_type)
      coords_df$marker_type <- marker_type
    }

    if ("label" %in% names(coords_df)) {
      label <- coords_df$label
    } else {
      if (!is.null(label)) {
        coords_df$label <- label
      }
    }

    if ("color" %in% names(coords_df)) {
      color <- coords_df$color
    } else {
      if (!is.null(color)) {
        coords_df$color <- color
      }
    }

    if ("url" %in% names(coords_df)) {
      url <- coords_df$url
    } else {
      if (!is.null(url)) {
        coords_df$url <- url
      }
    }
  } else {
    coords_df <- data.frame(
      longitude = longitude,
      latitude = latitude,
      marker_type = marker_type,
      label = label,
      color = color,
      url = url
    )
  }

  # Iterate through the coords_df to make a formatted list of markers
  marker_list <- purrr::map(1:nrow(coords_df), ~{
    r <- coords_df[.x, ]

    if (r$marker_type %in% c("pin-s", "pin-l")) {
      if (!is.na(r$label) && !is.na(r$color)) {
        return(sprintf("%s-%s+%s(%s,%s)",
                       r$marker_type,
                       tolower(r$label),
                       r$color,
                       r$longitude,
                       r$latitude))
      } else if (is.na(r$label) && !is.na(r$color)) {
        return(sprintf("%s-%s(%s,%s)",
                       r$marker_type,
                       r$color,
                       r$longitude,
                       r$latitude))
      } else if (!is.na(r$label) && is.na(r$color)) {
        return(sprintf("%s-%s(%s,%s)",
                       r$marker_type,
                       tolower(r$label),
                       r$longitude,
                       r$latitude))
      } else {
        return(sprintf("%s-(%s,%s)",
                       r$marker_type,
                       r$longitude,
                       r$latitude))
      }
    } else if (r$marker_type == "url") {
      if (is.na(url)) {
        stop("A valid URL must be provided.")
      }
      encoded_url <- utils::URLencode(r$url, reserved = TRUE)
      return(sprintf("url-%s(%s,%s)",
                     encoded_url,
                     r$longitude,
                     r$latitude))
    }
  })

  attr(marker_list, 'mapboxapi') <- "marker_spec"

  return(marker_list)
}


#' Use a Mapbox style in a Leaflet map
#'
#' @param map A map widget object created by \code{leaflet::leaflet()}
#' @param style_id The style ID of your Mapbox style
#' @param username Your Mapbox username
#' @param scaling_factor The scaling factor to use when rendering the tiles.  A scaling factor of 1 (the default) returns 512px by 512px tiles.  A factor of \code{0.5} returns 256x256 tiles, and a factor of \code{2} returns 1024x1024 tiles.
#' @param access_token Your Mapbox access token; can be set with \code{mb_access_token()}.
#' @param layerId the layer ID
#' @param group The name of the group the Mapbox tile layer should belong to (for use in Shiny and to modify layers control in a Leaflet workflow)
#' @param options A list of extra options (optional)
#' @param data The data object used to derive argument values; can be provided to the initial call to \code{leaflet::leaflet()}
#'
#' @return A pointer to the Mapbox Static Tiles API which will be translated appropriately by the leaflet R package.
#'
#' @examples \dontrun{
#'
#' library(leaflet)
#' library(mapboxapi)
#'
#' leaflet() %>%
#'   addMapboxTiles(style_id = "light-v9",
#'                  username = "mapbox") %>%
#'   setView(lng = -74.0051,
#'           lat = 40.7251,
#'           zoom = 13)
#'
#' }
#'
#' @export
addMapboxTiles <- function(map,
                           style_id,
                           username,
                           scaling_factor = c("1x", "0.5x", "2x"),
                           access_token = NULL,
                           layerId = NULL,
                           group = NULL,
                           options = leaflet::tileOptions(),
                           data = leaflet::getMapData(map)) {

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

  sfactor <- match.arg(scaling_factor)

  if (sfactor == "1x") {
    url <- sprintf("https://api.mapbox.com/styles/v1/%s/%s/tiles/{z}/{x}/{y}?access_token=%s", username, style_id, access_token)
  } else if (sfactor == "0.5x") {
    url <- sprintf("https://api.mapbox.com/styles/v1/%s/%s/tiles/256/{z}/{x}/{y}?access_token=%s", username, style_id, access_token)
  } else if (sfactor == "2x") {
    url <- sprintf("https://api.mapbox.com/styles/v1/%s/%s/tiles/{z}/{x}/{y}@2x?access_token=%s", username, style_id, access_token)
  }



  mb_attribution <- '&copy; <a href="https://www.mapbox.com/about/maps/">Mapbox</a> &copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a> <strong><a href="https://www.mapbox.com/map-feedback/" target="_blank">Improve this map</a></strong>'

  leaflet::addTiles(map = map,
                    urlTemplate = url,
                    attribution = mb_attribution,
                    layerId = layerId,
                    group = group,
                    options = options,
                    data = data)

}


#' Get static tiles from a Mapbox style for use as a basemap
#'
#' This function queries the Mapbox Static Tiles API and composites the tiles as a
#' raster suitable for use as a basemap in tmap or ggplot2 (with the layer_spatial()
#' function in ggspatial).  It returns a raster layer that corresponds either to
#' an input bounding box or a buffered area around an input shape.
#'
#' @param location An input location for which you would like to request tiles.
#'                 Can be a length-4 vector representing a bounding box, or an sf object.
#'                 If an input sf object is supplied, use the \code{buffer_dist} argument to
#'                 control how much area you want to capture around the layer.
#'                 While the input sf object can be in an arbitrary coordinate reference system,
#'                 if a length-4 bounding box vector is supplied instead it must represent
#'                 WGS84 longitude/latitude coordinates and be in the order
#'                 \code{c(xmin, ymin, xmax, ymax)}.
#' @param zoom The zoom level for which you'd like to return tiles.
#' @param style_id A Mapbox style ID; retrieve yours from your Mapbox account.
#' @param username A Mapbox username.
#' @param scaling_factor The scaling factor to use; one of \code{"1x"} or \code{"2x"}.
#' @param buffer_dist The distance to buffer around an input sf object for determining tile extent, specified in meters.  Defaults to 5000.
#' @param crop Whether or not to crop the result to the specified bounding box or buffer area.
#'             Defaults to \code{TRUE}; \code{FALSE} will return the extent of the overlapping
#'             tiles.
#' @param access_token Your Mapbox access token.  Supply yours here or set globally with the \code{mb_access_token()} function.
#'
#' @return A raster layer of tiles from the requested Mapbox style representing the area around the input location.  The raster layer is projected in the Web Mercator coordinate reference system.
#'
#' @examples \dontrun{
#'
#' library(mapboxapi)
#' library(tigris)
#' library(tmap)
#' library(ggspatial)
#' library(ggplot2)
#'
#' ny_tracts <- tracts("NY", "New York", cb = TRUE)
#'
#' ny_tiles <- get_static_tiles(
#'   location = ny_tracts,
#'   zoom = 10,
#'   style_id = "light-v9",
#'   username = "mapbox"
#' )
#'
#' # tmap usage:
#' tm_shape(ny_tiles) +
#'   tm_rgb() +
#'   tm_shape(ny_tracts) +
#'   tm_polygons(alpha = 0.5, col = "navy") +
#'   tm_credits("Basemap © Mapbox, © OpenStreetMap",
#'              position = c("RIGHT", "BOTTOM"))
#'
#' # ggplot2 usage:
#' ggplot() +
#'   layer_spatial(ny_tiles) +
#'   geom_sf(data = ny_tracts, fill = "navy", alpha = 0.5) +
#'   theme_void() +
#'   labs(caption = "Basemap © Mapbox, © OpenStreetMap")
#'
#' }
#'
#' @export
get_static_tiles <- function(
  location,
  zoom,
  style_id,
  username,
  scaling_factor = c("1x", "2x"),
  buffer_dist = 5000,
  crop = TRUE,
  access_token = NULL
) {

  message("Attribution is required if using Mapbox tiles on a map.\nAdd the text '© Mapbox, © OpenStreetMap' to your map for proper attribution.")

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

  if ("RasterLayer" %in% class(location)) {
    location <- location %>%
      sf::st_bbox() %>%
      sf::st_as_sfc()
  }

  # If object is from the sp package, convert to sf
  if (any(grepl("Spatial", class(location)))) {
    input <- sf::st_as_sf(location)
  }

  if ("sfc" %in% class(location)) {
    location <- sf::st_sf(location)
  }

  # If location is an sf object, get a buffered bbox to query the tiles
  if (any(grepl("^sf", class(location)))) {

    # If the input dataset is not a polygon, make it one
    geom_type <- unique(sf::st_geometry_type(location))

    # Consider at later date how to handle mixed geometries
    # Also consider whether to use concave hulls instead to avoid edge cases

    if (geom_type %in% c("POINT", "MULTIPOINT")) {
      # If it is one or two points, buffer it
      if (nrow(location) %in% 1:2) {
        sf_proj <- sf::st_buffer(sf_proj, units::as_units(1000, "m"))
      }

      sf_poly <- location %>%
        sf::st_union() %>%
        sf::st_convex_hull()
    } else if (geom_type %in% c("LINESTRING", "MULTILINESTRING")) {
      sf_poly <- location %>%
        sf::st_cast("MULTIPOINT") %>%
        sf::st_union() %>%
        sf::st_convex_hull()
    } else if (geom_type %in% c("POLYGON", "MULTIPOLYGON")) {
      sf_poly <- location %>%
        sf::st_union()
    }

    # Get a buffered bbox of custom size
    bbox <- sf_poly %>%
      sf::st_buffer(units::as_units(buffer_dist, "m")) %>%
      sf::st_transform(4326) %>%
      sf::st_bbox(.)
  } else {
    # Make sure a length-4 vector was supplied
    if (length(location) != 4) {
      stop("To use a bounding box vector as an input location, it must be of length 4 in the format `c(xmin, ymin, xmax, ymax)`.", call. = FALSE)
    }
    bbox <- sf::st_bbox(c(xmin = location[1],
                          ymin = location[2],
                          xmax = location[3],
                          ymax = location[4]),
                        crs = 4326)
  }

  tile_grid <- slippymath::bbox_to_tile_grid(bbox = bbox, zoom = zoom)

  tile_ids <- tile_grid$tiles

  sfactor <- match.arg(scaling_factor)

  tile_list <- purrr::map2(tile_ids$x, tile_ids$y, ~{
    # Build the request to Mapbox
    if (sfactor == "2x") {

      url <- sprintf("https://api.mapbox.com/styles/v1/%s/%s/tiles/%s/%s/%s@2x",
                     username, style_id, zoom, .x, .y)

    } else {
      url <- sprintf("https://api.mapbox.com/styles/v1/%s/%s/tiles/%s/%s/%s",
                     username, style_id, zoom, .x, .y)
    }

    box <- slippymath::tile_bbox(.x, .y, zoom)

    tmp <- tempdir()

    loc <- file.path(tmp, sprintf("%s_%s.png", .x, .y))

    request <- httr::GET(url, query = list(access_token = access_token),
                         httr::write_disk(loc, overwrite = TRUE))

    # Only try to read the data if there is data available
    if (request$status_code == 200) {

      # Most styles will be returned as PNG as they are composed entirely
      # of vector layers.  However, if a style includes satellite imagery it will
      # be returned as JPEG.  Check this at this step.
      my_img <- try(png::readPNG(loc), silent = TRUE)

      if ("try-error" %in% class(my_img)) {
        my_img <- jpeg::readJPEG(loc)
      }

      my_img <- my_img * 255

      merc <- sf::st_crs(3857)$proj4string

      ras <- raster::brick(my_img)
      raster::extent(ras) <- box
      suppressWarnings(raster::projection(ras) <- merc)

      return(ras)
    }

  })

  # Assemble the list of raster tiles
  # Adapted from Michael Sumner's ceramic code: https://github.com/hypertidy/ceramic/blob/master/R/raster.R
  if (length(tile_list) == 1) {
    out_brick <- tile_list[[1]]
  } else {

    target_crs <- suppressWarnings(raster::projection(tile_list[[1]]))

    out_raster <- suppressWarnings(tile_list %>%
      purrr::map(function(tile) {
        raster::extent(tile)
      }) %>%
      purrr::reduce(raster::union) %>%
      raster::raster(crs = target_crs))

    raster::res(out_raster) <- suppressWarnings(raster::res(tile_list[[1]]))

    raster_cells <- suppressWarnings(tile_list %>%
      map(function(tile) {
        raster::cellsFromExtent(out_raster, tile)
      }) %>%
      unlist(use.names = FALSE))

    raster_values <- suppressWarnings(tile_list %>%
      map(function(tile) {
        raster::values(tile)
      }) %>%
      reduce(rbind))

    out_brick <- suppressWarnings(raster::brick(out_raster, out_raster, out_raster) %>%
      raster::setValues(raster_values[order(raster_cells), ]))

  }

  if (crop) {
    bb_shape <- bbox %>%
      sf::st_as_sfc(crs = 4326) %>%
      sf::st_sf() %>%
      sf::st_transform(sf::st_crs(out_brick))

    cropped_brick <- raster::crop(out_brick, bb_shape)

    return(cropped_brick)
  } else {
    return(out_brick)
  }

}
