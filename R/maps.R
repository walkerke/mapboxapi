#' Upload dataset to your Mapbox account
#'
#' @param input An `sf` object, or the path to the dataset to upload as a
#'   character string.
#' @param username Your Mapbox username
#' @param access_token Your Mapbox access token; must have secret scope
#' @param tileset_id The ID of the tileset in your Mapbox account
#' @param tileset_name The name of the tileset in your Mapbox account
#' @param keep_geojson Whether or not to keep the temporary GeoJSON used to
#'   generate the tiles (if the input is an `sf` object)
#' @param multipart Whether or not to upload to the temporary AWS staging bucket
#'   as a multipart object; defaults to `FALSE`.
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
  access_token <- get_mb_access_token(access_token, secret_required = TRUE)

  # Allow input to be an `sf` object
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
  base1 <- sprintf(
    "https://api.mapbox.com/uploads/v1/%s/credentials",
    username
  )

  req <- httr::POST(base1, query = list(access_token = access_token))

  credentials <- httr::content(req, as = "text") %>%
    jsonlite::fromJSON()

  # Use these credentials to transfer to the staging bucket
  aws.s3::put_object(
    file = path,
    object = credentials$key,
    bucket = credentials$bucket,
    region = "us-east-1",
    key = credentials$accessKeyId,
    secret = credentials$secretAccessKey,
    session_token = credentials$sessionToken,
    check_region = FALSE,
    multipart = multipart
  )

  # Once done, generate the upload
  url <- sprintf(
    "http://%s.s3.amazonaws.com/%s",
    credentials$bucket,
    credentials$key
  )

  if (is.null(tileset_name)) {
    upload <- sprintf(
      '{"url": "%s", "tileset": "%s.%s"}',
      url, username, tileset_id
    )
  } else {
    upload <- sprintf(
      '{"url": "%s", "tileset": "%s.%s", "name": "%s"}',
      url, username, tileset_id, tileset_name
    )
  }

  base2 <- sprintf(
    "https://api.mapbox.com/uploads/v1/%s",
    username
  )

  request <- httr::POST(base2,
    httr::add_headers(
      "Content-Type" = "application/json",
      "Cache-Control" = "no-cache"
    ),
    body = upload,
    query = list(access_token = access_token)
  )

  response <- request %>%
    httr::content(as = "text") %>%
    jsonlite::fromJSON()

  if (request$status_code != "201") {
    stop(sprintf("Upload failed: your error message is %s", response),
      call. = FALSE
    )
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
  access_token <-
    get_mb_access_token(
      access_token,
      default = "MAPBOX_SECRET_TOKEN",
      secret_required = TRUE
      )

  status <- httr::GET(
    sprintf(
      "https://api.mapbox.com/uploads/v1/%s/%s",
      username, upload_id
    ),
    query = list(access_token = access_token)
  )

  status %>%
    httr::content(as = "text") %>%
    jsonlite::fromJSON()
}


#' Get information about features in a tileset using the Tilequery API
#'
#' @param location The location for which you'd like to query tiles, expressed
#'   as either a length-2 vector of longitude and latitude or an address you'd
#'   like to geocode.
#' @param tileset_id The tileset ID to query.
#' @param radius The radius around the point (in meters) for which you'd like to
#'   query features. For point-in-polygon queries (e.g. "what county is my point
#'   located in?") the default of 0 should be used.
#' @param limit How many features to return (defaults to 5). Can be an integer
#'   between 1 and 50.
#' @param dedupe Whether or not to return duplicate features as identified by
#'   their IDs. The default, TRUE, will de-duplicate your dataset.
#' @param geometry The feature geometry type to query - can be `"point"`,
#'   `"linestring"`, or `"polygon"`. If left blank, all geometry types
#'   will be queried.
#' @param layers A vector of layer IDs you'd like to query (recommended); if
#'   left blank will query all layers, with the limitation that at most 50
#'   features can be returned.
#' @param access_token A Mapbox access token, which can be set with
#'   [mb_access_token()].
#'
#' @return An R list containing the API response, which includes information
#'   about the requested features. Parse the list to extract desired elements.
#' @seealso
#'   <https://docs.mapbox.com/help/tutorials/find-elevations-with-tilequery-api/>
#'
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
  access_token <- get_mb_access_token(access_token)

  # If location is an address, geocode it
  if (length(location) == 1) {
    coords <- mb_geocode(location, access_token = access_token)
  } else if (length(location) == 2) {
    coords <- location
  } else {
    stop("The specified location must either be a
         coordinate pair or a valid address",
      call. = FALSE
    )
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
  base <- sprintf(
    "https://api.mapbox.com/v4/%s/tilequery/%s,%s.json",
    tileset_id, coords[1], coords[2]
  )

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
#' @param tileset_id The name of the tileset ID; names can be retrieved from
#'   your Mapbox account
#' @param location The location for which you'd like to retrieve tiles. If the
#'   input is an `sf` object, the function will return data for all tiles that
#'   intersect the object's bounding box. If the input is a coordinate pair or
#'   an address, data will be returned for the specific tile that contains the
#'   input.
#' @param zoom The zoom level of the request; larger zoom levels will return
#'   more detail but will take longer to process.
#' @param access_token A Mapbox access token; which can be set with
#'   [mb_access_token()].
#'
#' @return A list of `sf` objects representing the different layer types found
#'   in the requested vector tiles.
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
#' }
#'
#' @export
get_vector_tiles <- function(tileset_id,
                             location,
                             zoom,
                             access_token = NULL) {
  access_token <- get_mb_access_token(access_token)

  # If location is an `sf` object, get the bbox and the tiles that intersect it
  if (any(grepl("^sf", class(location)))) {
    bbox <- location %>%
      sf::st_transform(4326) %>%
      sf::st_bbox(.)

    tile_grid <- slippymath::bbox_to_tile_grid(bbox = bbox, zoom = zoom)

    tile_ids <- tile_grid$tiles

    message(sprintf("Requesting data for %s map tiles. To speed up your query
                    choose a smaller extent or zoom level.", nrow(tile_ids)))
    sf_list <- purrr::map2(tile_ids$x, tile_ids$y, ~ {
      # Build the request to Mapbox
      url <- sprintf(
        "https://api.mapbox.com/v4/%s/%s/%s/%s.mvt",
        tileset_id, zoom, .x, .y
      )


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
    max_ix <- purrr::map(sf_list, ~ length(.x)) %>% which.max()

    # Next, grab the names of that list element
    layer_names <- names(sf_list[[max_ix]])

    names(layer_names) <- layer_names

    # Now, iterate over the layer names, then the lists, keeping what you need
    # and combining
    master_list <- purrr::map(layer_names, function(name) {
      # print(name) # Leave here for de-bugging
      layer_list <- purrr::map(sf_list, ~ {
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
      geoms <- purrr::map_int(segment, ~ {
        sf::st_geometry_type(.x, by_geometry = FALSE)
      })

      # If there is only one geometry, return the combined segment across the
      # tiles
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

  # If location is a length-2 numeric vector of longitude/latitude, get the
  # specific tile IDs
  # If location is an address/text description, geocode it
  if (is.vector(location)) {
    if (inherits(location, "numeric")) {
      if (length(location) != 2) {
        stop("Location must be a length-2 vector of format c(lon, lat) if
             supplying coordinates as input.")
      }
    } else if (inherits(location, "character")) {
      location <- mb_geocode(location)
    }

    tile_id <-
      slippymath::lonlat_to_tilenum(
        location[1], location[2], zoom = zoom
        )


    # Build the request to Mapbox
    url <- sprintf(
      "https://api.mapbox.com/v4/%s/%s/%s/%s.mvt",
      tileset_id, zoom, tile_id$x, tile_id$y
    )


    request <- httr::GET(url, query = list(access_token = access_token))


    if (request$status_code != 200) {
      content <- httr::content(request, as = "text")
      stop(print(content$message), call. = FALSE)
    }

    sf_list <- protolite::read_mvt_sf(request$url)

    # Iterate through the elements and parse into sub-elements when geometry is
    # mixed
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
#' This function uses the [Mapbox Static Maps
#' API](https://www.mapbox.com/static-maps) to return a pointer to an
#' `"magick-image"` class image or a [httr::response] object from the static map
#' image URL.
#'
#' @inheritParams get_static_tiles
#' @param buffer_dist The distance to buffer around an input `sf` object for
#'   determining static map, specified in units. If location is a POINT object
#'   of 2 rows or less and `buffer_dist` is 0 or `NULL`, a 1 unit buffer is
#'   applied to try to ensure the creation of a valid bounding box for the map
#'   area.
#' @param units Units of `buffer_dist`; defaults to "m" (meters). If buffer_dist
#'   is a units class object, the units argument is ignored.
#' @param style_id A style ID (required if style_url is `NULL`).
#' @param username A Mapbox username (required if `style_url = NULL`).
#' @param style_url A Mapbox style url; defaults to `NULL`.
#' @param overlay_sf The overlay `sf` object (optional). The function will
#'   convert the `sf` object to GeoJSON then plot over the basemap style.
#'   Spatial data that are too large will trigger an error, and should be added
#'   to the style in Mapbox Studio instead.
#' @param overlay_style A named list of vectors specifying how to style the sf
#'   overlay. Possible names are "stroke", "stroke-width" (or "stroke_width"),
#'   "stroke-opacity" (or "stroke_opacity"), "fill", and "fill-opacity" (or
#'   "fill_opacity"). The fill and stroke color values can be specified as
#'   six-digit hex codes or color names, and the opacity and width values should
#'   be supplied as floating-point numbers. If overlay_style is `NULL`, the
#'   style values can be pulled from columns with the same names in
#'   `overlay_sf`.
#' @param overlay_markers The prepared overlay markers (optional). See the
#'   function [prep_overlay_markers] for more information on how to specify a
#'   marker overlay.
#' @param longitude,latitude The longitude and latitude of the map center. If an
#'   overlay is supplied, the map will default to the extent of the overlay
#'   unless longitude, latitude, and zoom are all specified.
#' @param zoom The map zoom. The map will infer this from the overlay unless
#'   longitude, latitude, and zoom are all specified.
#' @param width,height The map width and height; defaults to `NULL`
#' @param pitch,bearing The map pitch and bearing; defaults to `NULL`. pitch can
#'   range from 0 to 60, and bearing from -360 to 360.
#' @param scale ratio to scale the output image; `scale = 1` will return the
#'   largest possible image. defaults to 0.5
#' @param scaling_factor The scaling factor of the tiles; either `"1x"`
#'   (the default) or `"2x"`
#' @param attribution Controls whether there is attribution on the image.
#'   Defaults to `TRUE`. If `FALSE`, the watermarked attribution is removed from
#'   the image. You still have a legal responsibility to attribute maps that use
#'   OpenStreetMap data, which includes most maps from Mapbox. If you specify
#'   `attribution = FALSE`, you are legally required to include proper
#'   attribution elsewhere on the webpage or document.
#' @param logo Controls whether there is a Mapbox logo on the image. Defaults to
#'   `TRUE`.
#' @param before_layer A character string that specifies where in the hierarchy
#'   of layer elements the overlay should be inserted. The overlay will be
#'   placed just above the specified layer in the given Mapbox styles. List
#'   layer ids for a map style with `get_style(style_id = style_id, username =
#'   username, style_url = style_url, access_token =
#'   access_token)[["layers"]][["id"]]`
#' @param access_token A Mapbox access token; which can be set with
#'   [mb_access_token].
#' @param image If `FALSE`, return the a [httr::response] object from
#'   [httr::GET] using the static image URL; defaults to `TRUE`.
#' @param strip If `TRUE`, drop image comments and metadata when `image = TRUE`;
#'   defaults to `TRUE`.
#' @return A pointer to an image of class `"magick-image"` if `image = TRUE`.
#'   The resulting image can be manipulated further with functions from the
#'   {magick} package.
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
#' }
#'
#' @export
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @importFrom magick image_read
static_mapbox <- function(location = NULL,
                          buffer_dist = 1000,
                          units = "m",
                          style_id,
                          username,
                          style_url = NULL,
                          overlay_sf = NULL,
                          overlay_style = NULL,
                          overlay_markers = NULL,
                          longitude = NULL,
                          latitude = NULL,
                          zoom = NULL,
                          width = NULL,
                          height = NULL,
                          bearing = NULL,
                          pitch = NULL,
                          scale = 0.5,
                          scaling_factor = c("1x", "2x"),
                          attribution = TRUE,
                          logo = TRUE,
                          before_layer = NULL,
                          access_token = NULL,
                          image = TRUE,
                          strip = TRUE) {
  access_token <- get_mb_access_token(access_token)

  base <-
    set_static_map_style(
      style_url = style_url,
      username = username,
      style_id = style_id
    )

  # Next, figure out the overlay
  # Basically, the idea is that you can string together GeoJSON, markers, etc.
  # on a map and put it over a Mapbox style. overlay should accept such
  # components and format them accordingly.
  #
  # `overlay_sf` converts to GeoJSON to make the request.
  # The input GeoJSON should have internal information conforming to
  # simplestyle-spec Eventually, this function could be able to do that
  # internally but not yet

  base <-
    set_static_map_overlay(
      base = base,
      overlay_sf = overlay_sf,
      overlay_style = overlay_style,
      overlay_markers = overlay_markers
    )

  merc_bbox <- location_to_bbox(location, buffer_dist, units, crs = 3857)
  lonlat_bbox <- location_to_bbox(location, buffer_dist, units, crs = 4326)

  focus_args <- c(longitude, latitude, zoom)

  if (all(is.null(focus_args))) {
    if (is.null(location)) {
      focus <- "auto"
    } else {
      focus <- paste0("[", paste0(lonlat_bbox, collapse = ","), "]")
    }
  } else {
    if (!is.null(location) && all(sapply(c(longitude, latitude), is.null))) {
      center <- bbox_to_center(lonlat_bbox, crs = 4326)
      longitude <- center[1]
      latitude <- center[2]
    }

    if (is.null(zoom)) {
      zoom <- 11
    }

    stopifnot(
      "`zoom` must be between 0 and 22" = (zoom >= 0) && (zoom <= 22)
    )

    focus_args <- c(longitude, latitude, zoom)

    stopifnot(
      all(!is.null(focus_args))
    )

    if (is.null(bearing)) {
      bearing <- 0
    } else if ((bearing < 0) && (bearing >= -360)) {
      bearing <- 360 + bearing
    }

    if (is.null(pitch)) {
      pitch <- 0
    }

    stopifnot(
      "`bearing` must be between -360 and 360" = ((bearing >= 0) && (bearing <= 360)),
      "`pitch` must be between 0 and 60" = ((pitch >= 0) && (pitch <= 60))
    )

    focus_args <- c(focus_args, bearing, pitch)

    focus <- paste0(focus_args, collapse = ",")
  }

  base <- paste(base, focus, sep = "/")

  base1 <-
    set_static_map_dims(
      base = base,
      bbox = merc_bbox,
      width = width,
      height = height,
      scale = scale
    )

  scaling_factor <- match.arg(scaling_factor)

  if (scaling_factor == "2x") {
    base1 <- sprintf("%s@2x", base1)
  }

  if (nchar(base1) > 8192) {
    stop("Your request is too large likely due to the size of your overlay
         geometry. Consider simplifying your overlay geometry or adding your
         data to a style in Mapbox Studio to resolve this.", call. = FALSE)
  }

  if (!is.null(before_layer)) {
    before_layer <-
      match.arg(
        before_layer,
        get_style(
          style_id = style_id,
          username = username,
          style_url = style_url,
          access_token = access_token
          )[["layers"]][["id"]]
      )
  }

  request <-
    httr::GET(
      base1,
      query = list(
        access_token = access_token,
        attribution = tolower(attribution),
        logo = tolower(logo),
        before_layer = before_layer
      )
    )

  if (request$status_code != 200) {
    content <- httr::content(request, as = "text")
    stop(print(jsonlite::fromJSON(content)), call. = FALSE)
  }

  if (!image) {
    return(request)
  }

  if (!rlang::is_installed("magick") && rlang::is_interactive()) {
    rlang::check_installed("magick")
  }

  magick::image_read(httr::content(request), strip = strip)
}

#' Add username and style_id to API query for static_mapbox
#'
#' @noRd
#' @importFrom stringi stri_extract
set_static_map_style <- function(style_url = NULL, username, style_id) {
  if (!is.null(style_url)) {
    username <- stringi::stri_extract(style_url,
                                      regex = paste0("(?<=styles/).+(?=/)"))
    style_id <- stringi::stri_extract(style_url,
                                      regex = paste0("(?<=", username, "/).+"))
  }

  # Construct the request URL
  # First, do chunk 1
  base <- sprintf(
    "https://api.mapbox.com/styles/v1/%s/%s/static",
    username, style_id
  )

  return(base)
}

#' Add overlay to API query for static_mapbox
#'
#' @noRd
#' @importFrom sf st_sf
#' @importFrom utils URLencode
#' @importFrom geojsonsf sf_geojson
set_static_map_overlay <- function(base = NULL,
                                   overlay_sf = NULL,
                                   overlay_style = NULL,
                                   overlay_markers = NULL) {
  overlay <- NULL

  if (!is.null(overlay_sf)) {
    if (is_sfc(overlay_sf)) {
      overlay_sf <- sf::st_sf(overlay_sf)
    }

    overlay_sf <-
      set_overlay_style(
        overlay_sf = overlay_sf,
        overlay_style = overlay_style
      )

    if (sf::st_crs(overlay_sf)["input"] != "EPSG:4326") {
      overlay_sf <- sf::st_transform(overlay_sf, 4326)
    }

    overlay_json <- geojsonsf::sf_geojson(overlay_sf)

    overlay <- sprintf("geojson(%s)", overlay_json)
  }

  if (!is.null(overlay_markers)) {
    if (attr(overlay_markers, "mapboxapi") != "marker_spec") {
      stop("Overlay markers should be formatted with `prep_overlay_markers()`
           before using in a static map", call. = FALSE)
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
    return(paste(base, overlay, sep = "/"))
  }

  base
}

#' Set overlay style
#'
#' @noRd
#' @importFrom utils URLencode
set_overlay_style <-
  function(overlay_sf,
           overlay_style = NULL) {
    if (is.null(overlay_style)) {
      overlay_style <- overlay_sf[1, ]
    }

    style_names <- names(overlay_style)

    if ("stroke" %in% style_names) {
      if (!is_hex(overlay_style$stroke)) {
        overlay_style$stroke <- col2hex(overlay_style$stroke)
      }

      overlay_sf$stroke <-
        utils::URLencode(overlay_style$stroke, reserved = TRUE)
    }

    if ("stroke-opacity" %in% style_names) {
      overlay_sf$`stroke-opacity` <-
        utils::URLencode(
          as.character(overlay_style$`stroke-opacity`),
          reserved = TRUE
          )
    }

    if ("stroke_opacity" %in% style_names) {
      overlay_sf$`stroke-opacity` <-
        utils::URLencode(
          as.character(overlay_style$stroke_opacity),
          reserved = TRUE
          )
    }

    if ("stroke-width" %in% style_names) {
      overlay_sf$`stroke-width` <-
        utils::URLencode(
          as.character(overlay_style$`stroke-width`),
          reserved = TRUE
          )
    }

    if ("stroke_width" %in% style_names) {
      overlay_sf$`stroke-width` <-
        utils::URLencode(
          as.character(overlay_style$stroke_width),
          reserved = TRUE
          )
    }

    if ("fill" %in% style_names) {
      if (!is_hex(overlay_style$fill)) {
        overlay_style$fill <- col2hex(overlay_style$fill)
      }

      overlay_sf$fill <-
        utils::URLencode(overlay_style$fill, reserved = TRUE)
    }

    if ("fill_opacity" %in% style_names) {
      overlay_sf$`fill-opacity` <-
        utils::URLencode(
          as.character(overlay_style$fill_opacity),
          reserved = TRUE
          )
    }

    if ("fill-opacity" %in% style_names) {
      overlay_sf$`fill-opacity` <-
        utils::URLencode(
          as.character(overlay_style$`fill-opacity`),
          reserved = TRUE
          )
    }

    overlay_sf
  }

#' Add width/height to API query for static_mapbox
#'
#' @noRd
set_static_map_dims <- function(base = NULL,
                                bbox = NULL,
                                width = NULL,
                                height = NULL,
                                scale = 0.5) {
  if (all(is.null(c(width, height))) && !is.null(bbox)) {
    asp <- as.numeric(abs(bbox$xmax - bbox$xmin) / abs(bbox$ymax - bbox$ymin))
    max_size <- min(1280, round(1280 * scale))
    width <- min(max_size, round(max_size * asp))
    height <- min(max_size, round(max_size / asp))
  }

  sprintf("%s/%sx%s", base, width, height)
}


#' Make a static Mapbox ggplot2 layer or tmap basemap
#'
#' These functions wrap [static_mapbox()] and [ggspatial::layer_spatial()] or
#' [tmap::tm_rgb()] to support the use of images from the [Mapbox Static Maps
#' API](https://www.mapbox.com/static-maps) as
#' [{ggplot2}](https://ggplot2.tidyverse.org/) or
#' [{tmap}](https://r-tmap.github.io/tmap/) basemaps.
#'
#' This function uses a different approach [get_static_tiles()]. Instead,
#' [layer_static_mapbox()] is based largely on \code{layer_mapbox()} in the snapbox package
#' (available under a [MIT
#' license](https://github.com/anthonynorth/snapbox/blob/master/LICENSE). There
#' are a few key differences between [layer_static_mapbox()] and
#' \code{layer_mapbox()}. The "scale" parameter is equivalent to the
#' "scale_ratio" parameter for snapbox. Setting `scale_factor = "2x"` is
#' equivalent to setting `retina = TRUE.` Both functions return basemaps that
#' are no larger than a single tile (a maximum of 1280 by 1280 pixels).
#'
#' For [tm_static_mapbox()], [tmap::tm_shape] is called with `projection = 3857` and
#' [tmap::tm_rgb] is called with `max.value = 1`.
#'
#' @rdname layer_static_mapbox
#' @inheritParams static_mapbox
#' @param ... additional parameters passed to [ggspatial::layer_spatial] or [tmap::tm_rgb]
#' @export
#' @author Eli Pousson, \email{eli.pousson@gmail.com}
#' @author Anthony North, \email{anthony.jl.north@gmail.com}
#' @author Miles McBain, \email{miles.mcbain@gmail.com}
#' @importFrom rlang is_installed is_interactive check_installed
layer_static_mapbox <- function(location = NULL,
                                buffer_dist = 1000,
                                units = "m",
                                style_id,
                                username,
                                style_url = NULL,
                                overlay_sf = NULL,
                                overlay_style = NULL,
                                overlay_markers = NULL,
                                width = NULL,
                                height = NULL,
                                scale = 0.5,
                                scaling_factor = c("1x", "2x"),
                                attribution = TRUE,
                                logo = TRUE,
                                before_layer = NULL,
                                access_token = NULL,
                                ...) {
  if (!rlang::is_installed("ggspatial") && rlang::is_interactive()) {
    rlang::check_installed("ggspatial")
  }

  request <-
    static_mapbox(
      location = location,
      buffer_dist = buffer_dist,
      units = units,
      style_id = style_id,
      username = username,
      style_url = style_url,
      overlay_sf = overlay_sf,
      overlay_style = overlay_style,
      overlay_markers = overlay_markers,
      width = width,
      height = height,
      scale = scale,
      scaling_factor = scaling_factor,
      attribution = attribution,
      logo = logo,
      before_layer = before_layer,
      access_token = access_token,
      image = FALSE
    )

  ras <- request_to_raster(request, location, buffer_dist, units)

  ggspatial::layer_spatial(data = ras, ...)
}

#' @name tm_static_mapbox
#' @rdname layer_static_mapbox
#' @inheritParams static_mapbox
#' @importFrom rlang is_installed is_interactive check_installed
#' @export
tm_static_mapbox <- function(location = NULL,
                             buffer_dist = 1000,
                             units = "m",
                             style_id,
                             username,
                             style_url = NULL,
                             overlay_sf = NULL,
                             overlay_style = NULL,
                             overlay_markers = NULL,
                             width = NULL,
                             height = NULL,
                             scale = 0.5,
                             scaling_factor = c("1x", "2x"),
                             attribution = TRUE,
                             logo = TRUE,
                             before_layer = NULL,
                             access_token = NULL,
                             ...) {
  if (!rlang::is_installed("tmap") && rlang::is_interactive()) {
    rlang::check_installed("tmap")
  }

  request <-
    static_mapbox(
      location = location,
      buffer_dist = buffer_dist,
      units = units,
      style_id = style_id,
      username = username,
      style_url = style_url,
      overlay_sf = overlay_sf,
      overlay_style = overlay_style,
      overlay_markers = overlay_markers,
      width = width,
      height = height,
      scale = scale,
      scaling_factor = scaling_factor,
      attribution = attribution,
      logo = logo,
      before_layer = before_layer,
      access_token = access_token,
      image = FALSE
    )

  ras <- request_to_raster(request, location, buffer_dist, units)

  tmap::tm_shape(shp = ras, projection = 3857) +
    tmap::tm_rgb(max.value = 1, ...)
}

#' Convert static_mapbox API request to raster image
#'
#' @noRd
#' @importFrom sf st_crs
#' @importFrom raster brick extent projection
#' @importFrom httr content
request_to_raster <- function(request,
                              location = NULL,
                              buffer_dist = NULL,
                              units = "m") {
  ras <- raster::brick(httr::content(request))

  merc_bbox <- location_to_bbox(location, buffer_dist, units, crs = 3857)

  merc_proj <- sf::st_crs(3857)$proj4string

  raster::extent(ras) <- merc_bbox

  suppressWarnings(raster::projection(ras) <- merc_proj)

  ras
}

#' Prepare overlay markers for use in a Mapbox static map
#'
#' Markers are prepared to match GeoJSON
#' [marker-spec](https://github.com/mapbox/mapbox-gl-markers#geojson-marker-spec)
#' which is a partial implementation of the GeoJSON
#' [simplestyle-spec](https://github.com/mapbox/simplestyle-spec/tree/master/1.1.0)
#' (described as a work-in-progress by Mapbox).
#'
#' @param data An input data frame with longitude and latitude columns (X and Y
#'   or lon and lat as names are also acceptable) or an `sf` object with
#'   geometry type POINT.
#' @param marker_type The marker type; one of \code{"pin-s"}, for a small pin;
#'   \code{"pin-l"}, for a large pin; and \code{"url"}, for an image path. If
#'   marker_type is the same length as the rows in data, a mix of different
#'   marker types are allowed.
#' @param label The marker label (optional). Can be a letter, number (0 through
#'   99), or a valid Maki icon (see <https://labs.mapbox.com/maki-icons/>)
#'   for options).
#' @param color The marker color (optional). `color` can be specified as a color
#'   name or as a three or six-digit hexadecimal code (with or without the
#'   number sign).
#' @param longitude A vector of longitudes; inferred from the input dataset if
#'   \code{data} is provided.
#' @param latitude A vector of latitudes; inferred from the input dataset if
#'   \code{data} is provided.
#' @param url The URL of the image to be used for the icon if \code{marker_type
#'   = "url"}.
#' @name prep_overlay_markers
#' @return A formatted list of marker specifications that can be passed to the
#'   [static_mapbox] function.
#' @export
#' @importFrom sf st_geometry_type st_coordinates
#' @importFrom purrr map
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
        data <- suppressWarnings(sf::st_point_on_surface(data))
      }

      # Construct the marker data frame
      coords_df <- data %>%
        sf::st_transform(4326) %>%
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
    } else if (all(c("x", "y") %in% names(coords_df))) {
      coords_df$longitude <- coords_df$x
      coords_df$latitude <- coords_df$y
    }

    if (!all(c("longitude", "latitude") %in% names(coords_df))) {
      stop("Your input dataset must have longitude/latitude information denoted
           by columns x and y, lon and lat, or longitude and latitude.",
        call. = FALSE
      )
    }

    if ("marker_type" %in% names(coords_df)) {
      marker_type <- coords_df$marker_type
    } else {
      marker_type <-
        match.arg(
          marker_type, c("pin-s", "pin-l", "url"),
          several.ok = length(marker_type) == nrow(coords_df)
        )
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

    if (!all(is.null(coords_df$color))) {
      if (!is_hex(coords_df$color)) {
        coords_df$color <- col2hex(coords_df$color, TRUE)
      } else {
        coords_df$color <- rmv_hash(oords_df$color)
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
    if (!is.null(color)) {
      if (!is_hex(color)) {
        color <- col2hex(color, TRUE)
      } else {
        color <- rmv_hash(color)
      }
    }

    coords_df <-
      data.frame(
        longitude = longitude,
        latitude = latitude,
        marker_type = marker_type,
        label = label,
        color = color,
        url = url
      )
  }

  # Iterate through the coords_df to make a formatted list of markers
  marker_list <- purrr::map(1:nrow(coords_df), ~ {
    format_marker(.x, coords_df = coords_df, url = url)
  })

  attr(marker_list, "mapboxapi") <- "marker_spec"

  marker_list
}

#' Format marker using marker_spec based on n row of a coordinate data frame
#'
#' @noRd
#' @importFrom utils URLencode
format_marker <- function(n, coords_df = NULL, url = NULL) {
  r <- coords_df[n, ]

  if (r$marker_type %in% c("pin-s", "pin-l")) {
    if (!is.na(r$label) && !is.na(r$color)) {
      return(sprintf(
        "%s-%s+%s(%s,%s)",
        r$marker_type,
        tolower(r$label),
        r$color,
        r$longitude,
        r$latitude
      ))
    } else if (is.na(r$label) && !is.na(r$color)) {
      return(sprintf(
        "%s-%s(%s,%s)",
        r$marker_type,
        r$color,
        r$longitude,
        r$latitude
      ))
    } else if (!is.na(r$label) && is.na(r$color)) {
      return(sprintf(
        "%s-%s(%s,%s)",
        r$marker_type,
        tolower(r$label),
        r$longitude,
        r$latitude
      ))
    } else {
      return(sprintf(
        "%s-(%s,%s)",
        r$marker_type,
        r$longitude,
        r$latitude
      ))
    }
  } else if (r$marker_type == "url") {
    if (is.null(url)) {
      stop("A valid URL must be provided.")
    }
    encoded_url <- utils::URLencode(r$url, reserved = TRUE)
    return(sprintf(
      "url-%s(%s,%s)",
      encoded_url,
      r$longitude,
      r$latitude
    ))
  }
}

#' Use a Mapbox style in a Leaflet map
#'
#' See the [Mapbox Static Tiles API
#' documentation](https://docs.mapbox.com/api/maps/static-tiles/) for more
#' information.
#'
#' @param map A map widget object created by [leaflet::leaflet()]
#' @param style_url A Mapbox style URL
#' @param style_id The style ID of a Mapbox style
#' @param username A Mapbox username
#' @param scaling_factor The scaling factor to use when rendering the tiles. A
#'   scaling factor of `"1x"` (the default) returns 512px by 512px tiles. A
#'   factor of `"1x"` returns 256x256 tiles, and a factor of `"2x"` returns
#'   1024x1024 tiles.
#' @param access_token Your Mapbox access token; which can be set with
#'   [mb_access_token()].
#' @param layerId the layer ID
#' @param group The name of the group the Mapbox tile layer should belong to
#'   (for use in Shiny and to modify layers control in a Leaflet workflow)
#' @param options A list of extra options (optional)
#' @param data The data object used to derive argument values; can be provided
#'   to the initial call to [leaflet::leaflet()]
#' @param attribution If `TRUE`, pass a standard attribution to
#'   [leaflet::addTiles()]. If `FALSE`, attribution is `NULL`. attribution can
#'   also be a character string including HTML.
#' @return A pointer to the Mapbox Static Tiles API which will be translated
#'   appropriately by the leaflet R package.
#'
#' @examples \dontrun{
#'
#' library(leaflet)
#' library(mapboxapi)
#'
#' leaflet() %>%
#'   addMapboxTiles(
#'     style_id = "light-v9",
#'     username = "mapbox"
#'   ) %>%
#'   setView(
#'     lng = -74.0051,
#'     lat = 40.7251,
#'     zoom = 13
#'   )
#' }
#'
#' @export
addMapboxTiles <- function(map,
                           style_id,
                           username,
                           style_url = NULL,
                           scaling_factor = c("1x", "0.5x", "2x"),
                           access_token = NULL,
                           layerId = NULL,
                           group = NULL,
                           options = leaflet::tileOptions(),
                           data = leaflet::getMapData(map),
                           attribution = TRUE) {
  access_token <- get_mb_access_token(access_token)

  if (!is.null(style_url)) {
    username <- stringi::stri_extract(style_url, regex = paste0("(?<=styles/).+(?=/)"))
    style_id <- stringi::stri_extract(style_url, regex = paste0("(?<=", username, "/).+"))
  }

  sfactor <- match.arg(scaling_factor)

  url <-
    switch(sfactor,
      "1x" = sprintf("https://api.mapbox.com/styles/v1/%s/%s/tiles/{z}/{x}/{y}?access_token=%s", username, style_id, access_token),
      "0.5x" = sprintf("https://api.mapbox.com/styles/v1/%s/%s/tiles/256/{z}/{x}/{y}?access_token=%s", username, style_id, access_token),
      "2x" = sprintf("https://api.mapbox.com/styles/v1/%s/%s/tiles/{z}/{x}/{y}@2x?access_token=%s", username, style_id, access_token)
    )

  if (!is.character(attribution)) {
    if (attribution) {
      attribution <- '&copy; <a href="https://www.mapbox.com/about/maps/">Mapbox</a> &copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a> <strong><a href="https://www.mapbox.com/map-feedback/" target="_blank">Improve this map</a></strong>'
    } else {
      attribution <- NULL
    }
  }

  leaflet::addTiles(
    map = map,
    urlTemplate = url,
    attribution = attribution,
    layerId = layerId,
    group = group,
    options = options,
    data = data
  )
}


#' Get static tiles from a Mapbox style for use as a basemap
#'
#' This function queries the [Mapbox Static Tiles
#' API](https://docs.mapbox.com/api/maps/static-tiles/) and composites the tiles
#' as a raster suitable for use as a basemap in
#' [tmap](https://r-tmap.github.io/tmap/) or
#' [ggplot2](https://ggplot2.tidyverse.org/) (with the
#' [ggspatial::layer_spatial()] function. It returns a raster
#' layer that corresponds either to an input bounding box or a buffered area
#' around an input shape.
#'
#' @param location An input location for which you would like to request tiles.
#'                 Can be a length-4 vector representing a bounding box, or an `sf` object.
#'                 If an input `sf` object is supplied, use the \code{buffer_dist} argument to
#'                 control how much area you want to capture around the layer.
#'                 While the input `sf` object can be in an arbitrary coordinate reference system,
#'                 if a length-4 bounding box vector is supplied instead it must represent
#'                 WGS84 longitude/latitude coordinates and be in the order
#'                 \code{c(xmin, ymin, xmax, ymax)}.
#' @param zoom The zoom level for which you'd like to return tiles.
#' @param style_id A Mapbox style ID; retrieve yours from your Mapbox account.
#' @param username A Mapbox username.
#' @param style_url A Mapbox style URL.
#' @param scaling_factor The scaling factor to use; one of \code{"1x"} or \code{"2x"}.
#' @param buffer_dist The distance to buffer around an input `sf` object for determining tile extent, specified in units. Defaults to 5000.
#' @param units Units of `buffer_dist`; defaults to "m" (meters). If buffer_dist
#'   is a units class object, the units argument is ignored.
#' @param crop Whether or not to crop the result to the specified bounding box or buffer area.
#'             Defaults to `TRUE`; `FALSE` will return the extent of the overlapping
#'             tiles.
#' @param access_token A Mapbox access token. Supply yours here or set globally with the [mb_access_token()] function.
#'
#' @return A raster layer of tiles from the requested Mapbox style representing the area around the input location. The raster layer is projected in the Web Mercator coordinate reference system.
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
#'   tm_credits("Basemap (c) Mapbox, (c) OpenStreetMap",
#'     position = c("RIGHT", "BOTTOM")
#'   )
#'
#' # ggplot2 usage:
#' ggplot() +
#'   layer_spatial(ny_tiles) +
#'   geom_sf(data = ny_tracts, fill = "navy", alpha = 0.5) +
#'   theme_void() +
#'   labs(caption = "Basemap (c) Mapbox, (c) OpenStreetMap")
#' }
#'
#' @export
get_static_tiles <- function(location,
                             zoom,
                             style_id,
                             username,
                             style_url = NULL,
                             scaling_factor = c("1x", "2x"),
                             buffer_dist = 5000,
                             units = "m",
                             crop = TRUE,
                             access_token = NULL) {
  message("Attribution is required if using Mapbox tiles on a map.\nAdd the text '(c) Mapbox, (c) OpenStreetMap' to your map for proper attribution.")

  access_token <- get_mb_access_token(access_token)

  if (!is.null(style_url)) {
    username <- stringi::stri_extract(style_url, regex = paste0("(?<=styles/).+(?=/)"))
    style_id <- stringi::stri_extract(style_url, regex = paste0("(?<=", username, "/).+"))
  }

  bbox <- location_to_bbox(location, buffer_dist, units)

  tile_grid <- slippymath::bbox_to_tile_grid(bbox = bbox, zoom = zoom)

  tile_ids <- tile_grid$tiles

  sfactor <- match.arg(scaling_factor)

  tile_list <- purrr::map2(tile_ids$x, tile_ids$y, ~ {
    # Build the request to Mapbox
    if (sfactor == "2x") {
      url <- sprintf(
        "https://api.mapbox.com/styles/v1/%s/%s/tiles/%s/%s/%s@2x",
        username, style_id, zoom, .x, .y
      )
    } else {
      url <- sprintf(
        "https://api.mapbox.com/styles/v1/%s/%s/tiles/%s/%s/%s",
        username, style_id, zoom, .x, .y
      )
    }

    box <- slippymath::tile_bbox(.x, .y, zoom)

    tmp <- tempdir()

    loc <- file.path(tmp, sprintf("%s_%s.png", .x, .y))

    request <- httr::GET(url,
      query = list(access_token = access_token),
      httr::write_disk(loc, overwrite = TRUE)
    )

    # Only try to read the data if there is data available
    if (request$status_code == 200) {

      # Most styles will be returned as PNG as they are composed entirely
      # of vector layers. However, if a style includes satellite imagery it will
      # be returned as JPEG. Check this at this step.
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
