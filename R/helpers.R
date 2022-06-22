coords_to_tiles <- function(lon, lat, zoom) {
  lat_rad <- lat * pi / 180

  n <- 2.0^zoom

  xtile <- floor((lon + 180.0) / 360.0 * n)

  ytile <- floor((1.0 - log(tan(lat_rad) + (1 / cos(lat_rad))) / pi) / 2.0 * n)

  return(c(xtile, ytile))
  #  return(paste(paste("https://a.tile.openstreetmap.org", zoom, xtile, ytile, sep="/"),".png",sep=""))
}

#' Get a bounding box from location
#'
#' @rdname location_to_bbox
#' @importFrom sf st_bbox st_as_sfc st_as_sf st_sf st_geometry_type st_buffer st_union st_convex_hull st_cast st_transform
#' @importFrom units as_units
#' @noRd
location_to_bbox <- function(location, buffer_dist = 1000, units = "m", crs = 4326, null.ok = TRUE) {
  if (is.null(location) && null.ok) {
    return(location)
  }

  if ("RasterLayer" %in% class(location)) {
    location <- sf::st_bbox(location)
  }

  # If object is from the sp package, convert to sf
  if (any(grepl("Spatial", class(location)))) {
    input <- sf::st_as_sf(location)
  }

  if (is_sfc(location)) {
    location <- sf::st_sf(location)
  }

  if (is_bbox(location)) {
    location <- sf::st_as_sf(sf::st_as_sfc(location))
  }

  # If location is an `sf` object, get a buffered bbox to query the tiles
  if (is_sf(location)) {

    # If the input dataset is not a polygon, make it one
    geom_type <- unique(sf::st_geometry_type(location))

    # Consider at later date how to handle mixed geometries
    # Also consider whether to use concave hulls instead to avoid edge cases
    if (geom_type %in% c("POINT", "MULTIPOINT")) {
      # If it is one or two points, buffer it
      if (nrow(location) <= 2) {
        if (is.null(buffer_dist) | (buffer_dist == 0)) {
          sf_poly <- sf::st_buffer(location, as_dist_units(1, units))
        } else {
          sf_poly <- sf::st_buffer(location, as_dist_units(buffer_dist, units))
          buffer_dist <- 0
        }
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
      sf::st_buffer(as_dist_units(buffer_dist, units)) %>%
      sf::st_transform(crs) %>%
      sf::st_bbox(.)

    return(bbox)
  }

  # Make sure a length-4 vector was supplied
  if (length(location) != 4) {
    stop("To use a bounding box vector as an input location, it must be of length 4 in the format `c(xmin, ymin, xmax, ymax)`.", call. = FALSE)
  }

  sf::st_bbox(
    c(
      xmin = location[1],
      ymin = location[2],
      xmax = location[3],
      ymax = location[4]
    ),
    crs = crs
  )
}

#' Convert bbox to centroid coordinates
#'
#' @rdname bbox_to_center
#' @noRd
#' @importFrom sf st_transform st_sf st_as_sfc st_coordinates st_centroid
bbox_to_center <- function(bbox, crs = 4326) {
  location <-
    sf::st_transform(sf::st_sf(sf::st_as_sfc(bbox)), crs)

  center <-
    sf::st_coordinates(
      suppressWarnings(sf::st_centroid(location))
    )

  return(center)
}

#' Is this a units class object?
#'
#' @noRd
is_units <- function(x) {
  any("units" %in% class(x))
}

#' Is this a bbox class object?
#'
#' @noRd
is_bbox <- function(x) {
  any("bbox" %in% class(x))
}

#' Is this a sfc class object?
#'
#' @noRd
is_sfc <- function(x) {
  any(grepl("sfc", class(x)))
}

#' Is this a sf or sfc class object?
#'
#' @noRd
is_sf <- function(x) {
  any(grepl("^sf", class(x)))
}

#' Convert numeric vector to distance units objects or convert units for distance
#'
#' Adapted from [overedge::convert_dist_units()]
#'
#' @param x Numeric or units object
#' @param from Existing unit for dist, Default: `m`.
#' @param to Unit to convert distance to, Default: `NULL`.
#' @return units class object from [units::set_units()]
#' @noRd
#' @importFrom units as_units
as_dist_units <- function(x,
                          from = "m",
                          to = "m") {
  stopifnot(
    "`buffer_dist` must be a numeric or units class object." = (is.numeric(x) || is_units(x))
  )

  if (!is.null(from) && !is_units(x)) {
    x <- set_dist_units(x, from)
  }

  if (!is.null(to)) {
    x <- set_dist_units(x, to)
  }

  x
}

#' Set distance units
#'
#' @noRd
#' @importFrom units set_units
set_dist_units <- function(x, value, mode = "standard") {
  value <-
    match.arg(
      gsub(" ", "_", value),
      dist_unit_options
    )

  units::set_units(
    x = x,
    value = value,
    mode = mode
  )
}

#' Is x a hex value?
#'
#' @noRd
is_hex <- function(x) {
  all(grepl("^#", x))
}

#' Convert color name to hex value
#'
#' @noRd
col2hex <- function(color) {
  if (!rlang::is_installed("grDevices") && rlang::is_interactive()) {
    rlang::check_installed("grDevices")
  }

  grDevices::rgb(t(grDevices::col2rgb(color)), maxColorValue = 255)
}
