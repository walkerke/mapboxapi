coords_to_tiles <- function(lon, lat, zoom) {

  lat_rad <- lat * pi / 180

  n <- 2.0 ^ zoom

  xtile <- floor((lon + 180.0) / 360.0 * n)

  ytile = floor((1.0 - log(tan(lat_rad) + (1 / cos(lat_rad))) / pi) / 2.0 * n)

  return(c(xtile, ytile))
  #  return(paste(paste("https://a.tile.openstreetmap.org", zoom, xtile, ytile, sep="/"),".png",sep=""))
}

#' Lon/lat to world coordinates
#'
#' Compute web mercator world coordinates
#' @name lonlat_to_worldcoords
#' @param lon longitude
#' @param lat latitude
#' @param tile_size tile size
#' @seealso <https://en.wikipedia.org/wiki/Web_Mercator_projection#Formulas>
#'
#' @noRd
lonlat_to_worldcoords <- function(lon, lat, tile_size = 512) {
  rad <- function(deg) deg * pi / 180.0

  lambda <- rad(lon)
  phi <- rad(lat)

  x <- (tile_size / (2 * pi)) * (lambda + pi)
  y <- (tile_size / (2 * pi)) * (pi - log(tan(pi / 4 + phi / 2)))

  cbind(x, y)
}


#' Get a bounding box from location
#'
#' @rdname location_to_bbox
#' @importFrom sf st_bbox st_as_sfc st_as_sf st_sf st_geometry_type st_buffer st_union st_convex_hull st_cast st_transform
#' @importFrom units as_units
#' @noRd
location_to_bbox <- function(location, buffer_dist, crs = 4326) {

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
      sf::st_transform(crs) %>%
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
                        crs = crs)
  }

  return(bbox)

}

