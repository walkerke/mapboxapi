#' Geocode an address or place description using the Mapbox Geocoding API
#'
#' See the [Mapbox Geocoding API
#' documentation](https://docs.mapbox.com/api/search/geocoding/) for more
#' information.
#'
#' @param search_text The text to search, formatted as a character string. Can
#'   be an address, a location, or a description of a point of interest.
#' @param endpoint One of `'mapbox.places'` (the default) or
#'   `mapbox.places-permanent`. Per Mapbox's terms of service, you are only
#'   allowed to save results and perform batch geocoding with the
#'   places-permanent endpoint.
#' @param limit How many results to return; defaults to 1 (maximum 10).
#' @param types A vector of feature types to limit to which the search should be
#'   limited. Available options include `'country'`, `'region'`, `'postcode'`,
#'   `'district'`, `'place'`, `'locality'`, `'neighborhood'`, `'address'`, and
#'   `'poi'`. If left blank, all types will be searched.
#' @param search_within An `sf` object, or vector representing a bounding box of
#'   format `c(min_longitude, min_latitude, max_longitude, max_latitude)` used
#'   to limit search results. Defaults to NULL.
#' @param language The user's language, which can help with interpretation of
#'   queries. Available languages are found at
#'   <https://docs.mapbox.com/api/search/#language-coverage>.
#' @param output If `"coordinates"` (the default), returns a length-two vector
#'   of coordinates or a list of coordinates. If `"sf"`, returns an `sf` object
#'   with the result geometries. If `"full"`, returns the full response from the
#'   API.
#' @param access_token The Mapbox access token (required); can be set with
#'   [mb_access_token()].
#'
#' @examples \dontrun{
#'
#' whitehouse <- mb_geocode("1600 Pennsylvania Ave, Washington DC")
#' }
#'
#' @export
mb_geocode <- function(search_text,
                       endpoint = "mapbox.places",
                       limit = 1,
                       types = NULL,
                       search_within = NULL,
                       language = NULL,
                       output = "coordinates",
                       access_token = NULL) {
  access_token <- get_mb_access_token(access_token)

  if (!is.null(search_within)) {
    if (any(grepl("^sf", class(search_within)))) {
      bbox <- search_within %>%
        st_transform(4326) %>%
        st_bbox() %>%
        as.vector() %>%
        paste0(collapse = ",")
    } else {
      bbox <- paste0(search_within, collapse = ",")
    }
  } else {
    bbox <- NULL
  }

  if (!is.null(types)) {
    types <- paste0(types, collapse = ",")
  }

  search_text <- curl::curl_escape(search_text)

  base <- sprintf(
    "https://api.mapbox.com/geocoding/v5/%s/%s.json",
    endpoint, search_text
  )

  req <- httr::GET(base, query = list(
    access_token = access_token,
    limit = limit,
    bbox = bbox,
    types = types,
    language = language
  ))

  content <- httr::content(req, as = "text")


  if (req$status_code != 200) {
    pull <- RcppSimdJson::fparse(content)
    stop(pull$message, call. = FALSE)
  }



  if (output == "sf") {
    return(sf::st_read(content, quiet = TRUE))
  } else if (output == "coordinates") {
    cont2 <- RcppSimdJson::fparse(content)

    coords <- cont2$features$geometry$coordinates

    if (limit == 1) {
      return(coords[[1]])
    } else {
      return(coords)
    }
  } else if (output == "full") {
    return(RcppSimdJson::fparse(content))
  } else {
    stop("The requested output must be one of 'coordinates', 'sf', or 'full'.")
  }
}


#' Perform reverse geocoding for a coordinate pair
#'
#' @param coordinates The coordinates of a location in format `c(longitude,
#'   latitude)` for which you'd like to return information.
#' @param endpoint One of `'mapbox.places'` (the default) or
#'   `mapbox.places-permanent`. Per Mapbox's terms of service, you are only
#'   allowed to save results and perform batch geocoding with the
#'   places-permanent endpoint.
#' @param limit How many results to return; defaults to 1 (maximum 10).
#' @param language The user's language, which can help with interpretation of
#'   queries. Available languages are found at
#'   <https://docs.mapbox.com/api/search/#language-coverage>.
#' @param types A vector of feature types to limit to which the search should be
#'   limited. Available options include `'country'`, `'region'`, `'postcode'`,
#'   `'district'`, `'place'`, `'locality'`, `'neighborhood'`, `'address'`, and
#'   `'poi'`. If left blank, all types will be searched.
#' @param output one of `"text"` (the default), which will return a character
#'   string or list of character strings representing the returned results;
#'   `output = "sf"`, returning an `sf` object; or `"full"`, which will return a
#'   list with the full API response.
#' @param access_token The Mapbox access token (required); can be set with
#'   [mb_access_token()]
#' @rdname mb_geocode
#'
#' @return A character vector, list, or `sf` object representing the query
#'   results.
#'
#' @examples \dontrun{
#'
#' mb_reverse_geocode(c(77.5958768, 12.9667046), limit = 5, types = "poi")
#' }
#'
#' @export
mb_reverse_geocode <- function(coordinates,
                               endpoint = "mapbox.places",
                               limit = 1,
                               language = NULL,
                               types = NULL,
                               output = "text",
                               access_token = NULL) {
  access_token <- get_mb_access_token(access_token)

  coords <- paste0(coordinates, collapse = ",")

  if (!is.null(types)) {
    types <- paste0(types, collapse = ",")
  }

  base <- sprintf(
    "https://api.mapbox.com/geocoding/v5/%s/%s.json",
    endpoint, coords
  )

  req <- httr::GET(base, query = list(
    access_token = access_token,
    limit = limit,
    types = types,
    language = language
  ))

  content <- httr::content(req, as = "text")


  if (req$status_code != 200) {
    pull <- RcppSimdJson::fparse(content)
    stop(pull$message, call. = FALSE)
  }

  if (output == "text") {
    content2 <- RcppSimdJson::fparse(content)

    text <- content2$features$place_name

    return(text)
  } else if (output == "sf") {
    return(sf::st_read(content, quiet = TRUE))
  } else if (output == "full") {
    return(RcppSimdJson::fparse(content))
  }
}
