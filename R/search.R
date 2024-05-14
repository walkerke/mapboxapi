#' Geocode an address or place description using the Mapbox Geocoding API
#'
#' See the [Mapbox Geocoding API
#' documentation](https://docs.mapbox.com/api/search/geocoding/) for more
#' information.
#'
#' @param search_text The text to search, formatted as a character string. Can
#'   be an address, a location, or a description of a point of interest.
#' @param structured_input A named list of structured address inputs, to be used in place of \code{search_text} when more formal address inputs are desired.  Available parameters, to be used as the names of list elements, include 'address_line1', 'address_number', 'street', 'block', 'place', 'region', 'locality', 'neighborhood', and 'country'.  See here for more documentation: \url{https://docs.mapbox.com/api/search/geocoding/#forward-geocoding-with-structured-input}.
#' @param permanent Either FALSE (the default) when results are not intended to be stored, or TRUE if the results are planned to be stored.
#' @param autocomplete Whether or not to return autocomplete results.  Defaults to FALSE.
#' @param limit How many results to return; defaults to 1 (maximum 10).
#' @param types A vector of feature types to limit to which the search should be
#'   limited. Available options include `'country'`, `'region'`, `'postcode'`,
#'   `'district'`, `'place'`, `'locality'`, `'neighborhood'`, `'address'`, `street`, `block`, `address`. and `'secondary_address'`. If left blank, all types will be searched.
#' @param search_within An `sf` object, or vector representing a bounding box of
#'   format `c(min_longitude, min_latitude, max_longitude, max_latitude)` used
#'   to limit search results. Defaults to NULL.
#' @param language The user's language, which can help with interpretation of
#'   queries. Available languages are found at
#'   <https://docs.mapbox.com/api/search/#language-coverage>.
#' @param country A character string or vector of ISO 3166 alpha-2 country codes within which you would like to limit your search.
#' @param proximity Either a vector of coordinates or an IP address string to bias the results to favor locations near to the input location.
#' @param worldview Returns features intended for different regional or cultural groups.  The US (\code{'us'}) world view is returned by default.
#' @param output If `"coordinates"` (the default), returns a length-two vector
#'   of coordinates or a list of coordinates. If `"sf"`, returns an `sf` object
#'   with the result geometries. If `"full"`, returns the full response from the
#'   API as an sf object.
#' @param access_token The Mapbox access token (required); can be set with
#'   [mb_access_token()].
#'
#' @examples \dontrun{
#'
#' whitehouse <- mb_geocode("1600 Pennsylvania Ave, Washington DC")
#' }
#'
#' @export
mb_geocode <- function(search_text = NULL,
                       structured_input = NULL,
                       permanent = FALSE,
                       autocomplete = TRUE,
                       limit = 1,
                       types = NULL,
                       search_within = NULL,
                       language = NULL,
                       country = NULL,
                       proximity = NULL,
                       worldview = NULL,
                       output = "coordinates",
                       access_token = NULL) {
  access_token <- get_mb_access_token(access_token)

  if (permanent) {
    rlang::warn(c("You have requested permanent geocoding.",
                  "i" = "You are allowed to store these geocoded results per Mapbox's Terms of Service",
                  "i" = "However, permanent geocoding may incur significant charges to your Mapbox account."),
                .frequency = "once")
  }

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

  if (!is.null(proximity)) {
    proximity <- paste0(proximity, collapse = ",")
  }

  if (!is.null(search_text) && !is.null(structured_input)) {
    rlang::abort("Either `search_text` or `structured_input` should be supplied, but not both.")
  }

  base <- "https://api.mapbox.com/search/geocode/v6/forward"


  if (!is.null(search_text)) {
    req <- httr::GET(base, query = list(
      q = search_text,
      permanent = permanent,
      autocomplete = autocomplete,
      access_token = access_token,
      limit = limit,
      bbox = bbox,
      types = types,
      language = language,
      country = country,
      proximity = proximity,
      worldview = worldview
    ))
  } else if (!is.null(structured_input)) {
    req <- httr::GET(base, query = c(structured_input, list(
      permanent = permanent,
      autocomplete = autocomplete,
      access_token = access_token,
      limit = limit,
      bbox = bbox,
      types = types,
      language = language,
      country = country,
      proximity = proximity,
      worldview = worldview
    )))
  } else {
    rlang::abort("Either `search_text` or `structured_input` should be supplied, but not both.")

  }

  content <- httr::content(req, as = "text")


  if (req$status_code != 200) {
    pull <- jsonlite::fromJSON(content)
    stop(pull$message, call. = FALSE)
  }



  if (output == "sf") {

    out <- sf::st_read(content, quiet = TRUE)

    coords <- purrr::map_dfr(out$coordinates, function(x) {
      jsonlite::fromJSON(x)
    })

    out <- out[,c("id", "mapbox_id", "feature_type", "full_address")]

    out$longitude <- coords$longitude
    out$latitude <- coords$latitude

    return(out)
  } else if (output == "coordinates") {
    cont2 <- jsonlite::fromJSON(content)

    coords <- cont2$features$geometry$coordinates

    if (limit == 1) {
      return(coords[[1]])
    } else {
      return(coords)
    }
  } else if (output == "full") {
    return(sf::st_read(content, quiet = TRUE))
  } else {
    stop("The requested output must be one of 'coordinates', 'sf', or 'full'.")
  }
}


#' Perform reverse geocoding for a coordinate pair
#'
#' @param coordinates The coordinates of a location in format `c(longitude,
#'   latitude)` for which you'd like to return information.
#' @param permanent Either FALSE (the default) when results are not intended to be stored, or TRUE if the results are planned to be stored.
#' @param limit How many results to return; defaults to 1 (maximum 10).
#' @param language The user's language, which can help with interpretation of
#'   queries. Available languages are found at
#'   <https://docs.mapbox.com/api/search/#language-coverage>.
#' @param types A vector of feature types to limit to which the search should be
#'   limited. Available options include `'country'`, `'region'`, `'postcode'`,
#'   `'district'`, `'place'`, `'locality'`, `'neighborhood'`, `'address'`, `street`, `block`, `address`. and `'secondary_address'`. If left blank, all types will be searched.
#' @param country A character string or vector of ISO 3166 alpha-2 country codes within which you would like to limit your search.
#' @param worldview Returns features intended for different regional or cultural groups.  The US (\code{'us'}) world view is returned by default.
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
#' mb_reverse_geocode(c(77.5958768, 12.9667046), limit = 5, types = "address")
#' }
#'
#' @export
mb_reverse_geocode <- function(coordinates,
                               permanent = FALSE,
                               limit = 1,
                               language = NULL,
                               types = NULL,
                               country = NULL,
                               worldview = NULL,
                               output = "text",
                               access_token = NULL) {
  access_token <- get_mb_access_token(access_token)

  if (permanent) {
    rlang::warn(c("You have requested permanent geocoding.",
                  "i" = "You are allowed to store these geocoded results per Mapbox's Terms of Service",
                  "i" = "However, permanent geocoding may incur significant charges to your Mapbox account."),
                .frequency = "once")
  }

  if (!is.null(types)) {
    types <- paste0(types, collapse = ",")
  }

  base <- "https://api.mapbox.com/search/geocode/v6/reverse"

  req <- httr::GET(base, query = list(
    longitude = coordinates[1],
    latitude = coordinates[2],
    permanent = permanent,
    access_token = access_token,
    limit = limit,
    types = types,
    country = country,
    worldview = worldview,
    language = language
  ))

  content <- httr::content(req, as = "text")


  if (req$status_code != 200) {
    pull <- fromJSON(content)
    stop(pull$message, call. = FALSE)
  }

  if (output == "text") {
    content2 <- jsonlite::fromJSON(content)

    text <- content2$features$properties$full_address

    return(text)
  } else if (output == "sf") {

    out <- sf::st_read(content, quiet = TRUE)

    coords <- jsonlite::fromJSON(out$coordinates)

    out <- out[,c("id", "mapbox_id", "feature_type", "full_address")]

    out$longitude <- coords$longitude
    out$latitude <- coords$latitude

    return(out)

  } else if (output == "full") {
    return(sf::st_read(content, quiet = TRUE))

  }
}


#' Geocode addresses or locations in bulk using the Mapbox Batch Geocoding API
#'
#' @param data An input data frame
#' @param search_column A column that contains a description of the place to geocode, or a full address.  `search_column` cannot be used with address component arguments.
#' @param address_line1 The name of a column in `data` that contains the first line of an address, e.g. "1600 Pennsylvania Ave NW"
#' @param address_number The name of a column in `data` that contains the address number, e.g. "1600".  Not required when `address_line1` is used.
#' @param street The name of a column in `data` that contains the street name, e.g. "Pennsylvania Ave NW".  Not required when `address_line1` is used.
#' @param block The name of a column in `data` that describes the block, used in some Japanese addresses.
#' @param place The name of a column in `data` that contains the place name; typically a city, village, or municipality, e.g. "Washington"
#' @param region The name of a column in `data` that represents sub-national administrative features, such as states in Mexico or the United States.  Example: "DC"
#' @param postcode The name of a column in `data` representing the postal code of the address; this will be a ZIP code in the United States, e.g. "20500"
#' @param locality The name of a column in `data` that describes official sub-city locations, such as arrondissements in France.
#' @param neighborhood The name of a column in `data` that represents a colloquial neighborhood name for the location.
#' @param country A character string or vector of ISO 3166 alpha-2 country codes within which you would like to limit your search.
#' @param permanent Either FALSE (the default) when results are not intended to be stored, or TRUE if the results are planned to be stored.
#' @param limit How many results to return per address.  This is not currently accessible for users and can only be 1.
#' @param search_within An `sf` object, or vector representing a bounding box of
#'   format `c(min_longitude, min_latitude, max_longitude, max_latitude)` used
#'   to limit search results. Defaults to NULL.
#' @param language The user's language, which can help with interpretation of
#'   queries. Available languages are found at
#'   <https://docs.mapbox.com/api/search/#language-coverage>.
#' @param types A vector of feature types to limit to which the search should be
#'   limited. Available options include `'country'`, `'region'`, `'postcode'`,
#'   `'district'`, `'place'`, `'locality'`, `'neighborhood'`, `'address'`, `street`, `block`, `address`. and `'secondary_address'`. If left blank, all types will be searched.
#' @param proximity proximity Either a vector of coordinates or an IP address string to bias the results to favor locations near to the input location.
#' @param worldview Returns features intended for different regional or cultural groups.  The US (\code{'us'}) world view is returned by default.
#' @param allow_large_job A boolean indicating that the user is OK with potential charges incurred to their account due to a large geocoding job (over 1000 addresses).  The Mapbox Free Tier includes 100,000 free geocodes per month.  Defaults to `FALSE`.
#' @param access_token The Mapbox access token (required); can be set with
#'   [mb_access_token()]
#' @param sf A boolean that determines whether the output will be an sf POINT object (`TRUE`, the default) or a regular data frame (`FALSE`).
#'
#' @return The input dataset as an sf POINT object representing the geocoded locations, or the input dataset with longitude, latitude, and matched address columns included.
#' @export
mb_batch_geocode <- function(
  data,
  search_column = NULL,
  address_line1 = NULL,
  address_number = NULL,
  street = NULL,
  block = NULL,
  place = NULL,
  region = NULL,
  postcode = NULL,
  locality = NULL,
  neighborhood = NULL,
  country = NULL,
  permanent = FALSE,
  limit = 1,
  search_within = NULL,
  language = NULL,
  types = NULL,
  proximity = NULL,
  worldview = NULL,
  allow_large_job = FALSE,
  access_token = NULL,
  sf = TRUE
) {
  access_token <- get_mb_access_token(access_token)

  if (permanent) {
    rlang::warn(c("You have requested permanent geocoding.",
                  "i" = "You are allowed to store these geocoded results per Mapbox's Terms of Service",
                  "i" = "However, permanent geocoding may incur significant charges to your Mapbox account."),
                .frequency = "once")
  }

  # Check to see if large job is allowed
  if (nrow(data) > 1000) {
    if (!allow_large_job) {
      rlang::abort(message = c("The number of rows in your input dataset exceeds 1,000.",
                               "i" = "To perform this batch geocoding job, re-run `mb_batch_geocode()` with the argument `allow_large_job = TRUE`.",
                               "i" = "The limit for Mapbox's free tier is 100,000 geocodes per month. Beyond that, you will incur charges.",
                               "i" = "Please visit https://www.mapbox.com/pricing for more information."))
    } else {
      mb_batch_geocode_limited <- purrr::slowly(mb_batch_geocode, rate = rate_delay(60))

      data$ix <- c(0, rep(1:(nrow(data) - 1) %/% 1000))

      geocodes <- data %>%
        split(~ix) %>%
        purrr::map_dfr(function(x) {
          mb_batch_geocode_limited(
            data = x,
            search_column = search_column,
            address_line1 = address_line1,
            address_number = address_number,
            street = street,
            block = block,
            place = place,
            region = region,
            postcode = postcode,
            locality = locality,
            neighborhood = neighborhood,
            country = country,
            permanent = permanent,
            limit = 1,
            search_within = search_within,
            language = language,
            types = types,
            proximity = proximity,
            worldview = worldview,
            allow_large_job = allow_large_job,
            access_token = access_token,
            sf = sf
          )
        }, .progress = TRUE) %>%
        dplyr::select(-ix)

      return(geocodes)
    }
  }




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

  if (!is.null(proximity)) {
    proximity <- paste0(proximity, collapse = ",")
  }

  # if (!is.null(search_column) && !is.null(any(c(address_line1, address_number, street,
  #                                               block, place, region, postcode, locality,
  #                                               neighborhood)))) {
  #   rlang::abort("Either `search_column` or structured address inputs should be supplied, but not both.")
  # }

  # Construct the inputs
  if (!is.null(search_column)) {
    data_input <- dplyr::tibble(
      q = data[[search_column]],
      limit = 1,
      bbox = bbox,
      types = types,
      proximity = proximity,
      worldview = worldview,
      language = language,
      country = country
    )
  } else {
    safe_extract <- purrr::possibly(function(col) data[[col]], otherwise = NULL)

    data_input <- dplyr::tibble(
      limit = 1,
      bbox = bbox,
      types = types,
      proximity = proximity,
      worldview = worldview,
      language = language,
      country = country,
      address_line1 = safe_extract(address_line1),
      address_number = safe_extract(address_number),
      street = safe_extract(street),
      block = safe_extract(block),
      place = safe_extract(place),
      region = safe_extract(region),
      postcode = safe_extract(postcode),
      locality = safe_extract(locality),
      neighborhood = safe_extract(neighborhood)
    )
  }

  input_json <- jsonlite::toJSON(data_input)

  base <- "https://api.mapbox.com/search/geocode/v6/batch"

  batch_req <- httr::POST(url = base,
                          query = list(
                            access_token = access_token,
                            permanent = permanent
                          ),
                          body = input_json,
                          httr::add_headers(c("Content-Type" = "application/json")))

  batch_content <- httr::content(batch_req, as = "text") %>% jsonlite::fromJSON()

  if (batch_req$status_code != 200) {
    stop(batch_content$message, call. = FALSE)
  }

  combined <- batch_content$batch$features |> purrr::list_rbind()

  longitudes <- combined$properties$coordinates$longitude
  latitudes <- combined$properties$coordinates$latitude
  matched_addresses <- combined$properties$full_address

  if (is.null(combined$properties$coordinates$accuracy)) {
    accuracy <- "unreliable"
  } else {
    accuracy <- combined$properties$coordinates$accuracy
  }

  accuracy[is.na(accuracy)] <- "unreliable"

  if (is.null(combined$properties$match_code$confidence)) {
    confidence <- "unreliable"
  } else {
    confidence <- combined$properties$match_code$confidence
  }

  confidence[is.na(confidence)] <- "unreliable"


  data$longitude <- longitudes
  data$latitude <- latitudes
  data$matched_address <- matched_addresses
  data$accuracy <- accuracy
  data$confidence <- confidence

  if (sf) {
    data_sf <- sf::st_as_sf(data, coords = c("longitude", "latitude"), crs = 4326)

    return(data_sf)
  } else {
    return(data)
  }

}


#' Include Mapbox Geocoder dependencies
#'
#' This function loads necessary JavaScript and CSS for the Mapbox Geocoder.
#' @import htmltools
#' @keywords internal
get_geocoder_dependencies <- function() {
  path <- system.file("www", package = "mapboxapi")

  # Define local dependency for your package's JS
  mapboxGeocoderBinding <- htmlDependency(
    name = "mapboxGeocoderBinding",
    version = "1.0.0",
    src = c(file = path),
    script = "mapboxGeocoderBinding.js"
  )

  mapboxGeocoderLocalCSS <- htmlDependency(
    name = "mapboxGeocoderLocalCSS",
    version = "1.0.0",
    src = c(file = path),
    stylesheet = "mapboxGeocoder.css"
  )

  # External Mapbox GL JS
  mapboxGLJS <- htmlDependency(
    name = "mapboxGL",
    version = "3.3.0",
    src = c(href = "https://api.mapbox.com/mapbox-gl-js/v3.3.0"),
    script = "mapbox-gl.js"
  )

  # External Mapbox GL Geocoder CSS
  mapboxGeocoderCSS <- htmlDependency(
    name = "mapboxGeocoder",
    version = "5.0.0",
    src = c(href = "https://api.mapbox.com/mapbox-gl-js/plugins/mapbox-gl-geocoder/v5.0.0"),
    stylesheet = "mapbox-gl-geocoder.css"
  )

  # External Mapbox GL Geocoder JS
  mapboxGeocoderJS <- htmlDependency(
    name = "mapboxGeocoderJS",
    version = "5.0.0",
    src = c(href = "https://api.mapbox.com/mapbox-gl-js/plugins/mapbox-gl-geocoder/v5.0.0"),
    script = "mapbox-gl-geocoder.min.js"
  )

  # Include dependencies
  htmltools::tagList(mapboxGLJS, mapboxGeocoderCSS, mapboxGeocoderJS, mapboxGeocoderBinding,
                     mapboxGeocoderLocalCSS)
}


#' Use Mapbox's Geocoder widget in a Shiny application
#'
#' @param inputId The Shiny input ID
#' @param access_token The Mapbox access token (required); can be set with
#'   [mb_access_token()]
#' @param placeholder The placeholder to be used in the search box; defaults to 'Search'
#' @param search_within An `sf` object, or vector representing a bounding box of
#'   format `c(min_longitude, min_latitude, max_longitude, max_latitude)` used
#'   to limit search results. Defaults to NULL.
#' @param proximity A length-2 vector of longitude and latitude coordinates used to prioritize results near to that location.  Defaults to NULL.
#' @param limit The maximum number of results to show.  Defaults to 5.
#' @param min_length The minimum number of characters the user must enter before results are shown.  Defaults to 2.
#' @param language The language to use for the geocoder.  Per the Mapbox documentation, "Options are IETF language tags comprised of a mandatory ISO 639-1 language code and optionally one or more IETF subtags for country or script."
#'
#' @return A Mapbox geocoder widget as a Shiny input
#' @export
mapboxGeocoderInput <- function(
    inputId,
    access_token = NULL,
    placeholder = 'Search',
    search_within = NULL,
    proximity = NULL,
    limit = 5,
    min_length = 2,
    language = NULL

) {

  access_token <- get_mb_access_token(access_token)

  if (!is.null(search_within)) {
    if (any(grepl("^sf", class(search_within)))) {
      bbox <- search_within %>%
        st_transform(4326) %>%
        st_bbox() %>%
        as.vector()
    } else {
      bbox <- bbox
    }
  } else {
    bbox <- NULL
  }

  if (!is.null(proximity)) {
    proximity_list <- list(
      longitude = proximity[1],
      latitude = proximity[2]
    )
  }

  options <- list()

  options <- modifyList(options, list(proximity = proximity, placeholder = placeholder, bbox = bbox, limit = limit, minLength = min_length, language = language))

  div(id = inputId,
      class = "mapbox-geocoder",
      `data-access-token` = access_token,
      `data-options` = jsonlite::toJSON(options, auto_unbox = TRUE),
      get_geocoder_dependencies())
}



#' Convert the result of a `mapboxGeocoderInput()` geocoded location to an sf object
#'
#' @param input The name of the Shiny input using `mapboxGeocoderInput()`, likely something like `input$geocode`
#'
#' @return An sf object that can be used downstream in your Shiny applications.
#' @export
geocoder_as_sf <- function(input) {
  coords <- unlist(input$geometry$coordinates)

  sf_obj <- coords %>%
    sf::st_point() %>%
    sf::st_sfc(crs = 4326) %>%
    sf::st_sf()

  sf_obj$longitude <- coords[1]
  sf_obj$latitude <- coords[2]
  sf_obj$full_address <- input$place_name
  sf_obj$text <- input$text

  return(sf_obj)
}


#' Convert the results of a `mapboxGeocoderInput()` geocoded location to XY coordinates
#'
#' @param input The name of the Shiny input using `mapboxGeocoderInput()`, likely something like `input$geocode`
#'
#' @return A length-2 vector representing the geocoded longitude/latitude coordinates of the location.
#' @export
geocoder_as_xy <- function(input) {
  coords <- unlist(input$geometry$coordinates)

  return(coords)
}

