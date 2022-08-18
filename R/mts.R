make_newline_geojson <- function(data, file_loc) {

  # Credit to https://github.com/SymbolixAU/geojsonsf/issues/96
  # for example implementation
  data_gj <- geojsonsf::sf_geojson(data, atomise = TRUE)
  filecon <- file(file_loc)
  writeLines(data_gj, filecon, sep = "\n")
  close(filecon)

}

#' Create a Mapbox tileset source from a sf object using the Mapbox Tiling Service API
#'
#' The \code{mts_create_source()} function can be used to create, append to, or replace an existing tileset source.
#'
#' @param data An input simple features object
#' @param tileset_id The tileset ID. If the tileset ID already exists in your Mapbox account, this function will overwrite the existing source with a new source.
#' @param username Your Mapbox username
#' @param access_token Your Mapbox access token with secret scope. Install with \code{mb_access_token()} after you retrieve it from your Mapbox account.
#'
#' @return A list of the MTS API's responses, including the name of the tileset source in your Mapbox account.  You'll use this name to build a MTS recipe.
#' @export
mts_create_source <- function(data, tileset_id, username,
                              access_token = NULL) {

  # Grab & validate the access token
  # Secret scope is required
  access_token <- get_mb_access_token(access_token, secret_required = TRUE)

  # Make a temporary directory to store the newline-delimited GeoJSON
  tmp <- tempdir()

  # Write the dataset to the temporary directory
  output_name <- paste0(deparse(substitute(data)), ".geojson.ld")

  out_file <- file.path(tmp, output_name)

  rlang::inform("Processing data, please wait...")

  make_newline_geojson(data, out_file)

  # Once the object is created, send it to the MTS API
  url <- sprintf("https://api.mapbox.com/tilesets/v1/sources/%s/%s",
                 username, tileset_id)

  request <- httr::POST(url = url,
                        body = list(file = httr::upload_file(out_file)),
                        query = list(access_token = access_token),
                        httr::add_headers("Content-Type: multipart/form-data"),
                        httr::progress(type = "up"))

  # Capture the response from req and give back
  response <- request %>%
    httr::content(as = "text") %>%
    jsonlite::fromJSON()

  if (request$status_code != "200") {
    stop(sprintf("Attempt failed: your error message is %s", response),
         call. = FALSE
    )
  }

  rlang::inform(c("v" = sprintf("Successfully created tileset source with the ID\n'%s'.",
                              response$id),
                "Use your source ID to build your tileset's recipe."))

  # Return the response as a list
  return(response)

}


#' List tileset sources in your Mapbox account
#'
#' @param username Your Mapbox username
#' @param access_token Your Mapbox access token with secret scope.
#'
#' @return A data frame containing information on your tileset sources.
#' @export
mts_list_sources <- function(username,
                             access_token = NULL) {

  access_token <- get_mb_access_token(access_token, secret_required = TRUE)



  request <- httr::GET(
    url = sprintf("https://api.mapbox.com/tilesets/v1/sources/%s", username),
    query = list(access_token = access_token)
  )

  response <- request %>%
    httr::content(as = "text") %>%
    jsonlite::fromJSON()

  if (request$status_code != "200") {
    stop(sprintf("Request failed: your error message is %s", response),
         call. = FALSE
    )
  }

  return(response)

}


#' Make a recipe list for use with the Mapbox Tiling Service
#'
#' @param ... One or more named lists that represent layers in the Mapbox Tiling Service recipe specification (\url{https://docs.mapbox.com/mapbox-tiling-service/reference/#layer-example}).  These lists can be prepared with the helper function \code{recipe_layer()}, or prepared by hand if the user prefers. If multiple layers are included, a multi-layer recipe will be prepared that can produce tilesets with multiple sources.
#'
#' @return An R list representing an MTS recipe to be used to create a tileset.
#' @export
mts_make_recipe <- function(...) {

  recipe <- list(
      version = 1,
      layers = list(...)
    )

  return(recipe)

}


#' Build a recipe layer for use in an MTS recipe
#'
#' @param source The tileset source ID. This is returned by \code{mts_create_source()} or can be retrieved from your Mapbox account with \code{mts_list_sources()}.
#' @param minzoom The minimum zoom level at which a layer can be viewed.
#' @param maxzoom The maximum zoom level at which a layer is rendered; the layer will still be visible past the maximum zoom level due to overzooming.
#' @param features A list of feature options, possibly generated with \code{feature_options()}.
#' @param tiles A list of tile options, possibly generated with \code{tile_options()}
#'
#' @describeIn mts_make_recipe
#' @return A recipe layer list to be used in \code{mts_make_recipe()}.
#' @export
recipe_layer <- function(
    source,
    minzoom,
    maxzoom,
    features = feature_options(),
    tiles = tile_options()
  ) {

  output <- list(source = source,
                 minzoom = minzoom,
                 maxzoom = maxzoom,
                 features = features,
                 tiles = tiles)

  if (length(features) == 0) {
    features <- NULL
  }

  if (length(tiles) == 0) {
    tiles <- NULL
  }

  return(output)

}

#' Specify feature options for an MTS recipe layer
#'
#' @param id A column representing the feature ID. See \url{https://docs.mapbox.com/mapbox-tiling-service/reference/#id-expression}.
#' @param bbox A bounding box within which rendered features will be clipped. See \url{https://docs.mapbox.com/mapbox-tiling-service/reference/#bounding-box}.
#' @param attributes A named list of attribute transformations. \code{zoom_element} specifies how an attribute should be made available at different zoom levels; \code{set} allows you to calculate new attributes from existing attributes when processing the tiles; and \code{allowed_output} specifies which columns should be carried through to the output tiles.  See \url{https://docs.mapbox.com/mapbox-tiling-service/reference/#feature-attributes}.
#' @param filter An expression that determines how features in the tileset should be filtered. See \url{https://docs.mapbox.com/mapbox-tiling-service/reference/#feature-filters} for information on how to specify the filter.
#' @param simplification Rules for feature simplification.  See \url{https://docs.mapbox.com/mapbox-tiling-service/reference/#feature-simplification} for more information on how to specify this.
#'
#' @return A list of feature options, likely to be used in \code{recipe_layer()}.
#' @describeIn mts_make_recipe
#' @export
feature_options <- function(
    id = NULL,
    bbox = NULL,
    attributes = list(
      zoom_element = NULL,
      set = NULL,
      allowed_output = NULL
    ),
    filter = NULL,
    simplification = NULL
  ) {

  output <- leaflet::filterNULL(list(
    id = id,
    bbox = bbox,
    attributes = leaflet::filterNULL(attributes),
    filter = filter,
    simplification = simplification
  ))

  if (length(output$attributes) == 0) {
    output$attributes <- NULL
  }

  return(output)

}

#' Specify tile options for an MTS recipe layer
#'
#' @param bbox,extent,buffer_size,limit,union,filter,attributes,order,remove_filled,id,layer_size Tile options in the MTS recipe. See \url{https://docs.mapbox.com/mapbox-tiling-service/reference/#tile-configuration} for more information on the available options.
#' @return A list of tile options, likely to be used in \code{recipe_layer}.
#' @describeIn mts_make_recipe
#' @export
tile_options <- function(
    bbox = NULL,
    extent = NULL,
    buffer_size = NULL,
    limit = NULL,
    union = list(
      where = NULL,
      group_by = NULL,
      aggregate = NULL,
      maintain_direction = NULL,
      simplification = NULL
    ),
    filter = NULL,
    attributes = NULL,
    order = NULL,
    remove_filled = NULL,
    id = NULL,
    layer_size = NULL
  ) {

  output <- leaflet::filterNULL(list(
    bbox = bbox,
    extent = extent,
    buffer_size = buffer_size,
    limit = limit,
    union = leaflet::filterNULL(union),
    filter = filter,
    attributes = attributes,
    order = order,
    remove_filled = remove_filled,
    id = id,
    layer_size = layer_size
  )
  )

  if (length(output$union) == 0) {
    output$union <- NULL
  } else {
    output$union <- list(output$union)
  }

  return(output)
}



#' Validate a Mapbox Tiling Service recipe
#'
#' @param recipe A recipe list, created with \code{mts_make_recipe()}
#' @param access_token Your Mapbox access token.
#'
#' @return A response from the API indicating whether the MTS recipe is valid or not. If the recipe is invalid, the API response will tell you the reason why.
#' @export
mts_validate_recipe <- function(recipe,
                                access_token = NULL) {

  access_token <- get_mb_access_token(access_token)

  request <- httr::PUT(
    url = "https://api.mapbox.com/tilesets/v1/validateRecipe",
    body = recipe,
    encode = "json",
    query = list(access_token = access_token),
    httr::content_type_json()
  )

  response <- request %>%
    httr::content(as = "text") %>%
    jsonlite::fromJSON()

  if (request$status_code != "200") {
    stop(sprintf("Request failed; the response from the API is %s", response))

  } else {
    if (!response$valid) {
      rlang::inform(c("x" = "Your tileset recipe is invalid. Please view the returned object for error messages."))
      return(response)
    } else {
      rlang::inform(c("v" = "Your tileset recipe is valid!"))
      return(response$valid)
    }
  }


}


#' Create a tileset with the Mapbox Tiling Service API
#'
#' @param tileset_name The name of the MTS tileset you intend to create
#' @param username Your Mapbox username
#' @param recipe An MTS recipe, created with \code{mts_make_recipe()}
#' @param request_name The name of the request; defaults to the tileset name
#' @param access_token Your Mapbox access token
#'
#' @return The API response
#' @export
mts_create_tileset <- function(tileset_name,
                               username,
                               recipe,
                               request_name = tileset_name,
                               access_token = NULL) {

  access_token <- get_mb_access_token(access_token, secret_required = TRUE)

  tileset <- sprintf("%s.%s", username, tileset_name)

  url <- sprintf("https://api.mapbox.com/tilesets/v1/%s", tileset)

  # Prepare the request body
  req_body <- list(
    recipe = recipe,
    name = request_name
  )

  request <- httr::POST(
    url = url,
    body = req_body,
    encode = "json",
    query = list(access_token = access_token),
    httr::content_type_json()
  )

  response <- request %>%
    httr::content(as = "text") %>%
    jsonlite::fromJSON()

  if (request$status_code != "200") {
    stop(sprintf("Tileset creation failed: your error message is %s", response),
         call. = FALSE
    )
  }

  return(response)

}


#' Publish a tileset with Mapbox Tiling Service
#'
#' @param tileset_name The name of the tileset (as supplied to \code{mts_create_tileset()})
#' @param username Your Mapbox username
#' @param access_token Your Mapbox access token
#'
#' @return The API response
#' @export
mts_publish_tileset <- function(tileset_name,
                                username,
                                access_token = NULL) {

  access_token <- get_mb_access_token(access_token, secret_required = TRUE)

  tileset <- sprintf("%s.%s", username, tileset_name)

  url <- sprintf("https://api.mapbox.com/tilesets/v1/%s/publish",
                 tileset)

  request <- httr::POST(
    url = url,
    query = list(access_token = access_token)
  )

  response <- request %>%
    httr::content(as = "text") %>%
    jsonlite::fromJSON()

  if (request$status_code != "200") {
    stop(sprintf("Request failed: your error message is %s", response),
         call. = FALSE
    )
  }

  return(response)

}


#' Retrieve the recipe for an MTS tileset in your Mapbox account
#'
#' @param tileset_name The tileset name
#' @param username Your Mapbox username
#' @param access_token Your Mapbox access token with secret scope
#'
#' @return The recipe for your tileset as an R list
#' @export
mts_get_recipe <- function(tileset_name,
                           username,
                           access_token = NULL) {

  access_token <- get_mb_access_token(access_token, secret_required = TRUE)

  tileset <- sprintf("%s.%s", username, tileset_name)

  url <- sprintf("https://api.mapbox.com/tilesets/v1/%s/recipe",
                 tileset)

  request <- httr::GET(
    url = url,
    query = list(access_token = access_token)
  )

  response <- request %>%
    httr::content(as = "text") %>%
    jsonlite::fromJSON()

  if (request$status_code != "200") {
    stop(sprintf("Request failed: your error message is %s", response),
         call. = FALSE
    )
  }

  return(response$recipe)

}


#' Update a tileset's MTS recipe
#'
#' @param tileset_name The name of your Mapbox tileset
#' @param username Your Mapbox username
#' @param recipe The new recipe for your tileset, likely created with \code{mts_make_recipe()}.
#' @param access_token Your Mapbox access token
#'
#' @return If the update is successful, the function will print a message informing you of its success.  Otherwise, a list of responses from the API will be returned letting you know why the request was invalid.
#' @export
mts_update_recipe <- function(tileset_name,
                              username,
                              recipe,
                              access_token = NULL) {

  access_token <- get_mb_access_token(access_token, secret_required = TRUE)

  tileset <- sprintf("%s.%s", username, tileset_name)

  url <- sprintf("https://api.mapbox.com/tilesets/v1/%s/recipe", tileset)

  request <- httr::PATCH(
    url = url,
    body = recipe,
    encode = "json",
    query = list(access_token = access_token),
    httr::content_type_json()
  )

  if (request$status_code != "204") {

    response <- request %>%
      httr::content(as = "text") %>%
      jsonlite::fromJSON()


    stop(sprintf("Tileset creation failed: your error message is %s", response),
         call. = FALSE
    )
  } else {
    rlang::inform(c("v" = sprintf("Recipe for tileset %s.%s successfully updated.", username, tileset_name)))
  }

}

