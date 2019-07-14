#' Upload dataset to your Mapbox account
#'
#' @param input The path to the dataset to upload
#' @param username Your Mapbox username
#' @param access_token Your Mapbox password
#' @param tileset_name The name of the tileset
#'
#' @export
upload_tiles <- function(input,
                         username,
                         access_token = NULL,
                         tileset_name = NULL) {

  if (is.null(access_token)) {

    if (Sys.getenv("MAPBOX_ACCESS_TOKEN") != "") {
      access_token <- Sys.getenv("MAPBOX_ACCESS_TOKEN")
    } else {
      stop("A Mapbox secret access token is required.  Please locate yours from your Mapbox account.",
           call. = FALSE)
    }
  }
  # If input is an R object, write to a tempfile
  # temp <- tempdir()
  #
  # file_loc <- file.path(temp, input)

  dataset <- input

  # Get AWS credentials
  base1 <- sprintf("https://api.mapbox.com/uploads/v1/%s/credentials",
                   username)

  req <- POST(base1, query = list(access_token = token))

  credentials <- content(req, as = "text") %>% fromJSON()

  # Use these credentials to transfer to the staging bucket
  put_object(dataset,
             object = credentials$key,
             bucket = credentials$bucket,
             region = "us-east-1",
             key = credentials$accessKeyId,
             secret = credentials$secretAccessKey,
             session_token = credentials$sessionToken,
             check_region = FALSE)


  # Once done, generate the upload
  url <- sprintf("http://%s.s3.amazonaws.com/%s",
                 credentials$bucket,
                 credentials$key)

  upload <- sprintf('{"url": "%s", "tileset": "%s.%s"}',
                    url, username, tileset_name)

  base2 <- sprintf('https://api.mapbox.com/uploads/v1/%s',
                   username)

  test <- POST(base2,
               add_headers("Content-Type" = "application/json",
                           "Cache-Control" = "no-cache"),
               body = upload,
               query = list(access_token = token))

  t2 <- test %>%
    content(as = "text") %>%
    fromJSON()

  message(sprintf("Your upload ID is %s", t2$id))

}


#' Check the status of a Mapbox upload
#'
#' @param username Your account's username
#' @param id The upload ID
#' @param access_token Your Mapbox access token
#'
#' @export
check_upload_status <- function(username,
                                id,
                                access_token = NULL) {

  if (is.null(access_token)) {

    if (Sys.getenv("MAPBOX_ACCESS_TOKEN") != "") {
      access_token <- Sys.getenv("MAPBOX_ACCESS_TOKEN")
    } else {
      stop("A Mapbox secret access token is required.  Please locate yours from your Mapbox account.",
           call. = FALSE)
    }
  }

  status <- GET(sprintf("https://api.mapbox.com/uploads/v1/%s/%s",
                        username, id),
                query = list(access_token = access_token))

  status %>%
    content(as = "text") %>%
    fromJSON()

}


