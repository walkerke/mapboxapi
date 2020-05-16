#' Install a Mapbox access token in your .Renviron for repeated use
#'
#' @param token The Mapbox access token; can be public (starting with 'pk') or secret (starting with 'sk') scope, which the function will interpret for you
#' @param overwrite Whether or not to overwrite an existing Mapbox access token.  Defaults to FALSE.
#' @param install if TRUE, will install the key in your \code{.Renviron} file for use in future sessions.  Defaults to FALSE.
#'
#' @export
#' @examples \dontrun{
#' my_token <- "..." # The token generated from your Mapbox account
#' mb_access_token(my_token, install = TRUE)
#' Sys.getenv("MAPBOX_PUBLIC_TOKEN")
#' }
mb_access_token <- function(token, overwrite = FALSE, install = FALSE){

  if (grepl("^pk", token)) {
    type <- "MAPBOX_PUBLIC_TOKEN"
  } else if (grepl("^sk", token)) {
    type <- "MAPBOX_SECRET_TOKEN"
  } else {
    stop("Your supplied token appears to be invalid. Check your Mapbox account for details.")
  }

  if (install) {
    home <- Sys.getenv("HOME")
    renv <- file.path(home, ".Renviron")
    if(file.exists(renv)){
      # Backup original .Renviron before doing anything else here.
      file.copy(renv, file.path(home, ".Renviron_backup"))
    }
    if(!file.exists(renv)){
      file.create(renv)
    }
    else{
      if(isTRUE(overwrite)){
        message("Your original .Renviron will be backed up and stored in your R HOME directory if needed.")
        oldenv=read.table(renv, stringsAsFactors = FALSE)
        newenv <- oldenv[-grep(type, oldenv),]
        write.table(newenv, renv, quote = FALSE, sep = "\n",
                    col.names = FALSE, row.names = FALSE)
      }
      else{
        tv <- readLines(renv)
        if(any(grepl(type,tv))){
          stop(sprintf("A %s already exists. You can overwrite it with the argument overwrite=TRUE", type), call.=FALSE)
        }
      }
    }

    tokenconcat <- paste0(sprintf("%s='", type), token, "'")
    # Append access token to .Renviron file
    write(tokenconcat, renv, sep = "\n", append = TRUE)
    message(sprintf('Your access token has been stored in your .Renviron and can be accessed by Sys.getenv("%s"). \nTo use now, restart R or run `readRenviron("~/.Renviron")`', type))
    return(token)
  } else {
    message("To install your access token for use in future sessions, run this function with `install = TRUE`.")
    Sys.setenv(type = token)
  }

}
