#' Install a Mapbox access token in your .Renviron for repeated use
#'
#' @param token The Mapbox access token
#' @param overwrite Whether or not to overwrite an existing Mapbox access token.  Defaults to FALSE.
#' @param install if TRUE, will install the key in your \code{.Renviron} file for use in future sessions.  Defaults to FALSE.
#'
#' @export
#'
#' @examples \dontrun{
#' my_token <- "..." # The token generated from your Mapbox account
#' mb_access_token(my_token, install = TRUE)
#' Sys.getenv("MAPBOX_ACCESS_TOKEN")
#' }
mb_access_token <- function(token, overwrite = FALSE, install = FALSE){

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
        newenv <- oldenv[-grep("MAPBOX_ACCESS_TOKEN", oldenv),]
        write.table(newenv, renv, quote = FALSE, sep = "\n",
                    col.names = FALSE, row.names = FALSE)
      }
      else{
        tv <- readLines(renv)
        if(any(grepl("MAPBOX_ACCESS_TOKEN",tv))){
          stop("A MAPBOX_ACCESS_TOKEN already exists. You can overwrite it with the argument overwrite=TRUE", call.=FALSE)
        }
      }
    }

    tokenconcat <- paste0("MAPBOX_ACCESS_TOKEN='", token, "'")
    # Append access token to .Renviron file
    write(tokenconcat, renv, sep = "\n", append = TRUE)
    message('Your access token has been stored in your .Renviron and can be accessed by Sys.getenv("MAPBOX_ACCESS_TOKEN"). \nTo use now, restart R or run `readRenviron("~/.Renviron")`')
    return(token)
  } else {
    message("To install your access token for use in future sessions, run this function with `install = TRUE`.")
    Sys.setenv(MAPBOX_ACCESS_TOKEN = token)
  }

}
