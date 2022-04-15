skip_if_no_token <- function(frame = caller_env()) {
  testthat::skip_if(identical(get_mb_access_token(), ""))
}
