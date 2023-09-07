.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    paste0(
      c(
        "Usage of the Mapbox APIs is governed by the Mapbox Terms of Service.",
        "Please visit https://www.mapbox.com/legal/tos/ for more information."
      ),
      collapse = "\n"
    )
  )
}

utils::globalVariables(c(
  ".", "contour", "lat", "layer_area", "lon", "name",
  "roundabout_exits", "waypoint_index", "x", "y"
))
