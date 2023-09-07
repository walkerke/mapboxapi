.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Usage of the Mapbox APIs is governed by the Mapbox Terms of Service.\nPlease visit https://www.mapbox.com/legal/tos/ for more information.")
}

utils::globalVariables(c(
  ".", "contour", "lat", "layer_area", "lon", "name",
  "roundabout_exits", "waypoint_index", "x", "y"
))
