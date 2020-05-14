coords_to_tiles <- function(lon, lat, zoom) {

  lat_rad <- lat * pi / 180

  n <- 2.0 ^ zoom

  xtile <- floor((lon + 180.0) / 360.0 * n)

  ytile = floor((1.0 - log(tan(lat_rad) + (1 / cos(lat_rad))) / pi) / 2.0 * n)

  return(c(xtile, ytile))
  #  return(paste(paste("https://a.tile.openstreetmap.org", zoom, xtile, ytile, sep="/"),".png",sep=""))
}

