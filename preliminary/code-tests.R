library(httr)
library(tidyverse)
library(jsonlite)
library(curl)

req <- GET("https://api.mapbox.com/directions-matrix/v1/mapbox/driving/-122.42,37.78;-122.45,37.91;-122.48,37.73?access_token=pk.eyJ1Ijoia3dsaW5uYWVhbiIsImEiOiJjamZpOXZ3M2QwNDFzMnFxb3RtODU1bGZ6In0.qHG_4ONQDZPYBhZu3WeSAw")
re
cont <- content(req, as = "text") %>%
  fromJSON(flatten = TRUE)

address <- "3116 Odessa Ave Fort Worth TX 76109"

curl_escape(address)

geo <- GET("https://api.mapbox.com/geocoding/v5/mapbox.places/3116%20Odessa%20Ave%20Fort%20Worth%20TX%2076109.json?access_token=pk.eyJ1Ijoia3dsaW5uYWVhbiIsImEiOiJjamZpOXZ3M2QwNDFzMnFxb3RtODU1bGZ6In0.qHG_4ONQDZPYBhZu3WeSAw")

geo <- GET("https://api.mapbox.com/geocoding/v5/mapbox.places/3116 Odessa Ave Fort Worth TX 76109.json?access_token=pk.eyJ1Ijoia3dsaW5uYWVhbiIsImEiOiJjamZpOXZ3M2QwNDFzMnFxb3RtODU1bGZ6In0.qHG_4ONQDZPYBhZu3WeSAw")


geocont <- content(geo, as = "text") %>%
  fromJSON(flatten = TRUE)

address <- read_sf(geocont)


# Testing isochrones

library(V8)

ct <- v8()
ct$source("inst/app.js")


ct$eval('

  var x = isochrone([-121.4738,38.6194], {"token":"pk.eyJ1Ijoia3dsaW5uYWVhbiIsImEiOiJjamZpOXZ3M2QwNDFzMnFxb3RtODU1bGZ6In0.qHG_4ONQDZPYBhZu3WeSAw", "threshold":1800}, function(err, output){
		if (err) throw err;
		console.log(output);
	})


')
