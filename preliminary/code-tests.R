library(httr)
library(tidyverse)
library(jsonlite)

req <- GET("https://api.mapbox.com/directions-matrix/v1/mapbox/driving/-122.42,37.78;-122.45,37.91;-122.48,37.73?access_token=pk.eyJ1Ijoia3dsaW5uYWVhbiIsImEiOiJjamZpOXZ3M2QwNDFzMnFxb3RtODU1bGZ6In0.qHG_4ONQDZPYBhZu3WeSAw")

cont <- content(req, as = "text") %>%
  fromJSON(flatten = TRUE)
