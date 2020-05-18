## mapboxapi

An R interface to Mapbox APIs and web services

__Please note: the package API is very much in-progress and subject to change. Use at your own risk...__

The purpose of {mapboxapi} is to facilitate the use of [Mapbox web services](https://docs.mapbox.com/api/) for spatial data science tasks in R.  Current and future versions of the package allow R users to return Mapbox navigation requests as simple features (sf) objects, convert R objects to Mapbox vector tilesets, and query Mapbox tilesets from R, among other tasks.  The package is not a complete wrapper of the API (though new features will continually be added) nor is it an interface to [Mapbox GL JS](https://docs.mapbox.com/mapbox-gl-js/api/), Mapbox's web mapping API.  While {mapboxapi} itself does not have mapping capabilities, it can be used in combination with Mapbox Studio and the {mapdeck} R package to create maps of styled vector tilesets.  

More thorough documentation will be coming.  In the meantime, take a look at the following examples:

### Example 1: analyzing the relationship between 
