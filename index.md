## mapboxapi

__mapboxapi__ is an R package that interfaces with [Mapbox web services APIs](https://docs.mapbox.com/api/). Its purpose is to help R users incorporate the suite of Mapbox tools into their spatial data science projects. Install from CRAN with the following command: 

```r
install.packages("mapboxapi")
```

To get started, [sign up for a Mapbox account](https://account.mapbox.com/) and generate an access token. Set your public or secret token for use in the package with `mb_access_token()`: 

```r
library(mapboxapi)
mb_access_token("pk.eyas...", install = TRUE)
```

Once you've set your token, you are ready to get started using the package. The following example integrates Mapbox maps, navigation, and search services to plot a five-minute walking isochrone over an interactive Leaflet map using a Mapbox style:  

```r
library(leaflet)

walk_5min <- mb_isochrone("2850 S University Dr, Fort Worth TX 76129",
                          profile = "walking",
                          time = 5)

leaflet(walk_5min) %>%
  addMapboxTiles(style_id = "streets-v11",
                 username = "mapbox") %>%
  addPolygons()

```

<iframe src="img/isochrone.html" width = "100%" height = "500"></iframe>

Read through the following articles to see what you can do with the package: 

* [Mapping with __mapboxapi__](articles/mapping.html)
* [Using navigation services with __mapboxapi__](articles/navigation.html)
* [Creating and uploading Mapbox vector tilesets](articles/creating-tiles.html)
* [Geocoding with Mapbox and mapboxapi](articles/geocoding.html)
* [Dynamic web maps with Mapbox Tiling Service and mapgl](articles/dynamic-maps.html)

If you find this project useful, [consider supporting package development via PayPal](https://www.paypal.me/walkerdata), hiring me to give a workshop on __mapboxapi__ or hiring me to consult on your project. Send me a note at <kyle@walker-data.com> if this interests you!  You can also get updates on package development by signing up for my newsletter: 

<!-- Begin MailChimp Signup Form -->
<link href="//cdn-images.mailchimp.com/embedcode/slim-10_7.css" rel="stylesheet" type="text/css">
<style type="text/css">
#mc_embed_signup{background:#fff; clear:left; font:14px Helvetica,Arial,sans-serif; }
/* Add your own MailChimp form style overrides in your site stylesheet or in this style block.
We recommend moving this block and the preceding CSS link to the HEAD of your HTML file. */
</style>
<div id="mc_embed_signup">
<form action="//github.us15.list-manage.com/subscribe/post?u=1829a68a5eda3d301119fdcd6&amp;id=c4a53d2961" method="post" id="mc-embedded-subscribe-form" name="mc-embedded-subscribe-form" class="validate" target="_blank" novalidate>
<div id="mc_embed_signup_scroll">

<input type="email" value="" name="EMAIL" class="email" id="mce-EMAIL" placeholder="email address" required>
<!-- real people should not fill this in and expect good things - do not remove this or risk form bot signups-->
<div style="position: absolute; left: -5000px;" aria-hidden="true"><input type="text" name="b_1829a68a5eda3d301119fdcd6_c4a53d2961" tabindex="-1" value=""></div>
<div class="clear"><input type="submit" value="Subscribe" name="subscribe" id="mc-embedded-subscribe" class="button"></div>
</div>
</form>
</div>

<!--End mc_embed_signup-->


Please note: Use of Mapbox services through __mapboxapi__ is governed by [Mapbox's Terms of Service](https://www.mapbox.com/legal/tos/) and any account restrictions. 
