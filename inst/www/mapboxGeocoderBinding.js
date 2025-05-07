var mapboxGeocoderBinding = new Shiny.InputBinding();
$.extend(mapboxGeocoderBinding, {
  find: function(scope) {
    return $(scope).find(".mapbox-geocoder");
  },
  initialize: function(el) {
    var accessToken = $(el).data("access-token");
    var options = $(el).data("options");
    var width = $(el).data("width") || "100%";
    mapboxgl.accessToken = accessToken;
    
    // Clean undefined values to avoid issues
    var cleanOptions = {};
    for (var key in options) {
      if (options[key] !== null && options[key] !== undefined && key !== "customWidth") {
        cleanOptions[key] = options[key];
      }
    }
    
    var geocoder = new MapboxGeocoder({
      accessToken: mapboxgl.accessToken,
      mapboxgl: mapboxgl,
      ...cleanOptions
    });
    
    geocoder.addTo(el);
    
    // Apply custom width to the geocoder container
    setTimeout(function() {
      var geocoderContainer = $(el).find('.mapboxgl-ctrl-geocoder');
      if (geocoderContainer.length) {
        geocoderContainer.css({
          'min-width': 'unset',
          'width': width
        });
      }
    }, 0);
    
    // Capture the result event and send data to Shiny
    geocoder.on('result', function(e) {
      $(el).data("geocoder-result", e.result); // Store the result in jQuery data
      $(el).trigger('change');
    });
    
    // Add event listener for the 'clear' event
    geocoder.on('clear', function() {
      $(el).data("geocoder-result", null); // Reset the result to null
      $(el).trigger('change');
    });
    
    $(el).data("geocoder", geocoder);
  },
  getValue: function(el) {
    return $(el).data("geocoder-result"); // Send this data to Shiny
  },
  subscribe: function(el, callback) {
    $(el).on("change", function(e) {
      callback();
    });
  },
  unsubscribe: function(el) {
    $(el).off(".mapboxGeocoderBinding");
  }
});
Shiny.inputBindings.register(mapboxGeocoderBinding);
