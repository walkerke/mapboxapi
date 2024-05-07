var mapboxGeocoderBinding = new Shiny.InputBinding();

$.extend(mapboxGeocoderBinding, {
  find: function(scope) {
    return $(scope).find(".mapbox-geocoder");
  },
  initialize: function(el) {
    var accessToken = $(el).data("access-token");
    var options = $(el).data("options");
    mapboxgl.accessToken = accessToken;

    var geocoder = new MapboxGeocoder({
      accessToken: mapboxgl.accessToken,
      mapboxgl: mapboxgl,
      ...options
    });

    geocoder.addTo(el);

    // Capture the result event and send data to Shiny
    geocoder.on('result', function(e) {
      $(el).data("geocoder-result", e.result); // Store the result in jQuery data
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
