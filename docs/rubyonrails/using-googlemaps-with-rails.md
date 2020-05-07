---
metaTitle: "Ruby on Rails - Using GoogleMaps with Rails"
description: "Add the google maps javascript tag to the layout header, Geocode the model, Show addresses on a google map in the profile view, Set the markers on the map with javascript, Initialize the map using a coffee script class., Initialize the map markers using a coffee script class, Auto-zoom a map using a coffee script class, Exposing the model properties as json"
---

# Using GoogleMaps with Rails



## Add the google maps javascript tag to the layout header


In order to have google maps work properly with [turbolinks](https://github.com/turbolinks/turbolinks), add the javascript tag directly to the layout header rather than including it in a view.

```ruby
# app/views/layouts/my_layout.html.haml
!!!
%html{:lang => 'en'}
  %head
    - # ...
    = google_maps_api_script_tag

```

The `google_maps_api_script_tag` is best defined in a helper.

```ruby
# app/helpers/google_maps_helper.rb
module GoogleMapsHelper
  def google_maps_api_script_tag
    javascript_include_tag google_maps_api_source
  end

  def google_maps_api_source
    "https://maps.googleapis.com/maps/api/js?key=#{google_maps_api_key}"
  end

  def google_maps_api_key
    Rails.application.secrets.google_maps_api_key
  end
end

```

You can register your application with google and get your api key in the [google api console](https://console.developers.google.com). Google has a short [guide how to request an api key for the google maps javascript api](https://developers.google.com/maps/documentation/javascript/get-api-key?hl=de).

The api key is stored in the `secrets.yml`file:

```ruby
# config/secrets.yml
development:
  google_maps_api_key: '...'
  # ...
production:
  google_maps_api_key: '...'
  # ...

```

Don't forget to add `config/secrets.yml` to your `.gitignore` file and makre sure you don't commit the api key to the repository.



## Geocode the model


Suppose, your users and/or groups have profiles and you want to display address profile fields on a google map.

```ruby
# app/models/profile_fields/address.rb
class ProfileFields::Address < ProfileFields::Base
  # Attributes: 
  # label, e.g. "Work address"
  # value, e.g. "Willy-Brandt-Straße 1\n10557 Berlin"
end

```

A great way to geocode the addresses, i.e. provide `longitude` and `latitude` is the [geocoder gem](https://github.com/alexreisner/geocoder).

Add geocoder to your `Gemfile` and run `bundle` to install it.

```ruby
# Gemfile
gem 'geocoder', '~> 1.3'

```

Add database columns for `latitude` and `longitude` in order to save the location in the database. This is more efficient than querying the geocoding service every time you need the location. It's faster and you're not hitting the query limit so quickly.

```ruby
➜ bin/rails generate migration add_latitude_and_longitude_to_profile_fields \
    latitude:float longitude:float
➜ bin/rails db:migrate  # Rails 5, or:
➜ rake db:migrate       # Rails 3, 4

```

Add the geocoding mechanism to your model. In this example, the address string is stored in the `value` attribute. Configure the geocoding to perform when the record has changed, and only whan a value is present:

```ruby
# app/models/profile_fields/address.rb
class ProfileFields::Address < ProfileFields::Base
  geocoded_by :value
  after_validation :geocode, if: ->(address_field){ 
    address_field.value.present? and address_field.value_changed? 
  }

end

```

By default, geocoder uses google as lookup service. It has lots of interesting features like distance calculations or proximity search. Fore more information, have a look at the [geocoder README](https://github.com/alexreisner/geocoder).



## Show addresses on a google map in the profile view


On the profile view, show the profile fields of a user or group in a list as well as the address fields on a google map.

```ruby
- # app/views/profiles/show.html.haml
%h1 Contact Information
.profile_fields
  = render @profile_fields
.google_map{data: address_fields: @address_fields.to_json }

```

The appropriate `@profile_fields` and `@address_fields` are set in the controller:

```ruby
# app/controllers/profiles_controller.rb
class ProfilesController < ApplicationController
  def show
    # ...
    @profile_fields = @user_or_group.profile_fields
    @address_fields = @profile_fields.where(type: 'ProfileFields::Address')
  end
end

```

Initialize the map, place the markers, set the zoom and other map settings with javascript.

[<img src="http://i.stack.imgur.com/2n8le.png" alt="Example profile view" />](http://i.stack.imgur.com/2n8le.png)



## Set the markers on the map with javascript


Suppose, there is a `.google_map` div, which will become the map, and which has the address fields to show as markers as `data` attribute.

For example:

```ruby
<!-- http://localhost:3000/profiles/123 -->
<div class="google_map" data-address-fields="[
  {label: 'Work address', value: 'Willy-Brandt-Straße 1\n10557 Berlin', 
  position: {lng: ..., lat: ...}},
  ...
]"></div>

```

To make use of the `$(document).ready` event with [turbolinks](https://github.com/turbolinks/turbolinks) without managing the turbolinks events by hand, use the [jquery.turbolinks gem](https://github.com/kossnocorp/jquery.turbolinks).

If you want to perform some other operations with the map, later, for example filtering or info windows, it's convenient to have the map managed by a [coffee script class](http://coffeescript.org/#classes).

```ruby
# app/assets/javascripts/google_maps.js.coffee
window.App = {} unless App?
class App.GoogleMap
  constructor: (map_div)->
    # TODO: initialize the map
    # TODO: set the markers

```

When using several coffee script files, which are namespaced by default, it's convenient to define a global `App` namespace, which is shared by all coffee script files.

Then, loop through (possibly several) `.google_map` divs and create one instance of the `App.GoogleMap` class for each of them.

```ruby
# app/assets/javascripts/google_maps.js.coffee
# ...
$(document).ready ->
  App.google_maps = []
  $('.google_map').each ->
    map_div = $(this)
    map = new App.GoogleMap map_div
    App.google_maps.push map

```



## Initialize the map using a coffee script class.


Provided an `App.GoogleMap` [coffee script class](http://coffeescript.org/#classes), the google map can be initialized like this:

```ruby
# app/assets/javascripts/google_maps.js.coffee
# ...
class App.GoogleMap
  map_div: {}
  map: {}
  
  constructor: (map_div)->
    @map_div = map_div
    @init_map()
    @reference_the_map_as_data_attribute

  # To access the GoogleMap object or the map object itself
  # later via the DOM, for example 
  # 
  #     $('.google_map').data('GoogleMap')
  #
  # store references as data attribute of the map_div.
  #
  reference_the_map_as_data_attribute: ->
    @map_div.data 'GoogleMap', this
    @map_div.data 'map', @map

  init_map: ->
    @map = new google.maps.Map(@dom_element, @map_configuration) if google?

  # `@map_div` is the jquery object. But google maps needs
  # the real DOM element.
  #
  dom_element: ->
    @map_div.get(0)

  map_configuration: -> {
    scrollWheel: true
  }

```

To learn more about the possible `map_configuration` options, have a look at google's [MapOptions documentation](https://developers.google.com/maps/documentation/javascript/3.exp/reference#MapOptions) and their [guide to adding control elements](https://developers.google.com/maps/documentation/javascript/controls).

For reference, the class [`google.maps.Map`is extensively documented here](https://developers.google.com/maps/documentation/javascript/3.exp/reference#Map).



## Initialize the map markers using a coffee script class


Provided an `App.GoogleMap` [coffee script class](http://coffeescript.org/#classes) and the marker information being stored in the `data-address-fields` attribute of the `.google_map` div, the map markers can be initialized on the map like this:

```ruby
# app/assets/javascripts/google_maps.js.coffee
# ...
class App.GoogleMap
  # ...
  markers: []

  constructor: (map_div)->
    # ...
    @init_markers()

  address_fields: ->
    @map_div.data('address-fields')

  init_markers: ->
    self = this  # to reference the instance as `self` when `this` is redefined.
    self.markers = []
    for address_field in self.address_fields()
      marker = new google.maps.Marker {
        map: self.map,
        position: {
          lng: address_field.longitude,
          lat: address_field.latitude
        },
        # # or, if `position` is defined in `ProfileFields::Address#as_json`:
        # position: address_field.position,
        title: address_field.value
      }
      self.markers.push marker

```

To learn more about marker options, have a look at google's [MarkerOptions documentation](https://developers.google.com/maps/documentation/javascript/3.exp/reference#MarkerOptions) and their [guide to markers](https://developers.google.com/maps/documentation/javascript/markers).



## Auto-zoom a map using a coffee script class


Provided an `App.GoogleMap` [coffee script class](http://coffeescript.org/#classes)  with the `google.maps.Map` stored as `@map` and the `google.maps.Marker`s stored as `@markers`, the map can be auto-zoomed, i.e. adjusted that all markers are visible, like this:
on the map like this:

```ruby
# app/assets/javascripts/google_maps.js.coffee
# ...
class App.GoogleMap
  # ...
  bounds: {}

  constructor: (map_div)->
    # ...
    @auto_zoom()

  auto_zoom: ->
    @init_bounds()
    # TODO: Maybe, adjust the zoom to have a maximum or 
    # minimum zoom level, here.

  init_bounds: ->
    @bounds = new google.maps.LatLngBounds()
    for marker in @markers
      @bounds.extend marker.position
    @map.fitBounds @bounds

```

To learn more about bounds, have a look at google's [LatLngBounds documentation](https://developers.google.com/maps/documentation/javascript/3.exp/reference#LatLngBounds).



## Exposing the model properties as json


To display address profile fields as markers on a google map, the address field objects need to be passed as json objects to javascript.

### Regular database attributes

When calling `to_json` on an [`ApplicationRecord`](http://guides.rubyonrails.org/active_record_basics.html#creating-active-record-models) object, the database attributes are automatically exposed.

Given a `ProfileFields::Address` model with `label`, `value`, `longitude` and `latitude` attributes, `address_field.as_json` results in a `Hash`, e.g. representation,

```ruby
address_field.as_json  # =>
  {label: "Work address", value: "Willy-Brandt-Straße 1\n10557 Berlin",
    longitude: ..., latitude: ...}

```

which is converted to a json string by `to_json`:

```ruby
address_field.to_json  # =>
  "{\"label\":\"Work address\",\"value\":\"Willy-Brandt-Straße 1\\n
    10557 Berlin\",\"longitude\":...,\"latitude\":...}"

```

This is useful because it allows to use `label` and `value` later in javascript, for example to show tool tips for the map markers.

### Other attributes

Other virtual attributes can be exposed by overriding the `as_json` method.

For example, to expose a `title` attribute, include it in the merged `as_json` hash:

```ruby
# app/models/profile_fields/address.rb
class ProfileFields::Address < ProfileFields::Base
  # ...

  # For example: "John Doe, Work address"
  def title
    "#{self.parent.name}, #{self.label}"
  end

  def as_json
    super.merge {
      title: self.title
    }
  end
end

```

The above example uses [`super`](http://rubylearning.com/satishtalim/ruby_overriding_methods.html) to call the original `as_json` method, which returns the original attribute hash of the object, and merges it with the required position hash.

To understand the difference between `as_json` and `to_json`, have a look at [this blog post by jjulian](http://jonathanjulian.com/2010/04/rails-to_json-or-as_json/).

### Position

To render markers, the google maps api, by default, requires a `position` hash which has longitude and latitude stored as `lng` and `lat` respectively.

This position hash can be created in javascript, later, or here when defining the json representation of the address field:

To provide this `position` as json attribute of the address field, just override the `as_json` method on the model.

```ruby
# app/models/profile_fields/address.rb
class ProfileFields::Address < ProfileFields::Base
  # ...

  def as_json
    super.merge {
      # ...
      position: {
        lng: self.longitude,
        lat: self.latitude
      }
    }
  end
end

```

