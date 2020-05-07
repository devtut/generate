---
metaTitle: "Ruby on Rails - ActionCable"
description: "User Authentication, [Basic] Server Side, [Basic] Client Side (Coffeescript)"
---

# ActionCable



## User Authentication


```ruby
# app/channels/application_cable/connection.rb
module ApplicationCable
  class Connection < ActionCable::Connection::Base
    identified_by :current_user

    def connect
      self.current_user = find_verified_user
      logger.add_tags 'ActionCable', current_user.id 
      # Can replace current_user.id with usernames, ids, emails etc.
    end

    protected

    def find_verified_user
      if verified_user = env['warden'].user
        verified_user
      else
        reject_unauthorized_connection
      end
    end
  end
end

```



## [Basic] Server Side


```ruby
# app/channels/appearance_channel.rb
class NotificationsChannel < ApplicationCable::Channel
  def subscribed
    stream_from "notifications"
  end

  def unsubscribed
  end

  def notify(data)
    ActionCable.server.broadcast "notifications", { title: 'New things!', body: data }
  end
end

```



## [Basic] Client Side (Coffeescript)


### app/assets/javascripts/channels/notifications.coffee

```ruby
App.notifications = App.cable.subscriptions.create "NotificationsChannel",
  connected: -> 
    # Called when the subscription is ready for use on the server
    $(document).on "change", "input", (e)=>
      @notify(e.target.value)

  disconnected: ->
    # Called when the subscription has been terminated by the server
     $(document).off "change", "input"

  received: (data) ->
    # Called when there's incoming data on the websocket for this channel
    $('body').append(data)

  notify: (data)->
    @perform('notify', data: data)

```

### app/assets/javascripts/application.js # usually generated like this

```ruby
//= require jquery
//= require jquery_ujs
//= require turbolinks
//= require_tree .

```

### app/assets/javascripts/cable.js # usually generated like this

```ruby
//= require action_cable
//= require_self
//= require_tree ./channels

(function() {
  this.App || (this.App = {});

  App.cable = ActionCable.createConsumer();

}).call(this);

```



#### Remarks


[ActionCable](https://github.com/rails/rails/tree/master/actioncable) was available for Rails 4.x, and was bundled into Rails 5. It allows easy use of websockets for realtime communication between server and client.

