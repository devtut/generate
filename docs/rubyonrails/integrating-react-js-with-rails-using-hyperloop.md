---
metaTitle: "Ruby on Rails - Integrating React.js with Rails Using Hyperloop"
description: "Adding a simple react component (written in ruby) to your Rails app, Callbacks, Declaring component parameters (props), HTML Tags, Event Handlers, States"
---

# Integrating React.js with Rails Using Hyperloop


This topic covers integrating React.js with Rails using the [Hyperloop](http://ruby-hyperloop.io) gem

Other approaches not covered here are using the react-rails or react_on_rails gems.



## Adding a simple react component (written in ruby) to your Rails app


1. Add the hyperloop gem to your rails (4.0 - 5.1) Gemfile
1. `bundle install`
<li>Add the hyperloop manifest to the application.js file:
<pre>// app/assets/javascripts/application.js
...
//= hyperloop-loader
</pre>
</li>
<li>Create your react components, and place them in the `hyperloop/components` directory
<pre># app/hyperloop/components/hello_world.rb
class HelloWorld < Hyperloop::Component
  after_mount do
    every(1.second) { mutate.current_time(Time.now) }
  end
  render do
    "Hello World!  The time is now: #{state.current_time}"
  end
end
</pre>
</li>
<li>Components act just like views.  They are "mounted" using the `render_component` method in a controller:
<pre># somewhere in a controller:
  ...
  def hello_world
    render_component # renders HelloWorld based on method name
  end
</pre>
</li>



## Callbacks




## Declaring component parameters (props)




## HTML Tags




## Event Handlers




## States


Note that states can be shared between components using [Hyperloop::Stores](http://ruby-hyperloop.io/start/stores/)



#### Remarks


Component classes simply generate the equivalent javascript component classes.

You can also access javascript components and libraries directly from your ruby component classes.

Hyperloop will "prerender" the view server side so your initial view will load just like ERB or HAML templates.  Once loaded on the client react takes over and will incrementally update the DOM as state changes due to inputs from the user, HTTP requests or incoming web socket data.

Besides Components, Hyperloop has Stores to manage shared state, Operations to encapsulate isomorphic business logic, and Models which give direct access to your ActiveRecord models on the client using the standard AR syntax.

More info here: [http://ruby-hyperloop.io/](http://ruby-hyperloop.io/)

