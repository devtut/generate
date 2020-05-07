---
metaTitle: "Ruby on Rails - React with Rails using react-rails gem"
description: "React installation for Rails using rails_react gem, Using react_rails within your application, Rendering & mounting"
---

# React with Rails using react-rails gem



## React installation for Rails using rails_react gem


Add react-rails to your Gemfile:

```ruby
gem 'react-rails'

```

And install:

```ruby
bundle install

```

Next, run the installation script:

```ruby
rails g react:install

```

This will:

create a components.js manifest file and a app/assets/javascripts/components/ directory, where you will put your components
place the following in your application.js:

```ruby
//= require react
//= require react_ujs
//= require components

```



## Using react_rails within your application


**React.js builds**

You can pick which React.js build (development, production, with or without add-ons) to serve in each environment by adding a config. Here are the defaults:

```ruby
# config/environments/development.rb
MyApp::Application.configure do
  config.react.variant = :development
end


# config/environments/production.rb
MyApp::Application.configure do
  config.react.variant = :production
end

```

To include add-ons, use this config:

```ruby
MyApp::Application.configure do
  config.react.addons = true # defaults to false
end

```

After restarting your Rails server, //= require react will provide the build of React.js which was specified by the configurations.

react-rails offers a few other options for versions & builds of React.js. See VERSIONS.md for more info about using the react-source gem or dropping in your own copies of React.js.

JSX

After installing react-rails, restart your server. Now, .js.jsx files will be transformed in the asset pipeline.

BabelTransformer options

You can use babel's transformers and custom plugins, and pass options to the babel transpiler adding following configurations:

```ruby
config.react.jsx_transform_options = {
  blacklist: ['spec.functionName', 'validation.react', 'strict'], # default options
  optional: ["transformerName"],  # pass extra babel options
  whitelist: ["useStrict"] # even more options[enter link description here][1]
}

```

Under the hood, react-rails uses [ruby-babel-transpiler](https://github.com/babel/ruby-babel-transpiler), for transformation.



## Rendering & mounting


`react-rails` includes a view helper `(react_component)` and an unobtrusive JavaScript driver (react_ujs) which work together to put React components on the page. You should require the UJS driver in your manifest after react (and after turbolinks if you use Turbolinks).

The view helper puts a div on the page with the requested component class & props. For example:

```ruby
<%= react_component('HelloMessage', name: 'John') %>
<!-- becomes: -->
<div data-react-class="HelloMessage" data-react-props="{&quot;name&quot;:&quot;John&quot;}"></div>

```

On page load, the react_ujs driver will scan the page and mount components using data-react-class and data-react-props.

If Turbolinks is present components are mounted on the page:change event, and unmounted on page:before-unload. Turbolinks >= 2.4.0 is recommended because it exposes better events.

In case of Ajax calls, the UJS mounting can be triggered manually by calling from javascript:

ReactRailsUJS.mountComponents()
The view helper's signature is:

```ruby
react_component(component_class_name, props={}, html_options={})

```

`component_class_name` is a string which names a globally-accessible    component class. It may have dots (eg, "MyApp.Header.MenuItem").

```

  `props` is either an object that responds to `#to_json` or an    already-stringified JSON object (eg, made with Jbuilder, see note    below).

```

`html_options` may include:    `tag:` to use an element other than a div to embed data-react-class and data-react-props.    `prerender: true` to render the component on the server.
`**other` Any other arguments (eg class:, id:) are passed through to content_tag.

