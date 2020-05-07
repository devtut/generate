---
metaTitle: "Ruby on Rails - Configure Angular with Rails"
description: "Angular with Rails 101"
---

# Configure Angular with Rails



## Angular with Rails 101


### Step 1: Create a new Rails app

```ruby
gem install rails -v 4.1
rails new angular_example

```

### Step 2: Remove Turbolinks

Removing turbolinks requires removing it from the Gemfile.

```ruby
gem 'turbolinks'

```

Remove the `require` from `app/assets/javascripts/application.js`:

```ruby
//= require turbolinks

```

### Step 3: Add AngularJS to the asset pipeline

In order to get Angular to work with the Rails asset pipeline we need to add to the Gemfile:

```ruby
gem 'angular-rails-templates'
gem 'bower-rails'

```

Now run the command

```ruby
bundle install

```

Add `bower` so that we can install the AngularJS dependency:

```ruby
rails g bower_rails:initialize json

```

Add Angular to `bower.json`:

```ruby
{
  "name": "bower-rails generated dependencies",
  
  "dependencies": {

    "angular": "latest",
    "angular-resource": "latest",
    "bourbon": "latest",
    "angular-bootstrap": "latest",
    "angular-ui-router": "latest"
  }
}

```

Now that `bower.json` is setup with the right dependencies, let’s install them:

```ruby
bundle exec rake bower:install

```

### Step 4: Organize the Angular app

Create the following folder structure in `app/assets/javascript/angular-app/`:

```ruby
templates/
modules/
filters/
directives/
models/
services/
controllers/

```

In `app/assets/javascripts/application.js`, add `require` for Angular, the template helper, and the Angular app file structure. Like this:

```ruby
//= require jquery
//= require jquery_ujs

//= require angular
//= require angular-rails-templates
//= require angular-app/app

//= require_tree ./angular-app/templates
//= require_tree ./angular-app/modules
//= require_tree ./angular-app/filters
//= require_tree ./angular-app/directives
//= require_tree ./angular-app/models
//= require_tree ./angular-app/services
//= require_tree ./angular-app/controllers

```

### Step 5: Bootstrap the Angular app

Create `app/assets/javascripts/angular-app/app.js.coffee`:

```ruby
@app = angular.module('app', [ 'templates' ])

@app.config([ '$httpProvider', ($httpProvider)->
$httpProvider.defaults.headers.common['X-CSRF-Token'] = $('meta[name=csrftoken]').attr('content') ])  @app.run(->   console.log 'angular app running' )

```

Create an Angular module at `app/assets/javascripts/angular-app/modules/example.js.coffee.erb`:

```ruby
@exampleApp = angular.module('app.exampleApp', [     # additional dependencies here   ])   .run(->     console.log 'exampleApp running'   )

```

Create an Angular controller for this app at `app/assets/javascripts/angular-app/controllers/exampleCtrl.js.coffee`:

```ruby
angular.module('app.exampleApp').controller("ExampleCtrl", [   '$scope',   ($scope)->     console.log 'ExampleCtrl running'      $scope.exampleValue = "Hello angular and rails"  ])

```

Now add a route to Rails to pass control over to Angular. In`config/routes.rb`:

```ruby
Rails.application.routes.draw do   get 'example' => 'example#index' end

```

Generate the Rails controller to respond to that route:

```ruby
rails g controller Example

```

In `app/controllers/example_controller.rb`:

```ruby
class ExampleController < ApplicationController
    def index
    end
end

```

In the view, we need to specify which Angular app and which Angular controller will drive this page. So in `app/views/example/index.html.erb`:

```ruby
<div ng-app='app.exampleApp' ng-controller='ExampleCtrl'>
  
  <p>Value from ExampleCtrl:</p>
  <p>{{ exampleValue }}</p>
  
</div>

```

To view the app, start your Rails server and visit [http://localhost:3000/example](http://localhost:3000/example).

