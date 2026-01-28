---
metaTitle: "Ruby on Rails - Rails 5"
description: "How to install Ruby on Rails 5 on RVM, Creating a Ruby on Rails 5 API"
---

# Rails 5



## How to install Ruby on Rails 5 on RVM


RVM is a great tool to manage your ruby versions and set up your working environment.

Assuming you already have RVM installed, to get the latest version of ruby, which is needed for these examples, open a terminal and run:

```ruby
$ rvm get stable
$ rvm install ruby --latest

```

Check your ruby version by running:

```ruby
$ ruby -v
> ruby 2.3.0p0

```

To install Rails 5, first create a new gemset using the latest ruby version and then install rails:

```ruby
$ rvm use ruby-2.3.0@my_app --create
$ gem install rails

```

To check your rails version, run:

```ruby
$ rails -v
> Rails 5.0.0

```



## Creating a Ruby on Rails 5 API


To create a new Rails 5 API, open a terminal and run the following command:

```ruby
rails new app_name --api

```

The following file structure will be created:

```

 create  
  create  README.rdoc
  create  Rakefile
  create  config.ru
  create  .gitignore
  create  Gemfile
  create  app
  create  app/assets/javascripts/application.js
  create  app/assets/stylesheets/application.css
  create  app/controllers/application_controller.rb
  create  app/helpers/application_helper.rb
  create  app/views/layouts/application.html.erb
  create  app/assets/images/.keep
  create  app/mailers/.keep
  create  app/models/.keep
  create  app/controllers/concerns/.keep
  create  app/models/concerns/.keep
  create  bin
  create  bin/bundle
  create  bin/rails
  create  bin/rake
  create  bin/setup
  create  config
  create  config/routes.rb
  create  config/application.rb
  create  config/environment.rb
  create  config/secrets.yml
  create  config/environments
  create  config/environments/development.rb
  create  config/environments/production.rb
  create  config/environments/test.rb
  create  config/initializers
  create  config/initializers/assets.rb
  create  config/initializers/backtrace_silencers.rb
  create  config/initializers/cookies_serializer.rb
  create  config/initializers/filter_parameter_logging.rb
  create  config/initializers/inflections.rb
  create  config/initializers/mime_types.rb
  create  config/initializers/session_store.rb
  create  config/initializers/wrap_parameters.rb
  create  config/locales
  create  config/locales/en.yml
  create  config/boot.rb
  create  config/database.yml
  create  db
  create  db/seeds.rb
  create  lib
  create  lib/tasks
  create  lib/tasks/.keep
  create  lib/assets
  create  lib/assets/.keep
  create  log
  create  log/.keep
  create  public
  create  public/404.html
  create  public/422.html
  create  public/500.html
  create  public/favicon.ico
  create  public/robots.txt
  create  test/fixtures
  create  test/fixtures/.keep
  create  test/controllers
  create  test/controllers/.keep
  create  test/mailers
  create  test/mailers/.keep
  create  test/models
  create  test/models/.keep
  create  test/helpers
  create  test/helpers/.keep
  create  test/integration
  create  test/integration/.keep
  create  test/test_helper.rb
  create  tmp/cache
  create  tmp/cache/assets
  create  vendor/assets/javascripts
  create  vendor/assets/javascripts/.keep
  create  vendor/assets/stylesheets
  create  vendor/assets/stylesheets/.keep

```

This file structure will be created inside a new folder called `app_name`. It contains all the assets and code needed to start your project.

Enter the folder and install the dependencies:

```ruby
cd app_name
bundle install

```

You should also start your database. Rails uses SQLite as a default database. To create it, run:

```ruby
rake db:setup

```

Now run your appplication:

```ruby
$ rails server

```

When you open your browser at `http://localhost:3000`, your shiny new (empty) API should be running!

