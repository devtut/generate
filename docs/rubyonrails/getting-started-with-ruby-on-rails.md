---
metaTitle: "Ruby on Rails - Getting started with Ruby on Rails"
description: "Creating a Ruby on Rails Application, Create a new Rails app with your choice of database and including the RSpec Testing Tool, Generating A Controller, Installing Rails, Create a new Rails app with a non-standard database adapter, Creating Rails APIs in JSON, Generate a Resource with Scaffolds"
---

# Getting started with Ruby on Rails



## Creating a Ruby on Rails Application


This example assumes **Ruby** and **Ruby on Rails** have already been installed properly.
If not, you can find how to do it **[here](http://stackoverflow.com/documentation/ruby-on-rails/225/getting-started-with-ruby-on-rails/10888/installing-rails#t=201704140137472306273)**.

Open up a command line or terminal. To generate a new rails application, use [rails new](http://guides.rubyonrails.org/command_line.html#rails-new) command followed by the name of your application:

```ruby
$ rails new my_app

```

If you want to create your Rails application with a specific Rails version then you can specify it at the time of generating the application. To do that, use `rails _version_ new` followed by the application name:

```ruby
$ rails _4.2.0_ new my_app 

```

This will create a Rails application called `MyApp` in a `my_app` directory and install the gem dependencies that are already mentioned in `Gemfile` using `bundle install`.

To switch to your newly created app's directory, use the `cd` command, which stands for `change directory`.

```ruby
$ cd my_app

```

The `my_app` directory has a number of auto-generated files and folders that make up the structure of a Rails application. Following is a list of files and folders that are created by default:

|File/Folder|Purpose
|---|---|---|---|---|---|---|---|---|---
|app/|Contains the controllers, models, views, helpers, mailers and assets for your application.
|bin/|Contains the rails script that starts your app and can contain other scripts you use to setup, update, deploy or run your application.
|config/|Configure your application's routes, database, and more.
|config.ru|Rack configuration for Rack based servers used to start the application.
|db/|Contains your current database schema, as well as the database migrations.
|Gemfile Gemfile.lock|These files allow you to specify what gem dependencies are needed for your Rails application. These files are used by the Bundler gem.
|lib/|Extended modules for your application.
|log/|Application log files.
|public/|The only folder seen by the world as-is. Contains static files and compiled assets.
|Rakefile|This file locates and loads tasks that can be run from the command line. The task definitions are defined throughout the components of Rails.
|README.md|This is a brief instruction manual for your application. You should edit this file to tell others what your application does, how to set it up etc
|test/|Unit tests, fixtures, and other test apparatus.
|temp/|Temporary files (like cache and pid files).
|vendor/|A place for all third-party code. In a typical Rails application this includes vendored gems.

Now you need to create a database from your `database.yml` file:

```ruby
rake db:create
# OR
rails db:create

```

```ruby
rake db:create

```

Now that we've created the database, we need to run migrations to set up the tables:

```ruby
rake db:migrate
# OR
rails db:migrate

```

```ruby
rake db:migrate

```

To start the application, we need to fire up the server:

```ruby
$ rails server
# OR
$ rails s

```

By default, rails will start the application at port 3000. To start the application with different port number, we need to fire up the server like,

```ruby
$ rails s -p 3010

```

If you navigate to [http://localhost:3000](http://localhost:3000) in your browser, you will see a Rails welcome page, showing that your application is now running.

If it throws an error, there may be several possible problems:

- There is a problem with the `config/database.yml`
- You have dependencies in your `Gemfile` that have not been installed.
- You have pending migrations. Run `rails db:migrate`
- In case you move to the previous migration `rails db:rollback`

If that still throws an error, then you should check your `config/database.yml`



## Create a new Rails app with your choice of database and including the RSpec Testing Tool


Rails uses `sqlite3` as the default database, but you can generate a new rails application with a database of your choice. Just add the `-d` option followed by the name of the database.

```ruby
$ rails new MyApp -T -d postgresql

```

This is a (non-exhaustive) list of available database options:

- mysql
- oracle
- postgresql
- sqlite3
- frontbase
- ibm_db
- sqlserver
- jdbcmysql
- jdbcsqlite3
- jdbcpostgresql
- jdbc

The -T command indicate to skip the installation of minitest.
To install an alternative test suite like [RSpec](http://rspec.info/), edit the Gemfile and add

```ruby
group :development, :test do
  gem 'rspec-rails', 
end

```

Then launch the following command from the console:

```ruby
rails generate rspec:install

```



## Generating A Controller


To generate a controller (for example `Posts`), navigate to your project directory from a command line or terminal, and run:

```ruby
$ rails generate controller Posts

```

You can shorten this code by replacing `generate` with `g`, for example:

```ruby
$ rails g controller Posts

```

If you open up the newly generated app/controllers/**posts_controller.rb** you'll see a controller with no actions:

```ruby
class PostsController < ApplicationController
    # empty
end

```

It's possible to create default methods for the controller by passing in controller name arguments.

```ruby
$ rails g controller ControllerName method1 method2

```

To create a controller within a module, specify the controller name as a path like `parent_module/controller_name`. For example:

```ruby
$ rails generate controller CreditCards open debit credit close
# OR
$ rails g controller CreditCards open debit credit close

```

This will generate the following files:

```ruby
Controller: app/controllers/credit_cards_controller.rb
Test:       test/controllers/credit_cards_controller_test.rb
Views:      app/views/credit_cards/debit.html.erb [...etc]
Helper:     app/helpers/credit_cards_helper.rb

```

A controller is simply a class that is defined to inherit from `ApplicationController`.

It's inside this class that you'll define methods that will become the actions for this controller.



## Installing Rails


**Installing Rails on Ubuntu**

On a clean ubuntu, installation of Rails should be straight forward

Upgrading ubuntu packages

```ruby
sudo apt-get update
sudo apt-get upgrade

```

Install Ruby and Rails dependecies

```ruby
sudo apt-get install git-core curl zlib1g-dev build-essential libssl-dev libreadline-dev libyaml-dev libsqlite3-dev sqlite3 libxml2-dev libxslt1-dev libcurl4-openssl-dev python-software-properties libffi-dev

```

Installing ruby version manager. In this case the easy one is using rbenv

```ruby
git clone https://github.com/rbenv/rbenv.git ~/.rbenv
echo 'export PATH="$HOME/.rbenv/bin:$PATH"' >> ~/.bashrc
echo 'eval "$(rbenv init -)"' >> ~/.bashrc

```

Installing Ruby Build

```ruby
git clone https://github.com/rbenv/ruby-build.git ~/.rbenv/plugins/ruby-build
echo 'export PATH="$HOME/.rbenv/plugins/ruby-build/bin:$PATH"' >> ~/.bashrc

```

Restart Shell

```ruby
exec $SHELL

```

Install ruby

```ruby
rbenv install 2.3.1
rbenv global 2.3.1
rbenv rehash

```

Installing rails

```ruby
gem install rails

```

**Installing Rails on Windows**

Step 1: ****Installing Ruby****

We need Ruby programming language installed. We can use a precompiled version of Ruby called RubyInstaller.

- Download and run Ruby Installer from [rubyinstaller.org](http://rubyinstaller.org/).
- Run the installer. Check "Add Ruby executables to your PATH", then install.
- To access Ruby, go to the Windows menu, click All Programs, scroll down to Ruby, and click “Start Command Prompt with Ruby”. A command prompt terminal will open. If you type `ruby -v` and press Enter, you should see the Ruby version number that you installed.

Step 2: ****Ruby Development Kit****

After installing Ruby, we can try to install Rails. But some of the libraries Rails depends on need some build tools in order to be compiled, and Windows lacks those tools by default. You can identify this if you see an error while attempting to install Rails `Gem::InstallError: The ‘[gem name]’ native gem requires installed build tools.` To fix this, we need to install the Ruby Development Kit.

- Download the [DevKit](http://rubyinstaller.org/downloads/)
- Run the installer.
- We need to specify a folder where we’re going to permanently install the DevKit. I recommend installing it in the root of your hard drive, at `C:\RubyDevKit`. (Don’t use spaces in the directory name.)

Now we need to make the DevKit tools available to Ruby.

- In your command prompt, change to the DevKit directory. `cd C:\RubyDevKit` or whatever directory you installed it in.
<li>We need to run a Ruby script to initialize the DevKit setup. Type
`ruby dk.rb init`. Now we’ll tell that same script to add the DevKit to our Ruby installation. Type `ruby dk.rb install`.</li>

The DevKit should now be available for your Ruby tools to use when installing new libraries.

Step 3: ****Rails****

Now we can install Rails. Rails comes as a Ruby gem. In your command prompt, type:

`gem install rails`

Once you press Enter, the `gem` program will download and install that version of the Rails gem, along with all the other gems Rails depends on.

Step 4: ****Node.js****

Some libraries that Rails depends on require a JavaScript runtime to be installed. Let’s install Node.js so that those libraries work properly.

- Download the Node.js installer from **[here](https://nodejs.org/en/download/)**.
- When the download completes, visit your downloads folder, and run the `node-v4.4.7.pkg` installer.
- Read the full license agreement, accept the terms, and click Next through the rest of the wizard, leaving everything at the default.
- A window may pop up asking if you want to allow the app to make changes to your computer. Click “Yes”.
- When the installation is complete, you’ll need to restart your computer so Rails can access Node.js.

Once your computer restarts, don’t forget to go to the Windows menu, click “All Programs”, scroll down to Ruby, and click “Start Command Prompt with Ruby”.



## Create a new Rails app with a non-standard database adapter


Rails is shipped by default with `ActiveRecord`, an ORM (Object Relational Mapping) derived from the pattern with the [same name](https://en.wikipedia.org/wiki/Active_record_pattern).

As an ORM, it is built to handle relational-mapping, and more precisely by handling SQL requests for you, hence the limitation to SQL databases only.

However, you can still create a Rails app with another database management system:

1. simply create your app without active-record

```ruby
$ rails app new MyApp --skip-active-record

```


1. add your own database management system in `Gemfile`

```ruby
gem 'mongoid', '~> 5.0'

```


1. `bundle install` and follow the installation steps from the desired database.

In this example, `mongoid` is an object mapping for `MongoDB` and - as many other database gems built for rails - it also inherits from `ActiveModel` the same way as `ActiveRecord`, which provides a common interface for many features such as validations, callbacks, translations, etc.

Other database adapters include, but are not limited to :

<li>
datamapper
</li>
<li>
sequel-rails
</li>



## Creating Rails APIs in JSON


This example assumes that you have experience in creating Rails applications.

To create an API-only app in Rails 5, run

```ruby
rails new name-of-app --api

```

Add active_model_serializers in Gemfile

```ruby
gem 'active_model_serializers'

```

install bundle in terminal

```ruby
bundle install

```

Set the `ActiveModelSerializer` adapter to use `:json_api`

```ruby
# config/initializers/active_model_serializer.rb
ActiveModelSerializers.config.adapter = :json_api
Mime::Type.register "application/json", :json, %w( text/x-json application/jsonrequest application/vnd.api+json )

```

Generate a new scaffold for your resource

```ruby
rails generate scaffold Task name:string description:text

```

This will generate the following files:

Controller: app/controllers/tasks_controller.rb

```ruby
Test:       test/models/task_test.rb
            test/controllers/tasks_controller_test.rb
Routes:     resources :tasks added in routes.rb
Migration:  db/migrate/_create_tasks.rb
Model:      app/models/task.rb
Serializer: app/serializers/task_serializer.rb
Controller: app/controllers/tasks_controller.rb

```



## Generate a Resource with Scaffolds


From guides.rubyonrails.org:

> 
Instead of generating a model directly . . . let's set up a scaffold. A scaffold in Rails is a full set of model, database migration for that model, controller to manipulate it, views to view and manipulate the data, and a test suite for each of the above.


Here's an example of scaffolding a resource called `Task` with a string name and a text description:

```ruby
rails generate scaffold Task name:string description:text

```

This will generate the following files:

```ruby
Controller: app/controllers/tasks_controller.rb
Test:       test/models/task_test.rb
            test/controllers/tasks_controller_test.rb
Routes:     resources :tasks added in routes.rb
Views:      app/views/tasks
            app/views/tasks/index.html.erb
            app/views/tasks/edit.html.erb
            app/views/tasks/show.html.erb
            app/views/tasks/new.html.erb
            app/views/tasks/_form.html.erb
Helper:     app/helpers/tasks_helper.rb
JS:         app/assets/javascripts/tasks.coffee 
CSS:        app/assets/stylesheets/tasks.scss
            app/assets/stylesheets/scaffolds.scss

```

example to delete files generated by scaffold for the resource called `Task`

```ruby
rails destroy scaffold Task

```



#### Remarks


[<img src="https://i.stack.imgur.com/bU21T.png" alt="logo" />](https://i.stack.imgur.com/bU21T.png)

Ruby on Rails (RoR), or Rails, is an open-source popular web application framework.  Rails uses Ruby, HTML, CSS, and JavaScript to create a web application that runs on a web server.  Rails uses the model-view-controller (MVC) pattern and provides a fullstack of libraries from the database all the way to the view.

