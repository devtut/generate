---
metaTitle: "Ruby on Rails - Rails Engine -  Modular Rails"
description: "Create a modular app"
---

# Rails Engine -  Modular Rails


**Quick overview of Rails engines**

Engines are small Rails applications that can be used to add functionalities to the application hosting them. The class defining a Ruby on Rails application is `Rails::Application` which actually inherits a lot of its behavior from `Rails::Engine`, the class defining an engine. We can say that a regular Rails application is simply an engine with more features.



## Create a modular app


First, let’s generate a new Ruby on Rails application:

```ruby
rails new ModularTodo

```

The next step is to generate an engine!

```ruby
cd ModularTodo && rails plugin new todo --mountable

```

We will also create an ‘engines’ folder to store the engines (even if we just have one!).

```ruby
mkdir engines && mv todo ./engines

```

Engines, just like gems, come with a gemspec file. Let’s put some real values to avoid warnings.

```

#ModularTodo/engines/todo/todo.gemspec
$:.push File.expand_path("../lib", __FILE__)

#Maintain your gem's version:
require "todo/version"

#Describe your gem and declare its dependencies:
Gem::Specification.new do |s|
  s.name        = "todo"
  s.version     = Todo::VERSION
  s.authors     = ["Thibault Denizet"]
  s.email       = ["bo@samurails.com"]
  s.homepage    = "//samurails.com"
  s.summary     = "Todo Module"
  s.description = "Todo Module for Modular Rails article"
  s.license     = "MIT"

  #Moar stuff
  #...
end

```

<br>Now we need to add the Todo engine to the parent application Gemfile.<br>

```ruby
#ModularTodo/Gemfile
#Other gems
gem 'todo', path: 'engines/todo'

```

Let’s run `bundle install`. You should see the following in the list of gems:

```ruby
Using todo 0.0.1 from source at engines/todo

```

Great, our Todo engine is loaded correctly! Before we start coding, we have one last thing to do: mount the Todo engine. We can do that in the routes.rb file in the parent app.

```ruby
Rails.application.routes.draw do
  mount Todo::Engine => "/", as: 'todo'
end

```

We are mounting it at `/` but we could also make it accessible at `/todo`. Since we have only one module, `/` is fine.

Now you can fire up your server and check it in your browser. You should see the default Rails view because we didn’t define any controllers/views yet. Let’s do that now!

### Building the Todo list

We are going to scaffold a model named `Task` inside the Todo module but to correctly migrate the database from the parent application, we need to add a small initializer to the `engine.rb` file.

```ruby
#ModularTodo/engines/todo/lib/todo/engine.rb
module Todo
  class Engine < ::Rails::Engine
    isolate_namespace Todo

    initializer :append_migrations do |app|
      unless app.root.to_s.match(root.to_s)
        config.paths["db/migrate"].expanded.each do |p|
          app.config.paths["db/migrate"] << p
        end
      end
    end

  end
end

```

That’s it, now when we run migrations from the parent application, the migrations in the Todo engine will be loaded too.

<br>Let’s create the `Task` model. The `scaffold` command needs to be run from the engine folder.

```ruby
cd engines/todo && rails g scaffold Task title:string content:text

```

Run the migrations from the parent folder:

```ruby
rake db:migrate

```

Now, we just need to define the root route inside the Todo engine:

```ruby
#ModularTodo/engines/todo/config/routes.rb
Todo::Engine.routes.draw do
  resources :tasks
  root 'tasks#index'
end

```

You can play with it, create tasks, delete them… Oh wait, the delete is not working! Why?! Well, it seems JQuery is not loaded, so let’s add it to the `application.js` file inside the engine!

```ruby
// ModularTodo/engines/todo/app/assets/javascripts/todo/application.js
//= require jquery
//= require jquery_ujs
//= require_tree .

```

Yay, now we can destroy tasks!



#### Syntax


- rails plugin new my_module --mountable

