---
metaTitle: "Ruby on Rails - Naming Conventions"
description: "Controllers, Models, Filenames and autoloading, Views and Layouts, Models class from Controller name"
---

# Naming Conventions



## Controllers


Controller class names are pluralized. The reason is the controller controls multiple instances of object instance.

**For Example**: `OrdersController` would be the controller for an `orders` table. Rails will then look for the class definition in a file called `orders_controller.rb` in the `/app/controllers` directory.

**For Example**: `PostsController` would be the controller for a `posts` table.

If the controller class name has multiple capitalized words, the table name is assumed to have underscores between these words.

**For Example:** If a controller is named `PendingOrdersController` then assumed file name for this controller will be `pending_orders_controller.rb`.



## Models


The model is named using the class naming convention of unbroken MixedCase and is always the singular of the table name.

**For Example**: If a table was named `orders`, the associated model would be named `Order`

**For Example**: If a table was named `posts`, the associated model would be named `Post`

Rails will then look for the class definition in a file called `order.rb` in the `/app/models` directory.

If the model class name has multiple capitalized words, the table name is assumed to have underscores between these words.

**For Example:** If a model is named `BlogPost` then assumed table name will be `blog_posts`.



## Filenames and autoloading


Rails files - and Ruby files in general - should be named with `lower_snake_case` filenames. E.g.

```ruby
app/controllers/application_controller.rb

```

is the file that contains the `ApplicationController` class definition. Note that while `PascalCase` is used for class and module names, the files in which they reside should still be `lower_snake_case`.

Consistent naming is important since Rails makes use of auto-loading files as needed, and uses "inflection" to transform between different naming styles, such as transforming `application_controller` to `ApplicationController` and back again.

E.g. if Rails sees that the `BlogPost` class doesn't exist (hasn't been loaded yet), it'll look for a file named `blog_post.rb` and attempt to load that file.

It is therefore also important to name files for what they contain, since the autoloader expects file names to match content. If, for instance, the `blog_post.rb` instead contains a class named just `Post`, you'll see a `LoadError`: `Expected [some path]/blog_post.rb to define BlogPost`.

If you add a dir under `app/something/` (e.g. /models/products/), and

- want to namespace modules and classes inside new dir then you don't need to do anything and it'll be loaded itself. For example, in `app/models/products/ you would need to wrap your class in`module Products`.
- don't want to namespace modules and classes inside my new dir then you have to add `config.autoload_paths += %W( #{config.root}/app/models/products )` to your `application.rb` to autoload.

One more thing to pay attention to (especially if English is not your first language) is the fact that Rails accounts for irregular plural nouns in English. So if you have model named "Foot" the corresponding controller needs to be called "FeetController" rather than "FootsController" if you want rails "magic" routing (and many more such features) to work.



## Views and Layouts


When a controller action is rendered, Rails will attempt to find a matching layout and view based on the name of the controller.

Views and layouts are placed in the `app/views` directory.

Given a request to the `PeopleController#index` action, Rails will search for:

- the layout called `people` in `app/views/layouts/` (or `application` if no match is found)
- a view called `index.html.erb` in `app/views/people/` by default
- if you wish to render other file called `index_new.html.erb` you have to write code for that in `PeopleController#index` action like `render 'index_new'`
- we can set different `layouts` for every `action` by writing `render 'index_new', layout: 'your_layout_name'`



## Models class from Controller name


You can get a Model class from a Controller name this way (context is Controller class):

```ruby
class MyModelController < ActionController::Base

  # Returns corresponding model class for this controller
  # @return [ActiveRecord::Base]
  def corresponding_model_class
    # ... add some validation
    controller_name.classify.constantize
  end
end

```

