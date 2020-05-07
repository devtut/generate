---
metaTitle: "Ruby on Rails - ActionController"
description: "Basic REST Controller, Output JSON instead of HTML, Controllers (Basic), Parameters, Filtering parameters (Basic), Redirecting, Using Views, Display error pages for exceptions, Filters, Generating a controller, Rescuing ActiveRecord::RecordNotFound with redirect_to, 404 when record not found"
---

# ActionController


Action Controller is the C in MVC. After the router has determined which controller to use for a request, the controller is responsible for making sense of the request and producing the output.

The controller will receive the request, fetch or save data from a model and use a view to create output. A controller can be thought of as a middleman between models and views. It makes the model data available to the view so it can display to the user, and it saves or updates user data to the model.



## Basic REST Controller


```ruby
class PostsController < ApplicationController
  before_action :set_post, only: [:show, :edit, :update, :destroy]

  def index
    @posts = Post.all
  end

  def show
    
  end

  def new
    @post = Post.new
  end

  def edit

  end

  def create
    @post = Post.new(post_params)

    respond_to do |format|
      if @post.save
        format.html { redirect_to @post, notice: 'Post was successfully created.' }
        format.json { render :show, status: :created, location: @post }
      else
        format.html { render :new }
        format.json { render json: @post.errors, status: :unprocessable_entity }
      end
    end
  end

  def update
    respond_to do |format|
      if @post.update(post_params)
        format.html { redirect_to @post.company, notice: 'Post was successfully updated.' }
        format.json { render :show, status: :ok, location: @post }
      else
        format.html { render :edit }
        format.json { render json: @post.errors, status: :unprocessable_entity }
      end
    end
  end

  def destroy
    @post.destroy
    respond_to do |format|
      format.html { redirect_to posts_url, notice: 'Post was successfully destroyed.' }
      format.json { head :no_content }
    end
  end

  private
    def set_post
      @post = Post.find(params[:id])
    end

    def post_params
      params.require(:post).permit(:title, :body, :author)
    end
end

```



## Output JSON instead of HTML


In addition you will need the route:

This will respond in two different ways to requests on `/users`:

- If you visit `/users` or `/users.html`, it will show an html page with the content `Hello World`
- If you visit `/users.json`, it will display a JSON object containing:

You can **omit** `format.html { render inline: "Hello World" }` if you want to make sure that your route will answer only to JSON requests.



## Controllers (Basic)


This is a basic controller, with the addition of the following route (in routes.rb):

Will display the `Hello World` message in a webpage when you access the URL `/users`



## Parameters


Controllers have access to HTTP parameters (you might know them as `?name=foo` in URLs, but Ruby on Rails handle different formats too!) and output different responses based on them. There isn't a way to distinguish between GET and POST parameters, but you shouldn't do that in any case.

```ruby
class UsersController < ApplicationController
  def index
    respond_to do |format|
      format.html do
        if params[:name] == "john"
          render html: "Hello John"
        else
          render html: "Hello someone"
        end
      end
    end 
  end
end

```

As usual our route:

```ruby
resources :users, only: [:index]

```

Access the URL `/users?name=john` and the output will be `Hello John`, access `/users?name=whatever` and the output will be `Hello someone`



## Filtering parameters (Basic)


You can allow (or reject) some params so that only what you want will **pass through** and you won't have bad surprises like user setting options not meant to be changed.

Visiting `/users?name=john&sentence=developer` will display `Hello john developer`, however visiting `/users?name=smith&sentence=spy` will display `Hello smith` only, because `:sentence` is only allowed when you access as `john`



## Redirecting


Assuming the route:

You can redirect to a different URL using:

You can go back to the previous page the user visited using:

Note that in **Rails 5** the syntax for redirecting back is different:

Which will try to redirect to the previous page and in case not possible (the browser is blocking the HTTP_REFERRER header), it will redirect to `:fallback_location`



## Using Views


Assuming the route:

And the controller:

The view `app/users/index.html.erb` will be rendered. If the view is:

The output will be a webpage with the text: "Hello **World**"

If you want to render a different view, you can use:

And the file `app/views/pages/home.html.erb` will be used instead.

You can pass variables to views using controller instance variables:

And in the file `app/views/users/index.html.erb` you can use `@name`:

And the output will be: "Hello **john**"

An important note around the render syntax, you can omit the `render` syntax entirely, Rails assumes that if you omit it. So:

Can be written instead as:

Rails is smart enough to figure out that it must render the file `app/views/users/index.html.erb`.



## Display error pages for exceptions


If you want to display to your users meaningful errors instead of simple "sorry, something went wrong", Rails has a nice utility for the purpose.

Open the file `app/controllers/application_controller.rb` and you should find something like this:

We can now add a `rescue_from` to recover from specific errors:

It's recommended not to rescue from `Exception` or `StandardError` otherwise Rails won't be able to display helpful pages in case of errors.



## Filters


Filters are methods that are run "before", "after" or "around" a controller action. They are inherited, so if you set any in your `ApplicationController` they will be run for every request your application receives.

**Before Filter**

Before filters are executed before the controller action and can halt the request (and/or redirect). A common use is to verify if a user is logged in:

```ruby
class ApplicationController < ActionController::Base
  before_action :authenticate_user!

  def authenticate_user!
    redirect_to some_path unless user_signed_in?
  end
end

```

Before filters are run on requests before the request gets to the controller’s action. It can return a response itself and completely bypass the action.

Other common uses of before filters is validating a user’s authentication before granting them access to the action designated to handle their request. I’ve also seen them used to load a resource from the database, check permissions on a resource, or manage redirects under other circumstances.

**After Filter**

After filters are similar to "before" ones, but as they get executed after the action run they have access the response object that's about to be sent. So in short after filters are run after the action completes. It can modify the response. Most of the time if something is done in an after filter, it can be done in the action itself, but if there is some logic to be run after running any of a set of actions, then an after filter is a good place to do it.

Generally, I’ve seen after and around filters used for logging.

**Around Filter**

Around filters may have logic before and after the action being run. It simply yields to the action in whatever place is necessary. Note that it doesn’t need to yield to the action and may run without doing so like a before filter.

Around filters are responsible for running their associated actions by yielding, similar to how Rack middlewares work.

Around callbacks wrap the execution of actions. You can write an around callback in two different styles. In the first, the callback is a single chunk of code. That code is called before the action is executed. If the callback code invokes yield, the action is executed. When the action completes, the callback code continues executing. Thus, the code before the yield is like a before action callback and the code after the yield is the after action callback. If the callback code never invokes yield. the action is not run-this is the same as having a before action callback return false.

Here's an example of the around filter:

```ruby
around_filter :catch_exceptions
 
private
  def catch_exceptions
    begin
      yield
    rescue Exception => e 
      logger.debug "Caught exception! #{e.message}"
    end
  end

```

This will catch exception of any action and put the message in your log. You can use around filters for exception handling, setup and teardown, and a myriad of other cases.

**Only and Except**

All filters can be applied to specific actions, using `:only` and `:except`:

```ruby
class ProductsController < ApplicationController
  before_action :set_product, only: [:show, :edit, :update]

  # ... controller actions

  # Define your filters as controller private methods
  private

  def set_product
    @product = Product.find(params[:id])
  end
end

```

**Skipping Filter**

All filters (inherited ones too) can also be skipped for some specific actions:

```ruby
class ApplicationController < ActionController::Base
  before_action :authenticate_user!

  def authenticate_user!
    redirect_to some_path unless user_signed_in?
  end
end

class HomeController < ApplicationController
  skip_before_action :authenticate_user!, only: [:index]

  def index
  end
end

```

As they're inherited, filters can also be defined in a `namespace` "parent" controller. Say for example that you have an `admin` namespace, and you of course want only admin users to be able to access it. You could do something like this:

```ruby
# config/routes.rb
namespace :admin do
  resources :products
end

# app/controllers/admin_controller.rb
class AdminController < ApplicationController
  before_action :authenticate_admin_user!

  private

  def authenticate_admin_user!
    redirect_to root_path unless current_user.admin?
  end
end

# app/controllers/admin/products_controller.rb
class Admin::ProductsController < AdminController
  # This controller will inherit :authenticate_admin_user! filter
end

```

Beware that in **Rails 4.x** you could use `before_filter` along with `before_action`, but `before_filter` is currently deprecated in **Rails 5.0.0** and will be removed in **5.1**.



## Generating a controller


Rails provides a lot of generators, for controllers too of course.

You can generate a new controller by running this command in your app folder

```ruby
rails generate controller NAME [action action] [options]

```

**Note: You can also use `rails g` alias to invoke `rails generate`**

For example, to generate a controller for a `Product` model, with `#index` and `#show` actions you would run

```ruby
rails generate controller products index show

```

This will create the controller in `app/controllers/products_controller.rb`, with both the actions you specified

```ruby
class ProductsController < ApplicationController
  def index
  end

  def show
  end
end

```

It will also create a `products` folder inside `app/views/`, containing the two templates for your controller's actions (i.e. `index.html.erb` and `show.html.erb`, **note that the extension may vary according to your template engine, so if you're using `slim`, for example, generator will create `index.html.slim` and `show.html.slim`** )

Furthermore, if you specified any actions they will also be added to your `routes` file

```ruby
# config/routes.rb
get 'products/show'
get 'products/index'

```

Rails creates a helper file for you, in `app/helpers/products_helper.rb`, and also the assets files in `app/assets/javascripts/products.js` and `app/assets/stylesheets/products.css`. As for views, the generator changes this behaviour according to what's specified in your `Gemfile`: i.e., if you're using `Coffeescript` and `Sass` in your application, the controller generator will instead generator `products.coffee` and `products.sass`.

At last, but not least, Rails also generates test files for your controller, your helper and your views.

If you don't want any of these to be created for you can tell Rails to skip them, just prepend any option with

`--no-` or `--skip`, like this:

```ruby
rails generate controller products index show --no-assets --no-helper

```

And the generator will skip both `assets` and `helper`

If you need to create a controller for a specific **`namespace`** add it in front of `NAME`:

```ruby
rails generate controller admin/products

```

This will create your controller inside `app/controllers/admin/products_controller.rb`

Rails can also generate a complete RESTful controller for you:

```ruby
rails generate scaffold_controller MODEL_NAME # available from Rails 4
rails generate scaffold_controller Product

```



## Rescuing ActiveRecord::RecordNotFound with redirect_to


You can rescue a RecordNotFound exception with a redirect instead of showing an error page:

```ruby
class ApplicationController < ActionController::Base

  # your other stuff

  rescue_from ActiveRecord::RecordNotFound do |exception|
    redirect_to root_path, 404, alert: I18n.t("errors.record_not_found")
  end
end

```



## 404 when record not found


Rescue from record not found error instead of showing an exception or white page:

```ruby
class ApplicationController < ActionController::Base
  
  # ... your other stuff here 

  rescue_from ActiveRecord::RecordNotFound do |exception|
    redirect_to root_path, 404, alert: 'Record not found'
  end
end

```

