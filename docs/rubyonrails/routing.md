---
metaTitle: "Ruby on Rails - Routing"
description: "Resource Routing (Basic), Constraints, Scoping routes, Concerns, Root route, Additional RESTful actions, Split routes into multiple files, Member and Collection Routes, Redirection, Mount another application, Nested Routes, Scope available locales, Redirects and Wildcard Routes, URL params with a period"
---

# Routing




## Resource Routing (Basic)


Routes are defined in `config/routes.rb`. They are often defined as a group of related routes, using the [`resources`](http://api.rubyonrails.org/classes/ActionDispatch/Routing/Mapper/Resources.html#method-i-resources) or [`resource`](http://api.rubyonrails.org/classes/ActionDispatch/Routing/Mapper/Resources.html#method-i-resource) methods.

`resources :users` creates the following seven routes, all mapping to actions of `UsersController`:

```ruby
get       '/users',          to: 'users#index'
post      '/users',          to: 'users#create'
get       '/users/new',      to: 'users#new'
get       '/users/:id/edit', to: 'users#edit'
get       '/users/:id',      to: 'users#show'
patch/put '/users/:id',      to: 'users#update'
delete    '/users/:id',      to: 'users#destroy'

```

Action names are shown after the `#` in the `to` parameter above. Methods with those same names must be defined in `app/controllers/users_controller.rb` as follows:

```ruby
class UsersController < ApplicationController
  def index
  end

  def create
  end

  # continue with all the other methods…
end

```

You can limit the actions that gets generated with `only` or `except`:

```ruby
resources :users, only:   [:show]
resources :users, except: [:show, :index]

```

You can view all the routes of your application at any given time by running:

```ruby
$ rake routes

```

```ruby
$ rake routes
# OR
$ rails routes

```

```ruby
users     GET    /users(.:format)          users#index
          POST   /users(.:format)          users#create
new_user  GET    /users/new(.:format)      users#new
edit_user GET    /users/:id/edit(.:format) users#edit
user      GET    /users/:id(.:format)      users#show
          PATCH  /users/:id(.:format)      users#update
          PUT    /users/:id(.:format)      users#update
          DELETE /users/:id(.:format)      users#destroy

```

To see only the routes that map to a particular controller:

```ruby
$ rake routes -c static_pages
static_pages_home    GET    /static_pages/home(.:format)    static_pages#home
static_pages_help    GET    /static_pages/help(.:format)    static_pages#help

```

```ruby
$ rake routes -c static_pages
static_pages_home    GET    /static_pages/home(.:format)    static_pages#home
static_pages_help    GET    /static_pages/help(.:format)    static_pages#help

# OR

$ rails routes -c static_pages
static_pages_home    GET    /static_pages/home(.:format)    static_pages#home
static_pages_help    GET    /static_pages/help(.:format)    static_pages#help

```

You can search through routes using the `-g` option. This shows any route that partially matches the helper method name, the URL path or the HTTP verb:

```ruby
$ rake routes -g new_user     # Matches helper method
$ rake routes -g POST         # Matches HTTP Verb POST 

```

```ruby
$ rake routes -g new_user     # Matches helper method
$ rake routes -g POST         # Matches HTTP Verb POST 
# OR
$ rails routes -g new_user    # Matches helper method
$ rails routes -g POST        # Matches HTTP Verb POST 

```

Additionally, when running `rails` server in development mode, you can access a web page that shows all your routes with a search filter, matched in priority from top to bottom,  at `<hostname>/rails/info/routes`. It will look like this:

|Helper|HTTP Verb|Path|Controller#Action
|---|---|---|---|---|---|---|---|---|---
|Path / Url||[ Path Match ]|
|users_path|GET|/users(.:format)|users#index
||POST|/users(.:format)|users#create
|new_user_path|GET|/users/new(.:format)|users#new
|edit_user_path|GET|/users/:id/edit(.:format)|users#edit
|user_path|GET|/users/:id(.:format)|users#show
||PATCH|/users/:id(.:format)|users#update
||PUT|/users/:id(.:format)|users#update
||DELETE|/users/:id(.:format)|users#destroy

Routes can be declared available for only members (not collections) using the method `resource` instead of `resources` in `routes.rb`. With `resource`, an `index` route is not created by default, but only when explicitly asking for one like this:

```ruby
resource :orders, only: [:index, :create, :show]

```



## Constraints


You can filter what routes are available using constraints.

There are several ways to use constraints including:

- [segment constraints](http://guides.rubyonrails.org/routing.html#segment-constraints),
- [request based constraints](http://guides.rubyonrails.org/routing.html#request-based-constraints)
- [advanced constraints](http://guides.rubyonrails.org/routing.html#advanced-constraints)

For example, a requested based constraint to only allow a specific IP address to access a route:

```ruby
constraints(ip: /127\.0\.0\.1$/) do
  get 'route', to: "controller#action"
end

```

[See other similar examples ActionDispatch::Routing::Mapper::Scoping](http://api.rubyonrails.org/classes/ActionDispatch/Routing/Mapper/Scoping.html).

If you want to do something more complex you can use more advanced constraints and create a class to wrap the logic:

```ruby
# lib/api_version_constraint.rb
class ApiVersionConstraint
  def initialize(version:, default:)
    @version = version
    @default = default
  end

  def version_header
    "application/vnd.my-app.v#{@version}"
  end

  def matches?(request)
    @default || request.headers["Accept"].include?(version_header)
  end
end

# config/routes.rb
require "api_version_constraint"

Rails.application.routes.draw do
  namespace :v1, constraints: ApiVersionConstraint.new(version: 1, default: true) do
    resources :users # Will route to app/controllers/v1/users_controller.rb
  end

  namespace :v2, constraints: ApiVersionConstraint.new(version: 2) do
    resources :users # Will route to app/controllers/v2/users_controller.rb
  end
end

```

**One form, several submit buttons**

You can also use the value of the submit tags of a form as a constraint to route to a different action. If you have a form with multiple submit buttons (eg "preview" and "submit"), you could capture this constraint directly in your `routes.rb`, instead of writing javascript to change the form destination URL. For example with the [commit_param_routing](https://github.com/siliconsenthil/commit_param_routing) gem you can take advantage of rails `submit_tag`

Rails `submit_tag` first parameter lets you change the value of your form commit parameter

```ruby
# app/views/orders/mass_order.html.erb
<%= form_for(@orders, url: mass_create_order_path do |f| %>
    <!-- Big form here -->
  <%= submit_tag "Preview" %>
  <%= submit_tag "Submit" %>
  # => <input name="commit" type="submit" value="Preview" />
  # => <input name="commit" type="submit" value="Submit" />
  ...
<% end %>

# config/routes.rb
resources :orders do
  # Both routes below describe the same POST URL, but route to different actions 
  post 'mass_order', on: :collection, as: 'mass_order',
    constraints: CommitParamRouting.new('Submit'), action: 'mass_create' # when the user presses "submit"
  post 'mass_order', on: :collection,
    constraints: CommitParamRouting.new('Preview'), action: 'mass_create_preview' # when the user presses "preview"
  # Note the `as:` is defined only once, since the path helper is mass_create_order_path for the form url
  # CommitParamRouting is just a class like ApiVersionContraint
end

```



## Scoping routes


Rails provides several ways to organize your routes.

**Scope by URL**:

```ruby
scope 'admin' do
  get 'dashboard', to: 'administration#dashboard'
  resources 'employees'
end

```

This generates the following routes

```ruby
get       '/admin/dashboard',          to: 'administration#dashboard'
post      '/admin/employees',          to: 'employees#create'
get       '/admin/employees/new',      to: 'employees#new'
get       '/admin/employees/:id/edit', to: 'employees#edit'
get       '/admin/employees/:id',      to: 'employees#show'
patch/put '/admin/employees/:id',      to: 'employees#update'
delete    '/admin/employees/:id',      to: 'employees#destroy'

```

It may make more sense, on the server side, to keep some views in a different subfolder, to separate admin views from user views.

**Scope by module**

```ruby
scope module: :admin do
  get 'dashboard', to: 'administration#dashboard'
end

```

`module` looks for the controller files under the subfolder of the given name

```ruby
get       '/dashboard',          to: 'admin/administration#dashboard'

```

You can rename the path helpers prefix by adding an `as` parameter

```ruby
scope 'admin', as: :administration do
  get 'dashboard'
end

# => administration_dashboard_path

```

Rails provides a convenient way to do all the above, using the `namespace` method. The following declarations are equivalent

```ruby
namespace :admin do
end

scope 'admin', module: :admin, as: :admin

```

**Scope by controller**

```ruby
scope controller: :management do
  get 'dashboard'
  get 'performance'
end

```

This generate these routes

```ruby
get       '/dashboard',          to: 'management#dashboard'
get       '/performance',        to: 'management#performance'

```

**Shallow Nesting**

Resource routes accept a `:shallow` option that helps to shorten URLs where possible. Resources shouldn't be nested more than one level deep. One way to avoid this is by creating shallow routes. The goal is to leave off parent collection URL segments where they are not needed. The end result is that the only nested routes generated are for the `:index` , `:create` , and `:new` actions. The rest are kept in their own shallow URL context. There are two options for scope to custom shallow routes:

<li>
**:shallow_path**: Prefixes member paths with a specified parameter

```ruby
scope shallow_path: "sekret" do
  resources :articles do
    resources :comments, shallow: true
  end
end

```


</li>
<li>
**:shallow_prefix**: Add specified parameters to named helpers

```ruby
scope shallow_prefix: "sekret" do
  resources :articles do
    resources :comments, shallow: true
  end
end

```


</li>

We can also illustrate `shallow` routes more by:

```ruby
resources :auctions, shallow: true do
  resources :bids do
   resources :comments
  end
end 

```

alternatively coded as follows (if you’re block-happy):

```ruby
resources :auctions do
 shallow do
   resources :bids do
     resources :comments
   end
 end
end

```

The resulting routes are:

<th align="left">Prefix</th><th align="left">Verb</th><th align="left">URI Pattern</th>
|---|---|---|---|---|---|---|---|---|---
<td align="left">bid_comments</td><td align="left">GET</td><td align="left">/bids/:bid_id/comments(.:format)</td>
<td align="left"></td><td align="left">POST</td><td align="left">/bids/:bid_id/comments(.:format)</td>
<td align="left">new_bid_comment</td><td align="left">GET</td><td align="left">/bids/:bid_id/comments/new(.:format)</td>
<td align="left">edit_comment</td><td align="left">GET</td><td align="left">/comments/:id/edit(.:format)</td>
<td align="left">comment</td><td align="left">GET</td><td align="left">/comments/:id(.:format)</td>
<td align="left"></td><td align="left">PATCH</td><td align="left">/comments/:id(.:format)</td>
<td align="left"></td><td align="left">PUT</td><td align="left">/comments/:id(.:format)</td>
<td align="left"></td><td align="left">DELETE</td><td align="left">/comments/:id(.:format)</td>
<td align="left">auction_bids</td><td align="left">GET</td><td align="left">/auctions/:auction_id/bids(.:format)</td>
<td align="left"></td><td align="left">POST</td><td align="left">/auctions/:auction_id/bids(.:format)</td>
<td align="left">new_auction_bid</td><td align="left">GET</td><td align="left">/auctions/:auction_id/bids/new(.:format)</td>
<td align="left">edit_bid</td><td align="left">GET</td><td align="left">/bids/:id/edit(.:format)</td>
<td align="left">bid</td><td align="left">GET</td><td align="left">/bids/:id(.:format)</td>
<td align="left"></td><td align="left">PATCH</td><td align="left">/bids/:id(.:format)</td>
<td align="left"></td><td align="left">PUT</td><td align="left">/bids/:id(.:format)</td>
<td align="left"></td><td align="left">DELETE</td><td align="left">/bids/:id(.:format)</td>
<td align="left">auctions</td><td align="left">GET</td><td align="left">/auctions(.:format)</td>
<td align="left"></td><td align="left">POST</td><td align="left">/auctions(.:format)</td>
<td align="left">new_auction</td><td align="left">GET</td><td align="left">/auctions/new(.:format)</td>
<td align="left">edit_auction</td><td align="left">GET</td><td align="left">/auctions/:id/edit(.:format)</td>
<td align="left">auction</td><td align="left">GET</td><td align="left">/auctions/:id(.:format)</td>
<td align="left"></td><td align="left">PATCH</td><td align="left">/auctions/:id(.:format)</td>
<td align="left"></td><td align="left">PUT</td><td align="left">/auctions/:id(.:format)</td>
<td align="left"></td><td align="left">DELETE</td><td align="left">/auctions/:id(.:format)</td>

If you analyze the routes generated carefully, you’ll notice that the nested parts of the URL are only included when they are needed to determine what data to display.



## Concerns


To avoid repetition in nested routes, concerns provide a great way of sharing common resources that are reusable. To create a concern use the method `concern` within the `routes.rb` file.  The method expects a symbol and block:

```ruby
concern :commentable do
  resources :comments
end

```

While not creating any routes itself, this code allows using the `:concerns` attribute on a resource.  The simplest example would be:

```ruby
resource :page, concerns: :commentable

```

The equivalent nested resource would look like this:

```ruby
resource :page do
  resource :comments
end

```

This would build, for example, the following routes:

```ruby
/pages/#{page_id}/comments
/pages/#{page_id}/comments/#{comment_id}

```

For concerns to be meaningful, there must be multiple resources that utilize the concern.  Additional resources could use any of the following syntax to call the concern:

```ruby
resource :post, concerns: %i(commentable)
resource :blog do
  concerns :commentable
end

```



## Root route


You can add a home page route to your app with the `root` method.

```ruby
# config/routes.rb
Rails.application.routes.draw do
  root "application#index"
  # equivalent to:
  # get "/", "application#index"  
end

# app/controllers/application_controller.rb
class ApplicationController < ActionController::Base
  def index
    render "homepage"
  end
end

```

And in terminal, `rake routes` (`rails routes` in Rails 5) will produce:

```ruby
root     GET    /         application#index

```

Because the homepage is usually the most important route, and routes are prioritized in the order they appear, the `root` route should usually be the first in your routes file.



## Additional RESTful actions


```ruby
get       '/photos/:id/preview',          to: 'photos#preview'
get       '/photos/dashboards',           to: 'photos#dashboard'

```

If you want to do this for single lines, you can use:

```ruby
get       '/photos/new/preview',          to: 'photos#preview'

```

Be mindful when adding actions to your RESTful routes, probably you are missing another resource!



## Split routes into multiple files


If your routes file is overwhelmingly big, you can put your routes in multiple files and include each of the files with Ruby’s [`require_relative`](http://apidock.com/ruby/Kernel/require_relative) method:

```ruby
YourAppName::Application.routes.draw do
  require_relative 'routes/admin_routes'
  require_relative 'routes/sidekiq_routes'
  require_relative 'routes/api_routes'
  require_relative 'routes/your_app_routes'
end

```

```ruby
YourAppName::Application.routes.draw do
  namespace :api do
    # ...
  end
end

```



## Member and Collection Routes


Defining a member block inside a resource creates a route that can act on an individual member of that resource-based route:

```ruby
resources :posts do
  member do
    get 'preview'
  end
end

```

This generates the following member route:

```ruby
get '/posts/:id/preview', to: 'posts#preview'
# preview_post_path

```

Collection routes allow for creating routes that can act on a collection of resource objects:

```ruby
resources :posts do
  collection do
    get 'search'
  end
end

```

This generates the following collection route:

```ruby
get '/posts/search', to: 'posts#search'
# search_posts_path

```

An alternate syntax:

```ruby
resources :posts do
  get 'preview', on: :member
  get 'search',  on: :collection
end

```



## Redirection


You can perform redirection in Rails routes as follows:

```ruby
get '/stories', to: redirect('/posts')

```

```ruby
match "/abc" => redirect("http://example.com/abc")

```

You can also redirect all unknown routes to a given path:

```ruby
match '*path' => redirect('/'), via: :get
# or
get '*path' => redirect('/')

```

```ruby
match '*path' => redirect('/')

```



## Mount another application


mount is used to mount another application (basically rack application) or rails engines to be used within the current application

**syntax:**

```ruby
mount SomeRackApp, at: "some_route"

```

Now you can access above mounted application using route helper `some_rack_app_path` or `some_rack_app_url`.

But if you want to rename this helper name you can do it as:

```ruby
mount SomeRackApp, at: "some_route", as: :myapp

```

This will generate the `myapp_path` and `myapp_url` helpers which can be used to navigate to this mounted app.



## Nested Routes


If you want to add nested routes you can write the following code in `routes.rb` file.

```ruby
resources :admins do
  resources :employees
end

```

This will generate following routes:

```

    admin_employees GET      /admins/:admin_id/employees(.:format)            employees#index
                     POST     /admins/:admin_id/employees(.:format)            employees#create
  new_admin_employee GET      /admins/:admin_id/employees/new(.:format)        employees#new
 edit_admin_employee GET      /admins/:admin_id/employees/:id/edit(.:format)   employees#edit
      admin_employee GET      /admins/:admin_id/employees/:id(.:format)        employees#show
                     PATCH    /admins/:admin_id/employees/:id(.:format)        employees#update
                     PUT      /admins/:admin_id/employees/:id(.:format)        employees#update
                     DELETE   /admins/:admin_id/employees/:id(.:format)        employees#destroy
              admins GET      /admins(.:format)                                admins#index
                     POST     /admins(.:format)                                admins#create
           new_admin GET      /admins/new(.:format)                            admins#new
          edit_admin GET      /admins/:id/edit(.:format)                       admins#edit
               admin GET      /admins/:id(.:format)                            admins#show
                     PATCH    /admins/:id(.:format)                            admins#update
                     PUT      /admins/:id(.:format)                            admins#update
                     DELETE   /admins/:id(.:format)                            admins#destroy

```



## Scope available locales


If your application is available in different languages, you usually show the current locale in the URL.

```ruby
scope '/(:locale)', locale: /#{I18n.available_locales.join('|')}/ do
    root 'example#root'
    # other routes
end

```

Your root will be accessible via the locales defined in `I18n.available_locales`.



## Redirects and Wildcard Routes


If you want to provide a URL out of convenience for your user but map it directly to another one you're already using. Use a redirect:

```ruby
# config/routes.rb
TestApp::Application.routes.draw do
  get 'courses/:course_name' => redirect('/courses/%{course_name}/lessons'), :as => "course"
end

```

Well, that got interesting fast. The basic principle here is to just use the `#redirect` method to send one route to another route. If your route is quite simple, it's a really straightforward method. But if you want to also send the original parameters, you need to do a bit of gymnastics by capturing the parameter inside `%{here}`. Note the single quotes around everything.

In the example above, we've also renamed the route for convenience by using an alias with the :as parameter. This lets us use that name in methods like the #_path helpers. Again, test out your `$ rake routes` with questions.



## URL params with a period


If you want to support a url parameter more complex than an id number, you may run into trouble with the parser if the value contains a period. Anything following a period will be assumed to be a format (i.e. json, xml).

You can work around this limitation by using a constraint to **broaden** the accepted input.

For example, if you want to reference a user record by email address in the url:



#### Remarks


"Routing" in general is how URL's are "handled" by your app.  In Rails case it's typically which controller and which action of that controller will handle a particular incoming URL.  In Rails apps, routes are usually placed in the `config/routes.rb` file.

