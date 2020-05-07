---
metaTitle: "Ruby on Rails - Caching"
description: "Russian Doll Caching, SQL Caching, Fragment caching, Page caching, HTTP caching, Action caching"
---

# Caching




## Russian Doll Caching


You may want to nest cached fragments inside other cached fragments. This is called `Russian doll caching`.

The advantage of `Russian doll caching` is that if a single product is updated, all the other inner fragments can be reused when regenerating the outer fragment.

As explained in the previous section, a cached file will expire if the value of `updated_at` changes for a record on which the cached file directly depends. However, this will not expire any cache the fragment is nested within.

For example, take the following view:

```ruby
<% cache product do %>
  <%= render product.games %>
<% end %>

```

Which in turn renders this view:

```ruby
<% cache game do %>
  <%= render game %>
<% end %>

```

If any attribute of game is changed, the `updated_at` value will be set to the current time, thereby expiring the cache.

However, because `updated_at` will not be changed for the product object, that cache will not be expired and your app will serve stale data. To fix this, we tie the models together with the touch method:

```ruby
class Product < ApplicationRecord
  has_many :games
end

class Game < ApplicationRecord
  belongs_to :product, touch: true
end

```



## SQL Caching


Query caching is a `Rails` feature that caches the result set returned by each query. If `Rails` encounters the same query again for that request, it will use the cached result set as opposed to running the query against the database again.

For example:

```ruby
class ProductsController < ApplicationController

  def index
    # Run a find query
    @products = Product.all

    ...

    # Run the same query again
    @products = Product.all
  end

end

```

The second time the same query is run against the database, it's not actually going to hit the database. The first time the result is returned from the query it is stored in the query cache (in memory) and the second time it's pulled from memory.

However, it's important to note that query caches are created at the start of an action and destroyed at the end of that action and thus persist only for the duration of the action. If you'd like to store query results in a more persistent fashion, you can with low level caching.



## Fragment caching


`Rails.cache`, provided by ActiveSupport, can be used to cache any serializable Ruby object across requests.

To fetch a value from the cache for a given key, use `cache.read`:

```ruby
Rails.cache.read('city')
# => nil

```

Use `cache.write` to write a value to the cache:

```ruby
Rails.cache.write('city', 'Duckburgh')
Rails.cache.read('city')
# => 'Duckburgh'

```

Alternatively, use `cache.fetch` to read a value from the cache and optionally write a default if there is no value:

```ruby
Rails.cache.fetch('user') do
  User.where(:is_awesome => true)
end

```

The return value of the passed block will be assigned to the cache under the given key, and then returned.

You can also specify a cache expiry:

```ruby
Rails.cache.fetch('user', :expires_in => 30.minutes) do
  User.where(:is_awesome => true)
end

```



## Page caching


You can use the [ActionPack page_caching gem](https://github.com/rails/actionpack-page_caching) to cache individual pages. This stores the result of one dynamic request as a static HTML file, which is served in place of the dynamic request on subsequent requests. The README contains full setup instructions. Once set up, use the `caches_page` class method in a controller to cache the result of an action:

```ruby
class UsersController < ActionController::Base
  caches_page :index
end

```

Use `expire_page` to force expiration of the cache by deleting the stored HTML file:

```ruby
class UsersController < ActionController::Base
  caches_page :index

  def index
    @users = User.all
  end

  def create
    expire_page :action => :index
  end
end

```

The syntax of `expire_page` mimics that of `url_for` and friends.



## HTTP caching


Rails >= 3 comes with HTTP caching abilities out of the box. This uses the `Cache-Control` and `ETag` headers to control how long a client or intermediary (such as a CDN) can cache a page.

In a controller action, use `expires_in` to set the length of caching for that action:

```ruby
def show
  @user = User.find params[:id]
  expires_in 30.minutes, :public => true
end

```

Use `expires_now` to force immediate expiration of a cached resource on any visiting client or intermediary:

```ruby
def show
  @users = User.find params[:id]
  expires_now if params[:id] == 1
end

```



## Action caching


Like page caching, action caching caches the whole page. The difference is that the request hits the Rails stack so before filters are run before the cache is served.
It's extracted from Rails to [actionpack-action_caching gem](https://github.com/rails/actionpack-action_caching).

A common example is caching of an action that requires authentication:

```ruby
class SecretIngredientsController < ApplicationController
  before_action :authenticate_user!, only: :index, :show
  caches_action :index
  
  def index
    @secret_ingredients = Recipe.find(params[:recipe_id]).secret_ingredients
  end
end

```

Options include `:expires_in`, a custom `:cache_path` (for actions with multiple routes that should be cached differently) and `:if`/`:unless` to control when the action should be cached.

```ruby
class RecipesController < ApplicationController
  before_action :authenticate_user!, except: :show
  caches_page :show
  caches_action :archive, expires_in: 1.day
  caches_action :index, unless: { request.format.json? }
end

```

When the layout has dynamic content, cache only the action content by passing `layout: false`.

