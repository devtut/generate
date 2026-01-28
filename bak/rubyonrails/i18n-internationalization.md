---
metaTitle: "Ruby on Rails - I18n - Internationalization"
description: "I18n with arguments, Translating ActiveRecord model attributes, Pluralization, Set locale through requests, Get locale from HTTP request, Use I18n with HTML Tags and Symbols, Use I18n in views"
---

# I18n - Internationalization




## I18n with arguments


You can pass parameters to **I18n** `t` method:

```ruby
# Example config/locales/en.yml
en:
  page:
    users: "%{users_count} users currently online"

# In models, controller, etc...
I18n.t('page.users', users_count: 12)

# In views

# ERB
<%= t('page.users', users_count: 12) %>

#SLIM
= t('page.users', users_count: 12)

# Shortcut in views - DRY!
# Use only the dot notation
# Important: Consider you have the following controller and view page#users

# ERB Example app/views/page/users.html.erb
<%= t('.users', users_count: 12) %>

```

And get the following output:

```ruby
"12 users currently online"

```



## Translating ActiveRecord model attributes


**`globalize`** gem is a great solution to add translations to your `ActiveRecord` models. You can install it adding this to your `Gemfile`:

```ruby
gem 'globalize', '~> 5.0.0'

```

If you're using `Rails 5` you will also need to add `activemodel-serializers-xml`

```ruby
gem 'activemodel-serializers-xml'

```

Model translations allow you to translate your models' attribute values, for example:

```ruby
class Post < ActiveRecord::Base
  translates :title, :text
end

I18n.locale = :en
post.title # => Globalize rocks!

I18n.locale = :he
post.title # => גלובאלייז2 שולט!

```

After you defined your model attributes that need to be translated you have to create a translation table, through a migration. **`globalize`** provides `create_translation_table!` and `drop_translation_table!`.

For this migration you need to use `up` and `down`, and **not** `change`. Also, in order to run this migration successfully, you have to define the translated attributes in your model first, like shown above. A proper migration for the previous `Post` model is this:

```ruby
class CreatePostsTranslationTable < ActiveRecord::Migration
  def up
    Post.create_translation_table! title: :string, text: :text
  end

  def down
    Post.drop_translation_table!
  end
end

```

You may also pass options for specific options, like:

```ruby
class CreatePostsTranslationTable < ActiveRecord::Migration
  def up
    Post.create_translation_table! title: :string,
      text: { type: :text, null: false, default: "Default text" }
  end

  def down
    Post.drop_translation_table!
  end
end

```

In case you already have any **existing data** in your needing translation columns, you can easily migrate it to the translations table, by adjusting your migration:

```ruby
class CreatePostsTranslationTable < ActiveRecord::Migration
  def up
    Post.create_translation_table!({
      title: :string,
      text: :text
    }, {
      migrate_data: true
    })
      
  end

  def down
    Post.drop_translation_table! migrate_data: true
  end
end

```

****Make sure you drop the translated columns from the parent table after all your data is safely migrated.**** To automatically remove the translated columns from the parent table after the data migration, add the option `remove_source_columns` to the migration:

```ruby
class CreatePostsTranslationTable < ActiveRecord::Migration
  def up
    Post.create_translation_table!({
      title: :string,
      text: :text
    }, {
      migrate_data: true,
      remove_source_columns: true
    })
      
  end

  def down
    Post.drop_translation_table! migrate_data: true
  end
end

```

You may also add new fields to a previously created translations table:

```ruby
class Post < ActiveRecord::Base
  # Remember to add your attribute here too.
  translates :title, :text, :author
end

class AddAuthorToPost < ActiveRecord::Migration
  def up
    Post.add_translation_fields! author: :text
  end

  def down
    remove_column :post_translations, :author
  end
end

```



## Pluralization


You can let **I18n** handle pluralization for you, just use `count` argument.

You need to set up your locale file like this:

```ruby
# config/locales/en.yml
en:
  online_users:
    one: "1 user is online"
    other: "%{count} users are online"

```

And then use the key you just created by passing the `count` argument to `I18n.t` helper:

```ruby
I18n.t("online_users", count: 1)
#=> "1 user is online"

I18n.t("online_users", count: 4)
#=> "4 users are online"

```



## Set locale through requests


In most cases, you may want to set **`I18n`** locale. One might want to set the locale for the current session, the current user, or based on a URL parameter This is easily achievable by implementing a `before_action` in one of your controllers, or in `ApplicationController` to have it in all of your controllers.

```ruby
class ApplicationController < ActionController::Base
  before_action :set_locale

  protected

  def set_locale
    # Remove inappropriate/unnecessary ones
    I18n.locale = params[:locale] ||    # Request parameter
      session[:locale] ||               # Current session
      (current_user.preferred_locale if user_signed_in?) ||  # Model saved configuration
      extract_locale_from_accept_language_header ||          # Language header - browser config
      I18n.default_locale               # Set in your config files, english by super-default
  end

  # Extract language from request header
  def extract_locale_from_accept_language_header
    if request.env['HTTP_ACCEPT_LANGUAGE']
      lg = request.env['HTTP_ACCEPT_LANGUAGE'].scan(/^[a-z]{2}/).first.to_sym
      lg.in?([:en, YOUR_AVAILABLE_LANGUAGES]) ? lg : nil
    end
  end

```

### URL-based

The `locale` param could come from an URL like this

```ruby
http://yourapplication.com/products?locale=en

```

Or

```ruby
http://yourapplication.com/en/products

```

To achieve the latter, you need to edit your `routes`, adding a `scope`:

```ruby
# config/routes.rb
scope "(:locale)", locale: /en|fr/ do
  resources :products
end

```

By doing this, visiting `http://yourapplication.com/en/products` will set your locale to `:en`. Instead, visiting `http://yourapplication.com/fr/products` will set it to `:fr`. Furthermore, you won't get a routing error when missing the `:locale` param, as visiting `http://yourapplication.com/products` will load the default **I18n** locale.

### Session-based or persistence-based

This assumes the user can click on a button/language flag to change the language. The action can route to a controller that sets the session to the current language (and eventually persist the changes to a database if the user is connected)

```ruby
class SetLanguageController < ApplicationController
  skip_before_filter :authenticate_user!
  after_action :set_preferred_locale

  # Generic version to handle a large list of languages
  def change_locale
    I18n.locale = sanitize_language_param
    set_session_and_redirect
  end

```

> 
You have to define sanitize_language_param with your list of available languages, and eventually handle errors in case the language doesn't exist.


If you have very few languages, it may be worth defining them like this instead:

```ruby
def fr
  I18n.locale = :fr
  set_session_and_redirect
end

def en
  I18n.locale = :en
  set_session_and_redirect
end

private

  def set_session_and_redirect
    session[:locale] = I18n.locale
    redirect_to :back
  end

  def set_preferred_locale
    if user_signed_in?
      current_user.preferred_locale = I18n.locale.to_s
      current_user.save if current_user.changed?
    end
  end
end

```

**Note: don't forget to add some routes to your `change_language` actions**

### Default Locale

Remember that you need to set your application default locale. You can do it by either setting it in `config/application.rb`:

```ruby
config.i18n.default_locale = :de

```

or by creating an initializer in the `config/initializers` folder:

```ruby
# config/initializers/locale.rb
I18n.default_locale = :it

```



## Get locale from HTTP request


Sometimes it can be useful to set your application locale based upon the request IP. You can easily achieve this using **`Geocoder`**. Among the many things **`Geocoder`** does, it can also tell the `location` of a `request`.

First, add **`Geocoder`** to your `Gemfile`

```ruby
# Gemfile
gem 'geocoder'

```

**`Geocoder`**  adds `location` and `safe_location` methods to the standard `Rack::Request` object so you can easily look up the location of any HTTP request by IP address. You can use this methods in a `before_action` in your `ApplicationController`:

```ruby
class ApplicationController < ActionController::Base
  before_action :set_locale_from_request

  def set_locale_from_request
    country_code = request.location.data["country_code"] #=> "US"
    country_sym = country_code.underscore.to_sym #=> :us

    # If request locale is available use it, otherwise use I18n default locale
    if I18n.available_locales.include? country_sym
      I18n.locale = country_sym
    end
end

```

Beware that this will not work in `development` and `test` environments, as things like `0.0.0.0` and `localhost` are valid valid Internet IP addresses.

### Limitations and alternatives

**`Geocoder`** is very powerful and flexible, but needs to be configured to work with a **geocoding service** (see [more details](https://github.com/alexreisner/geocoder#geocoding-service-lookup-configuration)); many of which place limits on usage. It's also worth bearing in mind that calling an external service on every request could impact performance.

To address these, it can also be worth considering:

### 1. An offline solution

Using a gem like **`GeoIP`** (see [here](https://github.com/cjheath/geoip)) allows lookups to happen against a local datafile. There may be a trade-off in terms of accuracy, as these datafiles need to be kept up-to-date.

### 2. Use CloudFlare

Pages served through CloudFlare have the option of being geocoded transparently, with the country code being added to the header (`HTTP_CF_IPCOUNTRY`). More detail can be found [here](https://support.cloudflare.com/hc/en-us/articles/200168236-What-does-Cloudflare-IP-Geolocation-do-).



## Use I18n with HTML Tags and Symbols


```ruby
# config/locales/en.yml
en:
  stackoverflow:
    header:
      title_html: "Use <strong>I18n</strong> with Tags &amp; Symbols"

```

Note the addition of extra `_html` after the name `title`.

And in Views,

```ruby
# ERB
<h2><%= t(:title_html, scope: [:stackoverflow, :header]) %></h2>

```



## Use I18n in views


Assuming you have this YAML locale file:

```ruby
# config/locales/en.yml
en:
  header:
    title: "My header title"

```

and you want to display your title string, you can do this

```ruby
# in ERB files
<%= t('header.title') %>

# in SLIM files
= t('header.title')

```



#### Syntax


- I18n.t("key")
- I18n.translate("key")  # equivalent to `I18n.t("key")`
- I18n.t("key", count: 4)
- I18n.t("key", param1: "Something", param2: "Else")
- I18n.t("doesnt_exist", default: "key")  # specify a default if the key is missing
- I18n.locale #=> :en
- I18n.locale = :en
- I18n.default_locale #=> :en
- I18n.default_locale = :en
- t(".key")  # same as `I18n.t("key")`, but scoped to the action/template it's called from

