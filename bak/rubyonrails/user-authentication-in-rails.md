---
metaTitle: "Ruby on Rails - User Authentication in Rails"
description: "Authentication using Devise, Devise Controller Filters & Helpers, Omniauth, has_secure_password, has_secure_token"
---

# User Authentication in Rails


Devise is a very powerful gem, it allows you to sign up, sign in and sign out options just after installing. Moreover user can add authentications and restrictions to its applications. Devise also come with its own views, if user wants to use. A user can also customize sign up and sign in forms according to its need and requirement.
It should be noted that Devise recommends that you implement your own login if you're new to rails.



## Authentication using Devise


Add gem to the Gemfile:

`gem 'devise'`

Then run the `bundle install` command.

Use command `$ rails generate devise:install` to generate required configuration file.

Set up the default URL options for the Devise mailer in each environment
In development environment add this line:

```ruby
config.action_mailer.default_url_options = { host: 'localhost', port: 3000 }

```

to your `config/environments/development.rb`

similarly in production this edit `config/environments/production.rb` file and add

```ruby
config.action_mailer.default_url_options = { host: 'your-site-url'}

```

Then create a model using:`$ rails generate devise USER`
Where `USER` is the class name for which you want to implement authentication.

Finally, run: `rake db:migrate` and you are all set.

### Custom views

If you need to configure your views, you can use the `rails generate devise:views` generator that will copy all views to your application. Then you can edit them as desired.

If you have more than one Devise model in your application (for example User and Admin), you will notice that Devise uses the same views for all models. Devise offers an easy way to customize views. Set `config.scoped_views = true` inside the `config/initializers/devise.rb` file.

You can also use the generator to create scoped views: `rails generate devise:views users`

If you would like to generate only a few sets of views, such as the ones for the registerable and confirmable module use the -v flag: `rails generate devise:views -v registrations confirmations`



## Devise Controller Filters & Helpers


To set up a controller with user authentication using devise, add this before_action: (assuming your devise model is 'User'):

`before_action :authenticate_user!`

To verify if a user is signed in, use the following helper:

`user_signed_in?`

For the current signed-in user, use this helper:

`current_user`

You can access the session for this scope:

`user_session`

- Note that if your Devise model is called `Member` instead of `User`, replace `user` above with `member`



## Omniauth


First choose your auth strategy and add it to your `Gemfile`. You can find a list of strategies here: [https://github.com/intridea/omniauth/wiki/List-of-Strategies](https://github.com/intridea/omniauth/wiki/List-of-Strategies)

```ruby
gem 'omniauth-github', :github => 'intridea/omniauth-github'
gem 'omniauth-openid', :github => 'intridea/omniauth-openid'

```

You can add this to your rails middleware like so:

```ruby
Rails.application.config.middleware.use OmniAuth::Builder do
  require 'openid/store/filesystem' 
  provider :github, ENV['GITHUB_KEY'], ENV['GITHUB_SECRET']
  provider :openid, :store => OpenID::Store::Filesystem.new('/tmp')
end

```

By default, OmniAuth will add `/auth/:provider` to your routes and you can start by using these paths.

By default, if there is a failure, omniauth will redirect to `/auth/failure`



## has_secure_password


### Create User Model

`rails generate model User email:string password_digest:string`

### Add has_secure_password module to User model

```ruby
user = User.new email: 'bob@bob.com', password: 'Password1', password_confirmation: 'Password1'

```

Verify password with authenticate method

```ruby
user.authenticate('somepassword')  

```



## has_secure_token


Create User Model

```ruby
# Schema: User(token:string, auth_token:string)
class User < ActiveRecord::Base
  has_secure_token
  has_secure_token :auth_token
end

```

Now when you create a new user a token and auth_token are automatically generated

```ruby
user = User.new
user.save
user.token # => "pX27zsMN2ViQKta1bGfLmVJE"
user.auth_token # => "77TMHrHJFvFDwodq8w7Ev2m7"

```

You can update the tokens using `regenerate_token` and `regenerate_auth_token`

```ruby
user.regenerate_token # => true
user.regenerate_auth_token # => true

```



#### Remarks


At the time of generating devise configs using `rails generate devise:install`, devise will list out bunch of instructions on the terminal to follow.

If you already have a `USER` model, running this command `rails generate devise USER` will append necessary columns to your existing `USER` model.

Use this helper method `before_action :authenticate_user!` at the top of your controller to check whether `user` is logged-in or not. if not then they will be redirected to sign-in page.

