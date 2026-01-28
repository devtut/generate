---
metaTitle: "Ruby on Rails - Authenticate Api using Devise"
description: "Getting Started"
---

# Authenticate Api using Devise


Devise is authentication solution for Rails. Before going any further i would like to add quick note on API. So API does not handle sessions (is stateless) which means one that provide response after you request, and then requires no further attention, which means no previous or future state is required for the system to work hence whenever we requesting to the server need to pass authentication details with all API and should tell Devise not to store authentication details.



## Getting Started


So first we will create rails project and setup device

create a rails application

```ruby
rails new devise_example

```

now add devise to gem list

> 
you can find a file named 'Gemfile' at the root of rails project


Then run `bundle install`

Next, you need to run the generator:

```ruby
rails generate devise:install

```

Now on console you can find few instructions just follow it.

Generate devise model

```ruby
rails generate devise MODEL

```

Then run `rake db:migrate`

For more details go to: [Devise Gem](https://github.com/plataformatec/devise#getting-started)

### Authentication Token

Authentication token is used to authenticate a user with a unique token, So
Before we proceed with the logic first we need to add `auth_token` field to a Devise model

Hence,

```ruby
rails g migration add_authentication_token_to_users

class AddAuthenticationTokenToUsers < ActiveRecord::Migration
  def change
    add_column :users, :auth_token, :string, default: ""
    add_index :users, :auth_token, unique: true
  end
end

```

Then run `rake db:migrate`

Now we are all set to do authentication using `auth_token`

In `app/controllers/application_controllers.rb`

First this line to it

```ruby
respond_to :html, :json 

```

this will help rails application respond with both html and json

Then

```ruby
protect_from_forgery with: :null

```

will change this `:null` as we are not dealing with sessions.

now we will add authentication method in application_controller

So, by default Devise uses email as unique field we can also use custom fields, for this case we will be authenticating using user_email and auth_token.

```

 before_filter do
    user_email = params[:user_email].presence
    user       = user_email && User.find_by_email(user_email)

    if user && Devise.secure_compare(user.authentication_token, params[:auth_token])
      sign_in user, store: false
    end
  end

```

> 
Note: Above code is purely based on your logic i am just trying to explain the working example


On line 6 in the above code you can see that i have set `store: false` which will prevent from creating a session on each requests hence we achieved stateless re

