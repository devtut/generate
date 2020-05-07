---
metaTitle: "Ruby on Rails - Friendly ID"
description: "Rails Quickstart"
---

# Friendly ID


FriendlyId is the "Swiss Army bulldozer" of slugging and permalink plugins for Active Record. It lets you create pretty URLs and work with human-friendly strings as if they were numeric ids.
With FriendlyId, it's easy to make your application use URLs like:

[http://example.com/states/washington](http://example.com/states/washington)



## Rails Quickstart


```ruby
rails new my_app
cd my_app

```

### Gemfile

```ruby
gem 'friendly_id', '~> 5.1.0' # Note: You MUST use 5.0.0 or greater for Rails 4.0+
rails generate friendly_id
rails generate scaffold user name:string slug:string:uniq
rake db:migrate

```

### edit app/models/user.rb

```ruby
class User < ApplicationRecord
  extend FriendlyId
  friendly_id :name, use: :slugged
end

User.create! name: "Joe Schmoe"

# Change User.find to User.friendly.find in your controller
User.friendly.find(params[:id])

```

### 

```ruby
rails server
GET http://localhost:3000/users/joe-schmoe

```

### 

```ruby
# If you're adding FriendlyId to an existing app and need
# to generate slugs for existing users, do this from the
# console, runner, or add a Rake task:
User.find_each(&:save)

Finders are no longer overridden by default. If you want to do friendly finds, you must do Model.friendly.find rather than Model.find. You can however restore FriendlyId 4-style finders by using the :finders addon


friendly_id :foo, use: :slugged # you must do MyClass.friendly.find('bar')
#or...
friendly_id :foo, use: [:slugged, :finders] # you can now do MyClass.find('bar')

```

A new "candidates" functionality which makes it easy to set up a list of alternate slugs that can be used to uniquely distinguish records, rather than appending a sequence. For example:

```ruby
class Restaurant < ActiveRecord::Base
  extend FriendlyId
  friendly_id :slug_candidates, use: :slugged

  # Try building a slug based on the following fields in
  # increasing order of specificity.
  def slug_candidates
    [
      :name,
      [:name, :city],
      [:name, :street, :city],
      [:name, :street_number, :street, :city]
    ]
  end
end

```

Set slug limit length using friendly_id gem?

```ruby
def normalize_friendly_id(string)
   super[0..40]
end

```

