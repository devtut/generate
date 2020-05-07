---
metaTitle: "Ruby on Rails - Mongoid"
description: "Fields, Installation, Creating a Model, Classic Associations, Embedded Associations, Database Calls"
---

# Mongoid



## Fields


As per the [Mongoid Documentation](https://docs.mongodb.com/ecosystem/tutorial/mongoid-documents/#fields), there are 16 valid field types:

- Array
- BigDecimal
- Boolean
- Date
- DateTime
- Float
- Hash
- Integer
- BSON::ObjectId
- BSON::Binary
- Range
- Regexp
- String
- Symbol
- Time
- TimeWithZone

To add a field (let's call it `name` and have it be a `String`), add this to your model file:

```ruby
field :name, type: String

```

To set a default value, just pass in the `default` option:

```ruby
field :name, type: String, default: ""

```



## Installation


First, add `Mongoid` to your `Gemfile`:

```ruby
gem "mongoid", "~> 4.0.0"

```

and then run `bundle install`. Or just run:

```ruby
$ gem install mongoid

```

After installation, run the generator to create the config file:

```ruby
$ rails g mongoid:config

```

which will create the file `(myapp)/config/mongoid.yml`.



## Creating a Model


Create a model (lets call it `User`) by running:

```ruby
$ rails g model User

```

which will generate the file `app/models/user.rb`:

```ruby
class User
  include Mongoid::Document

end

```

This is all you need to have a model (albeit nothing but an `id` field). Unlike `ActiveRecord`, there is no migration files. All the database information for the model is contained in the model file.

Timestamps are not automatically included in your model when you generate it. To add `created_at` and `updated_at` to your model, add

```ruby
include Mongoid::Timestamps

```

to your model underneath `include Mongoid::Document` like so:

```ruby
class User
  include Mongoid::Document
  include Mongoid::Timestamps

end

```



## Classic Associations


Mongoid allows the classic `ActiveRecord` associations:

- One-to-one: `has_one` / `belongs_to`
- One-to-many: `has_many` / `belongs_to`
- Many-to-many: `has_and_belongs_to_many`

To add an association (lets say the User `has_many` posts), you can add this to your `User` model file:

```ruby
has_many :posts

```

and this to your `Post` model file:

```ruby
belongs_to :user

```

This will add a `user_id` field in your `Post` model, add a `user` method to your `Post` class, and add a `posts` method to your `User` class.



## Embedded Associations


Mongoid allows Embedded Associations:

- One-to-one: `embeds_one` / `embedded_in`
- One-to-many: `embeds_many` / `embedded_in`

To add an association (lets say the User `embeds_many` addresses), add this to your `User` file:

```ruby
embeds_many :addresses

```

and this to your `Address` model file:

```ruby
embedded_in :user

```

This will embed `Address` in your `User` model, adding a `addresses` method to your `User` class.



## Database Calls


Mongoid tries to have similar syntax to `ActiveRecord` when it can. It supports these calls (and many more)

```ruby
User.first #Gets first user from the database

User.count #Gets the count of all users from the database

User.find(params[:id]) #Returns the user with the id found in params[:id]

User.where(name: "Bob") #Returns a Mongoid::Criteria object that can be chained
                        #with other queries (like another 'where' or an 'any_in')
                        #Does NOT return any objects from database

User.where(name: "Bob").entries #Returns all objects with name "Bob" from database

User.where(:name.in => ['Bob', 'Alice']).entries #Returns all objects with name "Bob" or "Alice" from database

User.any_in(name: ["Bob", "Joe"]).first #Returns the first object with name "Bob" or "Joe"
User.where(:name => 'Bob').exists? # will return true if there is one or more users with name bob

```

