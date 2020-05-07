---
metaTitle: "Ruby on Rails - ActiveRecord Associations"
description: "Polymorphic association, belongs_to, Self-Referential Association, has_one, has_many, The has_many :through association, The has_one :through association, The has_and_belongs_to_many association"
---

# ActiveRecord Associations




## Polymorphic association


This type of association allows an ActiveRecord model to belong to more than one kind of model record. Common example:

```ruby
class Human < ActiveRecord::Base
  has_one :address, :as => :addressable
end

class Company < ActiveRecord::Base
  has_one :address, :as => :addressable
end

class Address < ActiveRecord::Base
  belongs_to :addressable, :polymorphic => true
end

```

Without this association, you’d have all these foreign keys in your Address table but you only would ever have a value for one of them because an address, in this scenario, can only belong to one entity (Human or Company). Here is what it would look like:

```ruby
class Address < ActiveRecord::Base
  belongs_to :human
  belongs_to :company
end

```



## belongs_to


A `belongs_to` association sets up a one-to-one connection with another model, so each instance of the declaring model "belongs to" one instance of the other model.

For example, if your application includes users and posts, and each post can be assigned to exactly one user, you'd declare the post model this way:

```ruby
class Post < ApplicationRecord
  belongs_to :user
end

```

In your table structure you might then have

```ruby
create_table "posts", force: :cascade do |t|
  t.integer  "user_id",  limit: 4
end

```



## Self-Referential Association


Self-referential association is used to associate a model with itself. The most frequent example would be, to manage association between a friend and his follower.

ex.

```ruby
rails g model friendship user_id:references friend_id:integer

```

now you can associate models like;

```ruby
class User < ActiveRecord::Base
  has_many :friendships
  has_many :friends, :through => :friendships
  has_many :inverse_friendships, :class_name => "Friendship", :foreign_key => "friend_id"
  has_many :inverse_friends, :through => :inverse_friendships, :source => :user
end

```

and the other model will look like;

```ruby
class Friendship < ActiveRecord::Base
  belongs_to :user
  belongs_to :friend, :class_name => "User"
end

```



## has_one


A `has_one` association sets up a one-to-one connection with another model, but with different semantics. This association indicates that each instance of a model contains or possesses one instance of another model.

For example, if each user in your application has only one account, you'd declare the user model like this:

```ruby
class User < ApplicationRecord
  has_one :account
end

```

In Active Record, when you have a `has_one` relation, active record ensures that the only one record exists with the foreign key.

Here in our example: In accounts table, there can only be one record with a particular user_id. If you try to associate one more account for the same user, it makes the previous entry's foreign key as null(making it orphan) and creates a new one automatically. It makes the previous entry null even if the save fails for the new entry to maintain consistency.

```ruby
user = User.first
user.build_account(name: "sample")
user.save   [Saves it successfully, and creates an entry in accounts table with user_id 1]
user.build_account(name: "sample1")  [automatically makes the previous entry's foreign key null]
user.save  [creates the new account with name sample 1 and user_id 1]

```



## has_many


A `has_many` association indicates a one-to-many connection with another model. This association generally is located on the other side of a belongs_to association.

This association indicates that each instance of the model has zero or more instances of another model.

For example, in an application containing users and posts, the user model could be declared like this:

```ruby
class User < ApplicationRecord
  has_many :posts
end

```

The table structure of `Post` would remain the same as in the `belongs_to` example; in contrast, `User` would not require any schema changes.

If you want to get the list of all the published posts for the `User`, then you can add the following (i.e. you can add scopes to your association objects):

```ruby
class User < ApplicationRecord
  has_many :published_posts, -> { where("posts.published IS TRUE") }, class_name: "Post"
end

```



## The has_many :through association


A `has_many :through` association is often used to set up a `many-to-many` connection with another model. This association indicates that the declaring model can be matched with zero or more instances of another model by proceeding through a third model.

For example, consider a medical practice where patients make appointments to see physicians. The relevant association declarations could look like this:

```ruby
class Physician < ApplicationRecord
  has_many :appointments
  has_many :patients, through: :appointments
end

class Appointment < ApplicationRecord
  belongs_to :physician
  belongs_to :patient
end

class Patient < ApplicationRecord
  has_many :appointments
  has_many :physicians, through: :appointments
end

```



## The has_one :through association


A `has_one :through` association sets up a `one-to-one` connection with another model. This association indicates that the declaring model can be matched with one instance of another model by proceeding through a third model.

For example, if each `supplier` has one `account`, and each account is associated with one account history, then the supplier model could look like this:

```ruby
class Supplier < ApplicationRecord
  has_one :account
  has_one :account_history, through: :account
end

class Account < ApplicationRecord
  belongs_to :supplier
  has_one :account_history
end

class AccountHistory < ApplicationRecord
  belongs_to :account
end

```



## The has_and_belongs_to_many association


A `has_and_belongs_to_many` association creates a direct `many-to-many` connection with another model, with no intervening model.

For example, if your application includes `assemblies` and `parts`, with each assembly having many parts and each part appearing in many assemblies, you could declare the models this way:

```ruby
class Assembly < ApplicationRecord
  has_and_belongs_to_many :parts
end

class Part < ApplicationRecord
  has_and_belongs_to_many :assemblies
end

```

