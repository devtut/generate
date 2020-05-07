---
metaTitle: "Ruby on Rails - Single Table Inheritance"
description: "Basic example, Custom inheritance column, Rails model with type column and without STI"
---

# Single Table Inheritance


Single Table Inheritance (STI) is a design pattern which is based on the idea of saving the data of multiple models which are all inheriting from the same Base model, into a single table in the database.



## Basic example


First we need a table to hold our data

```ruby
class CreateUsers < ActiveRecord::Migration
  def change
    create_table :users do |t|
      t.string :name
      t.string :password
      t.string :type # <- This makes it an STI

      t.timestamps
    end
  end
end

```

Then lets create some models

```ruby
class User < ActiveRecord::Base
   validates_presence_of :password
   # This is a parent class. All shared logic goes here
end

class Admin < User
   # Admins must have more secure passwords than regular users
   # We can add it here
   validates :custom_password_validation
end

class Guest < User
   # Lets say that we have a guest type login. 
   # It has a static password that cannot be changed
   validates_inclusion_of :password, in: ['guest_password']
end

```

When you do a `Guest.create(name: 'Bob')` ActiveRecord will translate this to create an entry in the Users table with `type: 'Guest'`.

When you retrieve the record `bob = User.where(name: 'Bob').first` the object returned will be an instance of `Guest`, which can be forcibly treated as a User with `bob.becomes(User)`

becomes is most useful when dealing with shared partials or routes/controllers of the superclass instead of the subclass.



## Custom inheritance column


By default STI model class name is stored in a column named `type`. But its name can be changed by overriding `inheritance_column` value in a base class. E.g.:

```ruby
class User < ActiveRecord::Base
  self.inheritance_column = :entity_type # can be string as well
end

class Admin < User; end

```

Migration in this case will look as follows:

```ruby
class CreateUsers < ActiveRecord::Migration
  def change
    create_table :users do |t|
      t.string :name
      t.string :password
      t.string :entity_type

      t.timestamps
    end
  end
end

```

When you do `Admin.create`, this record will be saved in the users table with `entity_type = "Admin"`



## Rails model with type column and without STI


Having `type` column in a Rails model without invoking STI can be achieved by assigning `:_type_disabled` to `inheritance_column`:

```ruby
class User < ActiveRecord::Base
  self.inheritance_column = :_type_disabled
end

```

