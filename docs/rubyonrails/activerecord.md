---
metaTitle: "Ruby on Rails - ActiveRecord"
description: "Creating a Model via generator, Creating a Model manually, Introduction to Callbacks, Manually Testing Your Models, Creating A Migration, Create a Join Table using Migrations, Using a model instance to update a row"
---

# ActiveRecord



## Creating a Model via generator


Ruby on Rails provides a `model` generator you can use to create ActiveRecord models. Simply use `rails generate model` and provide the model name.

```ruby
$ rails g model user

```

In addition to the model file in `app/models`, the generator will also create:

- the Test in `test/models/user_test.rb`
- the Fixtures in `test/fixtures/users.yml`
- the database Migration in `db/migrate/XXX_create_users.rb`

You can also generate some fields for the model when generating it.

```ruby
$ rails g model user email:string sign_in_count:integer birthday:date

```

This will create the columns email, sign_in_count and birthday in your database, with the appropriate types.



## Creating a Model manually


While using scaffolding is a fast and easy if you are new to Rails or you are creating a new application, later it can be useful just to do it on your own ato avoid the need to go through the scaffold-generated code to slim it down (remove unused parts, etc.).

Creating a model can be as simple as creating a file under `app/models`.

The most simple model, in `ActiveRecord`, is a class that extends `ActiveRecord::Base`.

```ruby
class User < ActiveRecord::Base
end

```

Model files are stored in `app/models/`, and the file name corresponds to the singular name of the class:

```ruby
# user
app/models/user.rb

# SomeModel
app/models/some_model.rb

```

The class will inherit all the ActiveRecord features: query methods, validations, callbacks, etc.

```ruby
# Searches the User with ID 1
User.find(1)

```

Note: Make sure that the table for the corresponding model exists. If not, you can create the table by creating a [Migration](http://stackoverflow.com/documentation/ruby-on-rails/828/activerecord/2879/creating-a-migration)

You can generate a model and it's migration by terminal from the following command

`rails g model column_name1:data_type1, column_name2:data_type2, ...`

and  can also assign foreign key(relationship) to the model by following command

```ruby
rails g model column_name:data_type, model_name:references

```



## Introduction to Callbacks


A callback is a method that gets called at specific moments of an object's lifecycle (right before or after creation, deletion, update, validation, saving or loading from the database).

For instance, say you have a listing that expires within 30 days of creation.

One way to do that is like this:

```ruby
class Listing < ApplicationRecord
  after_create :set_expiry_date

  private

  def set_expiry_date
    expiry_date = Date.today + 30.days
    self.update_column(:expires_on, expiry_date)
  end
end

```

All of the available methods for callbacks are as follows, in the same order that they are called during the operation of each object:

**Creating an Object**

- before_validation
- after_validation
- before_save
- around_save
- before_create
- around_create
- after_create
- after_save
- after_commit/after_rollback

**Updating an Object**

- before_validation
- after_validation
- before_save
- around_save
- before_update
- around_update
- after_update
- after_save
- after_commit/after_rollback

**Destroying an Object**

- before_destroy
- around_destroy
- after_destroy
- after_commit/after_rollback

****NOTE:** after_save runs both on create and update, but always after the more specific callbacks after_create and after_update, no matter the order in which the macro calls were executed.**



## Manually Testing Your Models


Testing your Active Record models through your command line interface is simple.
Navigate to the app directory in your terminal and type in `rails console` to start the Rails console. From here, you can run active record methods on your database.

For example, if you had a database schema with a Users table having a `name:string` column and `email:string`, you could run:

```ruby
User.create name: "John", email: "john@example.com"

```

Then, to show that record, you could run:

```ruby
User.find_by email: "john@example.com"

```

Or if this is your first or only record, you could simply get the first record by running:

```ruby
User.first

```



## Creating A Migration


### Add/remove fields in existing tables

Create a migration by running:

```ruby
rails generate migration AddTitleToCategories title:string

```

This will create a migration that adds a `title` column to a `categories` table:

```ruby
class AddTitleToCategories < ActiveRecord::Migration[5.0]
  def change
    add_column :categories, :title, :string
  end
end

```

Similarly, you can generate a migration to remove a column: `rails generate migration RemoveTitleFromCategories title:string`

This will create a migration that removes a `title` column from the `categories` table:

```ruby
class RemoveTitleFromCategories < ActiveRecord::Migration[5.0]
  def change
    remove_column :categories, :title, :string
  end
end

```

While, strictly speaking, specifying **type** (`:string` in this case) is **not necessary** for removing a column, **it's helpful**, since it provides the information necessary for **rolling it back**.

### Create a table

Create a migration by running:

```ruby
rails g CreateUsers name bio

```

Rails recognizes the intent to create a table from the `Create` prefix, the rest of the migration name will be used as a table name. The given example generates the following:

```ruby
class CreateUsers < ActiveRecord::Migration
  def change
    create_table :users do |t|
      t.string :name
      t.string :bio
    end
  end
end

```

Notice that the creation command didn't specify types of columns and the default `string` was used.

### Create a join table

Create a migration by running:

```ruby
rails g CreateJoinTableParticipation user:references group:references

```

Rails detects the intent to create a join table by finding `JoinTable` in migration name. Everything else is determined from the names of the fields you give after the name.

```ruby
class CreateJoinTableParticipation < ActiveRecord::Migration
  def change
    create_join_table :users, :groups do |t|
      # t.index [:user_id, :group_id]
      # t.index [:group_id, :user_id]
    end
  end
end

```

Uncomment the necessary `index` statements and delete the rest.

### Precedence

Notice that the example migration name `CreateJoinTableParticipation` matches the rule for table creation: it has a `Create` prefix. But it did not generate a simple `create_table`. This is because migration generator ([source code](https://github.com/rails/rails/blob/ff044c3c3a0a85d26d7c000abeeed835a3bee0bf/activerecord/lib/rails/generators/active_record/migration/migration_generator.rb#L19)) uses a **first match** of the following list:

<li>
`(Add|Remove)<ignored>(To|From)<table_name>`
</li>
<li>
`<ignored>JoinTable<ignored>`
</li>
<li>
`Create<table_name>`
</li>



## Create a Join Table using Migrations


Specially useful for `has_and_belongs_to_many` relation, you can manually create a join table using the `create_table` method.
Suppose you have two models `Tags` and `Proyects`, and you'd like to associate them using a `has_and_belongs_to_many` relation. You need a join table to associate instances of both classes.

```ruby
class CreateProjectsTagsJoinTableMigration < ActiveRecord::Migration
  def change
    create_table :projects_tags, id: false do |t|
      t.integer :project_id
      t.integer :tag_id
    end
  end
end

```

The actual name of the table needs to follow this convention: the model which alphabetically precedes the other must go first. **P**roject preceds **T**ags so the name of the table is projects_tags.

Also since the purpose of this table is to route the association between the instances of two models, the actual id of every record in this table is not necessary. You specify this by passing `id: false`

Finally, as is convention in Rails, the table name must be the compound plural form of the individual models, but the column of the table must be in singular form.



## Using a model instance to update a row


Let's say you have a `User` model

```ruby
class User < ActiveRecord::Base
end

```

Now to update the `first_name` and `last_name` of a user with `id = 1`, you can write the following code.

```ruby
user = User.find(1)
user.update(first_name: 'Kashif', last_name: 'Liaqat')

```

Calling `update` will attempt to update the given attributes in a single transaction, returning `true` if successful and `false` if not.

