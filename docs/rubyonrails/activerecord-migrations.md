---
metaTitle: "Ruby on Rails - ActiveRecord Migrations"
description: "Adding multiple columns to a table, Add a reference column to a table, Rollback migrations, Add a new column with an index, Run specific migration, Redo migrations, Add a new column to a table, Remove an existing column from a table, Running migrations in different environments, Create a new table, Running migrations, Change an existing column’s type, Add column with default value, Create a join table, Create a hstore column, Add a self reference, Create an array column, Changing Tables, Add an unique column to a table, Checking migration status, Forbid null values, Adding a NOT NULL constraint to existing data"
---

# ActiveRecord Migrations




## Adding multiple columns to a table


To add multiple columns to a table, separate `field:type` pairs with spaces when using `rails generate migration` command.

The general syntax is:

```ruby
rails generate migration NAME [field[:type][:index] field[:type][:index]] [options]

```

For example, the following will add `name`, `salary` and `email` fields to the `users` table:

```ruby
rails generate migration AddDetailsToUsers name:string salary:decimal email:string

```

Which generates the following migration:

```ruby
class AddDetailsToUsers < ActiveRecord::Migration[5.0]
  def change
    add_column :users, :name, :string
    add_column :users, :salary, :decimal
    add_column :users, :email, :string
  end
end

```



## Add a reference column to a table


To add a reference to a `team` to the `users` table, run this command:

```ruby
$ rails generate migration AddTeamRefToUsers team:references

```

This generates the following migration:

```ruby
class AddTeamRefToUsers < ActiveRecord::Migration[5.0]
  def change
    add_reference :users, :team, foreign_key: true
  end
end

```

That migration will create a `team_id` column in the `users` table.

If you want to add an appropriate `index` and `foreign_key` on the added column, change the command to `rails generate migration AddTeamRefToUsers team:references:index`. This will generate the following migration:

```ruby
class AddTeamRefToUsers < ActiveRecord::Migration
  def change
    add_reference :users, :team, index: true
    add_foreign_key :users, :teams
  end
end

```

If you want to name your reference column other than what Rails auto generates, add the following to your migration: (E.g.: You might want to call the `User` who created the `Post` as `Author` in the `Post` table)

```ruby
class AddAuthorRefToPosts < ActiveRecord::Migration
  def change
    add_reference :posts, :author, references: :users, index: true
  end
end

```



## Rollback migrations


To `rollback` the latest migration, either by reverting the `change` method or by running the `down` method. Run command:

```ruby
rake db:rollback

```

```ruby
rails db:rollback

```

### Rollback the last 3 migrations

```ruby
rake db:rollback STEP=3

```

```ruby
rails db:rollback STEP=3

```

`STEP` provide the number of migrations to revert.

### Rollback all migrations

```ruby
rake db:rollback VERSION=0

```

```ruby
rails db:rollback VERSION=0

```



## Add a new column with an index


To add a new **indexed** column `email` to the `users` table, run the command:

```ruby
rails generate migration AddEmailToUsers email:string:index

```

This will generate the following migration:

```ruby
class AddEmailToUsers < ActiveRecord::Migration[5.0]
  def change
    add_column :users, :email, :string
    add_index :users, :email
  end
end

```



## Run specific migration


To run a specific migration up or down, use `db:migrate:up` or `db:migrate:down`.

Up a specific migration:

```ruby
rake db:migrate:up VERSION=20090408054555

```

```ruby
rails db:migrate:up VERSION=20090408054555 

```

Down a specific migration:

```ruby
rake db:migrate:down VERSION=20090408054555

```

```ruby
rails db:migrate:down VERSION=20090408054555

```

The version number in the above commands is the numeric prefix in the migration’s filename. For example, to migrate to the migration `20160515085959_add_name_to_users.rb`, you would use `20160515085959` as the version number.



## Redo migrations


You can rollback and then migrate again using the `redo` command. This is basically a shortcut that combines `rollback` and `migrate` tasks.

Run command:

```ruby
rake db:migrate:redo

```

```ruby
rails db:migrate:redo

```

You can use the `STEP` parameter to go back more than one version.

For example, to go back 3 migrations:

```ruby
rake db:migrate:redo STEP=3

```

```ruby
rails db:migrate:redo STEP=3

```



## Add a new column to a table


To add a new column `name` to the `users` table, run the command:

```ruby
rails generate migration AddNameToUsers name

```

This generates the following migration:

```ruby
class AddNameToUsers < ActiveRecord::Migration[5.0]
  def change
    add_column :users, :name, :string
  end
end

```

When the migration name is of the form `AddXXXToTABLE_NAME` followed by list of columns with data types, the generated migration will contain the appropriate `add_column` statements.



## Remove an existing column from a table


To remove existing column `name` from `users` table, run the command:

```ruby
rails generate migration RemoveNameFromUsers name:string

```

This will generate the following migration:

```ruby
class RemoveNameFromUsers < ActiveRecord::Migration[5.0]
  def change
    remove_column :users, :name, :string
  end
end

```

When the migration name is of the form `RemoveXXXFromYYY` followed by list of columns with data types then the generated migration will contain the appropriate `remove_column` statements.

While it’s not required to specify the data type (e.g. `:string`) as a parameter to `remove_column`, it is highly recommended. If the data type is **not** specified, then the migration will not be reversible.



## Running migrations in different environments


To run migrations in the `test` environment, run this shell command:

```ruby
rake db:migrate RAILS_ENV=test

```

Starting in Rails 5.0, you can use `rails` instead of `rake`:

```ruby
rails db:migrate RAILS_ENV=test

```



## Create a new table


To create a new `users` table with the columns `name` and `salary`, run the command:

```ruby
rails generate migration CreateUsers name:string salary:decimal

```

This will generate the following migration:

```ruby
class CreateUsers < ActiveRecord::Migration[5.0]
  def change
    create_table :users do |t|
      t.string :name
      t.decimal :salary  
    end
  end
end

```

When the migration name is of the form `CreateXXX` followed by list of columns with data types, then a migration will be generated that creates the table `XXX` with the listed columns.



## Running migrations


Run command:

```ruby
rake db:migrate

```

```ruby
rails db:migrate

```

Specifying target version will run the required migrations (up, down, change) until it has reached the specified version. Here, `version number` is the numerical prefix on the migration's filename.

```ruby
rake db:migrate VERSION=20080906120000

```

```ruby
rails db:migrate VERSION=20080906120000

```



## Change an existing column’s type


To modify an existing column in Rails with a migration, run the following command:

```ruby
rails g migration change_column_in_table

```

This will create a new migration file in `db/migration` directory (if it doesn’t exist already), which will contain the file prefixed with timestamp and migration file name which contains the below content:

```ruby
def change
  change_column(:table_name, :column_name, :new_type)
end

```

[Rails Guide – Changing Columns](http://guides.rubyonrails.org/active_record_migrations.html#changing-columns)

### A longer but safer method

The above code prevents the user from ever rolling back the migration. You can avoid this problem by splitting the `change` method into separate `up` and `down` methods:

```ruby
def up
  change_column :my_table, :my_column, :new_type
end

def down
  change_column :my_table, :my_column, :old_type
end

```



## Add column with default value


The following example adds a column `admin` to the `users` table, and gives that column the default value `false`.

```ruby
class AddDetailsToUsers < ActiveRecord::Migration[5.0]
  def change
    add_column :users, :admin, :boolean, default: false
  end
end

```

Migrations with defaults might take a long time in large tables with for example PostgreSQL. This is because each row will have to be updated with the default value for the newly added column. To circumvent this (and reduce downtime during deployments), you can split your migration into three steps:

1. Add a `add_column`-migration similar to the one above, but set no default
1. Deploy and update the column in a rake task or on the console while your app is running. Make sure your application already writes data to that colum for new/updated rows.
1. Add another `change_column` migration, which then changes the default of that column to the desired default value



## Create a join table


To create a join table between `students` and `courses`, run the command:

```ruby
$ rails g migration CreateJoinTableStudentCourse student course

```

This will generate the following migration:

```ruby
class CreateJoinTableStudentCourse < ActiveRecord::Migration[5.0]
  def change
    create_join_table :students, :courses do |t|
      # t.index [:student_id, :course_id]
      # t.index [:course_id, :student_id]
    end
  end
end

```



## Create a hstore column


`Hstore` columns can be useful to store settings. They are available in PostgreSQL databases after you enabled the extension.

```ruby
class CreatePages < ActiveRecord::Migration[5.0]
  def change
    create_table :pages do |t|
      enable_extension 'hstore' unless extension_enabled?('hstore')
      t.hstore :settings
      t.timestamps
    end
  end
end

```



## Add a self reference


A self reference can be useful to build a hierarchical tree. This can be achieved with `add_reference` in a migration.

```ruby
class AddParentPages < ActiveRecord::Migration[5.0]
  def change
    add_reference :pages, :pages
  end
end

```

The foreign key column will be `pages_id`. If you want to decide about the foreign key column name, you have to create the column first and add the reference after.

```ruby
class AddParentPages < ActiveRecord::Migration[5.0]
  def change
    add_column :pages, :parent_id, :integer, null: true, index: true
    add_foreign_key :pages, :pages, column: :parent_id
  end
end

```



## Create an array column


An `array` column is supported by PostgreSQL. Rails will automatically convert a PostgreSQL array to a Ruby array, and vice-versa.

Create a table with an `array` column:

```ruby
create_table :products do |t|
  t.string :name
  t.text :colors, array: true, default: []
end

```

Add an `array` column to an existing table:

```ruby
add_column :products, :colors, array: true, default: []

```

Add an index for an `array` column:

```ruby
add_index :products, :colors, using: 'gin'

```



## Changing Tables


If you have created a table with some wrong schema, then the easiest way to change the columns and their properties is `change_table`. Review the following example:

```ruby
change_table :orders do |t|
  t.remove :ordered_at # removes column ordered_at
  t.string :skew_number # adds a new column 
  t.index  :skew_number #creates an index
  t.rename :location, :state #renames location column to state
end

```

The above migration changes a table `orders`. Here is a line-by-line description of the changes:

1. `t.remove :ordered_at` removes the column `ordered_at` from the table `orders`.
1. `t.string :skew_number` adds a new string-type column named `skew_number` in the `orders` table.
1. `t.index :skew_number` adds an index on the `skew_number` column in the `orders` table.
1. `t.rename :location, :state` renames the `location` column in the `orders` table to `state`.



## Add an unique column to a table


To add a new **unique** column `email` to `users`, run the following command:

```ruby
rails generate migration AddEmailToUsers email:string:uniq

```

This will create the following migration:

```ruby
class AddEmailToUsers < ActiveRecord::Migration[5.0]
  def change
    add_column :users, :email, :string
    add_index :users, :email, unique: true
  end
end

```



## Checking migration status


We can check the status of migrations by running

```ruby
rake db:migrate:status

```

```ruby
rails db:migrate:status

```

The output will look like this:

```ruby
Status   Migration ID    Migration Name
--------------------------------------------------
up     20140711185212  Create documentation pages
up     20140724111844  Create nifty attachments table
up     20140724114255  Create documentation screenshots
up     20160213170731  Create owners
up     20160218214551  Create users
up     20160221162159  ********** NO FILE **********
up     20160222231219  ********** NO FILE **********

```

Under the status field, `up` means the migration has been run and `down` means that we need to run the migration.



## Forbid null values


To forbid `null` values in your table columns, add the `:null` parameter to your migration, like this:

```ruby
class AddPriceToProducts < ActiveRecord::Migration
  def change
    add_column :products, :float, null: false
  end
end

```



## Adding a NOT NULL constraint to existing data


Say you want to add a foreign key `company_id` to the `users` table, and you want to have a `NOT NULL` constraint on it. If you already have data in `users`, you will have to do this in multiple steps.

```ruby
class AddCompanyIdToUsers < ActiveRecord::Migration
  def up
    # add the column with NULL allowed
    add_column :users, :company_id, :integer

    # make sure every row has a value
    User.find_each do |user|
      # find the appropriate company record for the user
      # according to your business logic
      company = Company.first
      user.update!(company_id: company.id)
    end

    # add NOT NULL constraint
    change_column_null :users, :company_id, false
  end

  # Migrations that manipulate data must use up/down instead of change
  def down
    remove_column :users, :company_id
  end
end

```



#### Parameters


|Column type|Description
|---|---|---|---|---|---|---|---|---|---
|`:primary_key`|Primary key
|`:string`|Shorter string datatype. Allows `limit` option for maximum number of characters.
|`:text`|Longer amount of text. Allows `limit` option for maximum number of bytes.
|`:integer`|Integer. Allows `limit` option for maximum number of bytes.
|`:bigint`|Larger integer
|`:float`|Float
|`:decimal`|Decimal number with variable precision. Allows `precision` and `scale` options.
|`:numeric`|Allows `precision` and `scale` options.
|`:datetime`|DateTime object for dates/times.
|`:time`|Time object for times.
|`:date`|Date object for dates.
|`:binary`|Binary data. Allows `limit` option for maximum number of bytes.
|`:boolean`|Boolean



#### Remarks


<li>
Most migration files live in `db/migrate/` directory. They’re identified by a UTC timestamp at the beginning of their file name: `YYYYMMDDHHMMSS_create_products.rb`.
</li>
<li>
The `rails generate` command can be shortened to `rails g`.
</li>
<li>
If a `:type` is not passed to a field, it defaults to a string.
</li>

