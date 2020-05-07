---
metaTitle: "Ruby on Rails - Rails Cookbook - Advanced rails recipes/learnings and coding techniques"
description: "Playing with Tables using rails console, Rails methods - returning boolean values, Handling the error - undefined method `where' for #<Array:0x000000071923f8>"
---

# Rails Cookbook - Advanced rails recipes/learnings and coding techniques



## Playing with Tables using rails console


**View tables**

```ruby
ActiveRecord::Base.connection.tables

```

**Delete any table**.

```

   ActiveRecord::Base.connection.drop_table("users")
    ------------OR----------------------
    ActiveRecord::Migration.drop_table(:users)
    ------------OR---------------------
    ActiveRecord::Base.connection.execute("drop table users")

```

**Remove index from existing column**

```

ActiveRecord::Migration.remove_index(:users, :name => 'index_users_on_country')

```

where `country` is a column name in the migration file with **already** added index in `users` table as shown below:-

```

t.string :country,add_index: true

```

**Remove foreign key constraint**

```ruby
ActiveRecord::Base.connection.remove_foreign_key('food_items', 'menus')

```

where `menus has_many food_items` and their respective migrations too.

**Add column**

```ruby
ActiveRecord::Migration.remove_column :table_name, :column_name

```

for example:-

```

ActiveRecord::Migration.add_column :profiles, :profile_likes, :integer, :default => 0

```



## Rails methods - returning boolean values


Any method in Rails model can return boolean value.

**simple method-**

```

 ##this method return ActiveRecord::Relation
  def check_if_user_profile_is_complete
    User.includes( :profile_pictures,:address,:contact_detail).where("user.id = ?",self)
  end

```

**Again simple method returning boolean value-**

```

 ##this method return Boolean(NOTE THE !! signs before result)
  def check_if_user_profile_is_complete
    !!User.includes( :profile_pictures,:address,:contact_detail).where("user.id = ?",self)
  end

```

So,the same method will now return boolean instead of anything else :).



## Handling the error - undefined method `where' for #<Array:0x000000071923f8>


Sometimes we want to use a `where` query on a a collection of records returned which is not `ActiveRecord::Relation`.Hence we get the above error as `Where` clause is know to `ActiveRecord` and not to `Array`.

There is a precise solution for this by using `Joins`.

**EXAMPLE**:-

> 
Suppose i need to find all user profiles(UserProfile) which are active which is not a user(User) with an id=10.


```ruby
UserProfiles.includes(:user=>:profile_pictures]).where(:active=>true).map(&:user).where.not(:id=>10)

```

So above query will fail after `map` as `map` will return an `array` which will **not** work with `where` clause.

**But using joins,will make it work,**

```ruby
UserProfiles.includes(:user=>:profile_pictures]).where(:active=>true).joins(:user).where.not(:id=>10)

```

As `joins` will output similar records like `map` but they will be `ActiveRecord` and **not** an `Array`.

