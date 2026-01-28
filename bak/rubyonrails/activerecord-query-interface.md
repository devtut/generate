---
metaTitle: "Ruby on Rails - ActiveRecord Query Interface"
description: ".where, .where with an array, Scopes, Get first and last record, where.not, Ordering, Includes, .find_by, .delete_all, ActiveRecord case insensitive search, .group and .count, .distinct (or .uniq), Joins, Limit and Offset, ActiveRecord Bang (!) methods"
---

# ActiveRecord Query Interface


ActiveRecord is the M in MVC which is the layer of the system responsible for representing business data and logic. The technique that connects the rich objects of an application to tables in a relational database management system is **O**bject **R**elational **M**apper(**ORM**).

ActiveRecord will perform queries on the database for you and is compatible with most database systems. Regardless of which database system you're using, the ActiveRecord method format will always be the same.



## .where


The `where` method is available on any `ActiveRecord` model and allows querying the database for a set of records matching the given criteria.

The `where` method accepts a hash where the keys correspond to the column names on the table that the model represents.

As a simple example, we will use the following model:

```ruby
class Person < ActiveRecord::Base
  #attribute :first_name, :string
  #attribute :last_name, :string
end

```

To find all people with the first name of `Sven`:

```ruby
people = Person.where(first_name: 'Sven')
people.to_sql # "SELECT * FROM people WHERE first_name='Sven'"

```

To find all people with the first name of `Sven` and last name of `Schrodinger`:

```ruby
people = Person.where(first_name: 'Sven', last_name: 'Schrodinger')
people.to_sql # "SELECT * FROM people WHERE first_name='Sven' AND last_name='Schrodinger'"

```

In the above example, the sql output shows that records will only be returned if both the `first_name` and the `last_name` match.

**query with OR condition**

To find records with `first_name == 'Bruce'` OR `last_name == 'Wayne'`

```ruby
User.where('first_name = ? or last_name = ?', 'Bruce', 'Wayne')
# SELECT "users".* FROM "users" WHERE (first_name = 'Bruce' or last_name = 'Wayne')

```



## .where with an array


The `where` method on any ActiveRecord model can be used to generate SQL of the form `WHERE column_name IN (a, b, c, ...)`.  This is achieved by passing an array as argument.

As a simple example, we will use the following model:

```ruby
class Person < ActiveRecord::Base
  #attribute :first_name, :string
  #attribute :last_name, :string
end

people = Person.where(first_name: ['Mark', 'Mary'])
people.to_sql # "SELECT * FROM people WHERE first_name IN ('Mark', 'Mary')"

```

If the array contains a `nil`, the SQL will be modified to check if the column is `null`:

```ruby
people = Person.where(first_name: ['Mark', 'Mary', nil])
people.to_sql # "SELECT * FROM people WHERE first_name IN ('Mark', 'Mary') OR first_name IS NULL"

```



## Scopes


Scopes act as predefined filters on `ActiveRecord` models.

A scope is defined using the `scope` class method.

As a simple example, we will use the following model:

```ruby
class Person < ActiveRecord::Base
  #attribute :first_name, :string
  #attribute :last_name, :string
  #attribute :age, :integer

  # define a scope to get all people under 17
  scope :minors, -> { where(age: 0..17) }

  # define a scope to search a person by last name
  scope :with_last_name, ->(name) { where(last_name: name) }

end

```

Scopes can be called directly off the model class:

```ruby
minors = Person.minors

```

Scopes can be chained:

```ruby
peters_children = Person.minors.with_last_name('Peters')

```

The `where` method and other query type methods can also be chained:

```ruby
mary_smith = Person.with_last_name('Smith').where(first_name: 'Mary')

```

Behind the scenes, scopes are simply syntactic sugar for a standard class method. For example, these methods are functionally identical:

```ruby
scope :with_last_name, ->(name) { where(name: name) }

# This ^ is the same as this:

def self.with_last_name(name)
  where(name: name)
end

```

**Default Scope**

in your model to set a default scope for all operations on the model.

> 
There is one notable difference between the `scope` method and a class method: `scope`-defined scopes will **always** return an `ActiveRecord::Relation`, even if the logic within returns nil. Class methods, however, have no such safety net and can break chainability if they return something else.




## Get first and last record


Rails have very easy way to get `first` and `last` record from database.

To get the `first` record from `users` table we need to type following command:

```ruby
User.first

```

It will generate following `sql` query:

```ruby
SELECT  `users`.* FROM `users`  ORDER BY `users`.`id` ASC LIMIT 1

```

And will return following record:

```ruby
#<User:0x007f8a6db09920 id: 1, first_name: foo, created_at: Thu, 16 Jun 2016 21:43:03 UTC +00:00, updated_at: Thu, 16 Jun 2016 21:43:03 UTC +00:00 >

```

To get the `last` record from `users` table we need to type following command:

```ruby
User.last

```

It will generate following `sql` query:

```ruby
SELECT  `users`.* FROM `users`  ORDER BY `users`.`id` DESC LIMIT 1

```

And will return following record:

```ruby
#<User:0x007f8a6db09920 id: 10, first_name: bar, created_at: Thu, 16 Jun 2016 21:43:03 UTC +00:00, updated_at: Thu, 16 Jun 2016 21:43:03 UTC +00:00 >

```

Passing an integer to **first** and **last** method creates a **LIMIT** query and returns array of objects.

```ruby
User.first(5)

```

It will generate following `sql` query.

```ruby
SELECT  "users".* FROM "users"  ORDER BY "users"."id" ASC LIMIT 5

```

And

```ruby
User.last(5)

```

It will generate following `sql` query.

```ruby
SELECT  "users".* FROM "users"  ORDER BY "users"."id" DESC LIMIT 5

```



## where.not


`where` clauses can be negated using the `where.not` syntax:

Supported by ActiveRecord 4.0 and later.



## Ordering


You can order **ActiveRecord** query results by using **`.order`**:

```ruby
User.order(:created_at)
#=> => [#<User id: 2, created_at: "2015-08-12 21:36:23">, #<User id: 11, created_at: "2015-08-15 10:21:48">]

```

If not specified, ordering will be performed in ascending order. You can specify it by doing:

```ruby
User.order(created_at: :asc)
#=> => [#<User id: 2, created_at: "2015-08-12 21:36:23">, #<User id: 11, created_at: "2015-08-15 10:21:48">]

User.order(created_at: :desc)
#=> [#<User id: 7585, created_at: "2016-07-13 17:15:27">, #<User id: 7583, created_at: "2016-07-13 16:51:18">]

```

**`.order`** also accepts a string, so you could also do

```ruby
User.order("created_at DESC")
#=> [#<User id: 7585, created_at: "2016-07-13 17:15:27">, #<User id: 7583, created_at: "2016-07-13 16:51:18">]

```

As the string is raw SQL, you can also specify a table and not only an attribute. Assuming you want to order `users` according to their `role` name, you can do this:

```ruby
Class User < ActiveRecord::Base
    belongs_to :role
end

Class Role < ActiveRecord::Base
  has_many :users
end

User.includes(:role).order("roles.name ASC")

```

The `order` scope can also accept an Arel node:

```ruby
User.includes(:role).order(User.arel_table[:name].asc)

```



## Includes


ActiveRecord with `includes` ensures that all of the specified associations are loaded using the minimum possible number of queries. So when querying a table for data with an associated table, both tables are loaded into memory.

```ruby
@authors = Author.includes(:books).where(books: { bestseller: true } )

# this will print  results without additional db hitting
@authors.each do |author| 
  author.books.each do |book|
    puts book.title
  end
end

```

`Author.joins(:books).where(books: { bestseller: true } )` will load only **authors** with conditions into memory **without loading books**. Use `joins` when additional information about nested associations isn't required.

```ruby
@authors = Author.joins(:books).where(books: { bestseller: true } )

# this will print results without additional queries
@authors.each { |author| puts author.name }

# this will print results with additional db queries
@authors.each do |author| 
  author.books.each do |book|
    puts book.title
  end
end

```



## .find_by


You can find records by any field in your table using `find_by`.

So, if you have a `User` model with a `first_name` attribute you can do:

```ruby
User.find_by(first_name: "John")
#=> #<User id: 2005, first_name: "John", last_name: "Smith">

```

Mind that `find_by` doesn't throw any exception by default. If the result is an empty set, it returns `nil` instead of `find`.

If the exception is needed may use `find_by!` that raises an `ActiveRecord::RecordNotFound` error like `find`.



## .delete_all


If you need to delete a lot of records quickly, **ActiveRecord** gives **`.delete_all`** method. to be called directly on a model, to delete all records in that table, or a collection. Beware though, as **`.delete_all`** does not instantiate any object hence does not provide any callback (`before_*` and `after_destroy` don't get triggered).

```ruby
User.delete_all
#=> 39  <-- .delete_all return the number of rows deleted

User.where(name: "John").delete_all 

```



## ActiveRecord case insensitive search


If you need to search an ActiveRecord model for similar values, you might be tempted to use `LIKE` or `ILIKE` but this isn't portable between database engines.  Similarly, resorting to always downcasing or upcasing can create performance issues.

You can use ActiveRecord's underlying Arel `matches` method to do this in a safe way:

```ruby
addresses = Address.arel_table
Address.where(addresses[:address].matches("%street%"))

```

Arel will apply the appropriate LIKE or ILIKE construct for the database engine configured.



## .group and .count


We have a `Product` model and we want to group them by their `category`.

```ruby
Product.select(:category).group(:category)

```

This will query the database as follows:

```ruby
SELECT "product"."category" FROM "product" GROUP BY "product"."category"

```

Make sure that the grouped field is also selected. Grouping is especially useful for counting the occurrence - in this case - of `categories`.

```ruby
Product.select(:category).group(:category).count

```

As the query shows, it will use the database for counting, which is much more efficient, than retrieving all record first and do the counting in the code:

```ruby
SELECT COUNT("products"."category") AS count_categories, "products"."category" AS products_category FROM "products" GROUP BY "products"."category"

```



## .distinct (or .uniq)


If you want to remove duplicates from a result, you can use `.distinct()`:

```ruby
Customers.select(:country).distinct

```

This queries the database as follows:

```ruby
SELECT DISTINCT "customers"."country" FROM "customers"

```

`.uniq()` has the same effect. With Rails 5.0 it got deprecated and it will be removed from Rails with version 5.1. The reason is, that the word `unique` doesn't have the same meaning as distinct and it can be misleading. Furthermore `distinct` is closer to the SQL syntax.



## Joins


`joins()` allows you to join tables to your current model. For ex.

```ruby
User.joins(:posts)

```

will produce the following SQL query:

```ruby
"SELECT "users".* FROM "users" INNER JOIN "posts" ON "posts"."user_id" = "users"."id""

```

Having table joined, you will have access to it:

```ruby
User.joins(:posts).where(posts: { title: "Hello world" })

```

Pay attention on plural form. If your relation is `:has_many`, then the `joins()` argument should be pluralized. Otherwise, use singular.

Nested `joins`:

```ruby
User.joins(posts: :images).where(images: { caption: 'First post' })

```

which will produce:

```ruby
"SELECT "users".* FROM "users" INNER JOIN "posts" ON "posts"."user_id" = "users"."id" INNER JOIN "images" ON "images"."post_id" = "images"."id""

```



## Limit and Offset


You can use `limit` to tell the number of records to be fetched, and use `offset` to tell the number of records to skip before starting to return the records.

For Example

```ruby
User.limit(3) #returns first three records

```

It will generate following sql query.

```ruby
"SELECT  `users`.* FROM `users` LIMIT 3"

```

As offset is not mentioned in above query so it will return first three records.

```ruby
User.limit(5).offset(30) #returns 5 records starting from 31th i.e from 31 to 35

```

It will generate following sql query.

```ruby
"SELECT  `users`.* FROM `users` LIMIT 5 OFFSET 30"

```



## ActiveRecord Bang (!) methods


If you need an **ActiveRecord** method to raise an exception instead of a `false` value in case of failure, you can add **`!`** to them. This is very important. As some exceptions/failures are hard to catch if you don't use ! on them. I recommended doing this in your development cycle to write all your ActiveRecord code this way to save you time and trouble.

```ruby
Class User < ActiveRecord::Base
  validates :last_name, presence: true
end

User.create!(first_name: "John")
#=> ActiveRecord::RecordInvalid: Validation failed: Last name can't be blank

```

The **ActiveRecord** methods which accept a **bang** (`!`) are:

- `.create!`
- `.take!`
- `.first!`
- `.last!`
- `.find_by!`
- `.find_or_create_by!`
- `#save!`
- `#update!`
- all AR dynamic finders

