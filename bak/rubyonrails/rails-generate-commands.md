---
metaTitle: "Ruby on Rails - Rails generate commands"
description: "Rails Generate Model, Rails Generate Migration, Rails Generate Scaffold, Rails Generate Controller"
---

# Rails generate commands


Usage: `rails generate GENERATOR_NAME [args] [options]`.

Use `rails generate` to list available generators. Alias: `rails g`.



## Rails Generate Model


To generate an `ActiveRecord` model that automagically creates the correct db migrations & boilerplate test files for your model, enter this command

```ruby
rails generate model NAME column_name:column_type

```

'NAME' is the name of the model. 'field' is the name of the column in the DB table and 'type' is the column type (e.g. `name:string` or `body:text`). Check the Remarks section for a list of supported column types.

To setup foreign keys, add `belongs_to:model_name`.

So say you wanted to setup a `User` model that has a `username`, `email` and belongs to a `School`, you would type in the following

```ruby
rails generate model User username:string email:string school:belongs_to

```

`rails g` is shorthand for `rails generate`. This would produce the same result

```ruby
rails g model User username:string email:string school:belongs_to

```



## Rails Generate Migration


You can generate a rails migration file from the terminal using the following command:

```ruby
rails generate migration NAME [field[:type][:index] field[:type][:index]] [options]

```

For a list of all the options supported by the command, you could run the command without any arguments as in `rails generate migration`.

For example, if you want to add `first_name` and `last_name` fields to `users` table, you can do:

```ruby
rails generate migration AddNamesToUsers last_name:string first_name:string

```

Rails will create the following migration file:

```ruby
class AddNamesToUsers < ActiveRecord::Migration[5.0]
  def change
    add_column :users, :last_name, :string
    add_column :users, :first_name, :string
  end
end

```

Now, apply the pending migrations to the database by running the following in the terminal:

```ruby
rake db:migrate

```

```ruby
rails db:migrate

```

> 
**Note:** For even less typing, you can replace `generate` with `g`.




## Rails Generate Scaffold


****DISCLAIMER****: Scaffolding is not recommended unless it's for very conventional CRUD apps/testing. This may generate a lot of files(views/models/controllers) that are not needed in your web application thus causing headaches(bad :().

To generate a fully working scaffold for a new object, including model, controller, views, assets, and tests, use the `rails g scaffold` command.

Then you can run `rake db:migrate` to set up the database table.

Then you can visit [http://localhost:3000/widgets](http://localhost:3000/widgets) and you'll see a fully functional CRUD scaffold.



## Rails Generate Controller


we can create a new controller with `rails g controller` command.

```ruby
$ bin/rails generate controller controller_name

```

The controller generator is expecting parameters in the form of `generate controller ControllerName action1 action2`.

The following creates a Greetings controller with an action of hello.

```ruby
$ bin/rails generate controller Greetings hello

```

You will see the following output

```

    create  app/controllers/greetings_controller.rb
      route  get "greetings/hello"
     invoke  erb
     create    app/views/greetings
     create    app/views/greetings/hello.html.erb
     invoke  test_unit
     create    test/controllers/greetings_controller_test.rb
     invoke  helper
     create    app/helpers/greetings_helper.rb
     invoke  assets
     invoke    coffee
     create      app/assets/javascripts/greetings.coffee
     invoke    scss
     create      app/assets/stylesheets/greetings.scss

```

This generates the following

|File|Example
|---|---|---|---|---|---|---|---|---|---
|Controller File|`greetings_controller.rb`
|View File|`hello.html.erb`
|Functional Test File|`greetings_controller_test.rb`
|View Helper|`greetings_helper.rb`
|JavaScript File|`greetings.coffee`

It will also add routes for each action in `routes.rb`



#### Parameters


|Parameter|Details
|---|---|---|---|---|---|---|---|---|---
|`-h`/`--help`|Get help on any generator command
|`-p`/`--pretend`|Pretend Mode: Run generator but will not create or change any files
|`field:type`|'field-name' is the name of the column to be created and 'type' is the data-type of column. The possible values for 'type' in `field:type` are given in the Remarks section.



#### Remarks


The possible values for 'type' in `field:type` are:

|Data Type|Description
|---|---|---|---|---|---|---|---|---|---
|`:string`|For smaller pieces of text (usually has a character limit of 255)
|`:text`|For longer pieces of text, like a paragraph
|`:binary`|Storing data including images, audios and videos
|`:boolean`|Storing true or false values
|`:date`|Only the date
|`:time`|Only the time
|`:datetime`|Date and time
|`:float`|Storing floats without precision
|`:decimal`|Storing floats with precision
|`:integer`|Storing whole numbers

