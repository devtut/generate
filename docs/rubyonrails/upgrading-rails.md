---
metaTitle: "Ruby on Rails - Upgrading Rails"
description: "Upgrading from Rails 4.2 to Rails 5.0"
---

# Upgrading Rails



## Upgrading from Rails 4.2 to Rails 5.0


**Note: Before upgrading your Rails app, always make sure to save your code on a version control system, such as Git.**

To upgrade from Rails 4.2 to Rails 5.0, you must be using Ruby 2.2.2 or newer. After upgrading your Ruby version if required, go to your Gemfile and change the line:

```ruby
gem 'rails', '4.2.X'

```

to:

```ruby
gem 'rails', '~> 5.0.0'

```

and on the command line run:

```ruby
$ bundle update

```

Now run the update task using the command:

```ruby
$ rake rails:update

```

This will help you to update configuration files. You will be prompted to overwrite files and you have several options to input:

- Y – yes, overwrite
- n – no, do not overwrite
- a – all, overwrite this and all others
- q – quit, abort
- d – diff, show the differences between the old and the new
- h – help

Typically, you should check the differences between the old and new files to make sure you aren't getting any unwanted changes.

Rails 5.0 `ActiveRecord` models inherit from `ApplicationRecord`, rather than `ActiveRecord::Base`. `ApplicationRecord` is the superclass for all models, similar to how `ApplicationController` is the superclass for controllers. To account for this new way in which models are handled, you must create a file in your `app/models/` folder called `application_record.rb` and then edit that file's contents to be:

```ruby
class ApplicationRecord < ActiveRecord::Base
  self.abstract_class = true
end

```

Rails 5.0 also handles callbacks slightly different. Callbacks that return `false` won't halt the callback chain, which means subsequent callbacks will still run, unlike Rails 4.2. When you upgrade, the Rails 4.2 behavior will remain, though you can switch to the Rails 5.0 behavior by adding:

```ruby
ActiveSupport.halt_callback_chains_on_return_false = false

```

to the `config/application.rb` file. You can explicitly halt the callback chain by calling `throw(:abort)`.

In Rails 5.0, `ActiveJob` will inherit from `ApplicationJob`, rather than `ActiveJob::Base` like in Rails 4.2. To upgrade to Rails 5.0, create a file called `application_job.rb` in the `app/jobs/` folder. Edit that file's contents to be:

```ruby
class ApplicationJob < ActiveJob::Base
end

```

Then, you must change all of your jobs to inherit from `ApplicationJob` rather than `ActiveJob::Base`.

One of the other biggest changes of Rails 5.0 doesn't require any code changes, but will change the way you use the command line with your Rails apps. You will be able to use `bin/rails`, or just `rails`, to run tasks and tests. For example, instead of using `$ rake db:migrate`, you can now do `$ rails db:migrate`. If you run `$ bin/rails`, you can view all the available commands. Note that many of the tasks that can now be run with `bin/rails` still work using `rake`.

