---
metaTitle: "Ruby on Rails - Change a default Rails application enviornment"
description: "Running on a local machine, Running on a server"
---

# Change a default Rails application enviornment


This will discuss how to change the environment so when someone types `rails s` they boot in not development but in the environment they want.



## Running on a local machine


Normally when rails environment is run by typing. This just runs the default environment which is usually `development`

```ruby
rails s

```

The specific environment can be selected by using the flag `-e` for example:

```ruby
rails s -e test

```

Which will run the test environment.

The default environment can be changed in terminal by editing the `~/.bashrc` file, and adding the following line:

```ruby
export RAILS_ENV=production in your 

```



## Running on a server


If running on a remote server that is using Passenger change apache.conf to to the environment you want to use. For example this case you see `RailsEnv production`.

```ruby
<VirtualHost *:80>
  ServerName application_name.rails.local
  DocumentRoot "/Users/rails/application_name/public"
  RailsEnv production ## This is the default
</VirtualHost>

```

