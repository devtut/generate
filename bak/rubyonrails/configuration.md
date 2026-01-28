---
metaTitle: "Ruby on Rails - Configuration"
description: "Environments in Rails, Database Configuration, Rails General Configuration, Configuring assets, Configuring generators"
---

# Configuration



## Environments in Rails


Configuration files for rails can be found in `config/environments/`. By default rails has 3 environments, `development`, `production` and `test`. By editing each file you are editing the configuration for that environment only.

Rails also has a configuration file in `config/application.rb`. This is a common configuration file as any settings defined here are overwritten by the config specified in each environment.

You add or modify configuration options within the `Rails.application.configure do` block and configuration options start with `config.`



## Database Configuration


Database configuration of a rails project lies in a file `config/database.yml`. If you create a project using `rails new` command and don't specify a database engine to be used then rails uses `sqlite` as the default database. A typical `database.yml` file with default configuration will look similar to following.

```ruby
# SQLite version 3.x
#   gem install sqlite3
#
#   Ensure the SQLite 3 gem is defined in your Gemfile
#   gem 'sqlite3'
#
default: &default
  adapter: sqlite3
  pool: 5
  timeout: 5000

development:
  <<: *default
  database: db/development.sqlite3

# Warning: The database defined as "test" will be erased and
# re-generated from your development database when you run "rake".
# Do not set this db to the same as development or production.
test:
  <<: *default
  database: db/test.sqlite3

production:
  <<: *default
  database: db/production.sqlite3

```

If you want to change the default database while creating a new project you can specify database: `rails new hello_world --database=mysql`



## Rails General Configuration


The following configuration options should be called on a `Rails::Railtie` object

- **config.after_initialize**: Takes a block which will be run after rails has initialized the application.
- **config.asset_host**: This sets the host for the assets. This is useful when using a **Content Delivery Network**. This is shorthand for `config.action_controller.asset_host`
- **config.autoload_once_paths**: This option accepts an array of paths where Rails autoloads constants. The default value is an empty array
- **config.autoload_paths**: This accepts an array of paths where Rails autoloads constants. It defaults to all directories under `app`
- **config.cache_classes**: Determines if classes and modules should be reloaded on each request. In development mode, this defaults to `false` and in the production and test modes it defaults to `true`
- **config.action_view.cache_template_loading**: This determines if templates should be reloaded on each request. It defaults to the `config.cache_classes` setting
- **config.beginning_of_week**: This sets the default beginning of week. It requires a valid week day symbol (`:monday`)
- **config.cache_store**: Choose which cache store to use. Options include `:file_store`, `:memory_store`, `mem_cache_store` or `null_store`.
- **config.colorize_logging**: This controls whether logging information is colorized
- **config.eager_load**: Eager-loads all registered
- **config.encoding**: Specifies the application encoding. The default value is `UTF-8`
- **config.log_level**: Sets the verbosity of the Rails Logger. It defaults to `:debug` in all environments.
- **config.middleware**: Use this to configure the application's middleware
- **config.time_zone**: This sets the application's default time zone.



## Configuring assets


The following configuration options can be used for configuring assets

- **config.assets.enabled**: Determines whether the asset pipeline is enabled. This defaults to true
- **config.assets.raise_runtime_errors**: This enables runtime error checking. It's useful for `development mode`
- **config.assets.compress**: Lets assets be compressed. In production mode, this defaults to true
- **config.assets.js_compressor**: Specifies which JS compressor to use. Options include `:closure`, `:uglifier` and `:yui`
- **config.assets.paths**: Specifies which paths to search for assets.
- **config.assets.precompile**: Lets you choose additional assets to be precompiled when `rake assets:precompile` is run
- **config.assets.digest**: This option allows the use of `MD-5` fingerprints in the asset names. It defaults to true in development mode
- **config.assets.compile**: Toggles live `Sprockets` compilation in production mode



## Configuring generators


Rails allows you to configure what generators are used when running `rails generate` commands. This method, `config.generators` takes a block

```ruby
config.generators do |g|
  g.orm :active_record
  g.test_framework :test_unit
end

```

Here are some of the options

|Option|Description|Default
|---|---|---|---|---|---|---|---|---|---
|assets|Creates assets when generating scaffold|true
|force_plural|Allows pluralized model names|false
|helper|Determines whether to generate helpers|true
|integration_tool|Specify integration tool|`test_unit`
|javascript_engine|Configures JS engine|`:js`
|resource_route|Generates resource route|true
|stylesheet_engine|Configures stylesheet engine|`:cs`
|scaffold_stylesheet|Creates CSS upon scaffolding|true
|test_framework|Specify Test Framework|`Minitest`
|template_engine|Configures template engine|`:erb`

