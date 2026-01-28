---
metaTitle: "Ruby on Rails - Adding an Amazon RDS to your rails application"
description: "Consider we are connecting MYSQL RDS with your rails application."
---

# Adding an Amazon RDS to your rails application


Steps to create an AWS RDS instance and configure your database.yml file by installing the required connectors.



## Consider we are connecting MYSQL RDS with your rails application.


**Steps to create MYSQL database**

1. Login to amazon account and select RDS service
1. Select `Launch DB Instance` from the instance tab
1. By defaul MYSQL Community Edition will be selected, hence click the `select` button
1. Select the database purpose, say `production` and click `next step`
1. Provide the `mysql version, storage size, DB Instance Identifier, Master Username and Password` and click `next step`
1. Enter `Database Name` and click `Launch DB Instance`
1. Please wait until all the instance gets created. Once the instance gets created you will find an Endpoint, copy this entry point (which is referred as hostname)

**Installing connectors**

Add the MySQL database adapter to your project's gemfile,

```ruby
gem 'mysql2'

```

Install your added gems,

```ruby
bundle install

```

Some other database adapters are,

- `gem 'pg'` for PostgreSQL
- `gem 'activerecord-oracle_enhanced-adapter'` for Oracle
- `gem 'sql_server'` for SQL Server

**Configure your project's database.yml file**
Open your config/database.yml file

```ruby
production:
  adapter: mysql2
  encoding: utf8
  database: <%= RDS_DB_NAME %>  # Which you have entered you creating database
  username: <%= RDS_USERNAME %> # db master username
  password: <%= RDS_PASSWORD %> # db master password
  host: <%= RDS_HOSTNAME %>     # db instance entrypoint
  port: <%= RDS_PORT %>         # db post. For MYSQL 3306

```

