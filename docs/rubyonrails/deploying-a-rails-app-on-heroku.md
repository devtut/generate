---
metaTitle: "Ruby on Rails - Deploying a Rails app on Heroku"
description: "Deploying your application, Managing Production and staging environments for a Heroku"
---

# Deploying a Rails app on Heroku



## Deploying your application


Make sure you are in the directory that contains your Rails app, then create an app on Heroku.

```ruby
$ heroku create example
Creating ⬢ example... done
https://example.herokuapp.com/ | https://git.heroku.com/example.git

```

The first URL of the ouput, [http://example.herokuapp.com](http://example.herokuapp.com), is the location the app is available at. The second URL, git@heroku.com:example.git, is the remote git repository URL.

This command should only be used on an initialized git repository. The heroku create command automatically adds a git remote named “heroku” pointing at this URL.

The app name argument (“example”) is optional. If no app name is specified, a random name will be generated. Since Heroku app names are in a global namespace, you can expect that common names, like “blog” or “wiki”, will already be taken. It’s often easier to start with a default name and rename the app later.

Next, deploy your code:

```ruby
$ git push heroku master
remote: Compressing source files... done.
remote: Building source:
remote:
remote: -----> Ruby app detected
remote: -----> Compiling Ruby/Rails
remote: -----> Using Ruby version: ruby-2.3.1
remote: -----> Installing dependencies using bundler 1.11.2
remote:        Running: bundle install --without development:test --path vendor/bundle --binstubs vendor/bundle/bin -j4 --deployment
remote:        Warning: the running version of Bundler is older than the version that created the lockfile. We suggest you upgrade to the latest version of Bundler by running `gem install bundler`.
remote:        Fetching gem metadata from https://rubygems.org/..........
remote:        Fetching version metadata from https://rubygems.org/...
remote:        Fetching dependency metadata from https://rubygems.org/..
remote:        Installing concurrent-ruby 1.0.2
remote:        Installing i18n 0.7.0
remote:        Installing rake 11.2.2
remote:        Installing minitest 5.9.0
remote:        Installing thread_safe 0.3.5
remote:        Installing builder 3.2.2
remote:        Installing mini_portile2 2.1.0
remote:        Installing erubis 2.7.0
remote:        Installing pkg-config 1.1.7
remote:        Installing rack 2.0.1
remote:        Installing nio4r 1.2.1 with native extensions
remote:        Installing websocket-extensions 0.1.2
remote:        Installing mime-types-data 3.2016.0521
remote:        Installing arel 7.0.0
remote:        Installing coffee-script-source 1.10.0
remote:        Installing execjs 2.7.0
remote:        Installing method_source 0.8.2
remote:        Installing thor 0.19.1
remote:        Installing multi_json 1.12.1
remote:        Installing puma 3.4.0 with native extensions
remote:        Installing pg 0.18.4 with native extensions
remote:        Using bundler 1.11.2
remote:        Installing sass 3.4.22
remote:        Installing tilt 2.0.5
remote:        Installing turbolinks-source 5.0.0
remote:        Installing tzinfo 1.2.2
remote:        Installing nokogiri 1.6.8 with native extensions
remote:        Installing rack-test 0.6.3
remote:        Installing sprockets 3.6.3
remote:        Installing websocket-driver 0.6.4 with native extensions
remote:        Installing mime-types 3.1
remote:        Installing coffee-script 2.4.1
remote:        Installing uglifier 3.0.0
remote:        Installing turbolinks 5.0.0
remote:        Installing activesupport 5.0.0
remote:        Installing mail 2.6.4
remote:        Installing globalid 0.3.6
remote:        Installing activemodel 5.0.0
remote:        Installing jbuilder 2.5.0
remote:        Installing activejob 5.0.0
remote:        Installing activerecord 5.0.0
remote:        Installing loofah 2.0.3
remote:        Installing rails-dom-testing 2.0.1
remote:        Installing rails-html-sanitizer 1.0.3
remote:        Installing actionview 5.0.0
remote:        Installing actionpack 5.0.0
remote:        Installing actionmailer 5.0.0
remote:        Installing railties 5.0.0
remote:        Installing actioncable 5.0.0
remote:        Installing sprockets-rails 3.1.1
remote:        Installing coffee-rails 4.2.1
remote:        Installing jquery-rails 4.1.1
remote:        Installing rails 5.0.0
remote:        Installing sass-rails 5.0.5
remote:        Bundle complete! 15 Gemfile dependencies, 54 gems now installed.
remote:        Gems in the groups development and test were not installed.
remote:        Bundled gems are installed into ./vendor/bundle.
remote:        Bundle completed (31.86s)
remote:        Cleaning up the bundler cache.
remote:        Warning: the running version of Bundler is older than the version that created the lockfile. We suggest you upgrade to the latest version of Bundler by running `gem install bundler`.
remote: -----> Preparing app for Rails asset pipeline
remote:        Running: rake assets:precompile
remote:        I, [2016-07-08T17:08:57.046245 #1222]  INFO -- : Writing /tmp/build_49ba6c877f5502cd4029406e981f90b4/public/assets/application-1bf5315c71171ad5f9cbef00193d56b7e45263ddc64caf676ce988cfbb6570bd.js
remote:        I, [2016-07-08T17:08:57.046951 #1222]  INFO -- : Writing /tmp/build_49ba6c877f5502cd4029406e981f90b4/public/assets/application-1bf5315c71171ad5f9cbef00193d56b7e45263ddc64caf676ce988cfbb6570bd.js.gz
remote:        I, [2016-07-08T17:08:57.060208 #1222]  INFO -- : Writing /tmp/build_49ba6c877f5502cd4029406e981f90b4/public/assets/application-e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855.css
remote:        I, [2016-07-08T17:08:57.060656 #1222]  INFO -- : Writing /tmp/build_49ba6c877f5502cd4029406e981f90b4/public/assets/application-e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855.css.gz
remote:        Asset precompilation completed (4.06s)
remote:        Cleaning assets
remote:        Running: rake assets:clean
remote:
remote: ###### WARNING:
remote:        No Procfile detected, using the default web server.
remote:        We recommend explicitly declaring how to boot your server process via a Procfile.
remote:        https://devcenter.heroku.com/articles/ruby-default-web-server
remote:
remote: -----> Discovering process types
remote:        Procfile declares types     -> (none)
remote:        Default types for buildpack -> console, rake, web, worker
remote:
remote: -----> Compressing...
remote:        Done: 29.2M
remote: -----> Launching...
remote:        Released v5
remote:        https://example.herokuapp.com/ deployed to Heroku
remote:
remote: Verifying deploy... done.
To https://git.heroku.com/example.git
 * [new branch]      master -> master

```

If you are using the database in your application you need to manually migrate the database by running:

```ruby
$ heroku run rake db:migrate

```

Any commands after `heroku run` will be executed on a Heroku dyno. You can obtain an interactive shell session by running:

```ruby
$ heroku run bash

```

Ensure you have one dyno running the web process type:

```ruby
$ heroku ps:scale web=1

```

The heroku ps command lists the running dynos of your application:

```ruby
$ heroku ps
=== web (Standard-1X): bin/rails server -p $PORT -e $RAILS_ENV (1)
web.1: starting 2016/07/08 12:09:06 -0500 (~ 2s ago)

```

You can now visit the app in our browser with `heroku open`.

```ruby
$ heroku open

```

Heroku gives you a default web URL in the `herokuapp.com` domain. When you are ready to scale up for production, you can add your own custom domain.



## Managing Production and staging environments for a Heroku


Every Heroku app runs in at least two environments: on Heroku (we’ll call that production) and on your local machine (development). If more than one person is working on the app, then you’ve got multiple development environments - one per machine, usually. Usually, each developer will also have a test environment for running tests. Unfortunately, this approach breaks down as the environments become less similar. Windows and Macs, for instance, both provide different environments than the Linux stack on Heroku, so you can’t always be sure that code that works in your local development environment will work the same way when you deploy it to production.

The solution is to have a staging environment that is as similar to production as is possible. This can be achieved by creating a second Heroku application that hosts your staging application. With staging, you can check your code in a production-like setting before having it affect your actual users.

**Starting from scratch**

Assume you have an application running on your local machine, and you’re ready to push it to Heroku. We’ll need to create both remote environments, staging and production. To get in the habit of pushing to staging first, we’ll start with this:

```ruby
$ heroku create --remote staging
Creating strong-river-216.... done
http://strong-river-216.heroku.com/ | https://git.heroku.com/strong-river-216.git
Git remote staging added

```

By default, the heroku CLI creates projects with a heroku git remote. Here, we’re specifying a different name with the --remote flag, so pushing code to Heroku and running commands against the app look a little different than the normal git push heroku master:

```

$ git push staging master
...
 $ heroku ps --remote staging
=== web: `bundle exec puma -C config/puma.rb``
web.1: up for 21s

```

Once your staging app is up and running properly, you can create your production app:

```ruby
$ heroku create --remote production
Creating fierce-ice-327.... done
http://fierce-ice-327.heroku.com/ | https://git.heroku.com/fierce-ice-327.git
Git remote production added
$ git push production master
...
$ heroku ps --remote production
=== web: `bundle exec puma -C config/puma.rb
web.1: up for 16s

```

And with that, you’ve got the same codebase running as two separate Heroku apps – one staging and one production, set up identically. Just remember you will have to specify which app you are going to operate on your daily work. You can either use flag '--remote' or use your git config to specify a default app.

