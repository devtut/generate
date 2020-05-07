---
metaTitle: "Ruby on Rails - Rails API"
description: "Creating an API-only application"
---

# Rails API



## Creating an API-only application


To build a Rails application that will be an API server, you can start with a more limited subset of Rails in Rails 5.

To generate a new Rails API app:

```ruby
rails new my_api --api

```

What `--api` does is to remove functionality that is not needed when building an API. This includes sessions, cookies, assets, and anything that makes Rails work on a browser.

It will also configure the generators so that they don't generate views, helpers, and assets when generating a new resource.

When you compare the `ApplicationController` on a web app versus an API app, you will see that the web version extends from `ActionController::Base`, whereas the API version extends from `ActionController::API`, which includes a much smaller subset of functionality.

