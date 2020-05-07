---
metaTitle: "Ruby on Rails - Rails 5 API Authetication"
description: "Authentication with Rails authenticate_with_http_token"
---

# Rails 5 API Authetication



## Authentication with Rails authenticate_with_http_token


```ruby
authenticate_with_http_token do |token, options|
  @user = User.find_by(auth_token: token)
end

```

You can test this endpoint with `curl` by making a request like

```ruby
curl -IH "Authorization: Token token=my-token" http://localhost:3000

```

