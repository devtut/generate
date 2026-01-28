---
metaTitle: "Ruby - Environment Variables"
description: "Sample to get user profile path"
---

# Environment Variables



## Sample to get user profile path


```ruby
# will retrieve my home path
ENV['HOME'] # => "/Users/username"

# will try retrieve the 'FOO' environment variable. If failed, will get 'bar'
ENV.fetch('FOO', 'bar')

```



#### Syntax


- ENV[variable_name]
- ENV.fetch(variable_name, default_value)



#### Remarks


Let get user profile path in a dynamic way for scripting under windows

