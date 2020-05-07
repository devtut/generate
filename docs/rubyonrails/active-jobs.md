---
metaTitle: "Ruby on Rails - Active Jobs"
description: "Introduction, Sample Job, Creating an Active Job via the generator"
---

# Active Jobs




## Introduction


Available since Rails 4.2, Active Job is a framework for declaring jobs and making them run on a variety of queuing backends. Recurring or punctual tasks that are not blocking and can be run in parallel are good use cases for Active Jobs.



## Sample Job


```ruby
class UserUnsubscribeJob < ApplicationJob
  queue_as :default

  def perform(user)
    # this will happen later
    user.unsubscribe
  end
end

```



## Creating an Active Job via the generator


```ruby
$ rails g job user_unsubscribe

```

