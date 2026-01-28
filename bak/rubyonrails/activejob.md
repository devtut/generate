---
metaTitle: "Ruby on Rails - ActiveJob"
description: "Create the Job, Enqueue the Job"
---

# ActiveJob


Active Job is a framework for declaring jobs and making them run on a variety of queuing backends. These jobs can be everything from regularly scheduled clean-ups, to billing charges, to mailings. Anything that can be chopped up into small units of work and run in parallel, really.



## Create the Job


```ruby
class GuestsCleanupJob < ApplicationJob
  queue_as :default
 
  def perform(*guests)
    # Do something later
  end
end

```



## Enqueue the Job


```ruby
# Enqueue a job to be performed as soon as the queuing system is free.
GuestsCleanupJob.perform_later guest

```

