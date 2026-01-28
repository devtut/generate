---
metaTitle: "Ruby on Rails - Change default timezone"
description: "Change Rails timezone AND have Active Record store times in this timezone, Change Rails timezone, but continue to have Active Record save in the database in UTC"
---

# Change default timezone



## Change Rails timezone AND have Active Record store times in this timezone


```ruby
# application.rb
config.time_zone = 'Eastern Time (US & Canada)'
config.active_record.default_timezone = :local

```



## Change Rails timezone, but continue to have Active Record save in the database in UTC


```ruby
# application.rb
config.time_zone = 'Eastern Time (US & Canada)'

```



#### Remarks


> 
<p>**config.active_record.default_timezone** determines whether to use Time.local (if set to :local) or Time.utc (if set to :utc) when pulling dates and times from the database. The default is :utc.
[http://guides.rubyonrails.org/configuring.html](http://guides.rubyonrails.org/configuring.html)</p>


If you want to change **Rails** timezone, but continue to have **Active Record** save in the database in **UTC**, use

```ruby
# application.rb
config.time_zone = 'Eastern Time (US & Canada)'

```

If you want to change **Rails** timezone **AND** have **Active Record** store times in this timezone, use

```ruby
# application.rb
config.time_zone = 'Eastern Time (US & Canada)'
config.active_record.default_timezone = :local

```

**Warning**: you really should think twice, even thrice, before saving times in the database in a non-UTC format.

> 
<p>**Note**<br />
Do not forget to restart your Rails server after modifying `application.rb`.</p>


Remember that `config.active_record.default_timezone` can take only two values

- **:local** (converts to the timezone defined in `config.time_zone`)
- **:utc** (converts to UTC)

Here's how you can find all available timezones

```ruby
rake time:zones:all

```

