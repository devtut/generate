---
metaTitle: "Ruby - Time"
description: "How to use the strftime method, Creating time objects"
---

# Time



## How to use the strftime method


Converting a time to a string is a pretty common thing to do in Ruby. `strftime` is the method one would use to convert time to a string.

Here are some examples:

```ruby
Time.now.strftime("%Y-%m-d %H:%M:S") #=> "2016-07-27 08:45:42"

```

This can be simplified even further

```ruby
Time.now.strftime("%F %X")  #=> "2016-07-27 08:45:42"

```



## Creating time objects


Get current time:

```ruby
Time.now
Time.new # is equivalent if used with no parameters

```

Get specific time:

```ruby
Time.new(2010, 3, 10) #10 March 2010 (Midnight)
Time.new(2015, 5, 3, 10, 14) #10:14 AM on 3 May 2015 
Time.new(2050, "May", 3, 21, 8, 16, "+10:00") #09:08:16 PM on 3 May 2050

```

To convert a time to [epoch](https://en.wikipedia.org/wiki/Unix_time) you can use the `to_i` method:

```ruby
Time.now.to_i # => 1478633386

```

You can also convert back from epoch to Time using the `at` method:

```ruby
Time.at(1478633386) # => 2016-11-08 17:29:46 -0200

```



#### Syntax


- `Time.now`
- `Time.new([year], [month], [day], [hour], [min], [sec], [utc_offset])`

