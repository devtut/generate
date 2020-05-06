---
metaTitle: "Ruby - DateTime"
description: "DateTime from string, New, Add/substract days to DateTime"
---

# DateTime




## DateTime from string


`DateTime.parse` is a very useful method which construct a DateTime from a string, guessing its format.

```ruby
DateTime.parse('Jun, 8 2016')
# => #<DateTime: 2016-06-08T00:00:00+00:00 ((2457548j,0s,0n),+0s,2299161j)>
DateTime.parse('201603082330')
# => #<DateTime: 2016-03-08T23:30:00+00:00 ((2457456j,84600s,0n),+0s,2299161j)>
DateTime.parse('04-11-2016 03:50')
# => #<DateTime: 2016-11-04T03:50:00+00:00 ((2457697j,13800s,0n),+0s,2299161j)>
DateTime.parse('04-11-2016 03:50 -0300')
# => #<DateTime: 2016-11-04T03:50:00-03:00 ((2457697j,24600s,0n),-10800s,2299161j)

```

Note: There are lots of other formats that `parse` recognizes.



## New


```ruby
DateTime.new(2014,10,14)
# => #<DateTime: 2014-10-14T00:00:00+00:00 ((2456945j,0s,0n),+0s,2299161j)>

```

Current time:

```ruby
DateTime.now
# => #<DateTime: 2016-08-04T00:43:58-03:00 ((2457605j,13438s,667386397n),-10800s,2299161j)>

```

Note that it gives the current time in your timezone



## Add/substract days to DateTime


`DateTime` + `Fixnum` (days quantity)

```ruby
DateTime.new(2015,12,30,23,0) + 1
# => #<DateTime: 2015-12-31T23:00:00+00:00 ((2457388j,82800s,0n),+0s,2299161j)>

```

`DateTime` + `Float` (days quantity)

```ruby
DateTime.new(2015,12,30,23,0) + 2.5
# => #<DateTime: 2016-01-02T11:00:00+00:00 ((2457390j,39600s,0n),+0s,2299161j)>

```

`DateTime` + `Rational` (days quantity)

```ruby
DateTime.new(2015,12,30,23,0) + Rational(1,2)
# => #<DateTime: 2015-12-31T11:00:00+00:00 ((2457388j,39600s,0n),+0s,2299161j)>

```

`DateTime` - `Fixnum` (days quantity)

```ruby
DateTime.new(2015,12,30,23,0) - 1
# => #<DateTime: 2015-12-29T23:00:00+00:00 ((2457388j,82800s,0n),+0s,2299161j)>

```

`DateTime` - `Float` (days quantity)

```ruby
DateTime.new(2015,12,30,23,0) - 2.5
# => #<DateTime: 2015-12-28T11:00:00+00:00 ((2457385j,39600s,0n),+0s,2299161j)>

```

`DateTime` - `Rational` (days quantity)

```ruby
DateTime.new(2015,12,30,23,0) - Rational(1,2)
# => #<DateTime: 2015-12-30T11:00:00+00:00 ((2457387j,39600s,0n),+0s,2299161j)>

```



#### Syntax


- DateTime.new(year, month, day, hour, minute, second)



#### Remarks


Before using DateTime you need to `require 'date'`

