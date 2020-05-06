---
metaTitle: "Ruby - Range"
description: "Ranges as Sequences, Iterating over a range, Range between dates"
---

# Range



## Ranges as Sequences


The most important use of ranges is to express a sequence

**Syntax:**

```ruby
(begin..end) => this construct will include end value
(begin...end) => this construct will exclude end value

```

or

```ruby
Range.new(begin,end,exclude_end) => exclude_end is by default false

```

Most important `end` value must be greater the `begin`, otherwise it will return nothing.

**Examples:**

```ruby
(10..1).to_a            #=> []
(1...3)                 #=> [1, 2]
(-6..-1).to_a           #=> [-6, -5, -4, -3, -2, -1]
('a'..'e').to_a         #=> ["a", "b", "c", "d", "e"]
('a'...'e').to_a        #=> ["a", "b", "c", "d"]
Range.new(1,3).to_a     #=> [1, 2, 3] 
Range.new(1,3,true).to_a#=> [1, 2]

```



## Iterating over a range


You can easily do something to each element in a range.

```ruby
(1..5).each do |i|
    print i
end
# 12345

```



## Range between dates


```ruby
require 'date'

date1 = Date.parse "01/06/2016"
date2 = Date.parse "05/06/2016"

p "Period #{date1.strftime("%d/%m/%Y")} to #{date2.strftime("%d/%m/%Y")}"

(date1..date2).each do |date|
  p date.strftime("%d/%m/%Y")
end

# "01/06/2016"
# "02/06/2016"
# "03/06/2016"
# "04/06/2016"
# "05/06/2016"

```

