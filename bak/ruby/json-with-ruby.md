---
metaTitle: "Ruby - JSON with Ruby"
description: "Using JSON with Ruby, Using Symbols"
---

# JSON with Ruby



## Using JSON with Ruby


JSON (JavaScript Object Notation) is a lightweight data interchange format.
Many web applications use it to send and receive data.

In Ruby you can simply work with JSON.

At first you have to `require 'json'`, then you can parse a JSON string via the `JSON.parse()` command.

```ruby
require 'json'

j = '{"a": 1, "b": 2}'
puts JSON.parse(j)
>> {"a"=>1, "b"=>2}

```

What happens here, is that the parser generates a [Ruby Hash](http://stackoverflow.com/documentation/ruby/288/hashes#t=201608301314300578794) out of the JSON.

The other way around, generating JSON out of a Ruby hash is as simple as parsing. The method of choice is `to_json`:

```ruby
require 'json'

hash = { 'a' => 1, 'b' => 2 }
json = hash.to_json
puts json
>> {"a":1,"b":2}

```



## Using Symbols


You can use JSON together with Ruby symbols.
With the option symbolize_names for the parser, the keys in the resulting hash will be symbols instead of strings.

```ruby
json = '{ "a": 1, "b": 2 }'
puts JSON.parse(json, symbolize_names: true)
>> {:a=>1, :b=>2}

```

