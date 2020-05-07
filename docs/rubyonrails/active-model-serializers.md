---
metaTitle: "Ruby on Rails - Active Model Serializers"
description: "Using a serializer"
---

# Active Model Serializers


ActiveModelSerializers, or AMS for short, bring 'convention over configuration' to your JSON generation.
ActiveModelSerializers work through two components: serializers and adapters.
Serializers describe which attributes and relationships should be serialized.
Adapters describe how attributes and relationships should be serialized.



## Using a serializer


```ruby
class SomeSerializer < ActiveModel::Serializer
  attribute :title, key: :name
  attributes :body
end

```

