---
metaTitle: "Ruby on Rails - Multipurpose ActiveRecord columns"
description: "Saving an object, How To"
---

# Multipurpose ActiveRecord columns



## Saving an object


If you have an attribute that needs to be saved and retrieved to database as an object, then specify the name of that attribute using the `serialize` method and it will be handled automatically.

The attribute must be declared as a `text` field.

In the model you must declare the type of the field (`Hash` or `Array`)

More info at: [serialize >> apidock.com](http://apidock.com/rails/ActiveRecord/Base/serialize/class)



## How To


### In your migration

```ruby
class Users < ActiveRecord::Migration[5.0]
  def change
    create_table :users do |t|
      ...
      t.text :preference
      t.text :tag
      ...
      t.timestamps
    end
  end
end

```

### In your model

```ruby
class User < ActiveRecord::Base
    serialize :preferences, Hash  
    serialize :tags, Array
end

```



#### Syntax


- `serialize: <field_plural_symbol>`

