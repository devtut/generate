---
metaTitle: "Ruby on Rails - ActiveRecord Validations"
description: "Validating length of an attribute, Validating presence of an attribute, Custom validations, Validates format of an attribute, Validating numericality of an attribute, Grouping validation, Validates inclusion of an attribute, Validate uniqueness of an attribute, Skipping Validations, Conditional validation, Confirmation of attribute, Using :on option "
---

# ActiveRecord Validations



## Validating length of an attribute


```ruby
class Person < ApplicationRecord
  validates :name, length: { minimum: 2 }
  validates :bio, length: { maximum: 500 }
  validates :password, length: { in: 6..20 }
  validates :registration_number, length: { is: 6 }
end

```

The possible length constraint options are:

- `:minimum` - The attribute cannot have less than the specified length.
- `:maximum` - The attribute cannot have more than the specified length.
- `:in` (or `:within`) - The attribute length must be included in a given interval. The value for this option must be a range.
- `:is` - The attribute length must be equal to the given value.



## Validating presence of an attribute


This helper validates that the specified attributes are not empty.

```ruby
class Person < ApplicationRecord
  validates :name, presence: true
end

Person.create(name: "John").valid? # => true
Person.create(name: nil).valid? # => false

```

You can use the `absence` helper to validate that the specified attributes are absent. It uses the `present?` method to check for nil or empty values.

```ruby
class Person < ApplicationRecord
  validates :name, :login, :email, absence: true
end

```

**Note:**
In case the attribute is a `boolean` one, you cannot make use of the usual presence validation (the attribute would not be valid for a `false` value). You can get this done by using an inclusion validation:

```ruby
validates :attribute, inclusion: [true, false]

```



## Custom validations


You can add your own validations adding new classes inheriting from `ActiveModel::Validator` or from `ActiveModel::EachValidator`. Both methods are similar but they work in a slightly different ways:

### `ActiveModel::Validator` and `validates_with`

Implement the `validate` method which takes a record as an argument and performs the validation on it. Then use `validates_with` with the class on the model.

```ruby
# app/validators/starts_with_a_validator.rb
class StartsWithAValidator < ActiveModel::Validator
  def validate(record)
    unless record.name.starts_with? 'A'
      record.errors[:name] << 'Need a name starting with A please!'
    end
  end
end
 
class Person < ApplicationRecord
  validates_with StartsWithAValidator
end

```

### `ActiveModel::EachValidator` and `validate`

If you prefer to use your new validator using the common `validate` method on a single param, create a class inheriting from `ActiveModel::EachValidator` and implement the `validate_each` method which takes three arguments: `record`, `attribute`, and `value`:

```ruby
class EmailValidator < ActiveModel::EachValidator
  def validate_each(record, attribute, value)
    unless value =~ /\A([^@\s]+)@((?:[-a-z0-9]+\.)+[a-z]{2,})\z/i
      record.errors[attribute] << (options[:message] || 'is not an email')
    end
  end
end
 
class Person < ApplicationRecord
  validates :email, presence: true, email: true
end

```

More information on the [Rails guides](http://guides.rubyonrails.org/active_record_validations.html#performing-custom-validations).



## Validates format of an attribute


Validate that an attribute's value matches a regular expression using `format` and the `with` option.

```ruby
class User < ApplicationRecord
  validates :name, format: { with: /\A\w{6,10}\z/ }
end

```

You can also define a constant and set its value to a regular expression and pass it to the `with:` option. This might be more convenient for really complex regular expressions

```ruby
PHONE_REGEX = /\A\(\d{3}\)\d{3}-\d{4}\z/
validates :phone, format: { with: PHONE_REGEX }

```

The default error message is `is invalid`. This can be changed with the `:message` option.

```ruby
validates :bio, format: { with: /\A\D+\z/, message: "Numbers are not allowed" }

```

The reverse also replies, and you can specify that a value should **not** match a regular expression with the `without:` option



## Validating numericality of an attribute


This validation restricts the insertion of only numeric values.

```ruby
class Player < ApplicationRecord
  validates :points, numericality: true
  validates :games_played, numericality: { only_integer: true }
end

```

Besides `:only_integer`, this helper also accepts the following options to add constraints to acceptable values:

- `:greater_than` - Specifies the value must be greater than the supplied value. The default error message for this option is "must be greater than %{count}".
- `:greater_than_or_equal_to` - Specifies the value must be greater than or equal to the supplied value. The default error message for this option is "must be greater than or equal to %{count}".
- `:equal_to` - Specifies the value must be equal to the supplied value. The default error message for this option is "must be equal to %{count}".
- `:less_than` - Specifies the value must be less than the supplied value. The default error message for this option is "must be less than %{count}".
- `:less_than_or_equal_to` - Specifies the value must be less than or equal to the supplied value. The default error message for this option is "must be less than or equal to %{count}".
- `:other_than` - Specifies the value must be other than the supplied value. The default error message for this option is "must be other than %{count}".
- `:odd` - Specifies the value must be an odd number if set to true. The default error message for this option is "must be odd".
- `:even` - Specifies the value must be an even number if set to true. The default error message for this option is "must be even".

> 
By default, numericality doesn't allow nil values. You can use allow_nil: true option to permit it.




## Grouping validation


Sometimes it is useful to have multiple validations use one condition. It can be easily achieved using with_options.

```ruby
class User < ApplicationRecord
  with_options if: :is_admin? do |admin|
    admin.validates :password, length: { minimum: 10 }
    admin.validates :email, presence: true
  end
end

```

All validations inside of the with_options block will have automatically passed the condition if: :is_admin?



## Validates inclusion of an attribute


You can check if a value is included in an array using the `inclusion:` helper. The `:in` option and its alias, `:within` show the set of acceptable values.

```ruby
class Country < ApplicationRecord
  validates :continent, inclusion: { in: %w(Africa Antartica Asia Australia
                                            Europe North America South America) }
end

```

To check if a value is not included in an array, use the `exclusion:` helper

```ruby
class User < ApplicationRecord
  validates :name, exclusion: { in: %w(admin administrator owner) }
end

```



## Validate uniqueness of an attribute


This helper validates that the attribute's value is unique right before the object gets saved.

```ruby
class Account < ApplicationRecord
  validates :email, uniqueness: true
end

```

There is a `:scope` option that you can use to specify one or more attributes that are used to limit the uniqueness check:

```ruby
class Holiday < ApplicationRecord
  validates :name, uniqueness: { scope: :year,
    message: "should happen once per year" }
end

```

There is also a `:case_sensitive` option that you can use to define whether the uniqueness constraint will be case sensitive or not. This option defaults to `true`.

```ruby
class Person < ApplicationRecord
  validates :name, uniqueness: { case_sensitive: false }
end

```



## Skipping Validations


Use following methods if you want to skip the validations. These methods will save the object to the database even if it is invalid.

- decrement!
- decrement_counter
- increment!
- increment_counter
- toggle!
- touch
- update_all
- update_attribute
- update_column
- update_columns
- update_counters

You can also skip validation while saving by passing `validate` as an argument to `save`

```ruby
User.save(validate: false)

```



## Conditional validation


Sometimes you may need to validate record only under certain conditions.

```ruby
class User < ApplicationRecord
  validates :name, presence: true, if: :admin? 

  def admin?
    conditional here that returns boolean value
  end
end

```

If you conditional is really small, you can use a Proc:

```ruby
class User < ApplicationRecord
  validates :first_name, presence: true, if: Proc.new { |user| user.last_name.blank? }
end

```

For negative conditional you can use `unless`:

```ruby
class User < ApplicationRecord
  validates :first_name, presence: true, unless: Proc.new { |user| user.last_name.present? }
end

```

You can also pass a string, which will be executed via `instance_eval`:

```ruby
class User < ApplicationRecord
  validates :first_name, presence: true, if: 'last_name.blank?'
end

```



## Confirmation of attribute


You should use this when you have two text fields that should receive exactly the same content. For example, you may want to confirm an email address or a password. This validation creates a ****virtual**** attribute whose name is the name of the field that has to be confirmed with `_confirmation` appended.

```ruby
class Person < ApplicationRecord
  validates :email, confirmation: true
end

```

**Note** This check is performed only if `email_confirmation` is not nil.

To require confirmation, make sure to add a presence check for the confirmation attribute.

```ruby
class Person < ApplicationRecord
  validates :email,      confirmation: true
  validates :email_confirmation, presence: true
end

```

[Source](http://guides.rubyonrails.org/active_record_validations.html)



## Using :on option 


The `:on` option lets you specify when the validation should happen.
The default behavior for all the built-in validation helpers is to be run on save (both when you're creating a new record and when you're updating it).

```ruby
class Person < ApplicationRecord
  # it will be possible to update email with a duplicated value
  validates :email, uniqueness: true, on: :create
 
  # it will be possible to create the record with a non-numerical age
  validates :age, numericality: true, on: :update
 
  # the default (validates on both create and update)
  validates :name, presence: true
end

```

