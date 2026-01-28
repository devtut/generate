---
metaTitle: "Ruby on Rails - Factory Girl"
description: "Defining Factories"
---

# Factory Girl




## Defining Factories


If you have a ActiveRecord User class with name and email attributes, you could create a factory for it by making the FactoryGirl guess it:

```ruby
FactoryGirl.define do
  factory :user do # it will guess the User class
    name     "John"
    email    "john@example.com"
  end
end

```

Or you can make it explicit and even change its name:

```ruby
FactoryGirl.define do
  factory :user_jack, class: User do
    name     "Jack"
    email    "jack@example.com"
  end
end

```

Then in your spec you can use the FactoryGirl's methods with these, like this:

```ruby
# To create a non saved instance of the User class filled with John's data 
build(:user) 
# and to create a non saved instance of the User class filled with Jack's data
build(:user_jack)

```

The most common methods are:

```ruby
# Build returns a non saved instance
user = build(:user)

# Create returns a saved instance
user = create(:user)

# Attributes_for returns a hash of the attributes used to build an instance
attrs = attributes_for(:user)

```

