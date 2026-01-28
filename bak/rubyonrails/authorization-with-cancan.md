---
metaTitle: "Ruby on Rails - Authorization with CanCan"
description: "Getting started with CanCan, Handling large number of abilities, Defining abilities, Quickly test an ability"
---

# Authorization with CanCan


[CanCan](https://github.com/CanCanCommunity/cancancan) is a simple authorization strategy for Rails which is decoupled from user roles. All permissions are stored in a single location.



## Getting started with CanCan


[CanCan](https://github.com/CanCanCommunity/cancancan) is a a popular authorization library for Ruby on Rails which restricts user access to specific resources. The latest gem (CanCanCan) is a continuation of the dead project [CanCan](https://github.com/ryanb/cancan).

Permissions are defined in the `Ability` class and can be used from controllers, views, helpers, or any other place in the code.

To add authorization support to an app, add the CanCanCan gem to the `Gemfile`:

```ruby
gem 'cancancan'

```

Then define the ability class:

```ruby
# app/models/ability.rb
class Ability
  include CanCan::Ability

  def initialize(user)
  end
end

```

Then check authorization using `load_and_authorize_resource` to load authorized models into the controller:

```ruby
class ArticlesController < ApplicationController
  load_and_authorize_resource

  def show
    # @article is already loaded and authorized
  end
end

```

`authorize!` to check authorization or raise an exception

```ruby
def show
  @article = Article.find(params[:id])
  authorize! :read, @article
end

```

`can?` to check if an object is authorized against a particular action anywhere in the controllers, views, or helpers

```ruby
<% if can? :update, @article %>
  <%= link_to "Edit", edit_article_path(@article) %>
<% end %>

```

**Note:** This assumes the signed user is provided by the `current_user` method.



## Handling large number of abilities


Once the number of abilities definitions start to grow in number, it becomes more and more difficult to handle the Ability file.

The first strategy to handle these issue is to move abilities into meaningful methods, as per this example:

```ruby
class Ability
  include CanCan::Ability

  def initialize(user)
    anyone_abilities 

    if user
      if user.admin?
        admin_abilities
      else
        authenticated_abilities
      end
    else
      guest_abilities
    end
  end

  private
  
  def anyone_abilities
    # define abilities for everyone, both logged users and visitors
  end

  def guest_abilities
    # define abilities for visitors only
  end

  def authenticated_abilities
    # define abilities for logged users only
  end

  def admin_abilities
    # define abilities for admins only
  end
end

```

Once this class grow large enough, you can try breaking it into different classes to handle the different responsibilities like this:

```ruby
# app/models/ability.rb
class Ability
  include CanCan::Ability

  def initialize(user)
    self.merge Abilities::Everyone.new(user)

    if user
      if user.admin?
        self.merge Abilities::Admin.new(user)
      else
        self.merge Abilities::Authenticated.new(user)
      end
    else
      self.merge Abilities::Guest.new(user)
    end
  end
end

```

and then define those classes as:

```ruby
# app/models/abilities/guest.rb
module Abilities
  class Guest
    include CanCan::Ability

    def initialize(user)
      # Abilities for anonymous visitors only
    end
  end
end

```

and so on with `Abilities::Authenticated`, `Abilities::Admin` or any other else.



## Defining abilities


Abilities are defined in the `Ability` class using `can` and `cannot` methods. Consider the following commented example for basic reference:

```ruby
class Ability
  include CanCan::Ability

  def initialize(user)
    # for any visitor or user
    can :read, Article

    if user
      if user.admin?
        # admins can do any action on any model or action
        can :manage, :all
      else
        # regular users can read all content
        can :read, :all
        # and edit, update and destroy their own user only
        can [:edit, :destroy], User, id: user_id
        # but cannot read hidden articles
        cannot :read, Article, hidden: true
      end
    else
      # only unlogged visitors can visit a sign_up page:
      can :read, :sign_up
    end
  end
end

```



## Quickly test an ability


If you'd like to quickly test if an ability class is giving the correct permissions, you can initialize an ability in the console or on another context with the rails environment loaded, just pass an user instance to test against:

```ruby
test_ability = Ability.new(User.first)
test_ability.can?(:show, Post) #=> true
other_ability = Ability.new(RestrictedUser.first)
other_ability.cannot?(:show, Post) #=> true

```

More information: [https://github.com/ryanb/cancan/wiki/Testing-Abilities](https://github.com/ryanb/cancan/wiki/Testing-Abilities)



#### Remarks


Before using CanCan don't forget to create Users either by devise gem or manually. To get maximum functionality of CanCan do create an Admin user.

