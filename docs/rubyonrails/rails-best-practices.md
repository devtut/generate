---
metaTitle: "Ruby on Rails - Rails Best Practices"
description: "Fat Model, Skinny Controller, Domain Objects (No More Fat Models), Convention Over Configuration, Beware of  default_scope, Don't Repeat Yourself (DRY), You Ain’t Gonna Need it (YAGNI)"
---

# Rails Best Practices




## Fat Model, Skinny Controller


“Fat Model, Skinny Controller” refers to how the M and C parts of MVC ideally work together. Namely, any non-response-related logic should go in the model, ideally in a nice, testable method. Meanwhile, the “skinny” controller is simply a nice interface between the view and model.

In practice, this can require a range of different types of refactoring, but it all comes down to one idea: by moving any logic that isn’t about the response to the model (instead of the controller), not only have you promoted reuse where possible but you’ve also made it possible to test your code outside of the context of a request.

Let’s look at a simple example. Say you have code like this:

```ruby
def index
  @published_posts = Post.where('published_at <= ?', Time.now)
  @unpublished_posts = Post.where('published_at IS NULL OR published_at > ?', Time.now)
end

```

You can change it to this:

```ruby
def index
  @published_posts = Post.published
  @unpublished_posts = Post.unpublished
end

```

Then, you can move the logic to your post model, where it might look like this:

```ruby
scope :published, ->(timestamp = Time.now) { where('published_at <= ?', timestamp) }
scope :unpublished, ->(timestamp = Time.now) { where('published_at IS NULL OR published_at > ?', timestamp) }

```



## Domain Objects (No More Fat Models)


"Fat Model, Skinny Controller" is a very good first step, but it doesn't scale well once your codebase starts to grow.

Let's think on the [Single Responsibility](https://en.wikipedia.org/wiki/Single_responsibility_principle) of models. What is the single responsibility of models? Is it to hold business logic? Is it to hold non-response-related logic?

No. Its responsibility is to handle the persistence layer and its abstraction.

Business logic, as well as any non-response-related logic and non-persistence-related logic, should go in domain objects.

Domain objects are classes designed to have only one responsibility in the domain of the problem. Let your classes "[Scream Their Architecture](https://8thlight.com/blog/uncle-bob/2011/09/30/Screaming-Architecture.html)" for the problems they solve.

In practice, you should strive towards skinny models, skinny views and skinny controllers. The architecture of your solution shouldn't be influenced by the framework you're choosing.

**For example**

Let's say you're a marketplace which charges a fixed 15% commission to your customers via Stripe. If you charge a fixed 15% commission, that means that your commission changes depending on the order's amount because Stripe charges 2.9% + 30¢.

The amount you charge as commission should be: `amount*0.15 - (amount*0.029 + 0.30)`.

Don't write this logic in the model:

```ruby
# app/models/order.rb
class Order < ActiveRecord::Base
  SERVICE_COMMISSION = 0.15
  STRIPE_PERCENTAGE_COMMISSION = 0.029
  STRIPE_FIXED_COMMISSION = 0.30

  ...

  def commission
    amount*SERVICE_COMMISSION - stripe_commission  
  end

  private

  def stripe_commission
    amount*STRIPE_PERCENTAGE_COMMISSION + STRIPE_FIXED_COMMISSION
  end
end

```

As soon as you integrate with a new payment method, you won't be able to scale this functionality inside this model.

Also, as soon as you start to integrate more business logic, your `Order` object will start to lose [cohesion](https://thebojan.ninja/2015/04/08/high-cohesion-loose-coupling/).

Prefer domain objects, with the calculation of the commission completely abstracted from the responsibility of persisting orders:

```ruby
# app/models/order.rb
class Order < ActiveRecord::Base
  ...
  # No reference to commission calculation
end

# lib/commission.rb
class Commission
  SERVICE_COMMISSION = 0.15

  def self.calculate(payment_method, model)
    model.amount*SERVICE_COMMISSION - payment_commission(payment_method, model)  
  end

  private

  def self.payment_commission(payment_method, model)
    # There are better ways to implement a static registry,
    # this is only for illustration purposes.
    Object.const_get("#{payment_method}Commission").calculate(model)
  end
end

# lib/stripe_commission.rb
class StripeCommission
  STRIPE_PERCENTAGE_COMMISSION = 0.029
  STRIPE_FIXED_COMMISSION = 0.30

  def self.calculate(model)
    model.amount*STRIPE_PERCENTAGE_COMMISSION
      + STRIPE_PERCENTAGE_COMMISSION
  end
end

# app/controllers/orders_controller.rb
class OrdersController < ApplicationController
  def create
    @order = Order.new(order_params)
    @order.commission = Commission.calculate("Stripe", @order)
    ...
  end
end

```

Using domain objects has the following architectural advantages:

- it's extremely easy to unit test, as no fixtures or factories are required to instantiate the objects with the logic.
- works with everything that accepts the message `amount`.
- keeps each domain object small, with clearly defined responsibilities, and with higher cohesion.
- easily scales with new payment methods by [addition, not modification](https://en.wikipedia.org/wiki/Open/closed_principle).
- stops the tendency to have an ever-growing `User` object in each Ruby on Rails application.

I personally like to put domain objects in `lib`. If you do so, remember to add it to `autoload_paths`:

```ruby
# config/application.rb
config.autoload_paths << Rails.root.join('lib')

```

You may also prefer to create domain objects more action-oriented, following the Command/Query pattern. In such case, putting these objects in `app/commands` might be a better place as all `app` subdirectories are automatically added to the autoload path.



## Convention Over Configuration


In Rails, you find yourself looking at **controllers, views, and models** for your database.

To reduce the need for heavy configuration, Rails implements rules to ease up working with the application. You may define your own rules but for the beginning (and for later on) it's a good idea to stick to conventions that Rails offers.

These conventions will speed up development, keep your code concise and readable, and allow you an easy navigation inside your application.

Conventions also lower the barriers to entry for beginners. There are so many conventions in Rails that a beginner doesn’t even need to know about, but can just benefit from in ignorance. It’s possible to create great applications without knowing why everything is the way it is.

**For Example**

If you have a database table called `orders` with the primary key `id`, the matching model is called `order` and the controller that handles all the logic is named `orders_controller`. The view is split in different actions: if the controller has a `new` and `edit` action, there is also a `new` and `edit` view.

**For Example**

To create an app you simply run `rails new app_name`. This will generate roughly 70 files and folders that comprise the infrastructure and foundation for your Rails app.

It includes:

- Folders to hold your models (database layer), controllers, and views
- Folders to hold unit tests for your application
- Folders to hold your web assets like Javascript and CSS files
- Default files for HTTP 400 responses (i.e. file not found)
- Many others



## Beware of  default_scope


ActiveRecord includes `default_scope`, to automatically scope a model by default.

```ruby
class Post
  default_scope ->{ where(published: true).order(created_at: :desc) }
end

```

The above code will serve posts which are already published when you perform any query on the model.

```ruby
Post.all # will only list published posts 

```

That scope, while innocuous-looking, has multiple hidden side-effect that you may not want.

### `default_scope` and `order`

Since you declared an `order` in the `default_scope`, calling `order` on `Post` will be added as additional orders instead of overriding the default.

```ruby
Post.order(updated_at: :desc)

```

```ruby
SELECT "posts".* FROM "posts" WHERE "posts"."published" = 't' ORDER BY "posts"."created_at" DESC, "posts"."updated_at" DESC

```

This is probably not the behavior you wanted; you can override this by excluding the `order` from the scope first

```ruby
Post.except(:order).order(updated_at: :desc)

```

```ruby
SELECT "posts".* FROM "posts" WHERE "posts"."published" = 't' ORDER BY "posts"."updated_at" DESC

```

### `default_scope` and model initialization

As with any other `ActiveRecord::Relation`, `default_scope` will alter the default state of models initialized from it.

In the above example, `Post` has `where(published: true)` set by default, and so new models from `Post` will also have it set.

```ruby
Post.new # => <Post published: true>

```

### `unscoped`

`default_scope` can nominally be cleared by calling `unscoped` first, but this also has side-effects. Take, for example, an STI model:

```ruby
class Post < Document
  default_scope ->{ where(published: true).order(created_at: :desc) }
end

```

By default, queries against `Post` will be scoped to `type` columns containing `'Post'`. But `unscoped` will clear this along with your own `default_scope`, so if you use `unscoped` you have to remember to account for this as well.

```ruby
Post.unscoped.where(type: 'Post').order(updated_at: :desc)

```

### `unscoped` and Model Associations

Consider a relationship between `Post` and `User`

```ruby
class Post < ApplicationRecord
  belongs_to :user
  default_scope ->{ where(published: true).order(created_at: :desc) }
end

class User < ApplicationRecord
  has_many :posts
end

```

By getting an individual `User`, you can see the posts related to it:

```ruby
user = User.find(1)
user.posts

```

```ruby
SELECT "posts".* FROM "posts" WHERE "posts"."published" = 't' AND "posts"."user_id" = ? ORDER BY "posts"."created_at" DESC [["user_id", 1]]

```

But you want to clear the `default_scope` from the `posts` relation, so you use `unscoped`

```ruby
user.posts.unscoped

```

```ruby
SELECT "posts".* FROM "posts"

```

This wipes out the `user_id` condition as well as the `default_scope`.

### An example use-case for `default_scope`

Despite all of that, there are situations where using `default_scope` is justifiable.

Consider a multi-tenant system where multiple subdomains are served from the same application but with isolated data. One way to achieve this isolation is through `default_scope`. The downsides in other cases become upsides here.

```ruby
class ApplicationRecord < ActiveRecord::Base
  def self.inherited(subclass)
    super

    return unless subclass.superclass == self
    return unless subclass.column_names.include? 'tenant_id'

    subclass.class_eval do
      default_scope ->{ where(tenant_id: Tenant.current_id) }
    end
  end
end

```

All you need to do is set `Tenant.current_id` to something early in the request, and any table that contains `tenant_id` will automatically become scoped without any additional code. Instantiating records will automatically inherit the tenant id they were created under.

The important thing about this use-case is that the scope is set once per request, and it doesn't change. The only cases you will need `unscoped` here are special cases like background workers that run outside of a request scope.



## Don't Repeat Yourself (DRY)


To help to maintain clean code, Rails follows the principle of DRY.

It involves whenever possible, re-using as much code as possible rather than duplicating similar code in multiple places (for example, using partials). This reduces **errors**, keeps your code **clean** and enforces the principle of **writing code once** and then reusing it. It is also easier and more efficient to update code in one place than to update multiple parts of the same code. Thus making your code more modular and robust.

Also **Fat Model, Skinny Controller** is DRY, because you write the code in your model and in the controller only do the call, like:

```ruby
# Post model
scope :unpublished, ->(timestamp = Time.now) { where('published_at IS NULL OR published_at > ?', timestamp) } 


# Any controller
def index
    ....
    @unpublished_posts = Post.unpublished
    ....
end

def others
    ...
    @unpublished_posts = Post.unpublished
    ...
end

```

This also helps lead to an API driven structure where internal methods are hidden and changes are achieved through passing parameters in an API fashion.



## You Ain’t Gonna Need it (YAGNI)


If you can say “YAGNI” (You ain’t gonna need it) about a feature, you better not implement it. There can be a lot of development time saved through focussing onto simplicity.
Implementing such features anyway can lead to problems:

### Problems

### Overengineering

If a product is more complicated than it has to be, it is over engineered. Usually these “not yet used” features will never be used in the intended way they were written and have to be refactored if they ever get used.
Premature optimisations, especially performance optimisations, often lead to design decisions which will be proved wrong in the future.

### Code Bloat

Code Bloat means unnecessary complicated code. This can occur for example by abstraction, redundancy or incorrect application of design patterns. The code base becomes difficult to understand, confusing and expensive to maintain.

### Feature Creep

Feature Creep refers to adding new features that go beyond the core functionality of the product and lead to an unnecessarily high complexity of the product.

### Long development time

The time which could be used to develop necessary features is spent to develop unnecessary features. The product takes longer to deliver.

### Solutions

### KISS - Keep it simple, stupid

According to KISS, most systems work the best if they are designed simple. Simplicity should be a primary design goal to reduce complexity. It can be achieved by following the “Single Responsibility Principle” for example.

### YAGNI – You Ain’t Gonna Need it

Less is more. Think about every feature, is it really needed? If you can think of any way that it’s YAGNI, leave it away. It’s better to develop it when it’s needed.

### Continuous Refactoring

The product is being improved steadily. With refactoring, we can make sure that the product is being done according to best practice and does not degenerate to a patch work.

