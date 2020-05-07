---
metaTitle: "Ruby on Rails - Class Organization"
description: "Service Class, Model Class"
---

# Class Organization



## Service Class


Controller is an entry point to our application. However, it’s not the only possible entry point. I would like to have my logic accessible from:

- Rake tasks
- background jobs
- console
- tests

If I throw my logic into a controller it won’t be accessible from all these places. So let’s try “skinny controller, fat model” approach and move the logic to a model. But which one? If a given piece of logic involves `User`, `Cart` and `Product` models – where should it live?

A class which inherits from `ActiveRecord::Base` already has a lot of responsibilities. It handles query interface, associations and validations. If you add even more code to your model it will quickly become an unmaintainable mess with hundreds of public methods.

A service is just a regular Ruby object. Its class does not have to inherit from any specific class. Its name is a verb phrase, for example `CreateUserAccount` rather than `UserCreation` or `UserCreationService`. It lives in app/services directory. You have to create this directory by yourself, but Rails will autoload classes inside for you.

**A service object does one thing**

A service object (aka method object) performs one action. It holds the business logic to perform that action. Here is an example:

```ruby
# app/services/accept_invite.rb
class AcceptInvite
  def self.call(invite, user)
    invite.accept!(user)
    UserMailer.invite_accepted(invite).deliver
  end
end

```

The three conventions I follow are:

Services go under the `app/services directory`. I encourage you to use subdirectories for business logic-heavy domains. For instance:

- The file `app/services/invite/accept.rb` will define `Invite::Accept` while `app/services/invite/create.rb` will define `Invite::Create`
- Services start with a verb (and do not end with Service): `ApproveTransaction`, `SendTestNewsletter`, `ImportUsersFromCsv`
- Services respond to the `call` method. I found using another verb makes it a bit redundant: `ApproveTransaction.approve()` does not read well. Also, the `call` method is the de facto method for `lambda`, `procs`, and method objects.

**Benefits**

****Service objects show what my application does****

I can just glance over the services directory to see what my application does: `ApproveTransaction`, `CancelTransaction`, `BlockAccount`, `SendTransactionApprovalReminder`…

A quick look into a service object and I know what business logic is involved. I don’t have to go through the controllers, `ActiveRecord` model callbacks and observers to understand what “approving a transaction” involves.

****Clean-up models and controllers****

Controllers turn the request (params, session, cookies) into arguments, pass them down to the service and redirect or render according to the service response.

```ruby
class InviteController < ApplicationController
 def accept
    invite = Invite.find_by_token!(params[:token])
    if AcceptInvite.call(invite, current_user)
      redirect_to invite.item, notice: "Welcome!"
    else
      redirect_to '/', alert: "Oopsy!"
    end
  end
end

```

Models only deal with associations, scopes, validations and persistence.

```ruby
class Invite < ActiveRecord::Base
  def accept!(user, time=Time.now)
    update_attributes!(
      accepted_by_user_id: user.id,
      accepted_at: time
    )
  end
end

```

This makes models and controllers much easier to test and maintain!

**When to use Service Class**

Reach for Service Objects when an action meets one or more of these criteria:

- The action is complex (e.g. closing the books at the end of an accounting period)
- The action reaches across multiple models (e.g. an e-commerce purchase using Order, CreditCard and Customer objects)
- The action interacts with an external service (e.g. posting to social networks)
- The action is not a core concern of the underlying model (e.g. sweeping up outdated data after a certain time period).
- There are multiple ways of performing the action (e.g. authenticating with an access token or password).

**Sources**

[Adam Niedzielski Blog](http://blog.sundaycoding.com/blog/2014/11/25/my-take-on-services-in-rails/)

[Brew House Blog](http://brewhouse.io/blog/2014/04/30/gourmet-service-objects.html)

[Code Climate Blog](http://blog.codeclimate.com/blog/2012/10/17/7-ways-to-decompose-fat-activerecord-models/)



## Model Class


```ruby
class Post < ActiveRecord::Base
  belongs_to :user
  has_many :comments

  validates :user, presence: true
  validates :title, presence: true, length: { in: 6..40 }

  scope :topic, -> (topic) { joins(:topics).where(topic: topic) }

  before_save :update_slug
  after_create :send_welcome_email

  def publish!
    update(published_at: Time.now, published: true)
  end

  def self.find_by_slug(slug)
    find_by(slug: slug)
  end

  private

  def update_slug
    self.slug = title.join('-')
  end

  def send_welcome_email
    WelcomeMailer.welcome(self).deliver_now
  end
end

```

Models are typically responsible for:

- setting up relationships
- validating data
- providing access to data via scopes and methods
- Performing actions around persistence of data.

At the highest level, models describe domain concepts and manages their persistence.



#### Remarks


This seems like a simple thing to do but when you're classes start ballooning in size you'll be thankful you took the time to organize them.

