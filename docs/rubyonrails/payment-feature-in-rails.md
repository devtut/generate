---
metaTitle: "Ruby on Rails - Payment feature in rails"
description: "How to integrate with Stripe"
---

# Payment feature in rails


This document pretend to introduce you, with a complete example, how you can implement different payment methods with Ruby on Rails.

In the example, we will cover Stripe and Braintree two very well-known payment platforms.



## How to integrate with Stripe


Add Stripe gem to our `Gemfile`

```ruby
gem 'stripe'

```

Add `initializers/stripe.rb` file. This file contains the necessary keys for connecting with your stripe account.

```ruby
require 'require_all'

Rails.configuration.stripe = {
    :publishable_key => ENV['STRIPE_PUBLISHABLE_KEY'],
    :secret_key      => ENV['STRIPE_SECRET_KEY']
}

Stripe.api_key = Rails.configuration.stripe[:secret_key]

```

### How to create a new customer to Stripe

```ruby
Stripe::Customer.create({email: email, source: payment_token})

```

This code creates a new customer on Stripe with given email address and source.

`payment_token` is the token given from the client-side that contains a payment method like a credit card or bank account.
More info: [Stripe.js client-side](https://stripe.com/docs/stripe.js)

### How to retrieve a plan from Stripe

```ruby
Stripe::Plan.retrieve(stripe_plan_id)

```

This code retrieves a plan from Stripe by its id.

### How to create a subscription

When we have a customer and a plan we can create a new subscription on Stripe.

```ruby
Stripe::Subscription.create(customer: customer.id, plan: plan.id)

```

It will create a new subscription and will charge our User.
It's important to know what really happens on Stripe when we subscribe a user to a plan, you will find more info here: [Stripe Subscription lifecycle](https://stripe.com/docs/subscriptions/lifecycle).

### How to charge a user with a single payment

Sometimes we want to charge our users just a single time, for do that we will do the next.

```ruby
Stripe::Charge.create(amount:   amount, customer: customer, currency: currency)

```

In that case, we are charging our user one time for given amount.

Common errors:

<li>
The amount must be sent in integer form, that means, 2000 will be 20 units of currency. [Check this example](https://stripe.com/docs/charges)
</li>
<li>
You cannot charge a user in two currencies. If the user was charged in EUR at any moment in the past you cannot charge the user in USD.
</li>
<li>
You cannot charge user without source (payment method).
</li>



#### Remarks


Documentation.

[Stripe](https://stripe.com/docs/api/ruby)

[Braintree](https://developers.braintreepayments.com/reference/overview)

