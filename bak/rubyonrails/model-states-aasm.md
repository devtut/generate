---
metaTitle: "Ruby on Rails - Model states: AASM"
description: "Basic state with AASM"
---

# Model states: AASM



## Basic state with AASM


Usually you'll end up creating models which will contain a state, and that state will be changing during the lifespan of the object.

[AASM](https://github.com/aasm/aasm) is a finite state machine enabler library that can help you out with dealing with having an easy passing through the process design of your objects.

Having something like this in your model goes pretty aligned with the [Fat Model, Skinny Controller](http://stackoverflow.com/documentation/ruby-on-rails/1207/rails-best-practices/9609/fat-model-skinny-controller#t=201611091524540826623) idea, one of Rails best practices. The model is the sole responsible of managing its state, its changes and of generating the events triggered by those changes.

To install, in Gemfile

```ruby
gem 'aasm'

```

Consider an App where the user Quotes a product for a price.

```ruby
class Quote

  include AASM

  aasm do
    state :requested, initial: true  # User sees a product and requests a quote
    state :priced                    # Seller sets the price 
    state :payed                     # Buyer pays the price
    state :canceled                  # The buyer is not willing to pay the price
    state :completed                 # The product has been delivered.

    event :price do
        transitions from: requested, to: :priced
    end

    event :pay do
        transitions from: :priced, to: :payed, success: :set_payment_date
    end

    event :complete do
        transitions from: :payed, to: :completed, guard: product_delivered?
    end

    event :cancel do
        transitions from: [:requested, :priced], to: :canceled
        transitions from: :payed, to: canceled, success: :reverse_charges
    end

   
  end

  private

  def set_payment_date
    update payed_at: Time.zone.now
  end
end

```

The Quote class' states can go however it's best for your process.

You can think of the states as being past, like in the previous example or algo in other tense, for example: pricing, paying, delivering, etc. The naming of the states depends on you. From a personal point a view, past states work better because your end state will surely be a past action and links up better with the event names, which will be explained later.

**NOTE:** Be careful what names you use, you have to worry about not using Ruby or Ruby on Rails reserved keywords, like `valid`, `end`, `being`, etc.

Having defined the states and transitions we can now access some methods created by AASM.

For example:

```ruby
Quote.priced  # Shows all Quotes with priced events
quote.priced? # Indicates if that specific quote has been priced
quote.price!  # Triggers the event the would transition from requested to priced.

```

As you can see the event has transitions, this transitions determine the way the state will change upon the event call. If the event is invalid due to the current state an Error will be raised.

The events and transitions also have some other callbacks, for example

```ruby
guard: product_delivered?

```

Will call the `product_delivered?` method which will return a boolean. If it turns out false, the transition will not be applied and if the no other transitions are available, the state won't change.

```ruby
success: :reverse_charges

```

If that translation successfully happens the `:reverse_charges` method will be invoked.

There are several other methods in AASM with more callbacks in the process but this will help you creating your first models with finite states.

