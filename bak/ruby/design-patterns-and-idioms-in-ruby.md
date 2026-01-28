---
metaTitle: "Ruby - Design Patterns and Idioms in Ruby"
description: "Decorator Pattern, Observer, Singleton, Proxy"
---

# Design Patterns and Idioms in Ruby



## Decorator Pattern


Decorator pattern adds behavior to objects without affecting other objects of the same class. The decorator pattern is a useful alternative to creating sub-classes.

Create a module for each decorator. This approach is more flexible than inheritance because you can mix and match responsibilities in more combinations. Additionally, because the transparency allows decorators to be nested recursively, it allows for an unlimited number of responsibilities.

Assume the Pizza class has a cost method that returns 300:

```ruby
class Pizza
  def cost
    300
  end
end

```

Represent pizza with an added layer of cheese burst and the cost goes up by 50. The simplest approach is to create a `PizzaWithCheese` subclass that returns 350 in the cost method.

```ruby
class PizzaWithCheese < Pizza
  def cost
    350
  end
end

```

Next, we need to represent a large pizza that adds 100 to the cost of a normal pizza. We can represent this using a LargePizza subclass of Pizza.

```ruby
class LargePizza < Pizza
  def cost
    400
  end
end

```

We could also have an ExtraLargePizza which adds a further cost of 15 to our LargePizza. If we were to consider that these pizza types could be served with cheese, we would need to add LargePizzaWithChese and ExtraLargePizzaWithCheese subclasses.we end up with a total of 6 classes.

To simplify the approach, use modules to dynamically add behavior to Pizza class:

Module + extend + super decorator:->

```ruby
class Pizza
  def cost
    300
  end
end

module CheesePizza
  def cost
    super + 50
  end
end

module LargePizza
  def cost
    super + 100
  end
end

pizza = Pizza.new         #=> cost = 300
pizza.extend(CheesePizza) #=> cost = 350
pizza.extend(LargePizza)  #=> cost = 450
pizza.cost                #=> cost = 450

```



## Observer


The observer pattern is a software design pattern in which an object (called `subject`) maintains a list of its dependents (called `observers`), and notifies them automatically of any state changes, usually by calling one of their methods.

Ruby provides a simple mechanism to implement the Observer design pattern. The module `Observable` provides the logic to notify the subscriber of any changes in the Observable object.

For this to work, the observable has to assert it has changed and notify the observers.

Objects observing have to implement an `update()` method, which will be the callback for the Observer.

Let's implement a small chat, where users can subscribe to users and when one of them write something, the subscribers get notified.

```ruby
require "observer"

class Moderator
  include Observable

  def initialize(name)
    @name = name
  end

  def write
    message = "Computer says: No"
    changed
    notify_observers(message)
  end
end

class Warner
  def initialize(moderator, limit)
    @limit = limit
    moderator.add_observer(self)
  end
end

class Subscriber < Warner
  def update(message)
    puts "#{message}"
  end
end

moderator = Moderator.new("Rupert")
Subscriber.new(moderator, 1)
moderator.write
moderator.write

```

Producing the following output:

```ruby
# Computer says: No
# Computer says: No

```

We've triggered the method `write` at the Moderator class twice, notifying its subscribers, in this case just one.

The more subscribers we add the more the changes will propagate.



## Singleton


Ruby Standard Library has a Singleton module which implements the Singleton pattern. The first step in creating a Singleton class is to require and include the **`Singleton`** module in a class:

```ruby
require 'singleton'

class Logger
  include Singleton
end

```

If you try to instantiate this class as you normally would a regular class, a `NoMethodError` exception is raised. The constructor is made private to prevent other instances from being accidentally created:

```ruby
Logger.new

#=> NoMethodError: private method `new' called for AppConfig:Class    

```

To access the instance of this class, we need to use the `instance()`:

```ruby
first, second = Logger.instance, Logger.instance
first == second

#=> true

```

**Logger example**

```ruby
require 'singleton'


class Logger
  include Singleton

  def initialize
    @log = File.open("log.txt", "a")
  end

  def log(msg)
    @log.puts(msg)
  end
end

```

In order to use `Logger` object:

```ruby
Logger.instance.log('message 2')

```

**Without Singleton include**

The above singleton implementations can also be done without the inclusion of the Singleton module. This can be achieved with the following:

```ruby
class Logger
  def self.instance
    @instance ||= new
  end
end

```

which is a shorthand notation for the following:

```ruby
class Logger
  def self.instance
    @instance = @instance || Logger.new
  end
end

```

However, keep in mind that the Singleton module is tested and optimized, therefore being the better option to implement your singleton with.



## Proxy


Proxy object is often used to ensure guarded access to another object, which internal business logic we don't want to pollute with safety requirements.

Suppose we'd like to guarantee that only user of specific permissions can access resource.

Proxy definition: (it ensure that only users which actually can see reservations will be able to consumer reservation_service)

```ruby
class Proxy
  def initialize(current_user, reservation_service)
    @current_user = current_user
    @reservation_service = reservation_service
  end

  def highest_total_price_reservations(date_from, date_to, reservations_count)
    if @current_user.can_see_reservations?
      @reservation_service.highest_total_price_reservations(
        date_from, 
        date_to, 
        reservations_count
      )
    else
      []
    end
  end 
end

```

Models and ReservationService:

```ruby
class Reservation
  attr_reader :total_price, :date

  def initialize(date, total_price)
    @date = date
    @total_price = total_price
  end
end

class ReservationService
  def highest_total_price_reservations(date_from, date_to, reservations_count)
    # normally it would be read from database/external service
    reservations = [
      Reservation.new(Date.new(2014, 5, 15), 100),
      Reservation.new(Date.new(2017, 5, 15), 10),          
      Reservation.new(Date.new(2017, 1, 15), 50)
    ]

    filtered_reservations = reservations.select do |reservation|
      reservation.date.between?(date_from, date_to) 
    end

    filtered_reservations.take(reservations_count)
  end
end        

class User
  attr_reader :name

  def initialize(can_see_reservations, name)
    @can_see_reservations = can_see_reservations
    @name = name
  end

  def can_see_reservations?
    @can_see_reservations
  end
end

```

Consumer service:

```ruby
class StatsService
  def initialize(reservation_service)
    @reservation_service = reservation_service
  end

  def year_top_100_reservations_average_total_price(year)
    reservations = @reservation_service.highest_total_price_reservations(
      Date.new(year, 1, 1),
      Date.new(year, 12, 31),
      100
    )

    if reservations.length > 0
      sum = reservations.reduce(0) do |memo, reservation| 
        memo + reservation.total_price
      end

      sum / reservations.length
    else
      0
    end
  end
end

```

Test:

```ruby
def test(user, year)
  reservations_service = Proxy.new(user, ReservationService.new)
  stats_service = StatsService.new(reservations_service)
  average_price = stats_service.year_top_100_reservations_average_total_price(year)
  puts "#{user.name} will see: #{average_price}"
end

test(User.new(true, "John the Admin"), 2017)
test(User.new(false, "Guest"),         2017)

```


- **we're avoiding any changes in `ReservationService` when access restrictions are changed.**
- **we're not mixing business related data (`date_from`, `date_to`, `reservations_count`) with domain unrelated concepts (user permissions) in service.**
- **Consumer (`StatsService`) is free from permissions related logic as well**

- Proxy interface is always exactly the same as the object it hides, so that user that consumes service wrapped by proxy wasn't even aware of proxy presence.



#### Remarks


