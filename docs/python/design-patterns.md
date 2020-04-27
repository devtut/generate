---
metaTitle: Design Patterns
description: Introduction to design patterns and Singleton Pattern, Strategy Pattern, Proxy
---

# Design Patterns




## Introduction to design patterns and Singleton Pattern


Design Patterns provide solutions to the `commonly occurring problems` in software design. The design patterns were first introduced by `GoF(Gang of Four)` where they described the common patterns as problems which occur over and over again and solutions to those problems.

**Design patterns have four essential elements:**

1. `The pattern name` is a handle we can use to describe a design problem, its solutions, and consequences in a word or two.
1. `The problem` describes when to apply the pattern.
1. `The solution` describes the elements that make up the design, their relationships, responsibilities, and collaborations.
1. `The consequences` are the results and trade-offs of applying the pattern.

**Advantages of design patterns:**

1. They are reusable across multiple projects.
1. The architectural level of problems can be solved
1. They are time-tested and well-proven, which is the experience of developers and architects
1. They have reliability and dependence

**Design patterns can be classified into three categories:**

1. Creational Pattern
1. Structural Pattern
1. Behavioral Pattern

`Creational Pattern` - They are concerned with how the object can be created and they isolate the details of object creation.

`Structural Pattern` - They design the structure of classes and objects so that they can compose to achieve larger results.

`Behavioral Pattern` - They are concerned with interaction among objects and responsibility of objects.

**Singleton Pattern**:

It is a type of `creational pattern` which provides a mechanism to have only one and one object of a given type and provides a global point of access.

e.g. Singleton can be used in database operations, where we want database object to maintain data consistency.

**Implementation**

We can implement Singleton Pattern in Python by creating only one instance of Singleton class and serving the same object again.

```
class Singleton(object):
    def __new__(cls):
        # hasattr method checks if the class object an instance property or not.
        if not hasattr(cls, 'instance'):
            cls.instance = super(Singleton, cls).__new__(cls)
        return cls.instance

s = Singleton()
print ("Object created", s)

s1 = Singleton()
print ("Object2 created", s1)

```

**Output:**

```
('Object created', <__main__.Singleton object at 0x10a7cc310>)
('Object2 created', <__main__.Singleton object at 0x10a7cc310>)

```

Note that in languages like C++ or Java, this pattern is implemented by making the constructor private and creating a static method that does the object initialization. This way, one object gets created on the first call and class returns the same object thereafter. But in Python, we do not have any way to create private constructors.

**Factory Pattern**

Factory pattern is also a `Creational pattern`. The term `factory` means that a class is responsible for creating objects of other types. There is a class that acts as a factory which has objects and methods associated with it. The client creates an object by calling the methods with certain parameters and factory creates the object of the desired type and return it to the client.

```
from abc import ABCMeta, abstractmethod

class Music():
    __metaclass__ = ABCMeta
    @abstractmethod
    def do_play(self):
        pass

class Mp3(Music):
    def do_play(self):
        print ("Playing .mp3 music!")
    
class Ogg(Music):
    def do_play(self):
        print ("Playing .ogg music!")
    
class MusicFactory(object):
    def play_sound(self, object_type):
        return eval(object_type)().do_play()
    
if __name__ == "__main__":
    mf = MusicFactory()
    music = input("Which music you want to play Mp3 or Ogg")
    mf.play_sound(music)

```

**Output:**

```
Which music you want to play Mp3 or Ogg"Ogg"
Playing .ogg music!

```

`MusicFactory` is the factory class here that creates either an object of type `Mp3` or `Ogg` depending on the choice user provides.



## Strategy Pattern


This design pattern is called Strategy Pattern.
It is used to define a family of algorithms, encapsulates each
one, and make them interchangeable. Strategy
design pattern lets an algorithm vary independently
from clients that use it.

For example, animals can "walk" in many different ways. Walking could be considered a strategy that is implemented by different types of animals:

```
from types import MethodType


class Animal(object):
    
    def __init__(self, *args, **kwargs):
        self.name = kwargs.pop('name', None) or 'Animal'
        if kwargs.get('walk', None):
            self.walk = MethodType(kwargs.pop('walk'), self)

    def walk(self):
        """
        Cause animal instance to walk
        
        Walking funcionallity is a strategy, and is intended to
        be implemented separately by different types of animals.
        """
        message = '{} should implement a walk method'.format(
            self.__class__.__name__)
        raise NotImplementedError(message)


# Here are some different walking algorithms that can be used with Animal
def snake_walk(self):
    print('I am slithering side to side because I am a {}.'.format(self.name))

def four_legged_animal_walk(self):
    print('I am using all four of my legs to walk because I am a(n) {}.'.format(
        self.name))

def two_legged_animal_walk(self):
    print('I am standing up on my two legs to walk because I am a {}.'.format(
        self.name))

```

Running this example would produce the following output:

```
generic_animal = Animal()
king_cobra = Animal(name='King Cobra', walk=snake_walk)
elephant = Animal(name='Elephant', walk=four_legged_animal_walk)
kangaroo = Animal(name='Kangaroo', walk=two_legged_animal_walk)

kangaroo.walk()
elephant.walk()
king_cobra.walk()
# This one will Raise a NotImplementedError to let the programmer
# know that the walk method is intended to be used as a strategy.
generic_animal.walk()

    # OUTPUT:
    #
    # I am standing up on my two legs to walk because I am a Kangaroo.
    # I am using all four of my legs to walk because I am a(n) Elephant.
    # I am slithering side to side because I am a King Cobra.
    # Traceback (most recent call last):
    #   File "./strategy.py", line 56, in <module>
    #     generic_animal.walk()
    #   File "./strategy.py", line 30, in walk
    #     raise NotImplementedError(message)
    # NotImplementedError: Animal should implement a walk method 

```

Note that in languages like C++ or Java, this pattern is implemented using an abstract class or an interface to define a a strategy. In Python it makes more sense to just define some functions externally that can be added dynamically to a class using `types.MethodType`.



## Proxy


Proxy object is often used to ensure guarded access to another object, which internal business logic we don't want to pollute with safety requirements.

Suppose we'd like to guarantee that only user of specific permissions can access resource.

Proxy definition: (it ensure that only users which actually can see reservations will be able to consumer reservation_service)

```
from datetime import date
from operator import attrgetter

class Proxy:
    def __init__(self, current_user, reservation_service):
        self.current_user = current_user
        self.reservation_service = reservation_service

    def highest_total_price_reservations(self, date_from, date_to, reservations_count):
        if self.current_user.can_see_reservations:
            return self.reservation_service.highest_total_price_reservations(
                date_from,
                date_to,
                reservations_count
              )
        else:
            return []

#Models and ReservationService:

class Reservation:
    def __init__(self, date, total_price):
        self.date = date
        self.total_price = total_price

class ReservationService:
    def highest_total_price_reservations(self, date_from, date_to, reservations_count):
        # normally it would be read from database/external service
        reservations = [
            Reservation(date(2014, 5, 15), 100),
            Reservation(date(2017, 5, 15), 10),
            Reservation(date(2017, 1, 15), 50)
        ]

        filtered_reservations = [r for r in reservations if (date_from <= r.date <= date_to)]

        sorted_reservations = sorted(filtered_reservations, key=attrgetter('total_price'), reverse=True)

        return sorted_reservations[0:reservations_count]


class User:
    def __init__(self, can_see_reservations, name):
        self.can_see_reservations = can_see_reservations
        self.name = name

#Consumer service:

class StatsService:
    def __init__(self, reservation_service):
        self.reservation_service = reservation_service

    def year_top_100_reservations_average_total_price(self, year):
        reservations = self.reservation_service.highest_total_price_reservations(
            date(year, 1, 1),
            date(year, 12, 31),
            1
        )

        if len(reservations) > 0:
            total = sum(r.total_price for r in reservations)

            return total / len(reservations)
        else:
            return 0

#Test:
def test(user, year):
    reservations_service = Proxy(user, ReservationService())
    stats_service = StatsService(reservations_service)
    average_price = stats_service.year_top_100_reservations_average_total_price(year)
    print("{0} will see: {1}".format(user.name, average_price))

test(User(True, "John the Admin"), 2017)
test(User(False, "Guest"),         2017)

```

- **we're avoiding any changes in `ReservationService` when access restrictions are changed.**
- **we're not mixing business related data (`date_from`, `date_to`, `reservations_count`) with domain unrelated concepts (user permissions) in service.**
- **Consumer (`StatsService`) is free from permissions related logic as well**

- Proxy interface is always exactly the same as the object it hides, so that user that consumes service wrapped by proxy wasn't even aware of proxy presence.

