---
metaTitle: "Design Patterns"
description: "Method Chaining in PHP"
---

# Design Patterns


This topic provides examples of well known design patterns implemented in PHP.



## Method Chaining in PHP


Method Chaining is a technique explained in [Martin Fowler's book **Domain Specific Languages**](http://rads.stackoverflow.com/amzn/click/0321712943). Method Chaining is summarized as

> 
[**Makes modifier methods return the host object, so that multiple modifiers can be invoked in a single expression**](http://martinfowler.com/dslCatalog/methodChaining.html).


Consider this non-chaining/regular piece of code (ported to PHP from the aforementioned book)

```
$hardDrive = new HardDrive;
$hardDrive->setCapacity(150);
$hardDrive->external();
$hardDrive->setSpeed(7200);

```

Method Chaining would allow you to write the above statements in a more compact way:

```
$hardDrive = (new HardDrive)
    ->setCapacity(150)
    ->external()
    ->setSpeed(7200);

```

All you need to do for this to work is to `return $this` in the methods you want to chain from:

```
class HardDrive {
    protected $isExternal = false;
    protected $capacity = 0;
    protected $speed = 0;

    public function external($isExternal = true) {
        $this->isExternal = $isExternal;        
        return $this; // returns the current class instance to allow method chaining
    }

    public function setCapacity($capacity) {
        $this->capacity = $capacity;        
        return $this; // returns the current class instance to allow method chaining
    }

    public function setSpeed($speed) {
        $this->speed = $speed;        
        return $this; // returns the current class instance to allow method chaining
    }
}

```

### When to use it

The primary use cases for utilizing Method Chaining is when building internal Domain Specific Languages. Method Chaining is **a building block** in [Expression Builders](http://martinfowler.com/bliki/ExpressionBuilder.html) and [Fluent Interfaces](http://martinfowler.com/bliki/FluentInterface.html). [It is not synonymous with those, though](http://stackoverflow.com/a/17940086/208809). Method Chaining merely enables those. Quoting Fowler:

> 
I've also noticed a common misconception - many people seem to equate fluent interfaces with Method Chaining. Certainly chaining is a common technique to use with fluent interfaces, but true fluency is much more than that.


With that said, using Method Chaining just for the sake of avoiding writing the host object is considered a [code smell](http://martinfowler.com/bliki/CodeSmell.html) by many. It makes for unobvious APIs, especially when mixing with non-chaining APIs.

### Additional Notes

### Command Query Separation

[Command Query Separation is a design principle brought forth by Bertrand Meyer](http://martinfowler.com/bliki/CommandQuerySeparation.html). It states that methods mutating state (**commands**) should not return anything, whereas methods returning something (**queries**) should not mutate state. This makes it easier to reason about the system. Method Chaining violates this principle because we are mutating state **and** returning something.

### Getters

When making use of classes which implement method chaining, pay particular attention when calling getter methods (that is, methods which return something other than `$this`). Since getters must return a value other than `$this`, chaining an additional method onto a getter makes the call operate on the **gotten** value, not on the original object. While there are some use cases for chained getters, they may make code less readable.

### Law of Demeter and impact on testing

Method Chaining as presented above does not violate [Law of Demeter](https://en.wikipedia.org/wiki/Law_of_Demeter). Nor does it impact testing. That is because we are returning the host instance and not some collaborator. It's a common misconception stemming from people confusing mere Method Chaining with **Fluent Interfaces** and **Expression Builders**. It is only when Method Chaining returns **other objects than the host object** that you violate Law of Demeter and end up with Mock fests in your tests.

